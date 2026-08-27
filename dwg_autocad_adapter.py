# -*- coding: utf-8 -*-
"""Headless DWG text replacement through an installed AutoCAD host.

This Python wrapper is original integration code. It invokes the separate
TextReplacer.BFind.lsp adapter, which is a modified Lee Mac BFind derivative.
Do not publicly redistribute that LISP file without the required permission.
"""

from dataclasses import dataclass, field
import os
from pathlib import Path
import shutil
import subprocess
import tempfile
from typing import Iterable


@dataclass
class DwgReplaceResult:
    path: str
    kind: str = "autocad"
    count: int = 0
    changes: list[str] = field(default_factory=list)
    error: str | None = None


def find_autocad_hosts(year_from=2020, year_to=2027):
    """Find full AutoCAD and Core Console, preferring newer versions."""
    override = os.environ.get("TEXTREPLACER_AUTOCAD_HOST")
    if override:
        candidate = Path(override)
        return [candidate] if candidate.is_file() else []

    roots = []
    for name in ("ProgramFiles", "ProgramW6432"):
        value = os.environ.get(name)
        if value:
            root = Path(value) / "Autodesk"
            if root not in roots:
                roots.append(root)

    found = []
    for root in roots:
        if not root.is_dir():
            continue
        for year in range(year_to, year_from - 1, -1):
            install_dir = root / f"AutoCAD {year}"
            # Full AutoCAD provides the ActiveX model needed by some tables.
            # Core Console is a faster compatibility fallback.
            for executable in ("acad.exe", "accoreconsole.exe"):
                candidate = install_dir / executable
                if candidate.is_file() and candidate not in found:
                    found.append(candidate)
    return found


def _host_command(host, drawing_path, script_path):
    if host.name.lower() == "accoreconsole.exe":
        return [
            str(host),
            "/i", str(drawing_path),
            "/s", str(script_path),
            "/l", "en-US",
        ]
    return [
        str(host),
        str(drawing_path),
        "/b", str(script_path),
        "/nologo",
    ]


def _decode_console(data):
    if data.startswith(b"\xff\xfe"):
        return data.decode("utf-16", errors="replace")
    if data and data[:200].count(b"\x00") > 20:
        return data.decode("utf-16-le", errors="replace")
    for encoding in ("utf-8", "cp1251", "cp866"):
        try:
            return data.decode(encoding)
        except UnicodeDecodeError:
            pass
    return data.decode("utf-8", errors="replace")


def _has_dwg_header(path):
    try:
        with Path(path).open("rb") as stream:
            return stream.read(4) == b"AC10"
    except OSError:
        return False


def _unicode_escape(value):
    r"""Encode non-ASCII BMP characters in AutoCAD literal \U+XXXX form."""
    output = []
    for char in str(value):
        codepoint = ord(char)
        if 0x80 <= codepoint <= 0xFFFF:
            output.append(f"\\U+{codepoint:04X}")
        else:
            output.append(char)
    return "".join(output)


def _startup_settings():
    startupinfo = None
    if hasattr(subprocess, "STARTUPINFO"):
        startupinfo = subprocess.STARTUPINFO()
        startupinfo.dwFlags |= getattr(subprocess, "STARTF_USESHOWWINDOW", 1)
        startupinfo.wShowWindow = getattr(subprocess, "SW_HIDE", 0)
    return startupinfo, getattr(subprocess, "CREATE_NO_WINDOW", 0)


def _run_pair(host, drawing_path, adapter_path, find, replace, timeout):
    with tempfile.TemporaryDirectory(prefix="textreplacer_acad_") as tmp_name:
        tmp_dir = Path(tmp_name)
        script_path = tmp_dir / "run.scr"
        result_path = tmp_dir / "result.txt"

        adapter_ref = str(adapter_path).replace("\\", "/").replace('"', '\\"')
        script_path.write_text(
            '(setvar "SECURELOAD" 0)\n'
            f'(load "{adapter_ref}")\n'
            'TR_BFIND\n'
            '_.QSAVE\n'
            'TR_BFIND_DONE\n'
            '_.QUIT\n',
            encoding="ascii",
        )

        env = os.environ.copy()
        env["TEXTREPLACER_FIND"] = str(find)
        env["TEXTREPLACER_REPLACE"] = str(replace)
        env["TEXTREPLACER_FIND_ESCAPED"] = _unicode_escape(find)
        # Escaped source text is intentionally normalized to readable Unicode.
        env["TEXTREPLACER_REPLACE_ESCAPED"] = str(replace)
        env["TEXTREPLACER_RESULT"] = str(result_path)

        startupinfo, creationflags = _startup_settings()
        completed = subprocess.run(
            _host_command(host, drawing_path, script_path),
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            env=env,
            timeout=timeout,
            creationflags=creationflags,
            startupinfo=startupinfo,
        )

        status = (
            _decode_console(result_path.read_bytes()).strip()
            if result_path.exists() else ""
        )
        if not status.startswith("OK:"):
            details = (
                status
                or _decode_console(completed.stderr[-2000:])
                or _decode_console(completed.stdout[-2000:])
            )
            raise RuntimeError(
                details.strip() or f"AutoCAD exit code {completed.returncode}"
            )
        if completed.returncode != 0:
            raise RuntimeError(f"AutoCAD exit code {completed.returncode}")

        parts = status.split(":")
        count = int(parts[1])
        linked_skipped = int(parts[6]) if len(parts) > 6 else 0
        return count, linked_skipped


def replace_many_in_dwg(
    path,
    replacements: Iterable[tuple[str, str]],
    adapter_path=None,
    timeout=600,
):
    """Apply ordered replacement pairs to a checked same-directory DWG copy."""
    path = Path(path)
    result = DwgReplaceResult(str(path))
    replacements = list(replacements)

    if not path.is_file():
        result.error = f"DWG file not found: {path}"
        return result
    if not replacements:
        return result

    if adapter_path is None:
        adapter_path = Path(__file__).with_name("TextReplacer.BFind.lsp")
    else:
        adapter_path = Path(adapter_path)
    if not adapter_path.is_file():
        result.error = f"AutoLISP adapter not found: {adapter_path}"
        return result

    hosts = find_autocad_hosts()
    if not hosts:
        result.error = "AutoCAD 2020-2027 is not installed or was not found"
        return result

    errors = []
    for host in hosts:
        work_path = None
        try:
            handle, work_name = tempfile.mkstemp(
                prefix=f".{path.stem}.textreplacer_",
                suffix=".dwg",
                dir=str(path.parent),
            )
            os.close(handle)
            work_path = Path(work_name)
            shutil.copy2(path, work_path)

            total = 0
            linked_total = 0
            messages = []
            for find, replace in replacements:
                count, linked = _run_pair(
                    host, work_path, adapter_path, find, replace, timeout
                )
                total += count
                linked_total += linked
                if count:
                    messages.append(
                        f"DWG ({host.parent.name}): "
                        f"«{find}» → «{replace}» ({count} шт.)"
                    )

            if total == 0:
                work_path.unlink(missing_ok=True)
                work_path = None
                if linked_total:
                    result.error = (
                        "Matches were found only in an Excel-linked table "
                        f"({linked_total}). Edit the source workbook and "
                        "refresh the AutoCAD data link."
                    )
                return result

            if work_path.stat().st_size < 64 or not _has_dwg_header(work_path):
                raise RuntimeError("AutoCAD produced an invalid DWG file")

            os.replace(work_path, path)
            work_path = None
            result.count = total
            result.changes.extend(messages)
            if linked_total:
                result.changes.append(
                    "Excel-linked cells skipped: "
                    f"{linked_total}; edit the source workbook."
                )
            return result
        except Exception as exc:
            errors.append(f"{host.parent.name}: {exc}")
        finally:
            if work_path is not None:
                work_path.unlink(missing_ok=True)

    result.error = "DWG (AutoCAD): " + "; ".join(errors)
    return result


def replace_in_dwg(path, find, replace):
    """Drop-in adapter for Text-Replacer's public three-argument API."""
    return replace_many_in_dwg(path, [(find, replace)])
