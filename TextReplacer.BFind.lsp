;; TextReplacer.BFind.lsp
;; Non-interactive adapter for TextReplacer.
;;
;; The string replacement engine and object traversal are adapted from:
;;   Batch Find & Replace (BFind) v2.0
;;   Author: Lee Mac, Copyright (c) 2011 - www.lee-mac.com
;;   https://www.lee-mac.com/bfind.html
;;
;; This file is a MODIFIED derivative for private/internal use.  The original
;; copyright notice is retained as required by Lee Mac's terms of use:
;;   https://www.lee-mac.com/terms.html
;;
;; Modifications:
;; - removed the DCL user interface and directory processing;
;; - accepts one find/replace pair through environment variables;
;; - processes the active Core Console drawing and exposes two commands;
;; - reports a machine-readable replacement count after QSAVE;
;; - prefixes all symbols to avoid collisions with an installed BFind.lsp.

(vl-load-com)

(setq tr:bfind-rex nil
      tr:bfind-count 0
      tr:bfind-scanned 0
      tr:bfind-text-objects 0
      tr:bfind-rawhits 0
      tr:bfind-skipped 0
      tr:bfind-linked-skipped 0
      tr:bfind-find-escaped nil
      tr:bfind-replacement-escaped nil
      tr:bfind-status nil)

(defun tr:bfind-release (obj)
  (if (and obj (= 'VLA-OBJECT (type obj)) (not (vlax-object-released-p obj)))
    (vl-catch-all-apply 'vlax-release-object (list obj)))
  (princ))

(defun tr:bfind-write-result (value / path stream)
  (setq path (getenv "TEXTREPLACER_RESULT"))
  (if (and path (/= "" path) (setq stream (open path "w")))
    (progn (write-line value stream) (close stream)))
  (princ))

(defun tr:bfind-regex-replace (newstr pat string)
  (vlax-put tr:bfind-rex 'Pattern pat)
  (vlax-put tr:bfind-rex 'Global :vlax-true)
  (vlax-put tr:bfind-rex 'IgnoreCase :vlax-false)
  (vlax-invoke tr:bfind-rex 'Replace string newstr))

(defun tr:bfind-regex-execute (pat string matchcase / item result)
  (vlax-put tr:bfind-rex 'Pattern pat)
  (vlax-put tr:bfind-rex 'Global :vlax-true)
  (vlax-put tr:bfind-rex 'IgnoreCase (if matchcase :vlax-false :vlax-true))
  (vlax-for item (vlax-invoke tr:bfind-rex 'Execute string)
    (setq result
      (cons (list (vlax-get item 'Value) (vlax-get item 'FirstIndex)) result)))
  result)

;; Adapted from Lee Mac BFind v2.0 _ReplaceText.  Formatting control codes are
;; hidden while matching, so a visible word split by MText formatting can still
;; be found without destroying the formatting itself.
(defun tr:bfind-replace-text
  (newstr oldstr str matchcase whole /
   origstr oldlst lst offset nlst res xstr replst origlen
   slashflag lbrace rbrace pattern)

  (setq slashflag (strcat "<" (substr (rtos (getvar "CDATE") 2 8) 14) ">")
        str (tr:bfind-regex-replace slashflag "\\\\\\\\" str)
        lbrace (strcat "<L" (substr (rtos (getvar "CDATE") 2 8) 14) ">")
        str (tr:bfind-regex-replace lbrace "\\\\{" str)
        rbrace (strcat "<" (substr (rtos (getvar "CDATE") 2 8) 14) "R>")
        str (tr:bfind-regex-replace rbrace "\\\\}" str)
        str (tr:bfind-regex-replace "\\P" "\\n" str)
        origstr str
        nlst (vl-string->list oldstr))

  (foreach x (cdr (reverse nlst))
    (setq oldlst (cons x (cons 208 (cons 42 oldlst)))))
  (setq oldlst (append oldlst (list (last nlst)))
        oldstr (vl-list->string oldlst))

  (setq lst (tr:bfind-regex-execute "\\\\Q[-]?[0-9]*?[.]?[0-9]+;" str T)
        lst (append lst (tr:bfind-regex-execute "\\\\H[0-9]?[.]?[0-9]+x;" str T))
        lst (append lst (tr:bfind-regex-execute "\\\\[Cc][0-9]?[.]?[0-9]+;" str T))
        lst (append lst (tr:bfind-regex-execute "\\\\A[012];" str T))
        lst (append lst (tr:bfind-regex-execute "\\\\[Ff].*?;" str T))
        lst (append lst (tr:bfind-regex-execute "\\\\[Oo]" str T))
        lst (append lst (tr:bfind-regex-execute "\\\\[Ll]" str T))
        lst (append lst (tr:bfind-regex-execute "\\\\W[0-9]?[.]?[0-9]+;" str T))
        lst (append lst (tr:bfind-regex-execute "\\\\T[0-9]?[.]?[0-9]+;" str T))
        lst (append lst (tr:bfind-regex-execute "\\\\S.*?;" str T))
        lst (append lst (tr:bfind-regex-execute "\\\\P" str T))
        lst (append lst (tr:bfind-regex-execute "\\\\p.*?;" str T))
        lst (append lst (tr:bfind-regex-execute "\\\\P\\t|[0-9]+;\\t|\\t" str T))
        lst (append lst (tr:bfind-regex-execute "%%[uUoO]" str T)))

  (foreach x lst
    (setq str
      (vl-string-subst
        (tr:bfind-regex-replace (chr 208) "(.)" (car x))
        (car x) str (cadr x))))
  (setq str (tr:bfind-regex-replace (chr 208) "{|}" str))

  (foreach pattern
    '("\\\\" "\\^" "\\$" "\\+" "\\?" "\\." "\\(" "\\)"
      "\\|" "\\{" "\\}" "\\," "\\[" "\\]")
    (setq oldstr (tr:bfind-regex-replace pattern pattern oldstr)))
  (setq oldstr
    (tr:bfind-regex-replace
      "$1\\*" (strcat "([^" (chr 208) "]|^)\\*") oldstr))

  (foreach x
    (reverse
      (tr:bfind-regex-execute
        (if whole (strcat "\\b" oldstr "\\b") oldstr)
        str matchcase))
    (setq xstr (substr origstr (1+ (cadr x)) (strlen (car x)))
          replst (cons (list xstr (cadr x)) replst)))

  (setq origlen (strlen origstr)
        offset 0)
  (foreach x (reverse replst)
    (setq res (vl-string-subst newstr (car x) origstr (+ offset (cadr x)))
          origstr res
          offset (- (strlen res) origlen)
          tr:bfind-count (1+ tr:bfind-count)))
  (if (not res) (setq res origstr))
  (setq res (tr:bfind-regex-replace "\\\\" slashflag res)
        res (tr:bfind-regex-replace "\\{" lbrace res)
        res (tr:bfind-regex-replace "\\}" rbrace res)))

(defun tr:bfind-get-text (obj / ent data item value kind)
  (setq ent (vlax-vla-object->ename obj)
        data (entget ent)
        kind (cdr (assoc 0 data)))
  (cond
    ((wcmatch kind "TEXT,*DIMENSION") (cdr (assoc 1 data)))
    ((and (= "MULTILEADER" kind)
          (= acmtextcontent (cdr (assoc 172 (reverse data)))))
      (cdr (assoc 304 data)))
    ((wcmatch kind "ATTRIB,ATTDEF,MTEXT")
      (setq data (reverse data)
            value (cdr (assoc 1 data)))
      (while (setq item (assoc 3 data))
        (setq value (strcat (cdr item) value)
              data (cdr (member item data))))
      value)))

(defun tr:bfind-calc-inspt (obj value / data align)
  (setq data (entget (vlax-vla-object->ename obj))
        align (cdr (assoc 72 data)))
  (polar
    (vlax-get obj 'InsertionPoint)
    (vla-get-Rotation obj)
    (*
      (apply '+
        (mapcar
          '(lambda (a b) (- (car a) (car b)))
          (textbox data)
          (textbox (subst (cons 1 value) (assoc 1 data) data))))
      (cond ((or (= align 1) (= align 4)) 0.5)
            ((= align 2) 1.0)
            (T 0.0)))))

(defun tr:bfind-literal-replace (value find replacement / pos start count findlen replen)
  (setq start 0
        count 0
        findlen (strlen find)
        replen (strlen replacement))
  (if (> findlen 0)
    (while (setq pos (vl-string-search find value start))
      (setq value
        (strcat
          (if (> pos 0) (substr value 1 pos) "")
          replacement
          (substr value (+ pos findlen 1)))
        count (1+ count)
        start (+ pos replen))))
  (if (> count 0)
    (setq tr:bfind-count (+ tr:bfind-count count)))
  value)

(defun tr:bfind-encoded-search-p (value)
  (and value (vl-string-search (strcat (chr 92) "U+") value)))

(defun tr:bfind-replace-value (value find replacement / changed)
  (if (and value (vl-string-search find value))
    (setq tr:bfind-rawhits (1+ tr:bfind-rawhits)))
  (setq changed (tr:bfind-replace-text replacement find value T nil))
  ;; AutoCAD may store visible Cyrillic as literal Unicode escape sequences,
  ;; e.g. visible "ЦОД" as "\U+0426\U+041E\U+0414".  When the normal
  ;; formatting-aware pass finds nothing, try the pre-encoded form supplied by
  ;; Python and normalize the matched fragment to ordinary Unicode text.
  (if (and (tr:bfind-encoded-search-p tr:bfind-find-escaped)
           (vl-string-search tr:bfind-find-escaped changed))
    (progn
      (setq tr:bfind-rawhits (1+ tr:bfind-rawhits))
      (setq changed
        (tr:bfind-literal-replace
          changed tr:bfind-find-escaped tr:bfind-replacement-escaped))))
  changed)

(defun tr:bfind-replace-object
  (obj find replacement / name old new row column att before editable delta)
  (setq name (vla-get-ObjectName obj))
  (cond
    ((and (= "AcDbBlockReference" name)
          (= :vlax-true (vla-get-HasAttributes obj)))
      (foreach att (vlax-invoke obj 'GetAttributes)
        (setq old (tr:bfind-get-text att)
              new (tr:bfind-replace-value old find replacement))
        (if (/= old new)
          (progn
            (vla-put-InsertionPoint att (vlax-3D-point (tr:bfind-calc-inspt att new)))
            (vla-put-TextString att new)))))

    ((wcmatch (strcase name) "*DIMENSION*")
      (setq old (tr:bfind-get-text obj))
      (if (and old (/= "" old))
        (progn
          (setq new (tr:bfind-replace-value old find replacement))
          (if (/= old new) (vla-put-TextOverride obj new)))))

    ((or (= "AcDbMText" name)
         (= "AcDbText" name)
         (= "AcDbMLeader" name)
         (= "AcDbAttribute" name)
         (= "AcDbAttributeDefinition" name))
      (setq old (tr:bfind-get-text obj))
      (if old
        (progn
          (setq new (tr:bfind-replace-value old find replacement))
          (if (/= old new)
            (progn
              (if (= "AcDbText" name)
                (vla-put-InsertionPoint obj
                  (vlax-3D-point (tr:bfind-calc-inspt obj new))))
              (vla-put-TextString obj new))))))

    ((= "AcDbTable" name)
      (setq row (vla-get-Rows obj))
      (while (not (minusp (setq row (1- row))))
        (setq column (vla-get-Columns obj))
        (while (not (minusp (setq column (1- column))))
          (setq before tr:bfind-count
                old (vla-GetText obj row column)
                new (tr:bfind-replace-value old find replacement))
          (if (/= old new)
            (progn
              ;; Excel-linked and otherwise read-only cells accept SetText
              ;; only as a transient override.  It is discarded on data-link
              ;; refresh and can briefly overlap the linked value.  Never
              ;; touch such cells; report their match count to the caller.
              (setq editable
                (vl-catch-all-apply
                  'vlax-invoke-method (list obj 'IsContentEditable row column)))
              (if (and (not (vl-catch-all-error-p editable))
                       (= :vlax-false editable))
                (progn
                  (setq delta (- tr:bfind-count before)
                        tr:bfind-count before
                        tr:bfind-linked-skipped
                          (+ tr:bfind-linked-skipped (max 1 delta))))
                (vla-SetText obj row column new))))))))
  (princ))

(defun tr:bfind-safe-replace-object (obj find replacement / before outcome)
  (setq before tr:bfind-count
        outcome
          (vl-catch-all-apply
            'tr:bfind-replace-object (list obj find replacement)))
  (if (vl-catch-all-error-p outcome)
    (progn
      (setq tr:bfind-count before
            tr:bfind-skipped (1+ tr:bfind-skipped))
      nil)
    T))

(defun tr:bfind-unlock-layers (/ entry entity data flags locked)
  (setq entry (tblnext "LAYER" T))
  (while entry
    (setq entity (tblobjname "LAYER" (cdr (assoc 2 entry)))
          data (entget entity)
          flags (cdr (assoc 70 data)))
    (if (/= 0 (logand 4 flags))
      (progn
        (entmod (subst (cons 70 (- flags (logand 4 flags))) (assoc 70 data) data))
        (setq locked (cons (cons entity flags) locked))))
    (setq entry (tblnext "LAYER")))
  locked)

(defun tr:bfind-relock-layers (locked / item data)
  (foreach item locked
    (setq data (entget (car item)))
    (if data
      (entmod (subst (cons 70 (cdr item)) (assoc 70 data) data))))
  (princ))

(defun tr:bfind-mtext-value (data / item value)
  (setq value "")
  (foreach item data
    (if (member (car item) '(1 3))
      (setq value (strcat value (cdr item)))))
  value)

(defun tr:bfind-mtext-pairs (value / pairs)
  (setq pairs nil)
  (while (> (strlen value) 250)
    (setq pairs (append pairs (list (cons 3 (substr value 1 250))))
          value (substr value 251)))
  (append pairs (list (cons 1 value))))

(defun tr:bfind-replace-mtext-pairs (data pairs / inserted item output)
  (setq inserted nil
        output nil)
  (foreach item data
    (if (member (car item) '(1 3))
      (if (not inserted)
        (setq output (append output pairs)
              inserted T))
      (setq output (append output (list item)))))
  output)

(defun tr:bfind-process-entity
  (entity data kind find replacement / pair old new updated)
  (cond
    ((member kind '("TEXT" "ATTRIB" "ATTDEF"))
      (if (setq pair (assoc 1 data))
        (progn
          (setq old (cdr pair)
                new (tr:bfind-replace-value old find replacement))
          (if (/= old new)
            (progn
              (setq updated (subst (cons 1 new) pair data))
              (if (not (entmod updated))
                (vl-exit-with-error "TextReplacer: entmod failed"))
              (entupd entity))))))

    ((= kind "MTEXT")
      (setq old (tr:bfind-mtext-value data)
            new (tr:bfind-replace-value old find replacement))
      (if (/= old new)
        (progn
          (setq updated
            (tr:bfind-replace-mtext-pairs data (tr:bfind-mtext-pairs new)))
          (if (not (entmod updated))
            (vl-exit-with-error "TextReplacer: MTEXT entmod failed"))
          (entupd entity))))

    ((wcmatch kind "*DIMENSION")
      (if (setq pair (assoc 1 data))
        (progn
          (setq old (cdr pair)
                new (tr:bfind-replace-value old find replacement))
          (if (/= old new)
            (entmod (subst (cons 1 new) pair data))))))

    ((= kind "MULTILEADER")
      (if (setq pair (assoc 304 data))
        (progn
          (setq old (cdr pair)
                new (tr:bfind-replace-value old find replacement))
          (if (/= old new)
            (entmod (subst (cons 304 new) pair data)))))))
  (princ))

(defun tr:bfind-process-document
  (find replacement /
   block-entry block-name block-flags record entity data kind
   subentity subdata subkind selection index)
  (setq tr:bfind-locked (tr:bfind-unlock-layers)
        block-entry (tblnext "BLOCK" T))

  ;; Core Console does not expose AutoCAD.Application, so enumerate the native
  ;; block table directly.  This covers model space, layouts and block
  ;; definitions while still using BFind's object replacement engine.
  (while block-entry
    (setq block-name (cdr (assoc 2 block-entry))
          block-flags (cdr (assoc 70 block-entry))
          record (tblobjname "BLOCK" block-name))
    (if record
      (progn
        (setq entity (entnext record))
        (while entity
          (setq data (entget entity)
                kind (cdr (assoc 0 data)))
          (if (= "ENDBLK" kind)
            (setq entity nil)
            (progn
              (setq tr:bfind-scanned (1+ tr:bfind-scanned))
              (if (or (member kind '("TEXT" "MTEXT" "ATTRIB" "ATTDEF" "MULTILEADER"))
                      (wcmatch kind "*DIMENSION"))
                (setq tr:bfind-text-objects (1+ tr:bfind-text-objects)))
              (tr:bfind-process-entity entity data kind find replacement)
              (if (and (= "INSERT" kind)
                       (= 1 (cdr (assoc 66 data))))
                (progn
                  (setq subentity (entnext entity))
                  (while subentity
                    (setq subdata (entget subentity)
                          subkind (cdr (assoc 0 subdata)))
                    (if (= "SEQEND" subkind)
                      (progn
                        (setq entity (entnext subentity)
                              subentity nil))
                      (progn
                        (setq tr:bfind-scanned (1+ tr:bfind-scanned))
                        (if (= "ATTRIB" subkind)
                          (setq tr:bfind-text-objects (1+ tr:bfind-text-objects)))
                        (tr:bfind-process-entity
                          subentity subdata subkind find replacement)
                        (setq subentity (entnext subentity))))))
                (setq entity (entnext entity))))))))
    (setq block-entry (tblnext "BLOCK")))

  ;; Attribute references are subentities owned by INSERTs and are not exposed
  ;; by every block-record entnext chain in Core Console.  A global database
  ;; selection retrieves those references, matching BFind's GetAttributes path.
  (if (setq selection (ssget "_X" '((0 . "ATTRIB"))))
    (progn
      (setq index 0)
      (repeat (sslength selection)
        (setq entity (ssname selection index)
              data (entget entity)
              tr:bfind-scanned (1+ tr:bfind-scanned)
              tr:bfind-text-objects (1+ tr:bfind-text-objects))
        (tr:bfind-process-entity entity data "ATTRIB" find replacement)
        (setq index (1+ index)))))

  (tr:bfind-relock-layers tr:bfind-locked)
  (setq tr:bfind-locked nil)
  (princ))

(defun tr:bfind-replace-escaped-value (value / changed)
  ;; This second-pass helper intentionally handles only the encoded spelling.
  ;; Re-running the normal search would be unsafe when the replacement itself
  ;; contains the search text (for example, "A" -> "AB").
  (setq changed value)
  (if (and value
           (tr:bfind-encoded-search-p tr:bfind-find-escaped)
           (vl-string-search tr:bfind-find-escaped value))
    (progn
      (setq tr:bfind-rawhits (1+ tr:bfind-rawhits))
      (setq changed
        (tr:bfind-literal-replace
          value tr:bfind-find-escaped tr:bfind-replacement-escaped))))
  changed)

(defun tr:bfind-process-escaped-entity
  (entity data kind / pair old new updated)
  (cond
    ((member kind '("TEXT" "ATTRIB" "ATTDEF"))
      (if (setq pair (assoc 1 data))
        (progn
          (setq old (cdr pair)
                new (tr:bfind-replace-escaped-value old))
          (if (/= old new)
            (progn
              (setq updated (subst (cons 1 new) pair data))
              (if (not (entmod updated))
                (vl-exit-with-error "TextReplacer: encoded text entmod failed"))
              (entupd entity))))))

    ((= kind "MTEXT")
      (setq old (tr:bfind-mtext-value data)
            new (tr:bfind-replace-escaped-value old))
      (if (/= old new)
        (progn
          (setq updated
            (tr:bfind-replace-mtext-pairs data (tr:bfind-mtext-pairs new)))
          (if (not (entmod updated))
            (vl-exit-with-error "TextReplacer: encoded MTEXT entmod failed"))
          (entupd entity))))

    ((wcmatch kind "*DIMENSION")
      (if (setq pair (assoc 1 data))
        (progn
          (setq old (cdr pair)
                new (tr:bfind-replace-escaped-value old))
          (if (/= old new)
            (entmod (subst (cons 1 new) pair data))))))

    ((= kind "MULTILEADER")
      (if (setq pair (assoc 304 data))
        (progn
          (setq old (cdr pair)
                new (tr:bfind-replace-escaped-value old))
          (if (/= old new)
            (entmod (subst (cons 304 new) pair data)))))))
  (princ))

(defun tr:bfind-process-escaped-native
  (/ block-entry block-name block-flags record entity data kind selection index)
  ;; The ActiveX Layouts collection can omit stale or non-current paper-space
  ;; block records which are nevertheless present in the DWG database.  Scan
  ;; every local block-table record once more, but only for the literal
  ;; \U+XXXX spelling.  Xrefs and externally dependent records stay untouched.
  (if (tr:bfind-encoded-search-p tr:bfind-find-escaped)
    (progn
      (setq tr:bfind-locked (tr:bfind-unlock-layers)
            block-entry (tblnext "BLOCK" T))
      (while block-entry
        (setq block-name (cdr (assoc 2 block-entry))
              block-flags (cdr (assoc 70 block-entry))
              record (tblobjname "BLOCK" block-name))
        (if (and record (= 0 (logand block-flags 28)))
          (progn
            (setq entity (entnext record))
            (while entity
              (setq data (entget entity)
                    kind (cdr (assoc 0 data)))
              (if (= "ENDBLK" kind)
                (setq entity nil)
                (progn
                  (setq tr:bfind-scanned (1+ tr:bfind-scanned))
                  (if (or (member kind
                            '("TEXT" "MTEXT" "ATTRIB" "ATTDEF" "MULTILEADER"))
                          (wcmatch kind "*DIMENSION"))
                    (setq tr:bfind-text-objects (1+ tr:bfind-text-objects)))
                  (tr:bfind-process-escaped-entity entity data kind)
                  (setq entity (entnext entity)))))))
        (setq block-entry (tblnext "BLOCK")))

      ;; Orphaned/non-current paper-space records may be omitted even by the
      ;; native block table.  A global database selection reaches their text
      ;; objects as well as attribute references.  Objects already normalized
      ;; above are harmless because the encoded spelling has disappeared.
      (if (setq selection
            (ssget "_X"
              '((0 . "TEXT,MTEXT,ATTRIB,ATTDEF,MULTILEADER,*DIMENSION"))))
        (progn
          (setq index 0)
          (repeat (sslength selection)
            (setq entity (ssname selection index)
                  data (entget entity)
                  kind (cdr (assoc 0 data))
                  tr:bfind-scanned (1+ tr:bfind-scanned)
                  tr:bfind-text-objects (1+ tr:bfind-text-objects))
            (tr:bfind-process-escaped-entity entity data kind)
            (setq index (1+ index)))))

      (tr:bfind-relock-layers tr:bfind-locked)
      (setq tr:bfind-locked nil)))
  (princ))

(defun tr:bfind-process-document-vla
  (doc find replacement / block obj selection index entity)
  (setq tr:bfind-locked (tr:bfind-unlock-layers))

  ;; A drawing may retain paper-space block records which are absent from the
  ;; ActiveX Layouts collection but whose entities are still visible.  A global
  ;; database selection reaches those records and the ordinary current layouts
  ;; in one pass.  Process the selected entities through ActiveX so aligned
  ;; text, dimensions and linked-table safeguards keep their BFind behavior.
  (if (setq selection
        (ssget "_X"
          '((0 . "TEXT,MTEXT,ATTRIB,MULTILEADER,*DIMENSION,ACAD_TABLE"))))
    (progn
      (setq index 0)
      (repeat (sslength selection)
        (setq entity (ssname selection index)
              obj (vlax-ename->vla-object entity)
              tr:bfind-scanned (1+ tr:bfind-scanned)
              tr:bfind-text-objects (1+ tr:bfind-text-objects))
        (tr:bfind-safe-replace-object obj find replacement)
        (setq index (1+ index)))))

  ;; Original BFind "Block Definitions" path.
  (vlax-for block (vla-get-Blocks doc)
    (if (and (= :vlax-false (vla-get-IsLayout block))
             (= :vlax-false (vla-get-IsXRef block))
             (not (wcmatch (vla-get-Name block) "`*D*")))
      (vlax-for obj block
        (setq tr:bfind-scanned (1+ tr:bfind-scanned))
        (tr:bfind-safe-replace-object obj find replacement))))

  (tr:bfind-relock-layers tr:bfind-locked)
  (setq tr:bfind-locked nil)

  ;; Catch encoded text in local paper-space records omitted by ActiveX.
  (tr:bfind-process-escaped-native)
  (princ))

(defun tr:bfind-run (/ app doc find replacement outcome)
  (setq find (getenv "TEXTREPLACER_FIND")
        replacement (getenv "TEXTREPLACER_REPLACE")
        tr:bfind-find-escaped (getenv "TEXTREPLACER_FIND_ESCAPED")
        tr:bfind-replacement-escaped (getenv "TEXTREPLACER_REPLACE_ESCAPED")
        tr:bfind-count 0
        tr:bfind-scanned 0
        tr:bfind-text-objects 0
        tr:bfind-rawhits 0
        tr:bfind-skipped 0
        tr:bfind-linked-skipped 0
        tr:bfind-status nil
        tr:bfind-locked nil)
  (if (null replacement) (setq replacement ""))
  (if (null tr:bfind-find-escaped) (setq tr:bfind-find-escaped find))
  (if (null tr:bfind-replacement-escaped)
    (setq tr:bfind-replacement-escaped replacement))
  (if (or (null find) (= "" find))
    (setq tr:bfind-status "ERROR:empty search text")
    (progn
      (setq tr:bfind-rex (vlax-create-object "VBScript.RegExp"))
      (if (null tr:bfind-rex)
        (setq tr:bfind-status "ERROR:VBScript.RegExp is unavailable")
        (progn
          (setq app (vlax-get-acad-object))
          (setq outcome
            (if app
              (progn
                (setq doc (vla-get-ActiveDocument app))
                (vl-catch-all-apply
                  'tr:bfind-process-document-vla (list doc find replacement)))
              (vl-catch-all-apply
                'tr:bfind-process-document (list find replacement))))
          (if (vl-catch-all-error-p outcome)
            (setq tr:bfind-status
              (strcat "ERROR:" (vl-catch-all-error-message outcome)))
            (setq tr:bfind-status
              (strcat "OK:" (itoa tr:bfind-count)
                      ":" (itoa tr:bfind-scanned)
                      ":" (itoa tr:bfind-text-objects)
                      ":" (itoa tr:bfind-rawhits)
                      ":" (itoa tr:bfind-skipped)
                      ":" (itoa tr:bfind-linked-skipped)
                      ":" (itoa (strlen tr:bfind-find-escaped)))))))))
  (if tr:bfind-locked (tr:bfind-relock-layers tr:bfind-locked))
  (tr:bfind-release tr:bfind-rex)
  (setq tr:bfind-rex nil)
  (princ))

(defun c:TR_BFIND () (tr:bfind-run))

;; Called only after the script has completed QSAVE.
(defun c:TR_BFIND_DONE ()
  (tr:bfind-write-result
    (if tr:bfind-status tr:bfind-status "ERROR:replacement command did not run")))

(princ)
