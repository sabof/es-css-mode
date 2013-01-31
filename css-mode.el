;;; css-mode.el --- Major mode to edit CSS files -*- lexical-binding: t -*-

;; Copyright (C) 2006-2013 Free Software Foundation, Inc.

;; Author: Stefan Monnier <monnier@iro.umontreal.ca>
;; Keywords: hypermedia

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Yet another CSS mode.

;;; Todo:

;; - electric ; and }
;; - filling code with auto-fill-mode
;; - completion
;; - fix font-lock errors with multi-line selectors

;;; TODO:
;;  extra empty line indentation

;;; Code:

(require 'cl-lib)

(defgroup css nil
  "Cascading Style Sheets (CSS) editing mode."
  :group 'languages)

;; Debugging

(defvar css-debug-overlays nil)

(defun css-debug-overlay (beginning end color)
  (let ((overlay (make-overlay beginning end)))
    (overlay-put overlay 'face `(:background ,color))
    (push overlay css-debug-overlays)))

(defvar css-debug nil)

(defun css-debug-msg (name)
  (when css-debug
    (message "%s" name)))

(defun css-debug-goto-root-declaration ()
  (let* (( ppss (syntax-ppss))
         ( depth (nth 0  ppss)))
    (when (nth 3 ppss)
      (goto-char (nth 8 ppss)))
    (while (and (cl-plusp depth)
                (not (equal (char-after) ?\{ )))
      (up-list -1)
      (cl-decf depth))))

(defun css-debug-parser-current ()
  (interactive)
  (mapc 'delete-overlay css-debug-overlays)
  (let* (( point
           (save-excursion
             (back-to-indentation)
             (point)))
         ( parsed
           (save-excursion
             (css-debug-goto-root-declaration)
             (css-parse-curly)))
         ( relevant
           (css-pc-get-relevant parsed point)))
    (css-debug-overlay
     (car relevant)
     (cadr relevant)
     "DarkRed")
    nil))

(defun css-debug-highight-parsed (parsed)
  (mapc 'delete-overlay css-debug-overlays)
  (cl-mapc (lambda (item)
             (css-debug-overlay
              (car item)
              (cadr item)
              (case (caddr item)
                ('nested-selector  "DarkGreen")
                ('comment          "SaddleBrown")
                ( t                "DarkRed"))))
           parsed))

(defun css-debug-parser-all ()
  (interactive)
  (let* (( parsed
           (save-excursion
             (css-debug-goto-root-declaration)
             (css-parse-curly))))
    (css-debug-highight-parsed
     parsed)
    nil))

(defun css-debug-parser-inside ()
  (interactive)
  (let* (( parsed
           (save-excursion
             (css-debug-goto-root-declaration)
             (css-parse-curly)))
         ( inside
           (css-pc-inside-statement
            parsed (point))))
    (message "P %s" inside)))

;; EOF Debugging

(defun css-extract-keyword-list (res)
  (with-temp-buffer
    (url-insert-file-contents "http://www.w3.org/TR/REC-CSS2/css2.txt")
    (goto-char (point-max))
    (search-backward "Appendix H. Index")
    (forward-line)
    (delete-region (point-min) (point))
    (let ((result nil)
          keys)
      (dolist (re res)
        (goto-char (point-min))
        (setq keys nil)
        (while (re-search-forward (cdr re) nil t)
          (push (match-string 1) keys))
        (push (cons (car re) (sort keys 'string-lessp)) result))
      (nreverse result))))

(defun css-extract-parse-val-grammar (string env)
  (let ((start 0)
        (elems ())
        name)
    (while (string-match
            (concat "\\(?:"
                    (concat "<a [^>]+><span [^>]+>\\(?:"
                            "&lt;\\([^&]+\\)&gt;\\|'\\([^']+\\)'"
                            "\\)</span></a>")
                    "\\|" "\\(\\[\\)"
                    "\\|" "\\(]\\)"
                    "\\|" "\\(||\\)"
                    "\\|" "\\(|\\)"
                    "\\|" "\\([*+?]\\)"
                    "\\|" "\\({[^}]+}\\)"
                    "\\|" "\\(\\w+\\(?:-\\w+\\)*\\)"
                    "\\)[ \t\n]*")
            string start)
      ;; (assert (eq start (match-beginning 0)))
      (setq start (match-end 0))
      (cond
       ;; Reference to a type of value.
       ((setq name (match-string-no-properties 1 string))
        (push (intern name) elems))
       ;; Reference to another property's values.
       ((setq name (match-string-no-properties 2 string))
        (setq elems (delete-dups (append (cdr (assoc name env)) elems))))
       ;; A literal
       ((setq name (match-string-no-properties 9 string))
        (push name elems))
       ;; We just ignore the rest.  I.e. we ignore the structure because
       ;; it's too difficult to exploit anyway (it would allow us to only
       ;; complete top/center/bottom after one of left/center/right and
       ;; vice-versa).
       (t nil)))
    elems))

(defun css-extract-props-and-vals ()
  (with-temp-buffer
    (url-insert-file-contents "http://www.w3.org/TR/CSS21/propidx.html")
    (goto-char (point-min))
    (let ((props ()))
      (while (re-search-forward "#propdef-\\([^\"]+\\)\"><span class=\"propinst-\\1 xref\">'\\1'</span></a>" nil t)
        (let ((prop (match-string-no-properties 1)))
          (save-excursion
            (goto-char (match-end 0))
            (search-forward "<td>")
            (let ((vals-string (buffer-substring (point)
                                                 (progn
                                                   (re-search-forward "[ \t\n]+|[ \t\n]+<a href=\"cascade.html#value-def-inherit\" class=\"noxref\"><span class=\"value-inst-inherit\">inherit</span></a>")
                                                   (match-beginning 0)))))
              ;;
              (push (cons prop (css-extract-parse-val-grammar vals-string props))
                    props)))))
      props)))

;; Extraction was done with:
;; (css-extract-keyword-list
;;  '((pseudo . "^ +\\* :\\([^ \n,]+\\)")
;;    (at . "^ +\\* @\\([^ \n,]+\\)")
;;    (descriptor . "^ +\\* '\\([^ '\n]+\\)' (descriptor)")
;;    (media . "^ +\\* '\\([^ '\n]+\\)' media group")
;;    (property . "^ +\\* '\\([^ '\n]+\\)',")))

(defconst css-pseudo-ids
  '("active" "after" "before" "first" "first-child" "first-letter" "first-line"
    "focus" "hover" "lang" "last-child" "left" "link" "right" "visited")
  "Identifiers for pseudo-elements and pseudo-classes.")

(defconst css-at-ids
  '("charset" "font-face" "import" "media" "page")
  "Identifiers that appear in the form @foo.")

(defconst css-descriptor-ids
  '("ascent" "baseline" "bbox" "cap-height" "centerline" "definition-src"
    "descent" "font-family" "font-size" "font-stretch" "font-style"
    "font-variant" "font-weight" "mathline" "panose-1" "slope" "src" "stemh"
    "stemv" "topline" "unicode-range" "units-per-em" "widths" "x-height")
  "Identifiers for font descriptors.")

(defconst css-media-ids
  '("all" "aural" "bitmap" "continuous" "grid" "paged" "static" "tactile"
    "visual")
  "Identifiers for types of media.")

(defconst css-property-ids
  '("azimuth" "background" "background-attachment" "background-color"
    "background-image" "background-position" "background-repeat" "block"
    "border" "border-bottom" "border-bottom-color" "border-bottom-style"
    "border-bottom-width" "border-collapse" "border-color" "border-left"
    "border-left-color" "border-left-style" "border-left-width" "border-right"
    "border-right-color" "border-right-style" "border-right-width"
    "border-spacing" "border-style" "border-top" "border-top-color"
    "border-top-style" "border-top-width" "border-width" "bottom"
    "caption-side" "clear" "clip" "color" "compact" "content"
    "counter-increment" "counter-reset" "cue" "cue-after" "cue-before"
    "cursor" "dashed" "direction" "display" "dotted" "double" "elevation"
    "empty-cells" "float" "font" "font-family" "font-size" "font-size-adjust"
    "font-stretch" "font-style" "font-variant" "font-weight" "groove" "height"
    "hidden" "inline" "inline-table" "inset" "left" "letter-spacing"
    "line-height" "list-item" "list-style" "list-style-image"
    "list-style-position" "list-style-type" "margin" "margin-bottom"
    "margin-left" "margin-right" "margin-top" "marker-offset" "marks"
    "max-height" "max-width" "min-height" "min-width" "orphans" "outline"
    "outline-color" "outline-style" "outline-width" "outset" "overflow"
    "padding" "padding-bottom" "padding-left" "padding-right" "padding-top"
    "page" "page-break-after" "page-break-before" "page-break-inside" "pause"
    "pause-after" "pause-before" "pitch" "pitch-range" "play-during" "position"
    "quotes" "richness" "ridge" "right" "run-in" "size" "solid" "speak"
    "speak-header" "speak-numeral" "speak-punctuation" "speech-rate" "stress"
    "table" "table-caption" "table-cell" "table-column" "table-column-group"
    "table-footer-group" "table-header-group" "table-layout" "table-row"
    "table-row-group" "text-align" "text-decoration" "text-indent"
    "text-shadow" "text-transform" "top" "unicode-bidi" "vertical-align"
    "visibility" "voice-family" "volume" "white-space" "widows" "width"
    "word-spacing" "z-index")
  "Identifiers for properties.")

(defcustom css-electric-keys '(?\} ?\;) ;; '()
  "Self inserting keys which should trigger re-indentation."
  :version "22.2"
  :type '(repeat character)
  :options '((?\} ?\;))
  :group 'css)

(defvar css-mode-syntax-table
  (let ((st (make-syntax-table)))
    ;; C-style comments.
    (modify-syntax-entry ?/ ". 14" st)
    (modify-syntax-entry ?* ". 23" st)
    ;; Strings.
    (modify-syntax-entry ?\" "\"" st)
    (modify-syntax-entry ?\' "\"" st)
    ;; Blocks.
    (modify-syntax-entry ?\{ "(}" st)
    (modify-syntax-entry ?\} "){" st)
    ;; Args in url(...) thingies and other "function calls".
    (modify-syntax-entry ?\( "()" st)
    (modify-syntax-entry ?\) ")(" st)
    ;; To match attributes in selectors.
    (modify-syntax-entry ?\[ "(]" st)
    (modify-syntax-entry ?\] ")[" st)
    ;; Special chars that sometimes come at the beginning of words.
    (modify-syntax-entry ?@ "'" st)
    ;; (modify-syntax-entry ?: "'" st)
    (modify-syntax-entry ?# "'" st)
    ;; Distinction between words and symbols.
    (modify-syntax-entry ?- "_" st)
    st))

(defconst css-escapes-re
  "\\\\\\(?:[^\000-\037\177]\\|[0-9a-fA-F]+[ \n\t\r\f]?\\)")
(defconst css-nmchar-re (concat "\\(?:[-[:alnum:]]\\|" css-escapes-re "\\)"))
(defconst css-nmstart-re (concat "\\(?:[[:alpha:]]\\|" css-escapes-re "\\)"))
(defconst css-ident-re (concat css-nmstart-re css-nmchar-re "*"))
(defconst css-proprietary-nmstart-re ;; Vendor-specific properties.
  (concat "[-_]" (regexp-opt '("ms" "moz" "o" "khtml" "webkit")) "-"))
(defconst css-name-re (concat css-nmchar-re "+"))

(defface css-selector '((t :inherit font-lock-function-name-face))
  "Face to use for selectors."
  :group 'css)
(defface css-property '((t :inherit font-lock-variable-name-face))
  "Face to use for properties."
  :group 'css)
(defface css-proprietary-property '((t :inherit (css-property italic)))
  "Face to use for vendor-specific properties.")

(defvar css-font-lock-keywords
  `(("!\\s-*important" . font-lock-builtin-face)
    ;; Atrules keywords.  IDs not in css-at-ids are valid (ignored).
    ;; In fact the regexp should probably be
    ;; (,(concat "\\(@" css-ident-re "\\)\\([ \t\n][^;{]*\\)[;{]")
    ;;  (1 font-lock-builtin-face))
    ;; Since "An at-rule consists of everything up to and including the next
    ;; semicolon (;) or the next block, whichever comes first."
    (,(concat "@" css-ident-re) . font-lock-builtin-face)
    ;; Selectors.
    ;; FIXME: attribute selectors don't work well because they may contain
    ;; strings which have already been highlighted as f-l-string-face and
    ;; thus prevent this highlighting from being applied (actually now that
    ;; I use `append' this should work better).  But really the part of hte
    ;; selector between [...] should simply not be highlighted.

    ;; (,(concat "^\\([ \t]*[^@:{}\n][^:{}]+\\(?::" (regexp-opt css-pseudo-ids t)
    ;;           "\\(?:([^)]+)\\)?[^:{\n]*\\)*\\)\\(?:\n[ \t]*\\)*{")
    ;;  (1 'css-selector append))
    (,(concat "^\\([ \t]*[^@:{}\n][^:{}]+\\(?::" (regexp-opt css-pseudo-ids t)
              "\\(?:([^)]+)\\)?[^:{\n]*\\)*\\)\\(?:\n[ \t]*\\)*{")
      (1 'font-lock-function-name-face nil))
    ;; In the above rule, we allow the open-brace to be on some subsequent
    ;; line.  This will only work if we properly mark the intervening text
    ;; as being part of a multiline element (and even then, this only
    ;; ensures proper refontification, but not proper discovery).
    ("^[ \t]*{" (0 (save-excursion
                     (goto-char (match-beginning 0))
                     (skip-chars-backward " \n\t")
                     (put-text-property (point) (match-end 0)
                                        'font-lock-multiline t)
                     ;; No face.
                     nil)))
    ;; Properties.  Again, we don't limit ourselves to css-property-ids.
    (,(concat "\\(?:[{;]\\|^\\)[ \t]*\\("
              "\\(?:\\(" css-proprietary-nmstart-re "\\)\\|"
              css-nmstart-re "\\)" css-nmchar-re "*"
              "\\)\\s-*:")
      (1 (if (match-end 2) 'css-proprietary-property 'css-property)))))

(defvar css-font-lock-defaults
  '(css-font-lock-keywords nil t))

;;;###autoload
(define-derived-mode css-mode fundamental-mode
  "CSS" "Major mode to edit Cascading Style Sheets."
  (setq-local font-lock-defaults css-font-lock-defaults)
  (setq-local comment-start "/*")
  (setq-local comment-start-skip "/\\*+[ \t]*")
  (setq-local comment-end "*/")
  (setq-local comment-end-skip "[ \t]*\\*+/")
  (setq-local forward-sexp-function 'css-forward-sexp)
  (setq-local parse-sexp-ignore-comments t)
  (setq-local indent-line-function 'css-indent-line)
  (setq-local fill-paragraph-function 'css-fill-paragraph)
  (setq-local add-log-current-defun-function #'css-current-defun-name)
  (setq-local beginning-of-defun-function 'css-beginning-of-defun)
  (setq-local end-of-defun-function 'css-end-of-defun)
  (when css-electric-keys
    (let ((fc (make-char-table 'auto-fill-chars)))
      (set-char-table-parent fc auto-fill-chars)
      (dolist (c css-electric-keys)
        (aset fc c 'indent-according-to-mode))
      (setq-local auto-fill-chars fc))))

(defvar comment-continue)

(defun css-fill-paragraph (&optional justify)
  (save-excursion
    (let ((ppss (syntax-ppss))
          (eol (line-end-position)))
      (cond
       ((and (nth 4 ppss)
             (save-excursion
               (goto-char (nth 8 ppss))
               (forward-comment 1)
               (prog1 (not (bolp))
                 (setq eol (point)))))
        ;; Filling inside a comment whose comment-end marker is not \n.
        ;; This code is meant to be generic, so that it works not only for
        ;; css-mode but for all modes.
        (save-restriction
          (narrow-to-region (nth 8 ppss) eol)
          (comment-normalize-vars)      ;Will define comment-continue.
          (let ((fill-paragraph-function nil)
                (paragraph-separate
                 (if (and comment-continue
                          (string-match "[^ \t]" comment-continue))
                     (concat "\\(?:[ \t]*" (regexp-quote comment-continue)
                             "\\)?\\(?:" paragraph-separate "\\)")
                   paragraph-separate))
                (paragraph-start
                 (if (and comment-continue
                          (string-match "[^ \t]" comment-continue))
                     (concat "\\(?:[ \t]*" (regexp-quote comment-continue)
                             "\\)?\\(?:" paragraph-start "\\)")
                   paragraph-start)))
            (fill-paragraph justify)
            ;; Don't try filling again.
            t)))

       ((and (null (nth 8 ppss))
             (or (nth 1 ppss)
                 (and (ignore-errors
                        (down-list 1)
                        (when (<= (point) eol)
                          (setq ppss (syntax-ppss)))))))
        (goto-char (nth 1 ppss))
        (let ((end (save-excursion
                     (ignore-errors (forward-sexp 1) (copy-marker (point) t)))))
          (when end
            (while (re-search-forward "[{;}]" end t)
              (cond
               ;; This is a false positive inside a string or comment.
               ((nth 8 (syntax-ppss)) nil)
               ((eq (char-before) ?\})
                (save-excursion
                  (forward-char -1)
                  (skip-chars-backward " \t")
                  (unless (bolp) (newline))))
               (t
                (while
                    (progn
                      (setq eol (line-end-position))
                      (and (forward-comment 1)
                           (> (point) eol)
                           ;; A multi-line comment should be on its own line.
                           (save-excursion (forward-comment -1)
                                           (when (< (point) eol)
                                             (newline)
                                             t)))))
                (if (< (point) eol) (newline)))))
            (goto-char (nth 1 ppss))
            (indent-region (line-beginning-position 2) end)
            ;; Don't use the default filling code.
            t)))))))

;;; Navigation and indentation.

(defconst css-navigation-syntax-table
  (let ((st (make-syntax-table css-mode-syntax-table)))
    (map-char-table (lambda (c v)
                      ;; Turn punctuation (code = 1) into symbol (code = 1).
                      (if (eq (car-safe v) 1)
                          (set-char-table-range st c (cons 3 (cdr v)))))
                    st)
    st))

(defun css-backward-sexp (n)
  (let ((forward-sexp-function nil))
    (if (< n 0) (css-forward-sexp (- n))
      (while (> n 0)
        (setq n (1- n))
        (forward-comment (- (point-max)))
        (if (not (eq (char-before) ?\;))
            (backward-sexp 1)
          (while (progn (backward-sexp 1)
                        (save-excursion
                          (forward-comment (- (point-max)))
                          ;; FIXME: We should also skip punctuation.
                          (not (or (bobp) (memq (char-before) '(?\; ?\{))))))))))))

(defun css-forward-sexp (n)
  (let ((forward-sexp-function nil))
    (if (< n 0) (css-backward-sexp (- n))
      (while (> n 0)
        (setq n (1- n))
        (forward-comment (point-max))
        (if (not (eq (char-after) ?\;))
            (forward-sexp 1)
          (while (progn (forward-sexp 1)
                        (save-excursion
                          (forward-comment (point-max))
                          ;; FIXME: We should also skip punctuation.
                          (not (memq (char-after) '(?\; ?\})))))))))))

(defun css-beginning-of-defun (&optional arg)
  (unless arg (setq arg 1))
  (when (progn
          ;; What for?
          (unless (zerop (current-column))
            (end-of-line))
          (re-search-backward "^[^\n 	].+{[ ]?$" nil t arg))
    (while (save-excursion
             (and (zerop (forward-line -1))
                  (string-match-p
                   "^[^}[:space:]/]"
                   (buffer-substring
                    (line-beginning-position)
                    (line-end-position)))))
      (forward-line -1))))

(defun css-end-of-defun (&optional arg)
  (interactive)
  (unless arg (setq arg 1))
  (ignore-errors
    (when (cl-plusp (car (syntax-ppss)))
      (css-beginning-of-defun))
    (progn
      (search-forward "{" nil t arg)
      (backward-char)
      (forward-sexp)
      (ignore-errors
        (forward-char)))
    t))

(defun css-go-up ()
  (let* (( ppss (syntax-ppss)))
    (when (or (nth 3 ppss) (nth 4 ppss))
      (goto-char (nth 8 ppss)))
    (when (cl-plusp (nth 0 ppss))
      (up-list -1))))

(defmacro css-while-point-moving (&rest rest)
  (let ((old-point (cl-gensym)))
    `(let (,old-point)
       (while (not (equal (point) ,old-point))
         (setq ,old-point (point))
         ,@rest))))

(defun css-parse-curly ()
  (let (( start (point))
        ( indentation (current-indentation))
        ( end (save-excursion
                (forward-sexp)
                (point)))
        point result)
    (forward-char)
    (cl-loop named main-loop
             do
             (skip-chars-forward "\n\t " end)
             (when (>= (point) (1- end))
               (cl-return-from main-loop))
             (setq point (point))
             (if (forward-comment 1)
                 (push (list point (point) 'comment) result)
                 (progn
                   (cl-loop (unless (re-search-forward ";\\|{\\|}" end t)
                              (cl-return-from main-loop))
                            (unless (nth 4 (syntax-ppss))
                              (cl-return)))
                   (cond ( (equal (char-before) ?\{ )
                           (backward-char)
                           (forward-sexp)
                           (push (list point (point) 'nested-selector) result))
                         ( (equal (char-before) ?\} )
                           (backward-char)
                           (css-while-point-moving
                            (skip-chars-backward "\n\t " start)
                            (forward-comment -1))
                           (push (list point (point) 'statement) result))
                         ( t (push (list point (point) 'statement) result))))))
    (nreverse result)))

(defun css-pc-get-relevant (parsed point)
  (car (reverse (cl-remove-if (apply-partially '< point)
                              parsed :key 'car))))

(defun css-pc-inside-statement (parsed point)
  (cl-some (lambda (item)
           (and (<= (car item) point)
                (<= point (cadr item))))
         parsed))

(defun css-indent-calculate ()
  (save-match-data
    (condition-case error
        (with-syntax-table css-navigation-syntax-table
          (back-to-indentation)
          (let* ((point (point))
                 (ppss (syntax-ppss))
                 ( css-curly-parsed
                   (save-excursion
                     (css-go-up)
                     (when (equal (char-after) ?{ )
                       (css-parse-curly))))
                 css-parsed-relevant
                 (block-ending-line
                  (member (char-after
                           (save-excursion
                             (back-to-indentation)
                             (point)))
                          '( ?\} ?\) ) )))
            (cond ( (nth 4 ppss)
                    ;; Inside a multiline comment
                    (css-debug-msg "MC")
                    (save-excursion
                      (forward-line -1)
                      (skip-chars-forward " \t")
                      (if (>= (nth 8 ppss) (point))
                          (progn
                            (goto-char (nth 8 ppss))
                            (if (eq (char-after point) ?*)
                                (forward-char 1)
                                (if (not (looking-at comment-start-skip))
                                   (error "Internal css-mode error")
                                   (goto-char (match-end 0))))
                           (current-column))
                         (current-column))))
                 ( ;; If "outside" indent to 0
                  (zerop (nth 0 ppss))
                  (css-debug-msg "ZERO")
                  0)
                 ( ;; inside curly brackets
                  (and css-curly-parsed
                       (not block-ending-line)
                       (setq css-parsed-relevant
                             (css-pc-get-relevant
                              css-curly-parsed point))
                       (not (eq (nth 2 css-parsed-relevant)
                                'nested-selector)))
                  (css-debug-msg "C")
                  (+ css-indent-offset
                     (save-excursion
                       (css-go-up)
                       (current-indentation))
                     (if (and (not (equal (line-number-at-pos
                                           (car css-parsed-relevant))
                                          (line-number-at-pos)))
                              (css-pc-inside-statement
                               css-curly-parsed point))
                         css-indent-offset 0)))
                 ( ;; Inside parentheses, closing brackets
                  t
                  (css-debug-msg "P")
                  (+ (save-excursion
                       (css-go-up)
                       (current-indentation))
                     (if block-ending-line
                         0 css-indent-offset))))))
      (error ;; My best error-less guess
       (css-debug-msg "Err")
       (* (car (syntax-ppss))
          css-indent-offset)))))

(defun css-indent-line ()
  "Indent current line according to CSS indentation rules."
  (interactive)
  (save-excursion
    (indent-line-to (css-indent-calculate)))
  (when (< (current-column) (current-indentation))
    (back-to-indentation)))

(defcustom css-indent-offset 4
  "Basic size of one indentation step."
  :version "22.2"
  :type 'integer
  :group 'css)

(defun css-current-defun-name ()
  "Return the name of the CSS section at point, or nil."
  (save-excursion
    (let ((max (max (point-min) (- (point) 1600))))  ; approx 20 lines back
      (when (search-backward "{" max t)
        (skip-chars-backward " \t\r\n")
        (beginning-of-line)
        (if (looking-at "^[ \t]*\\([^{\r\n]*[^ {\t\r\n]\\)")
            (match-string-no-properties 1))))))

(provide 'css-mode)
;;; css-mode.el ends here
