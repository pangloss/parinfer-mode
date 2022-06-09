;;; parinfer-strategies.el --- Strageties -*- lexical-binding: t -*-

;;; Commentary:

;; Entry point: parinfer-strategy-match-p

;;; Code:

(defvar parinfer-strategies '((default)
                              (instantly)
                              (skip))
  "Parinfer invoke strategy.

Use `parinfer-strategy-add' to mark functions as using a
particular strategy. Use `parinfer-strategy-parse' to read
values.

Its elements is like below:

 (STRATEGY-NAME COMMAND COMMAND ...)

Each COMMAND is a symbol or a regexp string used to match commands.

 strategy name    Description
 --------------   -------------------------------------------
 default          Invoke parinfer (delay on large sexp)
 instantly        Invoke parinfer instantly
 skip             Do not invoke parinfer")

(defun parinfer-strategy-parse (strategy-name)
  "Parse strategy, which is named STRATEGY-NAME in `parinfer-strategies'.

Its output is a plist that looks like

 (:commands cmd1 cmd2 cmd3
  :regexps regexp1 regexp2 regexp3)"
  (let ((list (cdr (assq strategy-name parinfer-strategies))))
    (list :commands (cl-remove-if-not #'symbolp list)
          :regexps  (cl-remove-if-not #'stringp list))))

(defun parinfer-strategy-add (strategy &rest commands)
  "Append new commands to STRATEGY in `parinfer-strategy'.

COMMANDS is a list of commands, which may be a symbol or a regexp
string."
  (declare (indent 1))
  (let* ((orig-value (cdr (assq strategy parinfer-strategies)))
         (keys (mapcar #'car parinfer-strategies))
         (new-value (cl-remove-duplicates
                     (append orig-value commands)
                     :test #'equal
                     :from-end t))
         output)
    (dolist (x parinfer-strategies)
      (if (eq (car x) strategy)
          (push (cons strategy new-value) output)
        (push x output)))
    (when (not (memq strategy keys))
      (push (cons strategy new-value) output))
    (setq parinfer-strategies (reverse output))))

(parinfer-strategy-add 'default
  "paredit-"
  'comment-dwim
  'comment-line
  'comment-or-uncomment-region
  'delete-char
  'delete-indentation
  'kill-line
  'kill-region
  'kill-word
  'newline-and-indent
  'evil-shift-left
  'evil-shift-left-line
  'evil-shift-right
  'evil-shift-right-line
  'sp-insert-pair)
(parinfer-strategy-add 'instantly
  'parinfer-double-quote
  'delete-region
  'self-insert-command
  'newline
  'evil-change
  'evil-change-line
  'evil-change-whole-line
  'evil-delete
  'evil-delete-backward-char
  'evil-delete-char
  'evil-delete-line
  'evil-exit-visual-state
  'evil-force-normal-state
  'evil-normal-state
  'evil-paste-after
  'evil-paste-before
  'evil-substitute)
(parinfer-strategy-add 'skip
  'evil-previous-line
  'evil-forward-char
  'evil-backward-char
  'evil-next-line
  'evil-forward-word
  'evil-forward-word-begin
  'evil-backward-word-begin
  'evil-backward-end
  'evil-scroll-page-down
  'evil-scroll-up)

(defun parinfer-strategy-match-p (command strategy-name)
  "Return non-nil if COMMAND's parinfer invoke strategy is STRATEGY-NAME."
  (let* ((output (parinfer-strategy-parse strategy-name))
         (cmds (plist-get output :commands))
         (regexps (plist-get output :regexps)))
    (or (member command cmds)
        (cl-some (lambda (regexp)
                   (string-match-p
                    regexp (symbol-name command)))
                 regexps))))

(provide 'parinfer-strategies)

;;; parinfer-strategies.el ends here
