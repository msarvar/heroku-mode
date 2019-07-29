;;; heroku.el -*- lexical-binding: t; -*-
;;; Code:

(defconst heroku--list-sort-key
  '("App" . nil)
  "Sort table on this key.")

(define-infix-argument heroku-transient:--tail ()
      :description "Tail"
      :class 'transient-option
      :shortarg "-t"
      :argument "--tail=")

(define-transient-command heroku-transient ()
  "Heroku Commands"
  ["Arguments"
    ("-f" "Follow" "-f")
    (heroku-transient:--tail)]
  ["Actions"
    ("c" "Config" heroku-get-config)])

(defun heroku-get-config (&optional args)
  "Get Heroku config for an app. Use ARGS to specify arguments."
  (interactive (list (transient-args 'heroku-transient)))
  (let ((process "*heroku*")
        (buffer "*heroku-config*")
        (app (aref (tabulated-list-get-entry) 0)))
    (if (member "-f" args)
      (apply #'start-process process buffer "heroku" "config" args "-a" app))
    (apply #'call-process "heroku" nil buffer nil "config" args "-a" app)
    (switch-to-buffer buffer)))

(defvar heroku-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "l") 'heroku-transient)
    map)
  "Heroku for `heroku-mode'.")

(defun heroku ()
  "Turn on heroku mode."
  (interactive)
  (switch-to-buffer "*heroku*")
  (heroku-mode))

(define-derived-mode heroku-mode tabulated-list-mode "Heroku"
  "Heroku Mode"
  (let ((columns [("App" 100)])
        (rows (cdr (mapcar (lambda (x) `(nil [,x]))
                           (split-string (shell-command-to-string "heroku apps") "\n")))))
    (setq tabulated-list-format columns)
    (setq tabulated-list-entries rows)
    (setq tabulated-list-sort-key heroku--list-sort-key)
    (setq major-mode 'heroku-mode)
    (use-local-map heroku-mode-map)
    (tabulated-list-init-header)
    (tabulated-list-print)
    (run-mode-hooks 'heroku-mode-hook)))

(provide 'heroku)
;;; heroku.el ends here
