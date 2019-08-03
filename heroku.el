;;; heroku.el -*- lexical-binding: t; -*-
;;; Code:

(defconst heroku--list-sort-key
  '("App" . nil)
  "Sort table on this key.")

(defconst heroku--list-format
  [("App" 30)
   ("Owner" 30)])

(define-infix-argument heroku-transient:--tail ()
      :description "Tail"
      :class 'transient-option
      :shortarg "-t"
      :argument "--tail=")

(define-transient-command kubel-help-popup ()
  "Heroku Menu")
(define-transient-command heroku-transient ()
  "Heroku Commands"
  ["Arguments"
    ("-f" "Follow" "-f")
    ("-a" "All Apps" "-a")
    (heroku-transient:--tail)]
  ["Actions"
    ("a" "Apps" heroku-list-apps)
    ("c" "Config" heroku-get-config)])

(defun heroku-get-config (&optional args)
  "Get Heroku config for an app. Use ARGS to specify arguments."
  (interactive (list (transient-args 'heroku-transient)))
  (let ((process "*heroku*")
        (buffer "*heroku-config*")
        (app (aref (tabulated-list-get-entry) 0)))
    (if (member "-f" args)
      (apply #'start-process process buffer "heroku" "config" app args))
    (apply #'call-process "heroku" nil buffer nil "config" "-a" app)
    (switch-to-buffer buffer)))

(defun heroku-list-apps ()
  "Create the entries for the service list."
  (interactive)
  (let ((temp (list)))
    (with-temp-buffer
      (insert (shell-command-to-string "heroku apps -A"))
      (goto-char (point-min))
      (while (re-search-forward "^\\([0-9a-zA-Z\-]+\\)\s+\\(.*\\)$" (point-max) t)
        (setq temp (append temp (list (list (match-string 1) (vector (match-string 1) (match-string 2))))))))
    temp))

(defun heroku-get-apps (&optional args)
  "Get a list of Heroku Apps. Use ARGS for arguments."
  (interactive (list (transient-args 'heroku-apps-transient)))
  (let ((temp (list)))
    (if (member "-A" args)
        (setq temp (cdr (mapcar (lambda (x) `(nil [,x]))
                                (split-string (shell-command-to-string "heroku apps -A") "\n"))))
      (setq temp (cdr (mapcar (lambda (x) `(nil [,x]))
                              (split-string (shell-command-to-string "heroku apps -A") "\n")))))
    temp))

        

(defvar heroku-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "l") 'heroku-transient)
   map))

(defun heroku ()
  "Turn on heroku mode."
  (interactive)
  (switch-to-buffer "*heroku*")
  (heroku-mode))

(define-derived-mode heroku-mode tabulated-list-mode "Heroku"
  "Heroku Mode"
  (buffer-disable-undo)
  (kill-all-local-variables)
  (setq truncate-lines t)
  (setq mode-name "Heroku")
  (setq major-mode 'heroku-mode)
  (use-local-map heroku-mode-map)
  (setq tabulated-list-format heroku--list-format)
  (setq tabulated-list-entries 'heroku-list-apps)
  (setq tabulated-list-sort-key heroku--list-sort-key)
  (tabulated-list-init-header)
  (tabulated-list-print)
  (hl-line-mode 1)
  (run-mode-hooks 'heroku-mode-hook))

(provide 'heroku)
;;; heroku.el ends here
