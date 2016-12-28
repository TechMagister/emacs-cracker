;;; cracker.el --- code completion, goto-definition and docs browsing for Crystal via cracker  -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:
(require 'crystal-mode)
(require 'company)
(require 'json)


(defgroup cracker nil
  "Code completion, goto-definition and docs browsing for Crystal via cracker."
  :link '(url-link "https://github.com/TechMagister/emacs-racer/")
  :group 'crystal-mode)

(defcustom cracker-cmd
  (or (executable-find "cracker")
      "/usr/local/bin/cracker")
  "Path to the cracker binary."
  :type 'file
  :group 'cracker)

(defcustom cracker-crystal-src-path
  "/opt/crystal/src"
  "Path to the crystal source code."
  :type 'folder
  :group 'cracker
  )

(defun company-cracker--format-meta (candidate)
  (let ((name (plist-get candidate :name))
        (file (plist-get candidate :file))
        (location (plist-get candidate :location))
        (type (plist-get candidate :type))
        )
    (if (string-equal type "Function")
        (progn
          (setq split (split-string name "#"))
          (if (eq (length split) 1)
              (setq split (split-string name "\\."))
            )
          (nth 1 split)
          )
      name
      )
    )
  )

(defun company-cracker--invoke-autocomplete ()
    (let ((temp-buffer (generate-new-buffer "*cracker*")))
      (prog2
          (apply #'call-process-region
                 (point-min)
                 (+ (point) 1)
                 cracker-cmd
                 nil
                 temp-buffer
                 nil
                 '("client" "--context"))
          (with-current-buffer temp-buffer (buffer-string))
        (kill-buffer temp-buffer)
        ))
    )



(defun company-cracker--make-candidate (candidate)
  "Prepare and format CANDIDATE."
  (setq name (plist-get candidate :name))
  (setq type (plist-get candidate :type))
  (setq contents (if (string-equal type "Function")
                     (progn
                       (setq split (split-string name "#"))
                       (if (eq (length split) 1)
                           (setq split (split-string name "\\."))
                         )
                       (nth 1 split)
                       )
                   name
                   ))
  (setq contents (nth 0 (split-string contents "(")))
  (if (and (string-equal type "Function")
           (not (string-match "()" name)))
      (setq contents (concat contents "("))
      )
  (propertize contents
              'meta (company-cracker--format-meta candidate))
  )

(defun company-cracker--candidates ()
  (let ((json-object-type 'plist))
    (setq raw (json-read-from-string
                  (company-cracker--invoke-autocomplete))))
  (setq results (append (plist-get raw :results) nil))
  (setq final (list))
  (dolist (candidate results)
    (push (company-cracker--make-candidate candidate) final)
    )
  final
  )

(defun company-cracker--prefix ()
  "Return the symbol to complete.
Also, if point is on a dot, triggers a completion immediately."
      (company-grab-symbol-cons "\\." 1)
      )

(defun company-cracker--annotation (meta)
  "Do some stuff with META."
  (print meta)
  (if (string-match "()" meta)
      (if (string-match ":" meta)
          (concat ":" (nth 1 (split-string meta ":")))
        ""
        )
    (if (string-match "(" meta)
        (nth 1 (split-string meta "("))
      ""
      )
    )
  )

;; do stuff
(defun company-cracker-backend (command &optional arg &rest ignored)
  (interactive (list 'interactive))

  (case command
    (interactive (company-begin-backend 'company-cracker-backend))
    (prefix (and (eq major-mode 'crystal-mode)
                 (not (company-in-string-or-comment))
                 (or (company-cracker--prefix) 'stop)
                 ))
    (meta (get-text-property 1 'meta arg))
    (candidates (company-cracker--candidates) )
    (annotation (company-cracker--annotation (get-text-property 0 'meta arg)))
    )
  )

(add-to-list 'company-backends 'company-cracker-backend)


(provide 'cracker)
;;; cracker.el ends here
