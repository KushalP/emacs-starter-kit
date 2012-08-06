;; Ack
(use-package ack-and-a-half
  :init (progn
          (add-to-list 'safe-local-variable-values
                       '(ack-and-a-half-arguments . t)))
  :bind ("C-c C-f" . ack-and-a-half)
  :config (progn
            (add-hook 'eproject-first-buffer-hook (lambda ()
                                                    (when (eproject-root)
                                                      (set (make-local-variable
                                                            'ack-and-a-half-root-directory-functions)
                                                           'ack-and-a-half-root-directory-functions)
                                                      (add-to-list 'ack-and-a-half-root-directory-functions
                                                                   'eproject-root))))))

;; eproject mode
(use-package eproject
  :config (progn
            ;; ruby on rails special type
            (define-project-type ruby-on-rails-git (generic-git)
              (and (look-for ".git")
                   (look-for "Gemfile")
                   (look-for "config/application.rb"))
              :main-file "Gemfile")

            (defun rails-eproject-test (&optional only-this-file)
              "Runs local tests"
              (interactive "P")
              (if (eq only-this-file nil)
                  (compile "bundle exec rake" t)
                (compile (concat "bundle exec ruby -I test " (buffer-file-name)) t)))

            (defun rails-eproject-hook ()
              "Set up some local variables"
              (add-to-list 'safe-local-variable-values '(scss-sass-command . t))

              (set (make-local-variable 'inf-ruby-default-implementation) "bundle-ruby")

              ;; run rake to compile
              (set (make-local-variable 'compile-command) "bundle exec rake")
              (local-set-key (kbd "C-c C-t") 'rails-eproject-test))

            (add-hook 'ruby-on-rails-git-project-file-visit-hook 'rails-eproject-hook)

            (defun bw-eproject-find-files ()
              "If we're in a Git project, use git ls-files to look up the files, because it won't try to open any .gitignored files."
              (interactive)
              (if (member (eproject-type) '(generic-git ruby-on-rails-git))
                  (let ((default-directory (eproject-root)))
                    (find-file
                     (concat
                      (eproject-root)
                      (ido-completing-read
                       (format "Find file: %s" (eproject-root))
                       (split-string (shell-command-to-string "git ls-files"))))))
                (eproject-find-file)))
            (use-package eproject-extras
              :bind ("C-c f" . bw-eproject-find-files))
            (setq eproject-completing-read-function 'eproject--ido-completing-read)))

;; IDO mode
(use-package ido
  :init (ido-mode t)
  :config (progn
            (setq
             ido-enable-prefix nil
             ido-enable-flex-matching t
             ido-create-new-buffer 'always
             ido-use-filename-at-point nil
             ido-max-prospects 20
             ido-case-fold t)))

;; JavaScript
(use-package js2-mode
  :mode ("\\.js\\'" . js2-mode)
  :config (progn
            ;; Camel case is popular in JS
            (subword-mode)

            (setq
             ;; highlight everything
             js2-highlight-level 3
             ;; 4 space indent
             js2-basic-offset 4
             ;; idiomatic closing bracket position
             js2-consistent-level-indent-inner-bracket-p t
             ;; allow for multi-line var indenting
             js2-pretty-multiline-decl-indentation-p t
             ;; Don't highlight missing variables in js2-mode: we have jslint for
             ;; that
             js2-highlight-external-variables nil
             ;; jslint shows missing semi-colons
             js2-strict-missing-semi-warning nil)))

;; JSON files
(use-package json-mode
  :mode ("\\.json\\'" . json-mode))

;; markdown
(use-package markdown-mode
  :mode (("\\.md$" . markdown-mode)
         ("\\.markdown$" . markdown-mode)))

;; Puppet manifests
(use-package puppet-mode
  :mode ("\\.pp$" . puppet-mode))

;; ERB templates
(use-package rhtml-mode
  :mode ("\\.html\\.erb\\'" . rhtml-mode))

;; Ruby mode
(use-package ruby-mode
  :mode (("\\.rabl$" . ruby-mode)
         ("\\.rake$" . ruby-mode)
         ("\\.rb\\'" . ruby-mode)
         ("\\.rhtml\\'" . ruby-mode)
         ("\\.rsel\\'" . ruby-mode)
         ("\\.prawn\\'" . ruby-mode)
         ("[vV]agrantfile$" . ruby-mode)
         ("[gG]emfile$" . ruby-mode)
         ("[rR]akefile$" . ruby-mode))
  :config (progn
            ;; Rails project setup
            (defun eproject-rails-config ()
              "Various settings for Rails projects"

              ;; We don't want to compile SCSS in Rails because the asset pipeline
              ;; does it for us
              (set (make-local-variable 'scss-compile-at-save) nil))

            (add-hook 'ruby-on-rails-project-file-visit-hook 'eproject-rails-config)

            ;; Ruby has a lot of camel case
            (subword-mode)

            ;; this variable is stupid - apparently Ruby needs its own indent
            (setq ruby-indent-level 2)))

;; add a bundle version of IRB shell
(use-package rinari
  :config
  (progn
    (add-to-list 'inf-ruby-implementations
                 '("bundle-ruby" . "bundle exec irb --inf-ruby-mode -r irb/completion"))))

(use-package ruby-test-mode)

;; Stops the annoying jump when moving around.
(use-package smooth-scroll)

;; YAML mode
(use-package yaml-mode
  :mode ("\\.yml$" . yaml-mode))
