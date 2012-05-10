(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages
  '(;; Starter Kit.
    starter-kit
    starter-kit-lisp
    starter-kit-bindings

    ;; UI related.
    smooth-scroll
    color-theme
    color-theme-solarized
    color-theme-twilight

    ;; Misc.
    org
    rvm

    ;; Languages.
    clojure-mode
    clojure-test-mode
    clojurescript-mode
    coffee-mode
    erlang
    feature-mode ;; Cucumber
    go-mode
    haskell-mode
    markdown-mode
    php-mode
    protobuf-mode
    python-mode
    scala-mode
    yaml-mode)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; tabs are 2 spaces
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)

(set-face-attribute
 'default nil
 :font    "Deja-Vu-Sans-Mono-14")

;; Simple function to check if we're on OS X.
(defun osxp () (string= "darwin" system-type))

;; Fix the PATH variable.
(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell (shell-command-to-string "$SHELL -i -c 'echo $PATH'")))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

;; Turn off visual bell for some cases.
(defun visual-bell-reducer ()
  (unless (memq this-command
                '(isearch-abort abort-recursive-edit exit-minibuffer
                                keyboard-quit mwheel-scroll
                                down up
                                next-line previous-line
                                backward-char forward-char))
    (ding)))

(when window-system
  ;; Show line numbers.
  ;; (global-linum-mode f)

  (require 'color-theme)
  (eval-after-load 'color-theme
    (progn
      ;; Solarized theme.
      ;; (require 'color-theme-solarized)
      ;; (load-theme 'solarized-dark t)

      ;; Twilight theme.
      (load-file "~/.emacs.d/elpa/color-theme-twilight-0.1/color-theme-twilight.el")
      (color-theme-twilight)))

  ;; Things to do when we're using the UI on a Mac.
  (when (osxp)
    ;; Rebind the # symbol.
    (fset 'insertPound "#")
    (global-set-key (kbd "M-3") 'insertPound)

    ;; Turn off visual bell when scrolling past EOF.
    (setq ring-bell-function 'visual-bell-reducer))

  ;; Fix the PATH (see called function).
  (set-exec-path-from-shell-PATH)

  ;; Set F7 to toggle fullscreen mode.
  (global-set-key [f7] 'ns-toggle-fullscreen))

;; Stops the annoying jump when moving around.
(require 'smooth-scroll)
