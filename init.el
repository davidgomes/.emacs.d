;; Configuration file by David Gomes (github.com/davidgomes)
;; Many of the files included in my configuration were not created by me.

;; Load plugins
(add-to-list 'load-path "~/.emacs.d/")

;; No splash screen
(setq inhibit-splash-screen t)

;; Indentation and tab-related stuff goes here
(setq-default indent-tabs-mode nil)
(setq c-basic-indent 2)
(setq tab-width 4)
(setq indent-tabs-mode nil)
(global-set-key (kbd "RET") 'newline-and-indent)

;; Trailing white space
;; (add-hook 'before-save-hook 'delete-trailing-whitespace)
;; (highlight-trailing-whitespace)

;; Don't add newline in end of file
(setq require-final-newline nil)

;; Lines and columns
(global-linum-mode 1)
;;(hlinum-activate t)
;; (global-hl-line-mode 1)
(column-number-mode 1)

;; Modern shortcuts
(cua-mode 1)

;; Enable recent files and disable both backup and autosave files
(recentf-mode 1)
(setq make-backup-files nil)
(setq auto-save-default nil)

;; Set up auto complete
(add-to-list 'load-path "/home/david/.emacs.d/")
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "/home/david/.emacs.d/ac-dict")
(ac-config-default)

;; Redo +
;;(require 'redo+)
;;(global-set-key (kbd "C-y") 'redo)

;; Vala Mode
(autoload 'vala-mode "vala-mode" "Major mode for editing Vala code." t)
(add-to-list 'auto-mode-alist '("\\.vala$" . vala-mode))
(add-to-list 'file-coding-system-alist '("\\.vala$" . utf-8))

;; Javascript stuff
(setq js-indent-level 2)
(autoload 'javascript-mode "javascript-mode" "Javascript editing mode." t)
(add-to-list 'auto-mode-alist '("\\.js$" . js-mode))
(add-to-list 'interpreter-mode-alist '("js" . js-mode))

;; CSS stuff
(setq css-indent-offset 2)

;; Lua Mode
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))
(setq lua-indent-level 2)
(defun lua-abbrev-mode-off () (abbrev-mode 0))
(add-hook 'lua-mode-hook 'lua-abbrev-mode-off)
(eval-after-load "auto-complete" '(add-to-list 'ac-modes 'lua-mode))

;; PHP Mode
(autoload 'php-mode "php-mode.el" "Php mode." t)
(setq auto-mode-alist (append '(("/*.\.php[345]?$" . php-mode)) auto-mode-alist))

;; Haxe Mode
(autoload 'php-mode "haxe-mode.el" "Haxe mode." t)
(setq auto-mode-alist (append '(("/*.\.hx?$" . haxe-mode)) auto-mode-alist))

;; C++ Mode
(c-set-offset 'access-label '-2)
(c-set-offset 'inclass '4)
(setq c-default-style "bsd"
      c-basic-offset 2)

;; Spit vertically
(setq split-height-threshold nil)
(setq split-width-threshold nil)

;; Smooth scrolling
(setq mouse-wheel-scroll-amount '(3 ((shift) . 3))) ; One line at a time
(setq mouse-wheel-progressive-speed nil)            ; Don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't)                  ; Scroll window under mouse
(setq scroll-step 1)                                ; Keyboard scroll one line at a time
(setq scroll-margin 4)                              ; Always 4 lines above/below cursor

(column-number-mode 1)
(show-paren-mode 1)
(size-indication-mode 1)

;; Highlight cool words
(require 'fixme-mode)
(fixme-mode 1)

;; Fix copy/paste between X applications and emacs
(setq x-select-enable-clipboard t)
(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)

;; Go fullscreen!
(defun toggle-fullscreen ()
  (interactive)
  (set-frame-parameter nil 'fullscreen (if (global-set-key (kbd "<f11>") 'toggle-fullscreen))))

;; Deactivate menu-bar, tool-bar and scroll-bar
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Fix all indentation (I love this so much)
(defun fix-indentation ()
  "indent whole buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

;; Bind my favourite shortcuts
(global-unset-key (kbd "C-s"))
(global-unset-key (kbd "C-b"))
(global-unset-key (kbd "C-x C-b"))
(global-unset-key (kbd "C-x C-z"))
(global-unset-key (kbd "C-x o"))
(global-unset-key (kbd "C-x 0"))
(global-unset-key (kbd "C-x 2"))
(global-unset-key (kbd "C-x 3"))
(global-unset-key (kbd "<f10>"))
(global-unset-key (kbd "C-w"))
(global-unset-key (kbd "C-t"))

(global-set-key (kbd "C-s") 'save-buffer)
(global-set-key (kbd "C-f") 'isearch-forward)
(global-set-key (kbd "C-d") (lambda () (interactive) (comment-or-uncomment-region (line-beginning-position) (line-end-position))))
(global-set-key (kbd "C-k") 'kill-whole-line)
(global-set-key (kbd "C-l") 'goto-line)
(global-set-key (kbd "<f11>") 'toggle-fullscreen)
(global-set-key (kbd "<C-tab>") 'next-multiframe-window)
(global-set-key (kbd "<C-S-iso-lefttab>") 'previous-multiframe-window)
(global-set-key (kbd "C-0") 'delete-window)
(global-set-key (kbd "C-2") 'split-window-below)
(global-set-key (kbd "C-3") 'split-window-right)
(global-set-key (kbd "<f12>") 'delete-trailing-whitespace)
(global-set-key (kbd "<f10>") 'fix-indentation)
(global-set-key (kbd "C-x C-h") (lookup-key global-map (kbd "C-h")))
(global-set-key (kbd "C-h") 'replace-string)
(global-set-key (kbd "C-t") 'anything)

;; Save my desktop, please.
(desktop-save-mode 1)

;; ERC Expand
(add-hook 'window-configuration-change-hook '(lambda () (setq erc-fill-column (- (window-width) 2))))

;; Set my default browser
(setq gnus-button-url 'browse-url-generic browse-url-generic-program "firefox" browse-url-browser-function gnus-button-url)

;; Appearance settings
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "DejaVu Sans Mono" :foundry "xos4" :slant normal :weight normal :height 100 :width normal)))))

(load-theme 'tango-dark t)
;;(set-fringe-style 'no-fringes)

;; Clear the eshell
(defun eshell/clear ()
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)))

;; Emacs compile/recompile
(global-set-key (kbd "<f9>") 'recompile)

;; Edited linum-update-window that adds a bit of padding to the right between linum and the text
(defun linum-update-window (win)
  "Update line numbers for the portion visible in window WIN."
  (goto-char (window-start win))
  (let ((line (line-number-at-pos))
        (limit (window-end win t))
        (fmt (cond ((stringp linum-format) linum-format)
                   ((eq linum-format 'dynamic)
                    (let ((w (length (number-to-string
                                      (count-lines (point-min) (point-max))))))
                      (concat " %" (number-to-string w) "d ")))))
        (width 0))
    (run-hooks 'linum-before-numbering-hook)
    ;; Create an overlay (or reuse an existing one) for each
    ;; line visible in this window, if necessary.
    (while (and (not (eobp)) (<= (point) limit))
      (let* ((str (if fmt
                      (propertize (format fmt line) 'face 'linum)
                    (funcall linum-format line)))
             (visited (catch 'visited
                        (dolist (o (overlays-in (point) (point)))
                          (when (equal-including-properties
                                 (overlay-get o 'linum-str) str)
                            (unless (memq o linum-overlays)
                              (push o linum-overlays))
                            (setq linum-available (delq o linum-available))
                            (throw 'visited t))))))
        (setq width (max width (length str)))
        (unless visited
          (let ((ov (if (null linum-available)
                        (make-overlay (point) (point))
                      (move-overlay (pop linum-available) (point) (point)))))
            (push ov linum-overlays)
            (overlay-put ov 'before-string
                         (propertize " " 'display `((margin left-margin) ,str)))
            (overlay-put ov 'linum-str str))))
      ;; Text may contain those nasty intangible properties, but that
      ;; shouldn't prevent us from counting those lines.
      (let ((inhibit-point-motion-hooks t))
        (forward-line))
      (setq line (1+ line)))
    (set-window-margins win width (cdr (window-margins win)))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365"
                              "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6"
                              "09c2fd812c3c046379d84beb4795db273da1fe84b008dfbb4c03f54a10cf7f0e"
                              "c1f0d3ec9563620ede55e0631ed3e959bcb619d5276b546f4ca1534f5c8db450"
                              "ac69b7e2e928dc1560d5a556043c99d8cb0614c30957bd03dfe82be4a9e917ee" default)))
 '(ecb-options-version "2.40"))

;; IBuffer is one of the best things about Emacs
(require 'ibuf-ext)
(add-to-list 'ibuffer-never-show-predicates "^\\*Messages")
(add-to-list 'ibuffer-never-show-predicates "^\\*Completions")
(global-set-key (kbd "C-b") 'ibuffer)
(kill-buffer "*scratch*")

;; Don't run default.el
;; (setq inhibit-default-init t)
(visual-line-mode -1)

;; Add ELPA package repository
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

;; For some reason I need this for ECB to work
(setq stack-trace-on-error t)

;; Disable ECB tip of the day
(setq ecb-tip-of-the-day nil)
(global-font-lock-mode 1)
(setq ecb-layout-name "left12")

;; Put minimap on the right
(setq minimap-window-location 'right)
