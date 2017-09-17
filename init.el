;; Configuration file by David Gomes (github.com/davidgomes)
;; Many of the files included in my configuration were not created by me.

(package-initialize)

;; Load plugins
(add-to-list 'load-path "~/.emacs.d/lisp/")
(add-to-list 'custom-theme-load-path "~/.emacs.d/lisp")

;; No splash screen
(setq inhibit-splash-screen t)

;; Indentation and tab-related stuff goes here
(setq-default indent-tabs-mode nil)
(setq c-basic-indent 2)
(setq tab-width 4)
(setq indent-tabs-mode nil)
(global-set-key (kbd "RET") 'newline-and-indent)

;; Don't add newline in end of file
(setq require-final-newline nil)

;; Lines, columns and other things
(global-linum-mode 1)
(column-number-mode 1)
(show-paren-mode 1)
(size-indication-mode 1)
(menu-bar-mode 1)
(scroll-bar-mode 0)
(tool-bar-mode 0)
(setq ns-use-native-fullscreen nil)
(setq-default show-trailing-whitespace t)

;; Modern keyboard shortcuts
(cua-mode 1)
;; Enable recent files and disable both backup and autosave files
(recentf-mode 1)
(setq make-backup-files nil)
(setq auto-save-default nil)

;; Set up auto complete
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "/Users/david/.emacs.d/ac-dict/")
(ac-config-default)
(global-auto-complete-mode t)

;; Redo +
;(require 'redo+)
;(global-set-key (kbd "C-y") 'redo)

;; Vala Mode
(autoload 'vala-mode "vala-mode" "Major mode for editing Vala code." t)
(add-to-list 'auto-mode-alist '("\\.vala$" . vala-mode))
(add-to-list 'file-coding-system-alist '("\\.vala$" . utf-8))

;; Javascript stuff
(setq js-indent-level 4)
(setq jsx-indent-level 4)
(setq js2-indent-switch-body t)
(autoload 'javascript-mode "javascript-mode" "Javascript editing mode." t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'interpreter-mode-alist '("js" . js2-mode))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("aea30125ef2e48831f46695418677b9d676c3babf43959c8e978c0ad672a7329" "5a39d2a29906ab273f7900a2ae843e9aa29ed5d205873e1199af4c9ec921aaab" "2b2188dc6e921b700377f408990741a1dd6d37307bccc9e451dbfa957d5378e1" "83c5800f7946a3a996dfd46860076e3878184adc412b78162b6e293c791b7be5" "6dbeb8cf8dccee0e5ff5699713b8b19a1d28c5c4a4a510ca56322ba81ef510f2" "3ff96689086ebc06f5f813a804f7114195b7c703ed2f19b51e10026723711e33" "ba9be9caf9aa91eb34cf11ad9e8c61e54db68d2d474f99a52ba7e87097fa27f5" "8aa7eb0cc23931423f719e8b03eb14c4f61aa491e5377073d6a55cba6a7bc125" "d3501680958e5ab7e641c70050576fbd72ac5902eaba370fae9c35925c319b46" "3b0a350918ee819dca209cec62d867678d7dac74f6195f5e3799aa206358a983" "579e9950513524d8739e08eae289419cfcb64ed9b7cc910dd2e66151c77975c4" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "0c311fb22e6197daba9123f43da98f273d2bfaeeaeb653007ad1ee77f0003037" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "6ace5b419f7997a7879ba299aa55ca783b5d60e505d7364ee358be4e94c6c73d" "1157a4055504672be1df1232bed784ba575c60ab44d8e6c7b3800ae76b42f8bd" "7f1263c969f04a8e58f9441f4ba4d7fb1302243355cb9faecb55aec878a06ee9" "8afd01e30f62250645c6d68dce8e3581b606aad57292ea88c3ae1bd37fc29285" "569dc84822fc0ac6025f50df56eeee0843bffdeceff2c1f1d3b87d4f7d9fa661" "73fe242ddbaf2b985689e6ec12e29fab2ecd59f765453ad0e93bc502e6e478d6" "f0ea6118d1414b24c2e4babdc8e252707727e7b4ff2e791129f240a2b3093e32" "2b5aa66b7d5be41b18cc67f3286ae664134b95ccc4a86c9339c886dfd736132d" "18d91d95e20450b0cdab4d7eed600e80c22cc7a4153a87989daa5a1c5aff3b83" "99cbc2aaa2b77374c2c06091494bd9d2ebfe6dc5f64c7ccdb36c083aff892f7d" "2affb26fb9a1b9325f05f4233d08ccbba7ec6e0c99c64681895219f964aac7af" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" "09c2fd812c3c046379d84beb4795db273da1fe84b008dfbb4c03f54a10cf7f0e" "c1f0d3ec9563620ede55e0631ed3e959bcb619d5276b546f4ca1534f5c8db450" "ac69b7e2e928dc1560d5a556043c99d8cb0614c30957bd03dfe82be4a9e917ee" default)))
 '(js-indent-level 4)
 '(js2-basic-offset 4)
 '(js2-bounce-indent-p t)
 '(js2-indent-switch-body t t)
 '(js2-strict-trailing-comma-warning nil)
 '(package-selected-packages
   (quote
    (magit handlebars-mode web-mode go-mode markdown-mode base16-theme sunshine cider rjsx-mode auto-complete jsx-mode helm-projectile js2-mode nyan-mode helm gist gccsense fuzzy f elpy django-mode crosshairs clojure-mode)))
 '(safe-local-variable-values
   (quote
    ((eval when
           (fboundp
            (quote rainbow-mode))
           (rainbow-mode 1)))))
 '(send-mail-function (quote mailclient-send-it)))

;; HTML/Web stuff
(add-to-list 'auto-mode-alist '("\\.html$" . web-mode))
(add-to-list 'interpreter-mode-alist '("html" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
(setq web-mode-markup-indent-offset 4)

(setq web-mode-content-types-alist
      '(("jsx" . "/some/path/.*\\.js[x]\\'")))

(add-hook 'web-mode-hook
          (lambda ()
            (web-mode-set-content-type "jsx")
            (message "now set to: %s" web-mode-content-type)))

;; CSS stuff
(setq css-indent-offset 4)

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

;; Haxe stuff (using actionscript-mode because haxe-mode doesn't seem to work)
(autoload 'actionscript-mode "actionscript-mode" "Major mode for actionscript." t)
(add-to-list 'auto-mode-alist '("\\.hx$" . actionscript-mode))

;; C++ Mode
;; (c-set-offset 'access-label '-2)
(c-set-offset 'inclass '2)
(setq c-default-style "bsd"
      c-basic-offset 2)

;; Go Mode
(add-hook 'go-mode-hook
          (lambda ()
            (setq tab-width 4)))

;; Spit widths
(setq split-height-threshold nil)
(setq split-width-threshold nil)

;; Smooth scrolling
(setq mouse-wheel-scroll-amount '(3 ((shift) . 3)))
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-follow-mouse 't)
(setq scroll-step 1)
(setq scroll-margin 4)

;; Highlight those important words
(require 'fixme-mode)
(fixme-mode 1)

;; Fix all indentation (I love this so much)
(defun fix-indentation ()
  "indent whole buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

;; Bind my favourite shortcuts
(global-unset-key (kbd "C-x C-b"))
(global-unset-key (kbd "C-x C-z"))
(global-unset-key (kbd "C-x o"))
(global-unset-key (kbd "C-x 0"))
(global-unset-key (kbd "C-x 2"))
(global-unset-key (kbd "C-x 3"))
(global-unset-key (kbd "<f10>"))
(global-unset-key (kbd "C-w"))
(global-unset-key (kbd "C-t"))
(global-unset-key (kbd "C-x C-f"))

(global-set-key (kbd "C-f") 'isearch-forward)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-d") (lambda () (interactive) (comment-or-uncomment-region (line-beginning-position) (line-end-position))))
(global-set-key (kbd "C-k") 'kill-whole-line)
(global-set-key (kbd "C-l") 'goto-line)
;; (global-set-key (kbd "<f11>") 'toggle-fullscreen)
(global-set-key (kbd "<C-tab>") 'next-multiframe-window)
(global-set-key (kbd "<C-S-iso-lefttab>") 'previous-multiframe-window)
(global-set-key (kbd "s-0") 'delete-window)
(global-set-key (kbd "s-2") 'split-window-below)
(global-set-key (kbd "s-3") 'split-window-right)
(global-set-key (kbd "<f12>") 'delete-trailing-whitespace)
(global-set-key (kbd "<f10>") 'fix-indentation)
(global-set-key (kbd "C-j") 'replace-string)
(global-set-key (kbd "s-t") 'helm-projectile)
(global-set-key (kbd "s-T") 'helm-for-files)
(global-set-key (kbd "s-b") 'helm-mini)
(global-set-key (kbd "<f9>") 'recompile)
;; (global-set-key (kbd "C-x k") 'kill-this-buffer)
(global-set-key (kbd "C-w") 'execute-extended-command)
(global-set-key (kbd "s-F") 'projectile-grep)
(global-set-key (kbd "<f6>") (lambda () (interactive) (toggle-frame-fullscreen) (toggle-frame-fullscreen)))

;; From http://ergoemacs.org/emacs/emacs_kill-ring.html
(defun my-delete-word (arg)
  "Delete characters forward until encountering the end of a word.
With argument, do this that many times.
This command does not push text to `kill-ring'."
  (interactive "p")
  (delete-region
   (point)
   (progn
     (forward-word arg)
     (point))))

(defun my-backward-delete-word (arg)
  "Delete characters backward until encountering the beginning of a word.
With argument, do this that many times.
This command does not push text to `kill-ring'."
  (interactive "p")
  (my-delete-word (- arg)))

(global-set-key (kbd "<C-backspace>") 'my-backward-delete-word)

;; Save my desktop Emacs, please.
(desktop-save-mode 1)

;; Appearance settings generated by Emacs
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Menlo" :foundry "xos4" :slant normal :weight normal :height 150 :width normal)))))

(load-theme 'bliss t)

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

;; I have no idea of what this is


;; IBuffer is one of the best things about Emacs
(require 'ibuf-ext)
(add-to-list 'ibuffer-never-show-predicates "^\\*Messages")
(add-to-list 'ibuffer-never-show-predicates "^\\*Completions")
(global-set-key (kbd "s-B") 'ibuffer)
(kill-buffer "*scratch*")

;; Add some package repositories
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

;; Put minimap on the right
(setq minimap-window-location 'right)

;; List all packages
(setq required-packages
      '(nyan-mode auto-complete popup crosshairs vline col-highlight vline hl-line+ django-mode elpy yasnippet pyvenv highlight-indentation find-file-in-project company f dash s find-file-in-project fuzzy gccsense gist gh logito pcache go-mode handlebars-mode haskell-mode helm-flycheck helm async flycheck let-alist pkg-info epl dash dash highlight-indentation hl-line+ idomenu iedit jabber jinja2-mode js2-mode json-mode json-snatcher json-reformat json-reformat json-snatcher jsx-mode less-css-mode let-alist logito lua-mode magit magit-popup dash git-commit with-editor dash dash with-editor dash magit-popup dash moe-theme nav nose paredit pcache pkg-info epl popup powerline processing-mode pyvenv quack s sass-mode haml-mode scheme-complete scss-mode solarized-theme dash vimrc-mode vline web-mode with-editor dash yasnippet noctilux-theme helm-projectile neotree))

;; Installs missing packages
(defun install-missing-packages ()
  "Installs required packages that are missing"
  (interactive)
  (mapc (lambda (package)
          (unless (package-installed-p package)
                  (package-install package)))
        required-packages)
  (message "Installed all missing packages!"))

;; A fix for Emacs on OS X and inserting brackets
(setq mac-option-modifier nil
      x-select-enable-clipboard t)

;; Another fix for Emacs on OS X, stop the beeping on scrolling
(setq ring-bell-function #'ignore)

;; Assembler for MIPS uses #, not ;
(add-hook 'asm-mode-set-comment-hook
          '(lambda ()
             (setq asm-comment-char ?#)))

;; No more typing the whole yes or no. Just y or n will do.
(fset 'yes-or-no-p 'y-or-n-p)

;; Haskell Mode Indentation
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

;; Define location of gnutls-cli on OS X, instaled via brew
(setq starttls-use-gnutls t
      starttls-gnutls-program "/usr/local/bin/gnutls-cli"
      starttls-extra-arguments nil)

;; Automatically refresh files when changed on disk
(global-auto-revert-mode t)

(global-ede-mode 1)

(projectile-global-mode)

;; http://andrewjamesjohnson.com/suppressing-ad-handle-definition-warnings-in-emacs/
(setq ad-redefinition-action 'accept)

(nyan-mode)

(setq sql-mysql-program "/usr/local/bin/mysql")
(setq sql-user "root")
(setq sql-password "")
(setq sql-server "172.17.0.2")
