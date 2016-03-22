;;; zonokai.el --- blue based theme for emacs

;; Copyright (C) 2013-2014

;; Author: Alex Sanchez <ar.sanchez@keythings.co>
;; URL: http://github.com/ZehCnaS34/zonokai-emacs.git
;; Version: 20140523.2151
;; X-Original-Version: 20140310.1330
;; X-Original-Version: 0.2.0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; A blue theme for emacs. Inspired by monokai but took a different lot turns
;; along the development path.
;;
;;; Code:


(require 'cl-lib)
(require 'dash)
(require 'color)

(unless (<= 24 emacs-major-version)
  (error "The Zonokai theme requires Emacs 24 or later!"))

(defgroup zonokai nil
  "Zonokai theme options.
The theme will have to be reloded after changing options."
  :group 'faces
  :prefix "zk-")

(defcustom zk-mode-line-box nil
  "the cyan outline around the modeline"
  :type 'boolean
  :group 'zonokai)

(defcustom zk-string-italics nil
  "Toggle if strings have italics"
  :type 'boolean
  :group 'zonokai)

(defcustom zk-distinct-fringe t
  "Make the fringe the same color as the active mode-line"
  :type 'boolean
  :group 'zonokai)

(defun in-terminal-p ()
  "Return true if in a terminal."
  (not (display-graphic-p)))


(defun do-times (v)
  "Helper function for rainbow-* stuff. Not implemented yet"
  (cl-labels ((t (n output)
                 (cond ((= n 0) output)
                       ((> n 0) (d (- n 1) (cons n output))))))
    (t v '())))

;; (defun cycle (xs elts)
;;   (cl-labels ((x (s e output)
;;                  (cond ((= e 0) output)
;;                        ((> e 0) (x (cons (car (last s))
;;                                          (- 1 elts)
;;                                          (cons (car s) output)))))))
;;     (x xs elts '())))



(defun quick-color (face fg &optional bg)
  "A function that generation a font face list
so instead of writting a lot, I could do something like
\",(quick-color 'font-lock blue)\" or I could also pass in
background color"
  (let ((asdf t))
    (if bg
        `(,face ((t (:foreground ,fg :background ,bg))))
      `(,face ((t (:foreground ,fg)))))))

(defun create-zonokai-theme (complement theme-name)
  "Create the zonokai theme.
Takes an optional `FRAME' as reference."
  (cl-flet* ((complement (color) (color-complement-hex color))
             (if-complement-s
              (color &optional fn) (if complement
                                       (if fn
                                           (funcall fn color)
                                         color)
                                     color))
             (if-complement
              (color &optional fn) (let ((complement-color (color-complement-hex color)))
                                     (if (not complement)
                                         color
                                       (if fn
                                           (funcall fn complement-color)
                                         complement-color)))))
    (let* ((class '((class color) (min-colors 256)))
           ;; background tones
           (base03    (if-complement "#011827"))
           (base02    (if-complement "#023658"))
           ;; content tones
           (base01    (if-complement "#c6c6c6"))
	   (base00    (if-complement "#eee"   ))

     ;; base derived tones
     (base03:d  (color-darken-name base03 3))
     (base03:dd (color-darken-name base03:d 6))
     (base02:d  (color-darken-name base02 3))
     (base02:dd (color-darken-name base02:d 6))
     (base01:d  (color-darken-name base01 3))
     (base01:dd (color-darken-name base01:d 6))
     (base00:d  (color-darken-name base00 3))
     (base00:dd (color-darken-name base00:d 6))

	   ;; base color pallet
	   (yellow    (if-complement "#E2D511"))
	   (yellow+10 (color-lighten-name yellow   20))
	   (yellow-10 (color-darken-name yellow    20))

	   (orange    (if-complement "#FF5C40"))
	   (orange+10 (color-lighten-name orange   20))
	   (orange-10 (color-darken-name orange    20))

	   (brown     (if-complement "#B26800"))
	   (brown+10  (color-lighten-name brown    20))
	   (brown-10  (color-darken-name brown     20))

	   (red       (if-complement "#CC1514" (lambda (c) (color-darken-name c 30))))
	   (red+10    (color-lighten-name red      20))
	   (red-10    (color-darken-name red       20))

	   (magenta   (if-complement "#E318FF"))
	   (magenta+10 (color-lighten-name magenta 20))
	   (magenta-10  (color-darken-name magenta 20))

	   (violet    (if-complement "#6C71C4"))
	   (violet+10  (color-lighten-name violet  20))
	   (violet-10  (color-darken-name violet   20))

	   (blue      (if-complement "#3D7599"))
	   (blue+10  (color-lighten-name blue      20))
	   (blue-10  (color-darken-name blue       20))

	   (cyan      (if-complement "#00FFDA"))
	   (cyan+10  (color-lighten-name cyan      20))
	   (cyan-10  (color-darken-name cyan       20))

	   (green     (if-complement "#A6E22E"))
	   (green+10  (color-lighten-name green    20))
	   (green-10  (color-darken-name green     20))

	   (dark-gray (if-complement "#444444"))
	   (lite-gray (if-complement "#eeeeee"))

	   ;; helpers
	   (region (if (not (in-terminal-p))
                 base03:dd
               base02))                        ;; in terminal
	   (builtin orange)
	   ;; is complement it reverts complement color to blue color
	   (comments  (if (not (in-terminal-p))
                    (if-complement-s blue+10 (lambda (c) blue-10)) ;; in gui
                 blue))                                           ;; in terminal
     (strings   (if-complement-s magenta (lambda (c) green-10)))


     (mode-line (if (not (in-terminal-p))
                    base03:dd
                  blue))

     (linum     (if (not (in-terminal-p))
                    (if (not complement)
                        base01:dd
                      base02:dd)
                  blue))

	   ;; rainbow scheme
	   (rb-1 green-10)
	   (rb-2 magenta-10)
	   (rb-3 orange-10)
	   (rb-4 red-10)
	   (rb-5 cyan-10)
	   )

      (custom-theme-set-faces
       theme-name

       ;; basic coloring

       `(link                                    ((t (:foreground ,cyan))))
       `(link-visited                            ((t (:foreground ,green))))
       `(highlight                               ((t (:background ,(color-darken-name base03 5)))))
       `(default                                 ((t (:foreground ,base00 :background ,base03))
                                                  (,class (:foreground ,base00 :background "black"))))
       `(fringe                                  ((t (:foreground ,base00 :background ,(if zk-distinct-fringe base03:d base03)))))
       `(linum                                   ((t (:inherit     fringe :foreground ,linum))))
       `(shadow                                  ((t (:foreground ,blue-10 :background ,base03))))
       `(match                                   ((t (:background ,base02))))
       `(cursor                                  ((t (:foreground ,base03 :background ,green :invserse-video t))))
       `(mouse                                   ((t (:foreground ,base03 :background ,base00 :inverse-video t))))
       `(button                                  ((t (:background ,base03 :foreground ,green :weight bold :underline t))))
       `(escape-glyph-face                       ((t (:foreground ,red))))
       `(region                                  ((t (:background ,region :foreground ,base00:d))))
       `(idle-highlight                          ((t (:foreground ,cyan :background ,blue))))
       `(hl-line                                 ((t (:background ,base02 :foreground nil))))
       `(widget-field                            ((t (:background ,(color-darken-name base00 60) :foreground ,(color-lighten-name magenta 10)))))
       `(variable-pitch                          ((t (:font-family "monospace"))))

       `(vhl/default-face                        ((t (:background ,base00:dd :foreground ,base03:dd))))


;;;;;; ido
       `(ido-subdir                              ((t (:foreground ,green))))
       `(ido-first-match                         ((t (:foreground ,yellow :weight bold))))
       `(ido-incomplete-regexp                   ((t (:foreground ,red :underline t :weight bold))))
       `(ido-only-match                          ((t (:foreground ,cyan :weight bold))))

;;;;;; Smart Mode Line



;;;;;; custom
       `(custom-button                           ((t (:background ,(color-darken-name base03 40) :foreground ,(color-lighten-name base00 10)))))
       `(custom-button-mouse                     ((t (:background ,(color-darken-name base00 60) :foreground ,(color-lighten-name cyan 60)))))
       `(custom-button-pressed                   ((t (:background ,(color-darken-name cyan 10) :foreground ,(color-lighten-name cyan 60)))))
       `(custom-variable-tag                     ((t (:foreground ,cyan))))
       `(custom-group-tag                        ((t (:foreground ,blue :weight bold))))
       `(custom-state                            ((t (:foreground ,orange))))
       `(custom-set                              ((t (:foreground ,base03 :background ,cyan))))

       `(custom-themed                           ((t (:background ,blue :foreground ,base01))))
       `(custom-changed                          ((t (:background ,(color-darken-name blue 20) :foreground ,base01))))
       `(custom-invalid                          ((t (:background ,red :foreground ,yellow))))
       `(custom-modified                         ((t (:background ,blue :foreground ,base01))))

       ;;; helper
       `(success                                 ((t (:foreground ,green))))


       ;; completion
       `(completions-annotations                  ((t (:foreground ,base00 :italic t))))
       `(completions-common-part                  ((t (:foreground ,green))))
       `(completions-first-difference             ((t (:foreground ,cyan+10 :weight bold :underline t))))


       ;; compilation
       `(compilation-warning                     ((t (:foreground ,red))))
       `(compilation-column-face                 ((t (:foreground ,cyan :underline nil))))
       `(compilation-column-number               ((t (:foreground ,base03 :foreground ,cyan))))

       ;; diary
       `(diary                                   ((t (:foreground ,yellow))))


     ;;; help
       `(help-argument-name                      ((t (:foreground ,cyan))))

     ;;; webmod
       `(web-mode-html-tag-face                  ((t (:foreground ,yellow :weight bold :underline t))))


       ;; fontlock
       `(font-lock-builtin-face                  ((t (:foreground ,orange :weight bold))))
       `(font-lock-comment-delimiter-face        ((t (:foreground ,comments))))
       `(font-lock-comment-face                  ((t (:foreground ,comments))))
       `(font-lock-constant-face                 ((t (:foreground ,yellow))))
       `(font-lock-string-face                   ((t (:foreground ,strings :italics ,zk-string-italics))))
       `(font-lock-keyword-face                  ((t (:foreground ,blue :weight bold))))
       `(font-lock-function-name-face            ((t (:foreground ,cyan :weight bold))))
       `(font-lock-type-face                     ((t (:foreground ,cyan))))
       `(font-lock-variable-name-face            ((t (:foreground ,green))))
       `(font-lock-doc-face                      ((t (:inherit font-lock-string-face))))
       `(font-lock-warning-face                  ((t (:foreground ,yellow :background ,base03 :underline  t :weight bold))))
       `(font-lock-regexp-grouping-construct ((t (:foreground ,yellow :weight bold))))
       `(font-lock-regexp-grouping-backslash ((t (:foreground ,green :weight bold))))
       `(font-lock-negation-char-face ((t (:foreground ,yellow :weight bold))))
       `(font-lock-preprocessor-face ((t (:foreground ,blue-10))))



;;;;;; modeline
       `(mode-line                               ((t (:background  ,mode-line :foreground ,base00 :box ,(if zk-mode-line-box `(:line-width 1 :color ,cyan-10) nil)))))
       `(mode-line-buffer-id                     ((t (:foreground ,green :weight bold))))
       `(mode-line-inactive                      ((t (:inherit mode-line :background ,base03:d :box nil))))
       `(mode-line-highlight                     ((t (:foreground ,cyan :background ,base03))))
       `(secondary-selection                     ((t (:foreground ,red))))

;;;;;; Enh-ruby
       `(enh-ruby-string-delimiter-face          ((t (:foreground ,orange :weight bold))))
       `(enh-ruby-op-face                        ((t (:foreground ,yellow :weight bold))))


;;;;;; rainbow blocks
       `(rainbow-blocks-depth-1-face             ((t (:foreground ,rb-1))))
       `(rainbow-blocks-depth-2-face             ((t (:foreground ,rb-2))))
       `(rainbow-blocks-depth-3-face             ((t (:foreground ,rb-3))))
       `(rainbow-blocks-depth-4-face             ((t (:foreground ,rb-4))))
       `(rainbow-blocks-depth-5-face             ((t (:foreground ,rb-5))))
       `(rainbow-blocks-depth-6-face             ((t (:foreground ,rb-1))))
       `(rainbow-blocks-depth-7-face             ((t (:foreground ,rb-2))))
       `(rainbow-blocks-depth-8-face             ((t (:foreground ,rb-3))))
       `(rainbow-blocks-depth-9-face             ((t (:foreground ,rb-4))))
       `(rainbow-blocks-depth-10-face            ((t (:foreground ,rb-5))))
       `(rainbow-blocks-depth-11-face            ((t (:foreground ,rb-1))))
       `(rainbow-blocks-depth-12-face            ((t (:foreground ,rb-2))))

;;;;;; Rainbow delimiters
       `(rainbow-delimiters-depth-1-face         ((t (:foreground ,rb-1))))
       `(rainbow-delimiters-depth-2-face         ((t (:foreground ,rb-2))))
       `(rainbow-delimiters-depth-3-face         ((t (:foreground ,rb-3))))
       `(rainbow-delimiters-depth-4-face         ((t (:foreground ,rb-4))))
       `(rainbow-delimiters-depth-5-face         ((t (:foreground ,rb-5))))
       `(rainbow-delimiters-depth-6-face         ((t (:foreground ,rb-1))))
       `(rainbow-delimiters-depth-7-face         ((t (:foreground ,rb-2))))
       `(rainbow-delimiters-depth-8-face         ((t (:foreground ,rb-3))))
       `(rainbow-delimiters-depth-9-face         ((t (:foreground ,rb-4))))
       `(rainbow-delimiters-depth-10-face        ((t (:foreground ,rb-5))))
       `(rainbow-delimiters-depth-11-face        ((t (:foreground ,rb-1))))
       `(rainbow-delimiters-depth-12-face        ((t (:foreground ,rb-2))))
       `(rainbow-delimiters-unmatched-face       ((t (:foreground ,red :background ,base03 :inverse-video t))))
       `(rainbow-delimiters-mismatched-face      ((t (:foreground ,base00 :background ,base03 :inverse-video t))))


;;;;;; column enforce
       `(column-enforce-face                     ((t (:foreground ,blue))))

;;;;;; git gutter +
       `(git-gutter+-added                       ((t (:inherit fringe :foreground ,green :weight bold))))
       `(git-gutter+-deleted                     ((t (:inherit fringe :foreground ,red :weight bold))))
       `(git-gutter+-modified                    ((t (:inherit fringe :foreground ,base01 :weight bold))))
       `(git-gutter:added                        ((t (:inherit git-gutter+-added))))
       `(git-gutter:deleted                      ((t (:inherit git-gutter+-deleted))))
       `(git-gutter:modified                     ((t (:inherit git-gutter+-modified))))
       `(git-gutter-fr:added                     ((t (:inherit git-gutter+-added))))
       `(git-gutter-fr:deleted                   ((t (:inherit git-gutter+-deleted))))
       `(git-gutter-fr:modified                  ((t (:inherit git-gutter+-modified))))

;;;;;; isearch
       `(isearch                                 ((t (:foreground ,base03 :background ,green :weight bold))))
       `(isearch-fail                            ((t (:foreground ,base03 :background ,red :weight bold))))

;;;;;; ace-jump-mode
       `(ace-jump-face-background                ((t (:foreground ,dark-gray :background ,base03))))
       `(ace-jump-face-foreground                ((t (:foreground ,cyan :background ,base03 :weight bold))))


;;;;;; fly spell
       `(flyspell-incorrect                      ((t (:foreground ,red :weight bold :underline t))))



;;;;;; dired
       `(dired-ignored                           ((t (:foreground ,(color-darken-name base00 60) :background "transparent"))))
       `(dired-directory                         ((t (:foreground ,cyan :weight normal))))
       `(dired-flagged                           ((t (:foreground ,red-10))))
       `(dired-header                            ((t (:foreground ,magenta :background ,base03))))
       `(dired-warning                           ((t (:foreground ,yellow :underline t :weight bold))))
       `(dired-mark                              ((t (:foreground ,orange))))
       `(dired-marked                            ((t (:foreground ,orange+10))))
       `(dired-symlink                           ((t (:foreground ,cyan+10 :weight bold))))
       `(dired-perm-write                        ((t (:foreground ,cyan+10))))

;;;;;; magit
       `(magit-tag                               ((t (:foreground ,base03 :background ,green))))
       `(git-commit-summary-face                 ((t (:inherit font-lock-string-face))))
       `(git-commit-branch-face                  ((t (:foreground ,cyan))))
       `(git-commit-comment-file-face            ((t (:foreground ,green))))
       `(git-commit-comment-heading-face         ((t (:foreground ,orange))))


       `(magit-diff-none                         ((t (:foreground ,base02 :background ,base03:d))))
       `(magit-diff-del                          ((t (:foreground ,red+10 :background ,red-10))))
       `(magit-diff-add                          ((t (:foreground ,green+10 :background ,green-10))))
       `(magit-section-title                     ((t (:foreground ,green :background ,base03 :weight bold))))
       `(magit-log-sha1                          ((t (:foreground ,cyan))))
       `(magit-branch                            ((t (:foreground ,magenta :background ,base03))))
       `(magit-key-mode-header-face              ((t (:foreground ,green))))
       `(magit-key-mode-button-face              ((t (:foreground ,yellow))))
       `(magit-item-highlight                    ((t (:background ,(color-darken-name base03 2) :foreground ,cyan))))


       `(magit-log-reflog-label-commit           ((t (:background ,cyan-10 :foreground ,base03 :box (:line-width 1 :color ,cyan+10)) )))
       `(magit-log-reflog-label-checkout         ((t (:background ,green-10 :foreground ,base03 :box (:line-width 1 :color ,green+10)))))
       `(magit-log-reflog-label-merge            ((t (:background ,magenta-10 :foreground ,base03 :box (:line-width 1 :color ,magenta+10)))))

;;;;;; egg
       `(agg-branch                              ((t (:inherit egg-header :foreground ,yellow :height 1.1))))
       `(egg-diff-del                            ((t (:inherit magit-diff-del))))
       `(egg-diff-add                            ((t (:inherit magit-diff-add))))
       `(egg-diff-conflict                       ((t (:background ,magenta-10 :foreground ,magenta+10))))
       `(egg-help-key                            ((t (:foreground ,yellow+10))))
       `(egg-text-help                           ((t (:foreground ,blue))))
       `(egg-help-header-1                       ((t (:inherit egg-text-base :foreground ,blue))))
       `(egg-help-header-2                       ((t (:inherit egg-text-1 :foreground ,cyan-10 :height .9))))
       `(egg-section-title                       ((t (:inherit egg-header :height 1.1 :foreground ,green))))
       `(egg-diff-file-header                    ((t (:inherit egg-header :foreground ,magenta-10))))
       `(egg-log-HEAD-name                       ((t (:inherit egg-branch-mono :box (:line-width 1 :color ,base00) :foreground ,yellow+10))))



;;;;;; erm
       `(erm-sym-errline                         ((t (:foreground ,base03:dd :background ,orange-10))))

;;;;;; rhtml-mode
       `(erb-out-delim-face                      ((t (:foreground ,magenta+10 :background ,base03:d))))
       `(erb-out-face                            ((t (:inherit erb-out-delim-face :foreground ,blue+10))))
       `(erb-exec-delim-face                      ((t (:foreground ,red+10 :background ,base03:d))))
       `(erb-exec-face                           ((t (:inherit erb-exec-delim-face))))
;;;;;; js2
       `(js2-function-call                       ((t (:foreground ,cyan))))
;;;;;; sp pair overlay face
       `(sp-pair-overlay-face                    ((t (:background ,base03))))
;;;;;; header line
       `(header-line                             ((t (:background ,base02 :foreground ,base00))))
;;;;;; tooling
       `(tooltip                                 ((t (:background ,cyan-10 :foreground ,base03:dd))))
;;;;;; Compnay
       `(company-tooltip                         ((t (:foreground ,base01:d :background ,base03:d))))
       `(company-tooltip-selection               ((t (:foreground ,base02 :background ,base01))))
       `(company-tooltip-mouse                   ((t (:background ,cyan-10))
                                                  (,class (:background ,cyan-10))))
       `(company-tooltip-common                  ((t (:foreground ,cyan-10 :background ,base03:d ))))
       `(company-tooltip-common-selection        ((t (:background ,base01:d :foreground ,magenta :weight bold :underline t))))
       `(company-scrollbar-fg                    ((t (:background ,blue+10))))
       `(company-scrollbar-bg                    ((t (:background ,blue-10))))
       `(company-preview                         ((t (:background ,cyan+10 :foreground ,base02:dd))))
       `(company-preview-common                  ((t (:background ,base03))))
       `(company-echo-common                     ((t (:foreground ,yellow))))



;;;;;; ediff
       `(ediff-current-diff-Ancestor             ((t (:foreground ,base03 :background ,magenta))))
       ;;; going to be ediff stuff here


;;;;;; eshell
       `(eshell-prompt                           ((t (:foreground ,yellow :weight bold))))
       `(eshell-ls-archive                       ((t (:foreground ,red :weight bold))))
       `(eshell-ls-backup                        ((t (:inherit font-lock-comment-face))))
       `(eshell-ls-clutter                       ((t (:inherit font-lock-comment-face))))
       `(eshell-ls-directory                     ((t (:foreground ,blue :weight bold))))
       `(eshell-ls-executable                    ((t (:foreground ,red :weight bold))))
       `(eshell-ls-unreadable                    ((t (:foreground ,base00))))
       `(eshell-ls-missing                       ((t (:inherit font-lock-warning-face))))
       `(eshell-ls-product                       ((t (:inherit font-lock-doc-face))))
       `(eshell-ls-special                       ((t (:foreground ,yellow :weight bold))))
       `(eshell-ls-symlink                       ((t (:foreground ,cyan :weight bold))))


;;;;;; smartparens
       `(sp-show-pair-mismatch-face              ((t (:foreground ,red :background ,base03 :weight bold))))
       `(sp-show-pair-match-face                 ((t (:background ,base03 :weight bold))))


;;;;;; undo-tree
       `(undo-tree-visualizer-active-branch-face ((t (:foreground ,base00 :weight bold))))
       `(undo-tree-visualizer-current-face       ((t (:foreground ,red :weight bold))))
       `(undo-tree-visualizer-default-face       ((t (:foreground ,base00))))
       `(undo-tree-visualizer-register-face      ((t (:foreground ,yellow))))
       `(undo-tree-visualizer-unmodified-face    ((t (:foreground ,cyan))))

;;;;;; emmet mode
       `(emmet-preview-input                     ((t (:foreground ,cyan :background ,(color-darken-name base03 10)))))



;;;;;; minibuffer promt
       `(minibuffer-prompt                       ((t (:foreground ,green))))

;;;;;; window number
       `(window-number-face                      ((t (:inherit mode-line :background ,base02 :box nil))))


;;;; helm
       `(helm-M-x-key                            ((t (:foreground ,yellow :underline t))))
       `(helm-source-header                      ((t (:foreground ,base03 :background ,blue+10))))
       `(helm-selection                          ((t (:foreground ,base02:d :background ,green))))
       `(helm-header                             ((t (:foreground ,base00 :background ,blue-10 :height 1.2))))
       `(helm-candidate-number                   ((t (:inherit mode-line :background ,cyan-10))))


;;;;;; highlight indentation face
       `(highlight-indentation-current-column-face ((t (:background ,base03:d))))
       `(highlight-indentation-face              ((t (:background ,base03:d))))

;;;;;; julia
       `(julia-macro-face                        ((t (:foreground ,yellow+10))))
       `(julia-quoted-symbol-face                ((t (:foreground ,red+10))))


;;;;;; whitespace-mode
       `(whitespace-space                        ((t (:background ,base03 :foreground ,base03))))
       `(whitespace-hspace                       ((t (:background ,base03 :foreground ,base03))))
       `(whitespace-tab                          ((t (:background ,(color-lighten-name base03 2)))))
       `(whitespace-newline                      ((t (:foreground ,base03))))
       `(whitespace-trailing                     ((t (:background ,(color-darken-name magenta 20)))))
       `(whitespace-line                         ((t (:background ,base03 :foreground ,magenta))))
       `(whitespace-space-before-tab             ((t (:background ,orange :foreground ,orange))))
       `(whitespace-indentation                  ((t (:background ,yellow :foreground ,red))))
       `(whitespace-empty                        ((t (:background ,yellow))))
       `(whitespace-space-after-tab              ((t (:background ,yellow :foreground ,red))))))))

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))


;; Local Variables:
;; no-bytpe-compile: t
;; eval: (when (fboundp 'rainbow-mode) (rainbow-mode 1))
;; fill-column: 95
;; End:

;; red theme development
(deftheme zonokai-red "Red version of zonokai")
(create-zonokai-theme t 'zonokai-red)
(provide-theme 'zonokai-red)

;; blue theme development
(deftheme zonokai-blue "Blue version of zonokai")
(create-zonokai-theme nil 'zonokai-blue)
(provide-theme 'zonokai-blue)


(provide 'zonokai)
;;; zonokai.el ends here
