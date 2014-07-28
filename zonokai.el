;; Local Variables:
;; no-bytpe-compile: t
;; eval: (when (fboundp 'rainbow-mode) (rainbow-mode 1))
;; End:



;;; zonokai-theme.el ends here
;;; zonokai-theme.el --- blue based theme for emacs

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
;; my personal touch on the Emacs monokai theme, with more a blue
;; base to the theme, with green and different hues of blue and red
;; for accenting
;;
;;; Code:

(require 'dash)
(require 'color)

(unless (>= 24 emacs-major-version)
  (error "The Zonokai theme requires Emacs 24 or later!"))

(defgroup zonokai nil
  "Zonokai theme options.
The theme will have to be reloded after changing options."
  :group 'faces
  :prefix "zk-")

(defun in-terminal-p ()
  "Return true if in a terminal."
  (not (display-graphic-p)))

(defun identity (x) x)


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
	     (if-complement-s (color &optional fn) (funcall fn color))
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
     (base03:d (color-darken-name base03 3))
     (base02:d (color-darken-name base02 3))
     (base01:d (color-darken-name base01 3))
     (base00:d (color-darken-name base00 3))

	   ;; base color pallet          
	   (yellow    (if-complement "#E2D511"))    
	   (yellow+10 (color-lighten-name yellow 20))
	   (yellow-10 (color-darken-name yellow 20))
	   (orange    (if-complement "#FF5C40"))
	   (orange+10 (color-lighten-name orange 20))
	   (orange-10 (color-darken-name orange 20))
	   (brown     (if-complement "#B26800"))
	   (brown+10  (color-lighten-name brown 20))
	   (brown-10  (color-darken-name brown 20))
	   (red       (if-complement "#CC1514" (lambda (c) (color-darken-name c 30))))
	   (red+10    (color-lighten-name red 20))
	   (red-10    (color-darken-name red 20))
	   (magenta   (if-complement "#E318FF"))
	   (magenta+10 (color-lighten-name magenta 20))
	   (magenta-10  (color-darken-name magenta 20))
	   (violet    (if-complement "#6C71C4"))
	   (violet+10  (color-lighten-name violet 20))
	   (violet-10  (color-darken-name violet 20))
	   (blue      (if-complement "#3D7599"))
	   (blue+10  (color-lighten-name blue 20))
	   (blue-10  (color-darken-name blue 20))
	   (cyan      (if-complement "#00FFDA"))
	   (cyan+10  (color-lighten-name cyan 20))
	   (cyan-10  (color-darken-name cyan 20))
	   (green     (if-complement "#A6E22E"))
	   (green+10  (color-lighten-name green 20))
	   (green-10  (color-darken-name green 20))
	   (dark-gray (if-complement "#444444"))
	   (lite-gray (if-complement "#eeeeee"))


	   ;; helpers
	   (region (color-darken-name base03 10))
	   (builtin orange)
	   ;; is complement it reverts complement color to blue color
	   (comments blue)


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

       `(link                                    ((,class (:foreground ,cyan))))
       `(link-visited                            ((,class (:foreground ,green))))
       `(highlight                               ((,class (:background ,(color-darken-name base03 5)))))
       `(default                                 ((,class (:foreground ,base00 :background ,base03))))
       `(fringe                                  ((,class (:foreground ,base02 :background ,base02))))
       `(shadow                                  ((,class (:foreground ,base01 :background ,base00))))
       `(match                                   ((,class (:background ,base02))))
       `(cursor                                  ((,class (:foreground ,base03 :background ,green :invserse-video t))))
       `(mouse                                   ((,class (:foreground ,base03 :background ,base00 :inverse-video t))))
       `(button                                  ((,class (:background ,base03 :foreground ,green :weight bold :underline t))))
       `(escape-glyph-face                       ((,class (:foreground ,red))))

       `(region                                  ((,class (:background ,region))))
       `(idle-highlight                          ((,class (:foreground ,cyan :background ,blue))))
       `(hl-line                                 ((,class (:background ,base02 :foreground nil))))
       `(widget-field                            ((,class (:background ,(color-darken-name base00 60) :foreground ,(color-lighten-name magenta 10)))))


;;;;;; ido
       `(ido-subdir                              ((,class (:foreground ,green))))
       `(ido-first-match                         ((,class (:foreground ,yellow :weight bold))))
       `(ido-incomplete-regexp                   ((,class (:foreground ,red :underline t :weight bold))))
       `(ido-only-match                          ((,class (:foreground ,cyan :weight bold))))


;;;;;; custom
       `(custom-button                           ((,class (:background ,(color-darken-name base03 40) :foreground ,(color-lighten-name base00 10)))))
       `(custom-button-mouse                     ((,class (:background ,(color-darken-name base00 60) :foreground ,(color-lighten-name cyan 60)))))
       `(custom-button-pressed                   ((,class (:background ,(color-darken-name cyan 10) :foreground ,(color-lighten-name cyan 60)))))
       `(custom-variable-tag                     ((,class (:foreground ,cyan))))
       `(custom-group-tag                        ((,class (:foreground ,blue :weight bold))))
       `(custom-state                            ((,class (:foreground ,orange))))
       `(custom-set                              ((,class (:foreground ,base03 :background ,cyan))))

       `(custom-themed                           ((,class (:background ,blue :foreground ,base01))))
       `(custom-changed                          ((,class (:background ,(color-darken-name blue 20) :foreground ,base01))))
       `(custom-invalid                          ((,class (:background ,red :foreground ,yellow))))
       `(custom-modified                         ((,class (:background ,blue :foreground ,base01))))

       ;;; helper
       `(success                                 ((,class (:foreground ,green))))


       ;; completion
       `(completions-annotations                  ((,class (:foreground ,base00 :italic t))))
       `(completions-common-part                  ((,class (:foreground ,green))))
       `(completions-first-difference             ((,class (:foreground ,cyan+10 :weight bold :underline t))))


       ;; compilation
       `(compilation-warning                     ((,class (:foreground ,red))))
       `(compilation-column-face                 ((,class (:foreground ,cyan :underline nil))))
       `(compilation-column-number               ((,class (:foreground ,base03 :foreground ,cyan))))

       ;; diary
       `(diary                                   ((,class (:foreground ,yellow))))


     ;;; help
       `(help-argument-name                      ((,class (:foreground ,cyan))))

     ;;; webmod
       `(web-mode-html-tag-face                  ((,class (:foreground ,yellow :weight bold :underline t))))


       ;; fontlock
       `(font-lock-builtin-face                  ((,class (:foreground ,orange :weight bold))))
       `(font-lock-comment-delimiter-face        ((,class (:foreground ,comments))))
       `(font-lock-comment-face                  ((,class (:foreground ,comments))))
       `(font-lock-constant-face                 ((,class (:foreground ,yellow))))
       `(font-lock-string-face                   ((,class (:foreground ,magenta  :style :italic))))
       `(font-lock-keyword-face                  ((,class (:foreground ,blue :weight bold))))
       `(font-lock-function-name-face            ((,class (:foreground ,cyan :weight bold))))
       `(font-lock-type-face                     ((,class (:foreground ,cyan))))
       `(font-lock-variable-name-face            ((,class (:foreground ,green))))
       `(font-lock-doc-face                      ((,class (:foreground ,magenta))))
       `(font-lock-warning-face                  ((,class (:foreground ,yellow :background ,base03 :underline  t :weight bold))))

;;;;;; modeline
       `(mode-line                               ((,class (:foreground ,base00 :weight bold :background ,(color-darken-name base03 7) :box (:line-width 1 :color ,(color-darken-name cyan 10))))))
       `(mode-line-buffer-id                     ((,class (:foreground ,green :weight bold))))
       `(mode-line-inactive                      ((,class (:foreground ,base00 :weight bold :background ,(color-darken-name base03 30)))))
       `(mode-line-highlight                     ((,class (:foreground ,cyan :background ,base03))))

       `(secondary-selection                     ((,class (:foreground ,red))))

;;;;;; Enh-ruby
       `(enh-ruby-string-delimiter-face          ((,class (:foreground ,orange :weight bold))))
       `(enh-ruby-op-face                        ((,class (:foreground ,yellow :weight bold))))


;;;;;; rainbow blocks
       `(rainbow-blocks-depth-1-face             ((,class (:foreground ,rb-1))))
       `(rainbow-blocks-depth-2-face             ((,class (:foreground ,rb-2))))
       `(rainbow-blocks-depth-3-face             ((,class (:foreground ,rb-3))))
       `(rainbow-blocks-depth-4-face             ((,class (:foreground ,rb-4))))
       `(rainbow-blocks-depth-5-face             ((,class (:foreground ,rb-5))))
       `(rainbow-blocks-depth-6-face             ((,class (:foreground ,rb-1))))
       `(rainbow-blocks-depth-7-face             ((,class (:foreground ,rb-2))))
       `(rainbow-blocks-depth-8-face             ((,class (:foreground ,rb-3))))
       `(rainbow-blocks-depth-9-face             ((,class (:foreground ,rb-4))))
       `(rainbow-blocks-depth-10-face            ((,class (:foreground ,rb-5))))
       `(rainbow-blocks-depth-11-face            ((,class (:foreground ,rb-1))))
       `(rainbow-blocks-depth-12-face            ((,class (:foreground ,rb-2))))

;;;;;; Rainbow delimiters
       `(rainbow-delimiters-depth-1-face         ((,class (:foreground ,rb-1))))
       `(rainbow-delimiters-depth-2-face         ((,class (:foreground ,rb-2))))
       `(rainbow-delimiters-depth-3-face         ((,class (:foreground ,rb-3))))
       `(rainbow-delimiters-depth-4-face         ((,class (:foreground ,rb-4))))
       `(rainbow-delimiters-depth-5-face         ((,class (:foreground ,rb-5))))
       `(rainbow-delimiters-depth-6-face         ((,class (:foreground ,rb-1))))
       `(rainbow-delimiters-depth-7-face         ((,class (:foreground ,rb-2))))
       `(rainbow-delimiters-depth-8-face         ((,class (:foreground ,rb-3))))
       `(rainbow-delimiters-depth-9-face         ((,class (:foreground ,rb-4))))
       `(rainbow-delimiters-depth-10-face        ((,class (:foreground ,rb-5))))
       `(rainbow-delimiters-depth-11-face        ((,class (:foreground ,rb-1))))
       `(rainbow-delimiters-depth-12-face        ((,class (:foreground ,rb-2))))
       `(rainbow-delimiters-unmatched-face       ((,class (:foreground ,red :background ,base03 :inverse-video t))))
       `(rainbow-delimiters-mismatched-face       ((,class (:foreground ,base00 :background ,base03 :inverse-video t))))


;;;;;; column enforce
       `(column-enforce-face                     ((,class (:foreground ,blue))))

;;;;;; git gutter +
       `(git-gutter+-added                       ((,class (:inherit fringe :foreground ,green :weight bold))))
       `(git-gutter+-deleted                     ((,class (:inherit fringe :foreground ,red :weight bold))))
       `(git-gutter+-modified                    ((,class (:inherit fringe :foreground ,base01 :weight bold))))
       `(git-gutter:added                        ((,class (:inherit git-gutter+-added))))
       `(git-gutter:deleted                      ((,class (:inherit git-gutter+-deleted))))
       `(git-gutter:modified                     ((,class (:inherit git-gutter+-modified))))
       `(git-gutter-fr:added                     ((,class (:inherit git-gutter+-added))))
       `(git-gutter-fr:deleted                   ((,class (:inherit git-gutter+-deleted))))
       `(git-gutter-fr:modified                  ((,class (:inherit git-gutter+-modified))))
       
;;;;;; isearch
       `(isearch                                 ((,class (:foreground ,base03 :background ,green :weight bold))))
       `(isearch-fail                            ((,class (:foreground ,base03 :background ,red :weight bold))))

;;;;;; ace-jump-mode
       `(ace-jump-face-background                ((,class (:foreground ,dark-gray :background ,base03))))
       `(ace-jump-face-foreground                ((,class (:foreground ,cyan :background ,base03 :weight bold))))


;;;;;; fly spell
       `(flyspell-incorrect                      ((,class (:foreground ,red :weight bold :underline t))))



;;;;;; dired
       `(dired-ignored                           ((,class (:foreground ,(color-darken-name base00 60) :background "transparent"))))
       `(dired-directory                         ((,class (:foreground ,cyan :weight normal))))
       `(dired-flagged                           ((,class (:foreground ,red-10))))
       `(dired-header                            ((,class (:foreground ,magenta :background ,base03))))
       `(dired-warning                           ((,class (:foreground ,yellow :underline t :weight bold))))
       `(dired-mark                              ((,class (:foreground ,orange))))
       `(dired-marked                            ((,class (:foreground ,orange+10))))
       `(dired-symlink                           ((,class (:foreground ,blue :weight bold))))
       `(dired-perm-write                        ((,class (:foreground ,blue+10))))

;;;;;; magit
       `(magit-tag                               ((,class (:foreground ,base03 :background ,green))))
       `(git-commit-summary-face                 ((,class (:foreground ,magenta))))
       `(git-commit-branch-face                  ((,class (:foreground ,cyan))))
       `(git-commit-comment-file-face            ((,class (:foreground ,green))))
       `(git-commit-comment-heading-face         ((,class (:foreground ,orange))))
       `(magit-diff-none                         ((,class (:foreground ,base02 :background ,base03:d))))
       `(magit-diff-del                          ((,class (:foreground ,red+10 :background ,red-10))))
       `(magit-diff-add                          ((,class (:foreground ,green+10 :background ,green-10))))
       `(magit-section-title                     ((,class (:foreground ,green :background ,base03 :weight bold))))
       `(magit-log-sha1                          ((,class (:foreground ,cyan))))
       `(magit-branch                            ((,class (:foreground ,magenta :background ,base03))))
       `(magit-key-mode-header-face              ((,class (:foreground ,green))))
       `(magit-key-mode-button-face              ((,class (:foreground ,yellow))))
       `(magit-item-highlight                    ((,class (:background ,(color-darken-name base03 2) :foreground ,cyan))))

;;;;;; erm
       `(erm-sym-errline                         ((,class (:foreground ,cyan :background ,orange))))

;;;;;; rhtml-mode
       `(erb-out-delim-face                      ((,class (:foreground ,magenta+10 :background ,base03:d))))
       `(erb-out-face                            ((,class (:inherit erb-out-delim-face :foreground ,blue+10))))
       `(erb-exec-delim-face                      ((,class (:foreground ,red+10 :background ,base03:d))))
       `(erb-exec-face                           ((,class (:inherit erb-exec-delim-face))))


;;;;;; js2
       `(js2-function-call                       ((,class (:foreground ,cyan))))

;;;;;; sp pair overlay face
       `(sp-pair-overlay-face                    ((,class (:background ,base03))))


;;;;;; header line
       `(header-line                             ((,class (:background ,base02 :foreground ,base00))))

;;;;;; Compnay
       `(company-tooltip                         ((,class (:foreground ,base01:d :background ,base03:d))))
       `(company-tooltip-selection               ((,class (:foreground ,base02 :background ,base01))))
       `(company-tooltip-mouse                   ((,class (:background ,cyan-10))))
       `(company-tooltip-common                  ((,class (:foreground ,cyan-10 :background ,base03:d ))))
       `(company-tooltip-common-selection        ((,class (:background ,base01:d :foreground ,magenta :weight bold :underline t))))
       `(company-scrollbar-fg                    ((,class (:background ,blue+10))))
       `(company-scrollbar-bg                    ((,class (:background ,blue-10))))
       `(company-preview                         ((,class (:background ,cyan+10))))
       `(company-preview-common                  ((,class (:background ,base03))))
       `(company-echo-common                     ((,class (:foreground ,yellow))))




;;;;;; eshell
       `(eshell-prompt                           ((,class (:foreground ,yellow :weight bold))))
       `(eshell-ls-archive                       ((,class (:foreground ,red :weight bold))))
       `(eshell-ls-backup                        ((,class (:inherit font-lock-comment-face))))
       `(eshell-ls-clutter                       ((,class (:inherit font-lock-comment-face))))
       `(eshell-ls-directory                     ((,class (:foreground ,blue :weight bold))))
       `(eshell-ls-executable                    ((,class (:foreground ,red :weight bold))))
       `(eshell-ls-unreadable                    ((,class (:foreground ,base00))))
       `(eshell-ls-missing                       ((,class (:inherit font-lock-warning-face))))
       `(eshell-ls-product                       ((,class (:inherit font-lock-doc-face))))
       `(eshell-ls-special                       ((,class (:foreground ,yellow :weight bold))))
       `(eshell-ls-symlink                       ((,class (:foreground ,cyan :weight bold))))


;;;;;; smartparens
       `(sp-show-pair-mismatch-face              ((,class (:foreground ,red :background ,base03 :weight bold))))
       `(sp-show-pair-match-face                 ((,class (:background ,base03 :weight bold))))


;;;;;; undo-tree
       `(undo-tree-visualizer-active-branch-face ((,class (:foreground ,base00 :weight bold))))
       `(undo-tree-visualizer-current-face       ((,class (:foreground ,red :weight bold))))
       `(undo-tree-visualizer-default-face       ((,class (:foreground ,base00))))
       `(undo-tree-visualizer-register-face      ((,class (:foreground ,yellow))))
       `(undo-tree-visualizer-unmodified-face    ((,class (:foreground ,cyan))))

;;;;;; emmet mode
       `(emmet-preview-input                     ((,class (:foreground ,cyan :background ,(color-darken-name base03 10)))))



;;;;;; minibuffer promt
       `(minibuffer-prompt                       ((,class (:foreground ,green))))

;;;;;; window number
       `(window-number-face                      ((,class (:inherit mode-line :background ,base02 :box nil))))
       


;;;;;; whitespace-mode
       `(whitespace-space                        ((,class (:background ,base03 :foreground ,base03))))
       `(whitespace-hspace                       ((,class (:background ,base03 :foreground ,base03))))
       `(whitespace-tab                          ((,class (:background ,(color-lighten-name base03 2)))))
       `(whitespace-newline                      ((,class (:foreground ,base03))))
       `(whitespace-trailing                     ((,class (:background ,(color-darken-name magenta 20)))))
       `(whitespace-line                         ((,class (:background ,base03 :foreground ,magenta))))
       `(whitespace-space-before-tab             ((,class (:background ,orange :foreground ,orange))))
       `(whitespace-indentation                  ((,class (:background ,yellow :foreground ,red))))
       `(whitespace-empty                        ((,class (:background ,yellow))))
       `(whitespace-space-after-tab              ((,class (:background ,yellow :foreground ,red))))))))

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



