;; Local Variables:
;; no-bytpe-compile: t
;; eval: (when (fboundp 'rainbow-mode) (rainbow-mode 1))
;; End:



;;; zonokai-theme.el ends here
;;; zonokai-theme.el --- blue based theme for emacs

;; Copyright (C) 2013-2014

;; Author: Alex Sanchez <ar.sanchez@keythings.co>
;; URL: http://github.com/ZehCnaS34/zonokai.git
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

(defcustom zk-compliment nil
  "Converts all colors to their complimental colors"
  :group 'zonokai
  :options '(t nil)
  :type 'boolean)

(deftheme zonokai "The zonokai color theme")

(setq zk-compliment nil)

(defun in-terminal-p ()
  "Return true if in a terminal."
  (not (display-graphic-p)))

(defun if-compliment (color)
  (if zk-compliment
      (color-desaturate-name (color-darken-name  (color-complement-hex color) 10) 10)
    color))

(defun create-zonokai-theme ()
  "Create the zonokai theme.
Takes an optional `FRAME' as reference."
  (let* ((class '((class color) (min-colors 256)))

         ;;; background tones 
         (base03    (if-compliment "#011827"))
         (base02    (if-compliment "#023658"))

         ;; content tones
         (base01    (if-compliment "#c6c6c6"))
         (base00    (if-compliment "#eee"))

         ;; accent tones
         (yellow    (if-compliment "#E2D511"))         
         (orange    (if-compliment "#FF5C40"))
         (brown     (if-compliment "#B26800"))
         (red       (if-compliment "#CC1514"))
         (magenta   (if-compliment "#E318FF"))
         (violet    (if-compliment "#6C71C4"))
         (blue      (if-compliment "#3D7599"))
         (cyan      (if-compliment "#00FFDA"))
         (green     (if-compliment "#A6E22E"))

         
         (dark-gray (if-compliment "#444444"))
         (lite-gray (if-compliment "#eeeeee"))

         ;; BG color
         (zk-hl       "#096BAA")
         (zk-fg       base00)
         (zk-bg       base03)
         (zk-hl-line  (color-darken-name zk-bg 5))
         (zk-emph     "#2F157F")
         (zk-comments (if-compliment "#00A1FF"))
         (zk-cursor   green)
         (zk-region   (color-darken-name zk-bg 10))

         ;; Accented colors

         ;; rainbow colors
         (rb-1 "#FF400D")
         (rb-2 "#008EFF")
         (rb-3 "#E80C6A")
         (rb-4 "#570CF8")
         (rb-5 "#AA309F"))

    (custom-theme-set-faces
     'zonokai

     ;; basic coloring
     `(highlight                               ((,class (:background ,(color-darken-name zk-bg 5)))))
     `(default                                 ((,class (:foreground ,zk-fg :background ,zk-bg))))
     `(fringe                                  ((,class (:foreground ,zk-comments))))
     `(shadow                                  ((,class (:foreground ,zk-comments :background ,zk-fg))))
     `(match                                   ((,class (:background ,zk-hl))))
     `(cursor                                  ((,class (:foreground ,zk-bg :background ,zk-cursor :invserse-video t))))
     `(mouse                                   ((,class (:foreground ,zk-bg :background ,zk-fg :inverse-video t))))
     `(button                                  ((,class (:background ,zk-bg :foreground ,green :weight bold :underline t))))
     `(escape-glyph-face                       ((,class (:foreground ,red))))
     `(fringe                                  ((,class (:foreground ,zk-fg :background ,zk-bg))))
     `(region                                  ((,class (:background ,zk-region))))
     `(idle-highlight                          ((,class (:foreground ,cyan :background ,blue))))
     `(hl-line                                 ((,class (:background ,zk-hl-line :foreground nil))))
     `(widget-field                            ((,class (:background ,(color-darken-name zk-fg 60) :foreground ,(color-lighten-name magenta 10)))))


;;;;;; ido
     `(ido-subdir                              ((,class (:foreground ,green))))
     `(ido-first-match                         ((,class (:foreground ,yellow :weight bold))))
     `(ido-incomplete-regexp                   ((,class (:foreground ,red :underline t :weight bold))))
     `(ido-only-match                          ((,class (:foreground ,cyan :weight bold))))


;;;;;; custom
     `(custom-button                           ((,class (:background ,(color-darken-name zk-bg 40) :foreground ,(color-lighten-name green 10)))))
     `(custom-button-mouse                     ((,class (:background ,(color-darken-name zk-fg 60) :foreground ,(color-lighten-name cyan 60)))))
     `(custom-button-pressed                   ((,class (:background ,(color-darken-name cyan 10) :foreground ,(color-lighten-name cyan 60)))))
     `(custom-variable-tag                     ((,class (:foreground ,cyan))))


     ;; compilation
     `(compilation-column-face                 ((,class (:foreground ,cyan :underline nil))))
     `(compilation-column-number               ((,class (:foreground ,zk-bg :foreground ,cyan))))

     ;; diary
     `(diary                                   ((,class (:foreground ,yellow))))


     ;;; help
     `(help-argument-name                      ((,class (:foreground ,cyan))))

     ;;; webmod
     `(web-mode-html-tag-face                  ((,class (:foreground ,yellow :weight bold :underline t))))


     ;; fontlock
     `(font-lock-builtin-face                  ((,class (:foreground ,orange :weight bold))))
     `(font-lock-comment-delimiter-face        ((,class (:foreground ,zk-comments))))
     `(font-lock-comment-face                  ((,class (:foreground ,zk-comments))))
     `(font-lock-constant-face                 ((,class (:foreground ,yellow))))
     `(font-lock-string-face                   ((,class (:foreground ,magenta  :style :italic))))
     `(font-lock-keyword-face                  ((,class (:foreground ,blue :weight bold))))
     `(font-lock-function-name-face            ((,class (:foreground ,cyan :weight bold))))
     `(font-lock-type-face                     ((,class (:foreground ,cyan))))
     `(font-lock-variable-name-face            ((,class (:foreground ,green))))
     `(font-lock-doc-face                      ((,class (:foreground ,magenta))))
     `(font-lock-warning-face                  ((,class (:foreground ,yellow :background ,zk-bg :underline  t :weight bold))))

;;;;;; modeline
     `(mode-line                               ((,class (:foreground ,zk-fg :weight bold :background ,(color-darken-name zk-bg 7) :box (:line-width 1 :color ,(color-darken-name cyan 10))))))
     `(mode-line-buffer-id                     ((,class (:foreground ,green :weight bold))))
     `(mode-line-inactive                      ((,class (:foreground ,zk-fg :weight bold :background ,(color-darken-name zk-bg 30)))))

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
     `(rainbow-delimiters-unmatched-face       ((,class (:foreground ,zk-fg :background ,zk-bg :inverse-video t))))


;;;;;; column enforce
     `(column-enforce-face                     ((,class (:foreground ,blue))))

;;;;;; git gutter +
     `(git-gutter+-modified                    ((,class (:foreground ,magenta  :background ,zk-bg))))
     `(git-gutter+-added                       ((,class (:foreground ,green  :background ,zk-bg))))
     `(git-gutter+-deleted                     ((,class (:foreground ,red   :background ,zk-bg))))

;;;;;; git gutter
     `(git-gutter:modified                     ((,class (:foreground ,magenta  :background ,zk-bg))))
     `(git-gutteradded                         ((,class (:foreground ,green  :background ,zk-bg))))
     `(git-gutter:deleted                      ((,class (:foreground ,red  :background ,zk-bg))))
     `(git-gutter-fr:added                     ((,class (:foreground ,green  :weight bold))))
     `(git-gutter-fr:deleted                   ((,class (:foreground ,red :weight bold))))
     `(git-gutter-fr:modified                  ((,class (:foreground ,magenta :weight bold))))
     
;;;;;; isearch
     `(isearch                                 ((,class (:foreground ,zk-bg :background ,green :weight bold))))
     `(isearch-fail                            ((,class (:foreground ,zk-bg :background ,red :weight bold))))

;;;;;; ace-jump-mode
     `(ace-jump-face-background                ((,class (:foreground ,dark-gray :background ,zk-bg :inverse-video nil :weight bold))))
     `(ace-jump-face-foreground                ((,class (:foreground ,red :background ,zk-bg :inverse-video nil :weight bold))))


;;;;;; fly spell
     `(flyspell-incorrect                      ((,class (:foreground ,red :weight bold :underline t))))



;;;;;; dired
     `(dired-ignored                           ((,class (:foreground ,(color-darken-name zk-fg 60) :background "transparent"))))
     `(dired-directory                         ((,class (:foreground ,cyan :weight normal))))
     `(dired-flagged                           ((,class (:foreground ,red))))
     `(dired-header                            ((,class (:foreground ,magenta :background ,zk-bg))))

;;;;;; magit
     `(magit-tag                               ((,class (:foreground ,zk-bg :background ,green))))
     `(git-commit-summary-face                 ((,class (:foreground ,magenta))))
     `(git-commit-branch-face                  ((,class (:foreground ,cyan))))
     `(git-commit-comment-file-face            ((,class (:foreground ,green))))
     `(git-commit-comment-heading-face         ((,class (:foreground ,orange))))
     `(magit-diff-none                         ((,class (:foreground ,(color-lighten-name zk-bg 25) :background ,(color-darken-name zk-bg 5)))))
     `(magit-diff-del                          ((,class (:foreground ,(color-lighten-name red 25) :background ,(color-darken-name red 25)))))
     `(magit-diff-add                          ((,class (:foreground ,(color-lighten-name green 25) :background ,(color-darken-name green 25)))))
     `(magit-section-title                     ((,class (:foreground ,green :background ,zk-bg :weight bold))))
     `(magit-log-sha1                          ((,class (:foreground ,cyan))))
     `(magit-branch                            ((,class (:foreground ,magenta :background ,zk-bg))))
     `(magit-key-mode-header-face              ((,class (:foreground ,green))))
     `(magit-key-mode-button-face              ((,class (:foreground ,yellow))))
     `(magit-item-highlight                    ((,class (:background ,(color-darken-name zk-bg 2) :foreground ,cyan))))

;;;;;; erm
     `(erm-sym-errline                         ((,class (:foreground ,cyan :background ,orange))))

;;;;;; rhtml-mode
     `(erb-out-delim-face                      ((,class (:foreground ,orange :background ,(color-darken-name zk-bg 5)))))
     `(erb-out-face                            ((,class (:background ,(color-darken-name zk-bg 5) :foreground ,yellow))))
     `(js2-function-call                       ((,class (:foreground ,cyan))))

;;;;;; sp pair overlay face
     `(sp-pair-overlay-face                    ((,class (:background ,zk-bg))))


;;;;;; Compnay
     `(company-tooltip                         ((,class (:foreground ,zk-fg :background ,(color-darken-name zk-bg 10)))))
     `(company-tooltip-selection               ((,class (:foreground ,dark-gray :background ,cyan))))
     `(company-tooltip-mouse                   ((,class (:background ,(color-darken-name red 30)))))
     `(company-tooltip-common                  ((,class (:foreground ,green ))))
     `(company-tooltip-common-selection        ((,class (:background ,(color-darken-name blue 10)))))
     `(company-scrollbar-fg                    ((,class (:background ,(color-darken-name cyan 20)))))
     `(company-scrollbar-bg                    ((,class (:background ,(color-darken-name cyan 40)))))
     `(company-preview                         ((,class (:background ,red))))
     `(company-preview-common                  ((,class (:background ,zk-bg))))

;;;;;; eshell
     `(eshell-prompt                           ((,class (:foreground ,yellow :weight bold))))
     `(eshell-ls-archive                       ((,class (:foreground ,red :weight bold))))
     `(eshell-ls-backup                        ((,class (:inherit font-lock-comment-face))))
     `(eshell-ls-clutter                       ((,class (:inherit font-lock-comment-face))))
     `(eshell-ls-directory                     ((,class (:foreground ,blue :weight bold))))
     `(eshell-ls-executable                    ((,class (:foreground ,red :weight bold))))
     `(eshell-ls-unreadable                    ((,class (:foreground ,zk-fg))))
     `(eshell-ls-missing                       ((,class (:inherit font-lock-warning-face))))
     `(eshell-ls-product                       ((,class (:inherit font-lock-doc-face))))
     `(eshell-ls-special                       ((,class (:foreground ,yellow :weight bold))))
     `(eshell-ls-symlink                       ((,class (:foreground ,cyan :weight bold))))


;;;;;; smartparens
     `(sp-show-pair-mismatch-face              ((,class (:foreground ,red :background ,zk-bg :weight bold))))
     `(sp-show-pair-match-face                 ((,class (:background ,zk-bg :weight bold))))


;;;;;; undo-tree
     `(undo-tree-visualizer-active-branch-face ((,class (:foreground ,zk-fg :weight bold))))
     `(undo-tree-visualizer-current-face       ((,class (:foreground ,red :weight bold))))
     `(undo-tree-visualizer-default-face       ((,class (:foreground ,zk-fg))))
     `(undo-tree-visualizer-register-face      ((,class (:foreground ,yellow))))
     `(undo-tree-visualizer-unmodified-face    ((,class (:foreground ,cyan))))

;;;;;; emmet mode
     `(emmet-preview-input                     ((,class (:foreground ,cyan :background ,(color-darken-name zk-bg 10)))))


;;;;;; whitespace-mode
     `(whitespace-space                        ((,class (:background ,zk-bg :foreground ,zk-bg))))
     `(whitespace-hspace                       ((,class (:background ,zk-bg :foreground ,zk-bg))))
     `(whitespace-tab                          ((,class (:background ,(color-lighten-name zk-bg 2)))))
     `(whitespace-newline                      ((,class (:foreground ,zk-bg))))
     `(whitespace-trailing                     ((,class (:background ,(color-darken-name magenta 20)))))
     `(whitespace-line                         ((,class (:background ,zk-bg :foreground ,magenta))))
     `(whitespace-space-before-tab             ((,class (:background ,orange :foreground ,orange))))
     `(whitespace-indentation                  ((,class (:background ,yellow :foreground ,red))))
     `(whitespace-empty                        ((,class (:background ,yellow))))
     `(whitespace-space-after-tab              ((,class (:background ,yellow :foreground ,red)))))))

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))
(create-zonokai-theme)
(provide-theme 'zonokai)

;; Local Variables:
;; no-bytpe-compile: t
;; eval: (when (fboundp 'rainbow-mode) (rainbow-mode 1))
;; End:

;;; zonokai-theme.el ends here
