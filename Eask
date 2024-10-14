(package "widget-extra"
         "1.0.0"
         "Extra widgets that extend built-in library")

(website-url "https://github.com/d12frosted/widget-extra")
(keywords "extensions")

(package-file "widget-extra.el")

(script "test" "echo \"Error: no test specified\" && exit 1")

(source "gnu")

(depends-on "emacs" "29.1")
(depends-on "dash" "2.19.1")
(depends-on "s" "1.13.0")

(add-hook 'eask-before-compile-hook
           (lambda ()
             (setq byte-compile-error-on-warn t)))

;; We are disabling the prefix check rule, as it enforces prefixing on widgets created
;; with `define-widget'. This requirement seems wrong - even built-in widgets aren't prefixed.
;;
;; Additionally, there appears to be no way to distinguish between standard functions and widgets.
(add-hook 'eask-before-lint/package-hook
           (lambda ()
             (with-eval-after-load 'package-lint
               (defun package-lint--get-package-prefix ()
                 "Return empty string."
                 ""))))
