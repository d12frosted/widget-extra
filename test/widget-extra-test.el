;;; widget-extra-test.el --- Tests for widget-extra -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tests for widget-extra widgets.

;;; Code:

(require 'ert)
(require 'widget-extra)

;;; * Test helpers

(defmacro with-widget-test-buffer (&rest body)
  "Execute BODY in a temporary widget buffer."
  (declare (indent 0) (debug t))
  `(with-temp-buffer
     (widget-minor-mode 1)
     ,@body))

(defun widget-test-click (widget)
  "Simulate a click on WIDGET."
  (widget-apply widget :action))

;;; * Toggle button tests

(ert-deftest widget-extra-test-toggle-button-default-value ()
  "Test toggle-button has nil default value."
  (with-widget-test-buffer
    (let ((widget (widget-create 'toggle-button)))
      (should (null (widget-value widget))))))

(ert-deftest widget-extra-test-toggle-button-initial-value ()
  "Test toggle-button respects initial value."
  (with-widget-test-buffer
    (let ((widget (widget-create 'toggle-button :value t)))
      (should (eq t (widget-value widget))))))

(ert-deftest widget-extra-test-toggle-button-toggle ()
  "Test toggle-button toggles value on action."
  (with-widget-test-buffer
    (let ((widget (widget-create 'toggle-button :value nil)))
      (should (null (widget-value widget)))
      (widget-test-click widget)
      (should (eq t (widget-value widget)))
      (widget-test-click widget)
      (should (null (widget-value widget))))))

(ert-deftest widget-extra-test-toggle-button-displays-on-off ()
  "Test toggle-button displays correct label."
  (with-widget-test-buffer
    (erase-buffer)
    (let ((widget (widget-create 'toggle-button :value nil :on "YES" :off "NO")))
      (should (string-match-p "NO" (buffer-string))))
    (erase-buffer)
    (let ((widget (widget-create 'toggle-button :value t :on "YES" :off "NO")))
      (should (string-match-p "YES" (buffer-string))))))

(ert-deftest widget-extra-test-toggle-button-notify ()
  "Test toggle-button calls notify on toggle."
  (with-widget-test-buffer
    (let ((notified nil))
      (let ((widget (widget-create 'toggle-button
                                   :value nil
                                   :notify (lambda (&rest _) (setq notified t)))))
        (widget-test-click widget)
        (should notified)))))

;;; * Action button tests

(ert-deftest widget-extra-test-action-button-notify ()
  "Test action-button triggers notify on click."
  (with-widget-test-buffer
    (let ((clicked nil))
      (let ((widget (widget-create 'action-button
                                   :value "Click me"
                                   :notify (lambda (&rest _) (setq clicked t)))))
        (widget-test-click widget)
        (should clicked)))))

(ert-deftest widget-extra-test-action-button-data ()
  "Test action-button passes action-data to notify."
  (with-widget-test-buffer
    (let ((received-data nil))
      (let ((widget (widget-create 'action-button
                                   :value "Click"
                                   :action-data '(foo . bar)
                                   :notify (lambda (w &rest _)
                                             (setq received-data (widget-get w :action-data))))))
        (widget-test-click widget)
        (should (equal '(foo . bar) received-data))))))

;;; * Link button tests

(ert-deftest widget-extra-test-link-button-url ()
  "Test link-button opens URL."
  (with-widget-test-buffer
    (let ((opened-url nil))
      (cl-letf (((symbol-function 'browse-url)
                 (lambda (url) (setq opened-url url))))
        (let ((widget (widget-create 'link-button
                                     :value "Google"
                                     :target "https://google.com")))
          (widget-test-click widget)
          (should (equal "https://google.com" opened-url)))))))

(ert-deftest widget-extra-test-link-button-function ()
  "Test link-button calls function target."
  (with-widget-test-buffer
    (let ((called nil))
      (let ((widget (widget-create 'link-button
                                   :value "Action"
                                   :target (lambda () (setq called t)))))
        (widget-test-click widget)
        (should called)))))

(ert-deftest widget-extra-test-link-button-has-link-face ()
  "Test link-button uses link face."
  (with-widget-test-buffer
    (let ((widget (widget-create 'link-button :value "Link")))
      (should (eq 'link (widget-get widget :button-face))))))

;;; * Label tests

(ert-deftest widget-extra-test-label-displays-value ()
  "Test label displays its value."
  (with-widget-test-buffer
    (erase-buffer)
    (widget-create 'label :value "Hello World")
    (should (string-match-p "Hello World" (buffer-string)))))

(ert-deftest widget-extra-test-label-truncate ()
  "Test label truncates long values."
  (with-widget-test-buffer
    (erase-buffer)
    (widget-create 'label :value "This is a very long string" :truncate 10)
    (should (<= (length (string-trim (buffer-string))) 10))))

(ert-deftest widget-extra-test-numeric-label ()
  "Test numeric-label formats numbers."
  (with-widget-test-buffer
    (erase-buffer)
    (widget-create 'numeric-label :value 42)
    (should (string-match-p "42" (buffer-string)))))

;;; * Field tests

(ert-deftest widget-extra-test-int-field-value ()
  "Test int-field stores integer value."
  (with-widget-test-buffer
    (let ((widget (widget-create 'int-field :value 123)))
      (should (= 123 (widget-value widget))))))

(ert-deftest widget-extra-test-bounded-int-field-match ()
  "Test bounded-int-field validates range."
  (with-widget-test-buffer
    (let ((widget (widget-create 'bounded-int-field
                                 :min-value 0
                                 :max-value 100
                                 :value 50)))
      (should (widget-apply widget :match 50))
      (should (widget-apply widget :match 0))
      (should (widget-apply widget :match 100))
      (should-not (widget-apply widget :match -1))
      (should-not (widget-apply widget :match 101)))))

;;; * Table tests

(ert-deftest widget-extra-test-table-with-menu-choice ()
  "Test table correctly handles menu-choice widget."
  (with-widget-test-buffer
    (let* ((selected nil)
           (widget (widget-create
                    'table
                    '(row (label :value "Type:"))
                    `(row (menu-choice
                           :value "a"
                           :tag "a"
                           :format "%[%t%]"
                           :notify (lambda (w &rest _)
                                     (setq selected (widget-value w)))
                           (choice-item :tag "a" :value "a")
                           (choice-item :tag "b" :value "b")
                           (choice-item :tag "c" :value "c"))))))
      ;; Verify table was created
      (should widget)
      ;; Verify menu-choice child exists and has :args populated
      (let ((children (widget-get widget :children)))
        (should children)
        ;; Find the menu-choice child
        (let ((menu-choice-child (--find (eq (widget-type it) 'menu-choice) children)))
          (should menu-choice-child)
          ;; Verify :args were converted (should be widget objects, not raw specs)
          (let ((args (widget-get menu-choice-child :args)))
            (should args)
            ;; Each arg should be a converted widget with :create method accessible
            (should (>= (length args) 3))))))))

(provide 'widget-extra-test)
;;; widget-extra-test.el ends here
