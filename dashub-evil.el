(require 'evil)
(require 'dashub)

(defgroup dashub-evil nil
  "Provides evil mapping"
  :group 'dashub-evil
  :prefix "dashub-evil")

(defvar dashub-evil-mode-map (make-sparse-keymap))

(define-minor-mode dashub-evil-mode
  "Brings evil keybindings to dashub"
  :lighter " dashub-evil"
  :keymap dashub-evil-mode-map
  :group 'dashub-evil)

(add-hook 'dashub-mode-hook 'dashub-evil-mode)

(evil-set-initial-state 'dashub-mode 'motion)

(evil-define-key '(motion visual) dashub-evil-mode-map
  (kbd "g") #'dashub
  (kbd "u") #'dashub-read-notif)

(provide 'dashub-evil)
