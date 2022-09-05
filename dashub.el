;;;  -*- lexical-binding: t -*-
(require 'ghub)
(require 'notifications)

(defvar dashub--notifs nil
  "Used to store the list of github notifications conveniently
parsed.")

(defcustom dashub--buffer-name "*dashub*"
  "Name of the main dashub buffer"
  :type 'string
  :group 'dashub)

(defcustom dashub--action #'dashub--browser-open "function to call when pressing RET on an item in the
list. Function receive the notif as parameter")

(defvar dashub--query-params '((unread . "true"))
  "params send to github API /notification endpoint")

(defvar dashub--timer nil "Store the TIMER object created by run-at-time, so we can cancel
it")

(defcustom dashub--notify-delay nil
  "Time to wait before refreshing the notifs list"
  :type 'number
  :group 'dashub
  :set (lambda (symbol val)
		 (when (timerp dashub--timer)
		   (cancel-timer dashub--timer)
		   (setq dashub--timer nil))
		 (when val
		   (setq dashub--timer (run-at-time t val
											(lambda ()
											  (dashub--refresh-list nil)))))
		 (custom-set-default symbol val)))

(defcustom dashub--favorite-repos nil
  "List of repos to actively watch"
  :type '(repeat 'string)
  :group 'dashub)


(defun dashub--parse-notif (notif)
  "Parse github notification into a simpler plist
(:id :title :repository :url :reason :date :type :unread)"
  (let* ((id (alist-get 'id notif))
		 (subject (alist-get 'subject notif))
		 (repo (alist-get 'full_name (alist-get 'repository notif)))
		 (title (alist-get 'title subject))
		 (type (alist-get 'type subject))
		 (url (alist-get 'url subject))
		 (reason (alist-get 'reason notif))
		 (unread (alist-get 'unread notif))
		 (date (alist-get 'updated_at notif)))
	(list
	 :id id
	 :title title
	 :repository repo
	 :url url
	 :reason reason
	 :date date
	 :type type
	 :unread unread)))

(defun dashub--format-notifs (notifs)
  "Format NOTIFS for tabulated-list-entries"
  (let ((result))
	(dolist (notif notifs)
	  (let* ((parsed-date (dashub--format-date (plist-get notif :date)))
			 (parsed-reason (plist-get notif :reason))
			 (id (plist-get notif :id))
			 (unread (dashub--notif-read-p id)))
		(push (list id (vector (dashub--propertize-repo unread (plist-get notif :repository))
							   (dashub--propertize-title unread (plist-get notif :title))
							   (dashub--propertize-type unread (plist-get notif :type))
							   parsed-reason
							   (propertize parsed-date 'font-lock-face 'font-lock-comment-face)))
			  result)))
	result))

(defun dashub--format-date (date)
  (format-time-string
   "%m/%d %H:%M"
   (encode-time
	(mapcar
	 (lambda (x) (or x 0))
	 (parse-time-string date))))
  )

(defun dashub--notif-read-p (notif-id)
  "Whether NOTIF-ID is read or not."
  (when-let ((notif (dashub--find-notif notif-id)))
	(plist-get notif :unread)))

(defun dashub--find-notif (notif-id)
  "Find notif with id NOTIF-ID in dashub--notifs"
  (catch 'notif
	(dolist (notif dashub--notifs)
	  (when (string= (plist-get notif :id) notif-id)
		(throw 'notif notif)))))

(defun dashub--propertize-title (unread title)
  "Propertize title"
  (if unread
	  (propertize title 'face 'bold)
	(propertize title 'font-lock-face 'font-lock-comment-face)))

(defun dashub--propertize-type (unread type)
  "Propertize type"
  (if unread
	  type
	(propertize type 'font-lock-face 'font-lock-comment-face)))

(defun dashub--propertize-repo (unread repo)
  "Propertize repo"
  (if unread
	  (propertize repo 'font-lock-face 'font-lock-keyword-face)
	(propertize repo 'font-lock-face 'font-lock-comment-face)))

(define-derived-mode dashub-mode tabulated-list-mode "dashub-mode"
  "Mode that displays github notifications"
  ;; (kill-all-local-variables)
  (setq mode-name "dashub-mode")
  (setq major-mode 'dashub-mode)
  (use-local-map dashub-mode-map)
  (setq tabulated-list-format [("Repository" 35 t)
							   ("Title" 80 t)
							   ("Type" 30 t)
							   ("Reason" 20 t)
							   ("Date" 5 t)])

  (setq tabulated-list-padding 2)
  (tabulated-list-init-header)
  (hl-line-mode 1)
  (run-mode-hooks 'vault-mode-hook)
  (dashub--refresh-list t)
  )


(defun dashub--notifier ()
  "Function run every time we refresh the notifs list"
  (let ((found
		 (catch 'found
		   (dolist (notif dashub--notifs)
			 (when (member (plist-get notif :repository) dashub--favorite-repos)
			   (throw 'found t))))))
	(when found
	  (notifications-notify
	   :title "DASHUB"
	   :body "Unread notifs on your favorites repo"
	   :urgency 'normal)
	  )))

;; (setq dashub--timer (run-at-time t 60 #'dashub--notifier))

;; (progn
;;   (when (timerp dashub--timer)
;; 	(cancel-timer dashub--timer))
;;   (setq dashub--timer nil))

(defun dashub--refresh-list (redraw)
  "Fetch github notifications"
  (message "fetching notifications from github..")
  (ghub-get "/notifications" dashub--query-params
			:callback (lambda (notifs &rest _)
						(setq dashub--notifs (mapcar #'dashub--parse-notif notifs))
						(dashub--notifier)
						(progn
						  (setq dashub--notifs (mapcar #'dashub--parse-notif notifs))
						  (when redraw
							(dashub--redraw-list))
						  (when (eq 0 (length notifs))
							(message "0 notification"))))))

(defun dashub--redraw-list ()
  "Redraw the tabulated-list-entries based on dashub--notifs
content"
  (with-current-buffer (get-buffer-create dashub--buffer-name)
	(setq tabulated-list-entries (dashub--format-notifs dashub--notifs))
	(tabulated-list-print t t)))


(defun dashub-read-notif ()
  "Mark notification as read."
  (interactive)
  (let* ((has-region-active (region-active-p))
		 (beg (if has-region-active (region-beginning) (point)))
		 (end (if has-region-active (region-end) (point)))
		 (notif-id (dashub--get-notif-under-region beg end)))
	(setq dashub--notifs
		  (mapc (lambda (n)
				  (when (member (plist-get n :id) notif-id)
					(setq n (plist-put n :unread (not (plist-get n :unread))))
					(dashub--mark-threads-as-read (plist-get n :id))))
				dashub--notifs)))

  ;; weird bug with evil mode that makes cursor jump to the beginning
  (when (fboundp 'evil-exit-visual-state)
	(call-interactively #'evil-exit-visual-state))
  (dashub--redraw-list)
  (forward-line 1))

(defun dashub--mark-threads-as-read (thread)
  "Call github API to mark THREAD as read."
  (ghub-patch (format "/notifications/threads/%s" thread)
			  nil
			  :callback (lambda (&rest _) (message "read"))))

(defun dashub ()
  "Point of entry of the mode. Create the buffer with
dashub-mode and switch to it"
  (interactive)
  (let ((buff (get-buffer-create dashub--buffer-name)))
	(with-current-buffer buff
	  (dashub-mode))
	(switch-to-buffer buff)))

(defun dashub--browser-open (notif)
  (let* ((url (plist-get notif :url))
		 (type (plist-get notif :type)))
	(cond ((string= "PullRequest" type)
		   (eww-browse-with-external-browser (dashub--get-pull-request-url url)))
		  t (message (format "%s not supported" type))))
  )

(defun dashub-enter ()
  (interactive)
  (when-let* ((notif-id (tabulated-list-get-id))
			  (notif (dashub--find-notif notif-id))
			  )
	(funcall dashub--action notif)))

(defun dashub--get-pull-request-url (url)
  "get browser URL from API url

ie https://api.github.com/repos/orga-name/repo-name/pulls/id =>
https://github.com/orga-name/repo-name/pull/id"
  (string-replace "api.github.com/repos" "github.com"
				  (string-replace "pulls" "pull" url)))


(defun dashub--get-notif-under-region (beg end)
  "Find all notifs between BEG and END region"
  (if (not (use-region-p))
	  (list (tabulated-list-get-id))
	(narrow-to-region beg end)
	(goto-char (point-min))
	(let ((result nil))
	  (while (< (point) end)
		(push (tabulated-list-get-id) result)
		(forward-line 1))
	  (widen)
	  result)))


;; (defun dashub-get-fake-notif-request ()
;;   (with-current-buffer (find-file-noselect "./mock.json")
;; 	(goto-char (point-min))
;; 	(json-parse-buffer
;; 	  'alist
;; 	 :false-object 'nil)))


(provide 'dashub)
