;; Tools useful for ui scripts for orgmode

(require 'org)

(defun org-convenience-agenda-active ()
  "Is the agenda active?"
  (string-prefix-p "*Org Agenda" (buffer-name)))

(defun org-convenience-priority-up ()
  "Like org-priority-up, but works in agenda."
  (interactive)
  (if (org-convenience-agenda-active) (org-agenda-priority-up) (org-priority-up)))

(defun org-convenience-priority-down ()
  "Like org-priority-down, but works in agenda."
  (interactive)
  (if (org-convenience-agenda-active) (org-agenda-priority-down) (org-priority-down)))

(defun org-convenience-next-visible-heading (&optional n)
  "Like org-next-visible-heading, but works in agenda."
  (interactive)
  (if (org-convenience-agenda-active)
	    (org-agenda-next-item n)
	  (org-next-visible-heading n)
	  ))
(defun org-convenience-previous-visible-heading (&optional n)
  "Like org-previous-visible-heading, but works in agenda."
  (interactive)
  (if (org-convenience-agenda-active)
	    (org-agenda-previous-item n)
	  (org-previous-visible-heading n)
	  ))


(defun org-convenience-goto-heading-text ()
  "Move point to the start of text after any drawers at the beginning of the headline."
  (interactive)
  (let (target-point)
    (org-back-to-heading t)
    (let* ((heading (org-element-at-point-no-context))
           (end (org-element-end heading)))
      ;; skip forward over any drawers until heading end
      (while (and (< (point) end)
                  (re-search-forward org-drawer-regexp end t))
        (let* ((drawer (org-element-at-point))
               (type (org-element-type drawer)))
          (when (memq type '(drawer property-drawer))
            ;; Make sure to skip drawer entirely or we might flag it
            ;; another time when matching its ending line with
            ;; `org-drawer-regexp'.
            (goto-char (org-element-end drawer))
            (re-search-forward "\s+" end t)))))
    (beginning-of-line)
    (when (= (point)
             (save-excursion
               (org-back-to-heading t)
               (point)))
      (previous-line)
      (when (org-at-drawer-p)
        (end-of-line)
        (newline)
        ))))

(defun org-convenience-toggle-tag (tag &optional onoff)
  "Like org-toggle-tag, but works in agenda."
  (if (org-convenience-agenda-active)
	    (org-agenda-set-tags tag onoff)
	  (org-toggle-tag tag onoff)))


(defmacro org-convenience-safe-from-agenda (&rest body)
	"Execute BODY after temporarily switching to the real buffer if in the agenda."
  `(let ((buf (current-buffer)))
		 (when (org-convenience-agenda-active)
			 (org-agenda-switch-to))
	   (save-excursion
	     (progn ,@body))
	   ;; save-excursion doesn't work for some weird reason
	   (unless (equal buf (current-buffer))
	     (switch-to-buffer buf))))

(defun org-convenience-property-set (key value)
  (org-convenience-safe-from-agenda
   (org-set-property key value)))

(defun org-convenience-category-make-explicit ()
  "Make inherited CATEGORY explicit."
  (interactive)
  (let ((value (org-entry-get-with-inheritance "CATEGORY")))
	  (when value
	    (org-entry-put nil "CATEGORY" value))))

(defun org-convenience-buffer-decrease-priority ()
  (interactive)
  (with-temp-message "Decreasing priority for all level 1 headlines..."
    (let ((inhibit-message t)
          (buf (clone-indirect-buffer (format "*org-convenience-buffer-decrease-priority-%s*" (buffer-name)) nil t)))
      (with-current-buffer
          buf
        (save-excursion
          (widen)
          (beginning-of-buffer)
          (org-next-visible-heading 1)
          (let ((lowest-priority-num 0))
            (cl-loop do (unless (or (org-entry-is-done-p)
                                    ;; lowest priority (a character value) is > highest priority
                                    ;; for priority calculation see `org-get-priority'
                                    (let ((prio (save-match-data
                                                  (beginning-of-line)
		                                              (and (looking-at org-heading-regexp)
			                                                 (org-get-priority (match-string 0))))))
                                      (<= prio
                                          lowest-priority-num)))
                          (org-priority-down))
                     while (let ((point (point)))
                             (org-forward-heading-same-level 1)
                             (/= point (point)))
                     ))))
      (kill-buffer buf))))

(defvar org-convenience-heading-edit-title-history nil "Minibuffer-history for `org-convenience-heading-edit-title'")

(defun org-convenience-heading-edit-title (point)
  (interactive "d")
  (save-excursion
    (org-back-to-heading)
    (let* ((headline (org-element-headline-parser))
           (title (car (org-element-property :title headline)))
           (new-title (read-string "Change headline title to: " title 'org-convenience-headline-edit-title-history title t)))
      (org-ml-update* it (org-ml-headline-set-title! new-title nil headline))
      (message "%s" title))))

;; org-element parser
;; (defun org-convenience-heading-goto-title (point)
;;   (interactive "d")
;;   (org-back-to-heading)
;;   (let* ((headline (org-element-headline-parser))
;;          (title (substring-no-properties (car (org-element-property :title headline))))
;;          )
;;     (message "%S" title)
;;     (search-forward title (org-element-property :end headline) t)
;;     (goto-char (match-beginning 0))))

;; regex based
(defun org-convenience-heading-goto-title ()
  (interactive)
  (org-back-to-heading)
  (when (looking-at org-heading-regexp)
    (goto-char (match-beginning 2)))
  (when (looking-at org-todo-regexp)
    (goto-char (match-end 0)))
  (when (looking-at org-priority-regexp)
    (goto-char (match-end 0)))
  (when (looking-at "[ 	]+")
    (goto-char (match-end 0))))


(defun org-convenience-add-tag-from-word-at-point ()
	(interactive)
	(let ((sym (word-at-point t)))
		(when sym
			(org-convenience-toggle-tag (downcase sym)))))

(defun org-convenience-set-tags-command-dwim ()
  (interactive)
  (if (use-region-p)
      (org-convenience-toggle-tag (downcase (buffer-substring-no-properties (region-beginning) (region-end))))
    (org-set-tags-command)))



(defun org-convenience-forward-heading-same-level-or-up-forward-heading-same-level ()
  (interactive)
  (let ((column (current-column)))
    (org-back-to-heading)
    (let ((point (point)))
      (org-forward-heading-same-level 1)
      (when (= point (point))
        (when (org-up-heading-safe)
          (org-convenience-forward-heading-same-level-or-up-forward-heading-same-level))))
    (move-to-column column)))
(defun org-convenience-backward-heading-same-level-or-up-heading ()
  (interactive)
  (let ((column (current-column)))
    (org-back-to-heading)

    (let ((point (point)))
      (org-backward-heading-same-level 1)
      (when (= point (point))
        (org-up-heading-safe))
      (move-to-column column))))

(defun org-convenience--up-element-or-backward-heading (continue-after-back-to-heading?-fun)
  (let ((column (current-column))
        (point (point)))
    (org-back-to-heading)
    (when (funcall continue-after-back-to-heading?-fun point)
      (let ((point (point)))
        (ignore-errors
          (org-up-element))
        (when (= point (point))
          (org-previous-visible-heading 1))
        (move-to-column column)))))

(defun org-convenience-up-element-or-backward-heading ()
  (interactive)
  (org-convenience--up-element-or-backward-heading (lambda (point) (= point (point)))))

(defun org-convenience-clipboard-yank-quote ()
  (interactive)
  (org-insert-structure-template "quote")
  (clipboard-yank)
  (unless (looking-back "
")
    (newline))
  (end-of-line)
  (newline)
  )






;; strongly inspired by https://mollermara.com/blog/Fast-refiling-in-org-mode-with-hydras/

(defun org-convenience-refile--refile (file &optional headline arg)
  "Refile to a specific location.
With a 'C-u' ARG argument, we jump to that location (see
`org-refile').
Use `org-agenda-refile' in `org-agenda' mode."
  (let* ((pos (with-current-buffer
				          ;; Is the file open in a buffer already?
				          (or (get-buffer file)
					            ;; Otherwise, try to find the file by name (Note, default-directory matters here if it isn't absolute)
					            (find-file-noselect file))
				        (if headline
					          (or (org-find-exact-headline-in-buffer headline)
						            (error "Can't find headline `%s'" headline))
				          nil)))
		     ;; If we're given a relative name, find absolute path
		     (filepath (if pos (buffer-file-name (marker-buffer pos)) (expand-file-name file)))
		     (rfloc (list headline filepath nil pos)))
	  ;; Don't use org-agenda-refile if we're just jumping
	  (if (and (eq major-mode 'org-agenda-mode)
			       (not (and arg (listp arg))))
		    (org-agenda-refile nil rfloc)
	    (org-refile arg nil rfloc))))

(defun org-convenience-refile (file &optional headline arg)
  "Refile to HEADLINE in FILE. Clean up org-capture if it's activated.
With a `C-u` ARG, just jump to the headline."
  (interactive "P")
  (let ((is-capturing (and (boundp 'org-capture-mode) org-capture-mode)))
	  (cond
	   ;; Are we jumping?
	   ((and arg (listp arg))
	    (org-convenience-refile--refile file headline arg))
	   ;; Are we in org-capture-mode?
	   (is-capturing
	    ;; Minor mode variable that's defined when capturing
	    (defun org-convenience-refile--refile-fix (&optional arg default-buffer rfloc msg)
		    "Call org-convenience-refile--refile with preset file headline arg instead"
		    (interactive)
		    (advice-remove 'org-refile #'org-convenience-refile--refile-fix)
		    (org-convenience-refile--refile file headline arg))
	    (advice-add 'org-refile :override #'org-convenience-refile--refile-fix)
	    (advice-remove 'org-refile #'org-convenience-refile--refile-fix)
	    (org-capture-refile)
	    )
	   (t
	    (org-convenience-refile--refile file headline arg)))
	  (when (or arg is-capturing)
	    (setq hydra-deactivate t))))

;; (advice-mapc (lambda (f ps) (advice-remove 'org-refile f)) 'org-refile)


(defun org-convenience-refile-next (file &optional headline arg)
	"Like org-convenience-refile-next, but works in agenda."
	(org-convenience-refile file headline arg)
	(org-convenience-next-visible-heading 0)
	(recenter))

(provide 'org-convenience)
