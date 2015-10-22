;; (defface gerta-comment-face
;;   '((t (:weight bold)))
;;   "Face used to display the probable translation."
;;   :group 'gerta)

(defface gerta-project-face
  '((t (:height 0.8 :weight bold :foreground "#b0c4de")))
  "Face used to display button \"Listen\"."
  :group 'gerta)

(defface gerta-project-subject-delimiter-face
  '((t (:height 0.8)))
  "Face used to display button \"Listen\"."
  :group 'gerta)

(defface gerta-subject-face
  '((t (:height 0.8)))
  "Face used to display button \"Listen\"."
  :group 'gerta)

(defface gerta-comment-face
  '((t (:height 0.6)))
  "Face used to display button \"Listen\"."
  :group 'gerta)

(defface gerta-author-face
  '((t (:height 0.7)))
  "Face used to display the suggestion label."
  :group 'gerta)

(defun gerta--process-filter (proc string)
  (let ((json-obj nil)
        (json-str nil)
        (event-type nil)
        (event-project nil)
        (temp-point nil))
    (setq json-str string)
    (setq json-obj (json-read-from-string json-str))
    ;; (prin1 (assoc 'author json-obj))
    (setq event-type (cdr (assoc 'type json-obj)))
    ;; event types: patchset-created
    (when (equal event-type "comment-added")
      (setq event-project (cdr (assoc 'project
                                      (assoc 'change json-obj))))
      ;; (when (or (equal event-project "openstack/fuel-web")
      ;;           (equal event-project "openstack/python-fuelclient")
      ;;           (equal event-project "openstack/fuel-agent"))
      (setq change-owner (cdr
                          (assoc 'username
                                 (assoc 'owner
                                        (assoc 'change json-obj)))))
      ;; (setq patch-set (assoc 'patchSet json-obj))
      (setq patch-set-author (cdr (assoc 'name (assoc 'author json-obj))))
      (setq event-comment (cdr (assoc 'comment json-obj)))
      (with-current-buffer (get-buffer-create "*gerta*")
        (end-of-buffer)
        (setq temp-point (point))
        (insert (format "[%s]" (substring event-project 10)))
        (facemenu-set-face 'gerta-project-face temp-point (point))
        (fill-region temp-point (point))
        (setq temp-point (point))
        (insert " > ")
        (facemenu-set-face 'gerta-project-subject-delimiter-face temp-point (point))
        (fill-region temp-point (point))
        (insert (format " %s"
                        (cdr
                         (assoc 'subject
                                (assoc 'change json-obj)))))
        (facemenu-set-face 'gerta-subject-face temp-point (point))
        (fill-region temp-point (point))
        (setq temp-point (point))
        (insert (format "\n\n» %s: \n" patch-set-author))
        (facemenu-set-face 'gerta-author-face temp-point (point))
        (fill-region temp-point (point))
        (setq temp-point (point))
        (insert (format "\n%s\n" event-comment))
        (facemenu-set-face 'gerta-comment-face temp-point (point))
        (fill-region temp-point (point))
        ;; (insert "---\n\n")
        (insert "\n")
        ))))

(defun gerta-start ()
  (interactive)
  (start-process "gerrit-stream"
                 "*gerrit-stream*"
                 "ssh" "-p" "29418"
                 "atykhonov@review.openstack.org" "gerrit" "stream-events")
  (set-process-filter (get-process "gerrit-stream") 'gerta--process-filter))
