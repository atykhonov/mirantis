(defun gerta--process-filter (proc string)
  (let ((json-obj nil)
        (json-str nil)
        (event-type nil)
        (event-project nil))
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
        (insert (format "[%s] > %s\n"
                        (substring event-project 10)
                        (cdr
                         (assoc 'subject
                                (assoc 'change json-obj)))))
        (insert (format "» %s: %s\n\n" patch-set-author event-comment))
        ))))

(defun gerta-start ()
  (interactive)
  (start-process "gerrit-stream"
                 "*gerrit-stream*"
                 "ssh" "-p" "29418"
                 "atykhonov@review.openstack.org" "gerrit" "stream-events")
  (set-process-filter (get-process "gerrit-stream") 'gerta--process-filter))
