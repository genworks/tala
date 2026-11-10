;;;; -*- Mode: Lisp; Package: tala -*-

;; Copyright © 2026 Genworks International
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

(in-package :tala)

;;
;; The operator's page, /tala/admin: who has an invite and how often
;; they used it; what each visitor did, session by session -- visits,
;; the forms their page sent by hand and how those went, samples
;; brought in, downloads, and each prompt with the forms the model
;; wrote and what became of them; and the sessions in memory, each
;; song's tracks, devices, clips and samples.  It reads what is already
;; kept: the gate's roster, the activity log (web.lisp, Gendl's
;; visitor-log), the prompt log (prompt.lisp) and the sessions table.
;; Both logs are off until the operator turns them on, and the page
;; says so when they are.  The one thing it changes is what it is asked
;; to delete from those two logs: a line, a prompt, or everything kept
;; of a visitor.
;;
;; Only a name on *admins* gets any of it; anyone else, invited or not,
;; is told there is no such page.  No invite token is in anything it
;; answers.  The page's look and the pieces its script is built from
;; are visitor-log's, shared with other applications' admin pages.
;;
;;   (tala:keep-activity! "/projects/.state/tala-activity/")
;;   (tala:configure-prompt-log! :directory "/projects/.state/tala-prompts/")
;;   (setf tala:*admins* '("Name on my invite"))
;;

(defparameter *admin-events* 300
  "How many of a visitor's lines and prompts the admin page is sent
unless it asks for more.")

(defun line-kind (line)
  "What a line of the activity log was: :visit, the page asked for;
:download; :asked, a prompt sent; :import, a sample brought in; :load,
a song file opened; :forms, forms the page sent by hand; :other."
  (let ((door (or (getf line :door) "")))
    (cond ((member door '("/tala" "/tala/") :test #'equal) :visit)
          ((eq (getf line :method) :get) :download)
          ((equal door "/tala/api/prompt") :asked)
          ((equal door "/tala/api/sample") :import)
          ((equal door "/tala/api/load") :load)
          ((equal door "/tala/api/forms") :forms)
          (t :other))))

(defun line-session (line)
  (visitor-log:query-part (getf line :query) "s"))

(defun line-field (line name)
  "The field NAME of LINE's body when that is whole JSON, as a string;
nil when it is not, as when the log cut a long body short."
  (let ((body (getf line :body)))
    (and (stringp body)
         (let ((parsed (ignore-errors (yason:parse body))))
           (and (hash-table-p parsed)
                (let ((value (gethash name parsed)))
                  (and (stringp value) value)))))))

;;; The prompt log, read back.

(defun turn-files ()
  "The prompt log's files, oldest first: log-turn! names each by when
its prompt ended."
  (when *prompt-log-directory*
    (sort (directory (merge-pathnames "*.sexp" *prompt-log-directory*))
          #'string< :key #'pathname-name)))

(defun turn-time (name)
  "When the prompt in the file called NAME ended, as the activity log
writes times; nil for a file not named as log-turn! names them."
  (and (stringp name) (> (length name) 15) (char= (char name 8) #\-)
       (every #'digit-char-p (subseq name 0 8))
       (every #'digit-char-p (subseq name 9 15))
       (format nil "~a-~a-~aT~a:~a:~aZ" (subseq name 0 4) (subseq name 4 6) (subseq name 6 8)
               (subseq name 9 11) (subseq name 11 13) (subseq name 13 15))))

(defun turn-head (file)
  "(:who name :session id) of the prompt in FILE, by the first words of
its record alone: a record holds the whole song in words."
  (ignore-errors
   (with-open-file (in file :external-format :utf-8)
     (with-standard-io-syntax
       (let ((*read-eval* nil) (*package* (find-package :common-lisp)) (head '()))
         (when (eql (peek-char t in) #\()
           (read-char in)
           (loop
             (when (eql (peek-char t in) #\)) (return))
             (let ((key (read in)))
               (unless (member key '(:who :session)) (return))
               (setf head (list* (read in) key head))))
           (nreverse head)))))))

(defun read-turn (file)
  "The whole record of the prompt in FILE, or nil when it will not
read.  In the package log-turn! wrote it in, with nothing evaluated."
  (ignore-errors
   (with-open-file (in file :external-format :utf-8)
     (with-standard-io-syntax
       (let ((*read-eval* nil) (*package* (find-package :common-lisp)))
         (let ((record (read in)))
           (and (listp record) (evenp (length record)) record)))))))

(defun logged-turns ()
  "Every prompt in the prompt log, oldest first, each (:turn name :time
time :file path :who name :session id)."
  (loop for file in (turn-files)
        collect (list* :turn (pathname-name file) :time (turn-time (pathname-name file))
                       :file file (turn-head file))))

(defun visitor-turns (key)
  "The prompts of the visitor whose file name is KEY."
  (remove-if-not (lambda (turn) (equal key (visitor-log:visitor-file-name (getf turn :who))))
                 (logged-turns)))

;;; What is kept, summed up.

(defun plain-text (value)
  "VALUE for the page: a string as it is, nil as JSON's null, anything
else printed."
  (cond ((null value) :null)
        ((stringp value) value)
        (t (let ((*print-readably* nil) (*print-case* :downcase) (*print-pretty* nil)
                 (*package* (find-package :tala-api)))
             (prin1-to-string value)))))

(defun session-rollup (lines turns)
  "The sessions that LINES, a visitor's activity, and TURNS, their
prompts, name: each (id :first time :last time :requests n :prompts n),
the most recent first."
  (let ((sessions (make-hash-table :test 'equal)))
    (flet ((note (id time count)
             (when (and (stringp id) (plusp (length id)))
               (let ((entry (or (gethash id sessions)
                                (list :first time :last time :requests 0 :prompts 0))))
                 (when (and time (string< time (or (getf entry :first) time)))
                   (setf (getf entry :first) time))
                 (when (and time (string> time (or (getf entry :last) "")))
                   (setf (getf entry :last) time))
                 (incf (getf entry count))
                 (setf (gethash id sessions) entry)))))
      (dolist (line lines) (note (line-session line) (getf line :time) :requests))
      (dolist (turn turns) (note (getf turn :session) (getf turn :time) :prompts)))
    (sort (loop for id being the hash-keys of sessions using (hash-value entry)
                collect (cons id entry))
          #'string> :key (lambda (entry) (or (getf (cdr entry) :last) "")))))

(defun track-summary (track)
  (obj "name" (plain-text (getf track :name))
       "instrument" (let ((instrument (getf track :instrument)))
                      (if instrument (getf instrument :kind) :null))
       "devices" (mapcar (lambda (device) (getf device :kind)) (getf track :devices))
       "clips" (length (getf track :clips))
       "notes" (reduce #'+ (getf track :clips) :key (lambda (clip) (length (getf clip :notes))))
       "mute" (jbool (getf track :mute))
       "solo" (jbool (getf track :solo))))

(defun song-summary (song)
  "SONG for the admin page: its tracks with what is on them, its
samples, and how much history it holds."
  (let ((spec (the-object song spec)))
    (obj "name" (plain-text (getf spec :name))
         "tempo" (getf spec :tempo)
         "signature" (format nil "~a/~a" (getf spec :numerator) (getf spec :denominator))
         "tracks" (mapcar #'track-summary (getf spec :tracks))
         "returns" (mapcar #'track-summary (getf spec :returns))
         "scenes" (length (getf spec :scenes))
         "samples" (mapcar (lambda (sample)
                             (obj "name" (plain-text (getf sample :name))
                                  "seconds" (sample-seconds sample)))
                           (getf spec :samples))
         "undo" (length (the-object song undo-entries))
         "redo" (length (the-object song redo-entries))
         "prompts" (length (the-object song prompt-log)))))

(defun live-sessions ()
  "The sessions in memory, the most recently asked for first: each (id
touched song), song a summary, :busy when a request has the session's
lock just now, or the words of what went wrong.  The lock is tried and
never waited for."
  (let ((sessions (bt:with-lock-held (*sessions-lock*)
                    (loop for id being the hash-keys of *sessions* using (hash-value session)
                          collect (list id (gethash id *session-touched* 0) session)))))
    (loop for (id touched session) in (sort sessions #'> :key #'second)
          collect (list id touched
                        (if (bt:acquire-lock (session-lock session) nil)
                            (unwind-protect
                                 (handler-case (song-summary (session-song session))
                                   (serious-condition (e)
                                     (let ((*print-readably* nil)) (princ-to-string e))))
                              (bt:release-lock (session-lock session)))
                            :busy)))))

(defun heap-used-fraction ()
  "How much of the heap is in use, 0 to 1, where the implementation can
say; nil elsewhere."
  #+sbcl (/ (float (sb-kernel:dynamic-usage) 1d0) (sb-ext:dynamic-space-size))
  #-sbcl nil)

(defun admin-summary ()
  "Everyone the roster, the activity log or the prompt log names, summed
up, and the sessions in memory: JSON for the admin page."
  (let ((turns (logged-turns))
        (visitors (make-hash-table :test 'equal))
        (order '())
        (whose (make-hash-table :test 'equal))
        (gate? (invite-gate:gate-on? *gate*)))
    (flet ((note (key indicator value &key unless-set)
             (unless (nth-value 1 (gethash key visitors)) (push key order))
             (unless (and unless-set (getf (gethash key visitors) indicator))
               (setf (getf (gethash key visitors) indicator) value))))
      (when gate?
        (dolist (invite (invites-report))
          (let ((key (visitor-log:visitor-file-name (getf invite :name))))
            (note key :name (getf invite :name))
            (note key :invite invite))))
      (dolist (key (visitor-log:visitor-keys *activity*))
        (let ((lines (visitor-log:lines *activity* key)))
          (note key :lines lines)
          (note key :name (getf (first lines) :who) :unless-set t)))
      (dolist (turn turns)
        (let ((key (visitor-log:visitor-file-name (getf turn :who))))
          (note key :turns (cons turn (getf (gethash key visitors) :turns)))
          (note key :name (getf turn :who) :unless-set t))))
    (let ((rows
            (loop for key in (reverse order)
                  for entry = (gethash key visitors)
                  for lines = (getf entry :lines)
                  for mine = (reverse (getf entry :turns))
                  for invite = (getf entry :invite)
                  for sessions = (session-rollup lines mine)
                  for times = (remove nil (append (mapcar (lambda (line) (getf line :time)) lines)
                                                  (mapcar (lambda (turn) (getf turn :time)) mine)))
                  for latest = (and times (reduce (lambda (a b) (if (string> a b) a b)) times))
                  for seen = (and (integerp (getf invite :last)) (plusp (getf invite :last))
                                  (visitor-log:utc-stamp (getf invite :last)))
                  do (dolist (session sessions)
                       (setf (gethash (car session) whose) (or (getf entry :name) key)))
                  collect (flet ((kind (kind) (count kind lines :key #'line-kind)))
                            (cons (format nil "~a ~a" (or latest "") (or seen ""))
                                  (obj "key" key
                                       "name" (plain-text (getf entry :name))
                                       "invited" (jbool invite)
                                       "issued" (plain-text (getf invite :issued))
                                       "uses" (if invite (getf invite :uses) :null)
                                       "seen" (or seen :null)
                                       "visits" (kind :visit)
                                       "forms" (kind :forms)
                                       "failed" (reduce #'+ lines :key (lambda (line)
                                                                        (if (integerp (getf line :failed))
                                                                            (getf line :failed)
                                                                            0)))
                                       "imports" (kind :import)
                                       "loads" (kind :load)
                                       "downloads" (kind :download)
                                       "prompts" (length mine)
                                       "first" (if times
                                                   (reduce (lambda (a b) (if (string< a b) a b)) times)
                                                   :null)
                                       "last" (or latest :null)
                                       "sessions" (mapcar (lambda (session)
                                                            (obj "id" (car session)
                                                                 "first" (plain-text (getf (cdr session) :first))
                                                                 "last" (plain-text (getf (cdr session) :last))
                                                                 "requests" (getf (cdr session) :requests)
                                                                 "prompts" (getf (cdr session) :prompts)))
                                                          sessions)))))))
      (json-text
       (obj "now" (visitor-log:utc-stamp)
            "server" (obj "lisp" (format nil "~a ~a" (lisp-implementation-type)
                                         (lisp-implementation-version))
                          "heapUsed" (or (heap-used-fraction) :null)
                          "gate" (jbool gate?)
                          "activityLog" (jbool (visitor-log:log-on? *activity*))
                          "promptLog" (jbool *prompt-log-directory*)
                          "promptsKept" (length turns)
                          "mostSessions" *most-sessions*)
            ;; the most recently active first; then those with nothing
            ;; recorded, by when their link was last used (an admin who
            ;; only looks is among these)
            "visitors" (mapcar #'cdr (stable-sort rows #'string> :key #'car))
            "sessions" (loop for (id touched song) in (live-sessions)
                             collect (obj "id" id
                                          "who" (plain-text (gethash id whose))
                                          "touched" (if (plusp touched)
                                                        (visitor-log:utc-stamp touched)
                                                        :null)
                                          "busy" (jbool (eq song :busy))
                                          "error" (if (stringp song) song :null)
                                          "song" (if (consp song) song :null))))))))

;;; One visitor, line by line.

(defun line-json (line)
  "A line of the activity log for the admin page."
  (let ((door (or (getf line :door) ""))
        (forms (line-field line "forms"))
        (prompt (line-field line "prompt")))
    (obj "time" (plain-text (getf line :time))
         "kind" (line-kind line)
         "door" (subseq door (1+ (or (position #\/ door :from-end t) -1)))
         "session" (plain-text (line-session line))
         "forms" (plain-text forms)
         "prompt" (plain-text prompt)
         "ran" (if (integerp (getf line :ran)) (getf line :ran) :null)
         "failed" (if (integerp (getf line :failed)) (getf line :failed) :null)
         "name" (plain-text (getf line :name))
         "bytes" (if (integerp (getf line :bytes)) (getf line :bytes) :null)
         "error" (plain-text (getf line :error))
         ;; a body the fields above do not account for, as it was kept
         "body" (if (or forms prompt) :null (plain-text (getf line :body)))
         ;; what the delete door is told to name this line
         "line" (getf line :index)
         "mark" (getf line :mark))))

(defun turn-json (turn)
  "The prompt of TURN, an entry of logged-turns, with the whole of its
record: the words, the model, each form it wrote with how it went, and
its reply."
  (let ((record (read-turn (getf turn :file))))
    (obj "time" (plain-text (getf turn :time))
         "kind" "prompt"
         "turn" (getf turn :turn)
         "session" (plain-text (getf turn :session))
         "prompt" (plain-text (getf record :prompt))
         "model" (plain-text (getf record :model))
         "seconds" (if (realp (getf record :seconds)) (getf record :seconds) :null)
         "rounds" (if (realp (getf record :rounds)) (getf record :rounds) :null)
         "reply" (plain-text (getf record :reply))
         "ok" (jbool (getf record :ok))
         "results" (mapcar (lambda (result)
                             (obj "form" (plain-text (getf result :form))
                                  "ok" (jbool (getf result :ok))
                                  "value" (plain-text (getf result :value))
                                  "error" (plain-text (getf result :error))))
                           (let ((results (getf record :results)))
                             (and (listp results) (remove-if-not #'listp results))))
         ;; a record from before each form's outcome was kept
         "forms" (if (getf record :results) :null (plain-text (getf record :forms)))
         "state" (plain-text (getf record :state)))))

(defun visitor-events (key &optional (limit *admin-events*))
  "The last LIMIT things kept of the visitor whose file name is KEY,
oldest first: the lines of their activity file and their prompts in the
prompt log, by time.  (values events how-many-in-all)."
  (let* ((lines (loop for line in (visitor-log:lines *activity* key)
                      for index from 0
                      collect (list* :index index :mark (visitor-log:line-mark line) line)))
         (events (stable-sort (append (mapcar (lambda (line) (cons (or (getf line :time) "") line)) lines)
                                      (mapcar (lambda (turn) (cons (or (getf turn :time) "") turn))
                                              (visitor-turns key)))
                              #'string< :key #'car)))
    (values (mapcar (lambda (event)
                      (if (getf (cdr event) :turn) (turn-json (cdr event)) (line-json (cdr event))))
                    (last events (max 1 limit)))
            (length events))))

;;; What is kept, deleted.

(defun visitor-key (key)
  "KEY when it is a visitor's file name as the summary gives them, or an
error: nothing else is looked for under the logs."
  (unless (and (stringp key) (plusp (length key)) (equal key (visitor-log:visitor-file-name key)))
    (error "Say which visitor: who, as the summary names them."))
  key)

(defun delete-turn! (key name)
  "The prompt NAME deleted from the prompt log; an error unless it is
one of the visitor's whose file name is KEY."
  (let ((turn (find name (visitor-turns key) :key (lambda (turn) (getf turn :turn)) :test #'equal)))
    (unless turn
      (error "That visitor has no such prompt."))
    (delete-file (getf turn :file))
    name))

(defun delete-kept! (key)
  "Everything the page shows of the visitor whose file name is KEY,
deleted: their activity file and their prompts in the prompt log.
Their invite stays, and so does a session of theirs in memory.  What
went, as (:lines n :prompts n)."
  (let ((turns (visitor-turns key)))
    (dolist (turn turns) (delete-file (getf turn :file)))
    (list :lines (getf (visitor-log:delete-visitor! *activity* key) :lines)
          :prompts (length turns))))

;;; The doors.

(defun handle-admin-visitor (req)
  "GET /tala/api/admin/visitor?who=KEY&limit=N: {who, total, events},
the last N things kept of one visitor, oldest first."
  (let ((key (net.aserve:request-query-value "who" req :post nil))
        (limit (or (ignore-errors
                    (parse-integer (net.aserve:request-query-value "limit" req :post nil)))
                   *admin-events*)))
    (multiple-value-bind (events total) (visitor-events (visitor-key key) (max 1 (min limit 20000)))
      (json-text (obj "who" key "total" total "events" events)))))

(defun handle-admin-delete (req)
  "POST /tala/api/admin/delete, a JSON body naming a visitor as the
summary does and what of theirs is to go: {who: KEY, everything: true},
everything kept of them; {who: KEY, line: N, mark: WORD}, one line of
their activity file, as the visitor door numbered and marked it; {who:
KEY, turn: NAME}, one of their prompts.  {deleted: {lines, prompts}}.
Nothing but a POST of JSON is taken, which a form on someone else's
page cannot send."
  (unless (and (eq (net.aserve:request-method req) :post)
               (search "application/json" (or (net.aserve:header-slot-value req :content-type) "")
                       :test #'char-equal))
    (error "Deleting takes a POST of JSON."))
  (let* ((body (ignore-errors (yason:parse (gwl:request-body-text req))))
         (key (visitor-key (and (hash-table-p body) (gethash "who" body))))
         (gone (cond ((eq (gethash "everything" body) t) (delete-kept! key))
                     ((gethash "turn" body)
                      (delete-turn! key (gethash "turn" body))
                      (list :lines 0 :prompts 1))
                     ((gethash "line" body)
                      (visitor-log:delete-line! *activity* key (gethash "line" body) (gethash "mark" body))
                      (list :lines 1 :prompts 0))
                     (t (error "Say what is to go: everything, a line or a turn.")))))
    (json-text (obj "deleted" (obj "lines" (getf gone :lines) "prompts" (getf gone :prompts))))))

(defun admin-html ()
  (visitor-log:admin-page
   "Tala admin"
   (list "/tala/admin-kit.js"
         (format nil "/tala/admin.js?v=~a"
                 (or (ignore-errors (file-write-date (merge-pathnames "web/admin.js" (tala-dir)))) 0)))))

(defun publish-admin! (server)
  "The admin page and its doors: behind the invite gate like every door
under /tala, and then for *admins* alone."
  (flet ((door (path content-type fn)
           (publish-door server path content-type fn :admin t)))
    (door "/tala/admin" "text/html; charset=utf-8"
          (lambda (req) (declare (ignore req)) (admin-html)))
    (door "/tala/admin-kit.js" "application/javascript; charset=utf-8"
          (lambda (req) (declare (ignore req)) visitor-log:*admin-script*))
    (door "/tala/admin.js" "application/javascript; charset=utf-8"
          (lambda (req) (declare (ignore req)) (web-file "admin.js")))
    (door "/tala/api/admin" "application/json"
          (lambda (req) (declare (ignore req)) (admin-summary)))
    (door "/tala/api/admin/visitor" "application/json" #'handle-admin-visitor)
    (door "/tala/api/admin/delete" "application/json" #'handle-admin-delete)))

(pushnew 'publish-admin! *tala-publishers*)
