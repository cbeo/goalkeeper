;;;; goalkeeper.lisp

(in-package #:goalkeeper)

(defparameter +data-store-directory-name+
  "goalkeeper-store")

(defparameter +session-cookie-key+
  "session")

(defparameter +config-file+
  "goalkeeper.conf")


;;; SYSTEM INITIALIZATION

(defun data-store-path ()
  (make-pathname
   :directory (append (pathname-directory (user-homedir-pathname))
                      (list +data-store-directory-name+))))

(defun config-file-path ()
  (merge-pathnames +config-file+ (user-homedir-pathname)))


(defun initialize-datastore ()
  (ensure-directories-exist (data-store-path))
  (make-instance 'store:mp-store
                 :directory (data-store-path)
                 :subsystems (list (make-instance 'store:store-object-subsystem)
                                   (make-instance 'store:blob-subsystem))))

(defun load-initial-users ()
  (unless (uiop:file-exists-p (config-file-path))
    (error "Cannot load initial users: cannot find config"))
  (let ((config
          (with-open-file (input (config-file-path))
            (read input))))
    (loop :for (username password) :in (getf config :players)
          :do (make-player username password))))


(defun start ()
  (initialize-datastore)
  (when (zerop (length (all-players)))
    (load-initial-users))
  (lzb:start))

(defun start-loop () 
  (start)
  (print "type :quit to quit")
  (force-output)
  (loop :for command = (read)
        :do (case command
              (:quit
               (print "quitting")
               (lzb:stop)
               (uiop:quit))
              (t
               (print "type :quit to quit")))))


;;; CLASSES 

(defclass player (store:store-object)
  ((username
    :accessor username
    :initarg :username
    :initform (error "USERs must have a name")
    :index-type idx:string-unique-index 
    :index-reader player-by-name
    :index-values all-players)
   (pw-hash
    :accessor pw-hash
    :initarg :pw
    :initform (error "USERs must have a password")))
  (:metaclass store:persistent-class ))

(defclass session (store:store-object)
  ((player
    :reader session-player
    :initarg :player)
   (cookie
    :reader session-cookie
    :initarg :cookie
    :index-type idx:string-unique-index
    :index-reader session-by-cookie))
  (:metaclass store:persistent-class))

(defclass game (store:store-object)
  ((prize
    :accessor game-prize
    :initarg :prizes
    :initform "")
   (players
    :accessor game-players
    :initarg :players
    :initform (list))
   (start-time
    :accessor start-time
    :initarg :starting
    :initform nil)
   (end-time
    :accessor end-time
    :initarg :ending
    :initform nil))
  (:metaclass store:persistent-class))

(defclass goal (store:store-object)
  ((player
    :reader goal-player
    :initarg :player
    :initform (error "All goals must have a PLAYER")
    :index-type idx:hash-index
    :index-reader goals-by-player)
   (game
    :reader goal-game
    :initarg :game
    :initform (error "All goals must belong to a GAME")
    :index-type idx:hash-index
    :index-reader goals-by-game)
   (title
    :accessor goal-title
    :initarg :title
    :initform "")
   (evidence
    :accessor goal-evidence
    :initform nil)
   (votes
    :accessor goal-votes
    :initform (list)
    :documentation "A list of players who have agreed that this goal
    has been met. Counts as met if a majority of players in this game
    have approved of this goal. (> 50%)"))
  (:metaclass store:persistent-class))

(defclass evidence-blob (store:blob)
  ((orig-file-name :initarg :orig-name :reader orig-file-name :initform ""))
  (:metaclass store:persistent-class))

;;; UTILITIES

(defun image-blob-p (blob)
  (member (store:blob-mime-type blob)
          '("image/png" "image/jpg" "image/svg"
            "image/jpg" "image/jpeg" "image/bmp"
            "image/gif")
          :test #'string-equal))

(defun video-blob-p (blob)
  (member (store:blob-mime-type blob)
          '("video/webm" "video/mp4" "video/ogg")
          :test #'string-equal))

(defun audio-blob-p (blob)
  (member (store:blob-mime-type blob)
          '("audio/wav" "audio/webm" "audio/ogg" "audio/mp3")
          :test #'string-equal))

(defun make-session (player)
  (make-instance 'session
                 :player player
                 :cookie (make-session-cookie (username player))))

(defun cookie-header-value (session)
  (format nil "~a=~a" +session-cookie-key+ (session-cookie session)))

(define-condition player-exists-error (error)
  ((username :reader username :initarg :username)))

(defun make-session-cookie (username)
  (remove #\=  (base64:string-to-base64-string 
                (format nil "~a~a"
                        (gensym username)
                        (get-universal-time)))))

(defun pw-digest (str)
  (flexi-streams:octets-to-string 
   (ironclad:digest-sequence
    :sha256
    (flexi-streams:string-to-octets str :external-format :utf-8))
   :external-format :latin1))

(defun make-player (username password)
  (when (player-by-name (string-downcase username))
    (error 'player-exists-error :username username))
  (make-instance 'player
                 :username (string-downcase username)
                 :pw (pw-digest password)))



(defun make-game (player)
  (make-instance 'game :players (list player)))

(defun all-games ()
  (store:store-objects-with-class 'game))

(defun games-by-player (player)
  (remove-if-not
   (lambda (game) (member player (game-players game)))
   (all-games)))

(defun game-concluded-p (game)
  (local-time:timestamp<
   (local-time:parse-timestring (end-time game))
   (local-time:now)))

(defun game-pending-p (game)
  (local-time:timestamp<
   (local-time:now)
   (local-time:parse-timestring (start-time game))))

(defun game-active-p (game)
  (let ((now (local-time:now)))
    (with-slots (start-time end-time) game
      (local-time:timestamp<=
       (local-time:parse-timestring start-time)
       now
       (local-time:parse-timestring end-time)))))

(defun goal-met-p (goal)
  "A goal is met if a majority of players have 'voted' on it"
  (> (length (goal-votes goal))
      (* 0.5 (length (game-players (goal-game goal))))))

(defun make-goal (player game title)
  (make-instance 'goal
                 :game game
                 :player player
                 :title title))

(defun get-header (req header)
  (gethash header (getf req :headers)))

(defun parse-cookie (cookie-string)
  "transforms a cookie string into an alist"
  (loop :for pair :in  (split-string "; " cookie-string)
        :collect (split-string "=" pair)))

(defun get-cookie (req cookie-key)
  (when-let ((cookie (get-header req "cookie")))
    (second (assoc cookie-key (parse-cookie cookie) :test #'equal))))

(defun split-string (delimiter string)
  (labels ((rec (acc start)
             (if-let (found (search delimiter string :start2 start))
               (rec (cons (subseq string start found) acc)
                    (+ found (length delimiter)))
               (reverse (cons (subseq string start) acc)))))
    (rec nil 0)))


;;; TRANSACTIONS 

(defun login-player (username password)
  "Looks up a PLAYER by username and password."
  (store:with-transaction () 
    (when-let (player (player-by-name (string-downcase username)))
      (when (equal (pw-digest password) (pw-hash player))
        (make-session player)))))

(defun set-password (player new-pass)
  (store:with-transaction ()
    (setf (pw-hash player) (pw-digest new-pass))))

(defun find-user-session (req)
  (when-let (session-cookie (get-cookie req +session-cookie-key+))
    (when-let (session (session-by-cookie session-cookie))
      (session-player session))))

(defun create-new-game (player &key prize start-date end-date)
  (store:with-transaction ()
    (make-instance 'game
                   :starting start-date
                   :ending end-date
                   :prizes prize
                   :players (list player))))

(defun create-goal (player game title)
  (store:with-transaction ()
    (make-goal player game title)))

(defun player-votes-for-goal (goal player)
  (store:with-transaction ()
    (pushnew player (goal-votes goal))))

(defun player-retracts-vote-for-goal (goal player)
  (store:with-transaction ()
    (setf (goal-votes goal)
          (remove player (goal-votes goal)))))

(defun add-evidence-text (goal evidence)
  (store:with-transaction ()
    (let ((current (goal-evidence goal)))
      (setf (goal-evidence goal)
            (cond ((equal "" current) (list evidence))
                  ((listp current) (cons evidence current))
                  (t (list evidence current)))))))

(defun add-evidence-blob (goal filename content-type file)
  (store:with-transaction () 
    (let ((blob (store:make-blob-from-file file 'evidence-blob
                                           :type content-type
                                           :orig-name filename)))
      (let ((current (goal-evidence goal)))
        (setf (goal-evidence goal)
              (cond ((equal "" current) (list blob))
                    ((listp current) (cons blob current))
                    (t (list blob current))))))))

(defun add-player-to-game (game player)
  (store:with-transaction ()
    (pushnew player (game-players game ))))

(defun delete-goal (goal)
  (store:with-transaction ()
    (store:delete-object goal)))


(defun remove-nth (n ls)
  (cond ((= n 0) (cdr ls))
        ((null ls) ls)
        (t 
         (cons (car ls)
               (remove-nth (1- n) (cdr ls))))))

(defun evidence-blob-p (object)
  (eql (class-of object)
       (find-class 'evidence-blob)))

(defun drop-evidence-from-goal (goal index)
  (store:with-transaction ()
    (if (listp (goal-evidence goal))
        (let ((to-drop (nth index (goal-evidence goal))))
          (setf (goal-evidence goal)
                (remove-nth index (goal-evidence goal)))
          (when (evidence-blob-p to-drop)
            (store:delete-object to-drop)))

        (setf (goal-evidence goal) (list)))))

;;; PAGES

(defun main-css ()
  (lass:compile-and-write
   '(:let ((bg-color "#232366")
           (text-color "#FFFFFF")
           (tab-color "#121255")
           (secondary-bg-color "#343477"))
     (body
      :background-color #(bg-color)
      :color #(text-color))

     (a
      :color #(text-color)
      )

     (.button
      :background-color #(secondary-bg-color)
      :color #(text-color)
      :border 1px solid #(text-color) 
      :text-decoration none
      :border-radius 4px
      :padding 4px
      :margin 6px)

     ((:and .button :hover)
      :cursor pointer)

     (h1
      :color #(text-color))

     (form
      :margin 12px
      ((:or input button label)
       :padding 2px
       :margin 6px))

     (.flex-container
      :flex-direction row
      :display flex)

     (.evidence-grid
      :display grid
      :grid-template-columns "repeat(auto-fit, minmax(300px, 1fr))"

      (div
       :margin 4px
       :border-radius 4px
       :background-color #(tab-color)))

     
     (.grid-container
      :width 100%
      :margin-top 20px
      :display grid
      :grid-column-gap 20px
      :grid-row-gap 10px
      :grid-template-columns "repeat(auto-fit, minmax(500px, 1fr))"
      )

     (.evidence-form
      :border 1px solid #(text-color)
      :border-radius 4px
      :padding 5px)

     (.right
      :float right)
     
     (.scorecard
      
      (h3
       :text-align center
       ))

     (.card
      :background-color #(secondary-bg-color)
      :padding 5px
      :border-radius 5px
      :margin 15px
      :border-top 5px solid #(text-color)
      )
     
     (nav
      :background-color #(tab-color)
      :width 100%
      :border-top #(text-color) 4px solid
      (a
       :color #(text-color)
       :padding 10px
       :text-align center
       :display inline-block
       :border none
       :font-size 1.2em
       :border-radius 5px)))))

(defun nav ()
  (html:with-html
    (:nav
     (:a :href "/" "Home")
     " "
     (:a :href "/account" "Account"))))

(defun view/change-password ()
  (html:with-html
    (:div 
     (:h3 "Change Password")
     (:form :method "POST" :action "/player/password"
            (:input :name "oldpw" :placeholder "Old Password" :type "password")
            (:input :name "newpw" :placeholder "New Password" :type "password")
            (:input :name "repeatnewpw" :placeholder "Repeat New Password" :type "password")            
            (:button :class "button" :type "submit" "New Password")))))

(defun page/account (player &key message)
  (http-ok
   "text/html"
   (html:with-html-string
     (:doctype)
     (:html
      (:head
       (:title "Goalkeeper - Account")
       (:style (main-css)))
      (:body
       (:h1 "Account")
       (:p (username player))
       (when message
         (:p :class "toast"
             message))
       (nav)
       (view/change-password))))))

(defun page/player-dash (player)
  (http-ok
   "text/html"
   (html:with-html-string 
     (:doctype)
     (:html
      (:head
       (:title "Goalkeeper - Dashboard")
       (:style (main-css)))
      (:body
       (:h1 "Dashboard")
       (nav)
       (:div :class "greeting" 
             (:p "Greetings " (username player)))
       (:a :href "/game/new" :class "button" "Add a Game" )
       (when-let (games (games-by-player player))
         (:h3 "active games")
         (:div :class "flex-container"
               (dolist (game (remove-if-not #'game-active-p games))
                 (listing/game game)))
         (:h3 "future games")
         (:div :class "flex-container"
               (dolist (game (remove-if-not #'game-pending-p games))
                 (listing/game game)))
         (:h3 "past games")
         (:div :class "flex-container"
               (dolist (game (remove-if-not #'game-concluded-p games))
                 (listing/game game)))))))))

(defun page/add-a-game ()
  (http-ok
   "text/html"
   (html:with-html-string
     (:doctype)
     (:html
      (:head
       (:title "Goalkeeper - Add A Game")
       (:style (main-css)))
      (:body
       (:h1 "Create A New Game")
       (nav)
       (:form :method "POST" :action "/game/new"
              (:input :name "prize" :type "text" :placeholder "What's the prize?")
              (:br)
              (:input :name "start-date" :type "date" :placeholder "Start Date")
              (:label :for "start-date" "Start Date")
              (:br)
              (:input :name "end-date" :type "date" :placeholder "End Date")
              (:label :for "end-date" "End Date")
              (:br)
              (:button :type "submit" :class "button" "Create Game")))))))

(defun page/login ()
  (http-ok
   "text/html"
   (html:with-html-string
     (:doctype)
     (:html 
      (:head
       (:title "Goalkeeper - Login")
       (:style (main-css)))
      (:body
       (:h1 "Goalkeeper")
       (:form :method "POST" :action "/login"
              (:input :placeholder "Username" :type "text" :name "username")
              (:input :placeholder "Password" :type "password" :name "password")
              (:button :type "submit" :class "button" "Login")))))))



(defun listing/game (game)
  (html:with-html
    (:div :class "card"
          
          (:h3 (start-time  game) " - " (end-time game) " "
               )
          (:a :href (format nil "/game/~a/view" (store:store-object-id game))
              :class "button"
              "view")
          (:p (:strong  "Prize: ") (game-prize game))
          (:p (:strong "Participants: "))
          (:ul
           (loop :for (player tally count) :in (scores game)
                 :do (:li (username player) " -  "
                          (format nil "~a" tally)
                          " out of "
                          (format nil "~a"  count)))))))

(defun scores (game)
  (let* ((all-goals
           (goals-by-game game))

         (players
           (game-players game))

         (score-for
           (lambda (player)
             (let ((goals
                     (remove-if-not
                      (lambda (goal) (equal player (goal-player goal)))
                      all-goals)))
               (list (count-if #'goal-met-p goals)
                     (length goals))))))

    (loop :for player :in players
          :collect (cons player (funcall score-for player)))))


(defun listing/goal (player goal editable)
  (html:with-html 
    (:div :class "card"
          (:p (:strong 
               (goal-title goal)))
          (:p (:strong  "Met? ")
              (if (goal-met-p goal) "Yes" "Not Yet"))
          (:div
           (when (game-active-p (goal-game goal))
             (if (member player (goal-votes goal))
                 (:a :href (format nil "/goal/~a/unvote"
                                   (store:store-object-id goal))
                     :class "button"
                      "❌")
                 (:a :href (format nil "/goal/~a/vote"
                                   (store:store-object-id goal))
                     :class "button"
                     " ✔ ")))
              (format nil " ~a out of ~a"
                      (length (goal-votes goal))
                      (length (game-players (goal-game goal))))
              " players.")
          (:div 
           (when (and editable (game-active-p (goal-game goal)))
             (:div
              :class "evidence-form" 
              (:h4 "Post some evidence:")
              (:form :method "POST"
                     :action  (format nil "/goal/~a/evidence"
                                      (store:store-object-id goal))
                     (:label :for "evidence"
                             "Either enter some text: ")
                     (:input :placeholder "Evidence"
                             :value (if (stringp (goal-evidence goal))
                                        (goal-evidence goal)
                                        "")
                             :name "evidence")
                     
                     (:button :type "submit" :class "button" "Update"))

              (:form :method "POST"
                     :action (format nil "/goal/~a/evidence-file"
                                     (store:store-object-id goal))
                     :enctype "multipart/form-data"
                     (:label :for "evidence-file" " or upload a file: ")
                     (:input :type "file"
                             :name "evidence-file"
                             :value "none")
                     (:button :type "submit" :class "button" "Upload"))))

           (view/evidence goal editable))

          (when (and editable (game-pending-p (goal-game goal)))
            (:div :style "margin-top: 20px"
             (:a :class "button"
                 :href (format nil "/goal/~a/delete"
                               (store:store-object-id goal))
                 "Drop Goal"))))))

(defun view/evidence (goal editable)
  (html:with-html 
    (:div
     (:h4 "Evidence:")
     (let ((evidence-list
             (if (not (listp  (goal-evidence goal)))
                 (list (goal-evidence goal))
                 (goal-evidence goal))))

       (:div
        :class "evidence-grid"
        (loop :for idx :from 0
              :for evidence :in evidence-list
              :do
                 (:div
                  :class "evidence"
                  
                  (cond
                    ((and (stringp evidence)
                          (< 4 (length evidence))
                          (string-equal "http" evidence :end2 4))
                     (:a :href evidence evidence " "))

                    ((stringp evidence)
                     (:p  evidence " "))
                    
                    ((image-blob-p evidence)
                     (let ((url (format nil "/files/~a"
                                        (store:store-object-id evidence))))
                       (:a :href url
                           (:img :src url :height "160"))))
                    
                    ((video-blob-p evidence)
                     (let ((url (format nil "/files/~a"
                                        (store:store-object-id evidence))))
                       (:video :controls "true" :width "160"
                               (:source :src url
                                        :type (store:blob-mime-type evidence)))))

                    ((audio-blob-p evidence)
                     (let ((url (format nil "/files/~a"
                                        (store:store-object-id evidence))))
                       (:audio :controls "true" :width "160"
                               (:source :src url))))
                    
                    
                    (t (:a :href (format nil "/files/~a"
                                         (store:store-object-id evidence))
                          (orig-file-name evidence) )))

                  (when (and editable (game-active-p (goal-game goal)))
                    (:a :href (format nil "/goal/~a/drop-evidence/~a"
                                      (store:store-object-id goal)
                                      idx)
                        :class "button right"
                        "❌"))
                  )))))))

(defun view/scorecard (game player editable)
  (html:with-html
    (:div :class "scorecard"
          (:h3 (username player) "'s goals")
          (dolist (goal (goals-by-game game))
            (when (eql player (goal-player goal))
              (listing/goal player goal editable))))))


(defun view/game-edit-forms (game)
  (html:with-html
    (:div
     (:p "You can edit this game until the start date")
     (:form  :method "POST"
             :action (format nil "/game/~a/prize"
                             (store:store-object-id game))
             (:input :placeholder "prize"
                     :name "prize"
                     :value (game-prize game))
             (:button :type "submit" :class "button" "Update Prize"))
     
     (:form :method "POST"
            :action (format nil "/game/~a/invite" (store:store-object-id game))
                                        ;(:label :for "playerid" "Add another player")
            (:select :name "playerid"
              (dolist (player (all-players))
                (:option :value (format nil "~a" (store:store-object-id player))
                         (username player))))
            (:button :type "submit" :class "button" "Add player"))
     (:form :method "POST" :action (format nil  "/goal/add/~a" (store:store-object-id game))
            (:input :placeholder "Goal title" :name "title")
            (:button :type "submit" :class "button" "Add A Goal")))))

(defun page/game-view (this-player game)
  (declare (ignorable this-player))
  (http-ok
   "text/html"
   (html:with-html-string
     (:doctype)
     (:html
      (:head
       (:title "Game View")
       (:style (main-css)))
      (:body
       (:h1 "Game period: " (start-time game) " - " (end-time game))
       (nav)
       (:div
        (if (game-pending-p game)
            (view/game-edit-forms game)
            (:div
             (:p (:strong "Prize: ") (game-prize game))
             (:p (:strong "Scores: "))
             (:ul
              (loop :for (player tally count) :in (scores game)
                    :do (:li (username player) " -  "
                             (format nil "~a" tally)
                             " out of "
                             (format nil "~a"  count)))))))
       (:div :class "grid-container"
             (dolist (player (game-players game))
               (view/scorecard game player (eql player this-player)))))))))

(defun page/games-list (player)
  (http-ok
   "text/html"
   (html:with-html-string
     (:doctype)
     (:html
      (:head
       (:title "Goalkeeper - Games")
       (:style (main-css)))
      (:body
       (:h1 "Games list")
       (nav)
       (:div :class "flex-container"
        (dolist (game (games-by-player player))
          (listing/game game)))
       )))))

;;; ROUTES

(defroute :get "/" 
  (if-let (player (find-user-session *req*))
    (page/player-dash player)
    (page/login)))

(defroute :get "/games"
  (if-let (player (find-user-session *req*))
    (page/games-list player)
    (http-err 403 "Forbidden")))

(defroute :post "/login"
  (if-let (session (login-player (getf *body* :username)
                                 (getf *body* :password)))
    (progn 
      (lzb:add-header :set-cookie (cookie-header-value session))
      (page/player-dash (session-player session)))
    (page/login)))

(defroute :get "/account"
  (if-let (player (find-user-session *req*))
    (page/account player)
    (http-err 403 "Forbidden")))

(defroute :get "/game/new"
  (if (find-user-session *req*)
      (page/add-a-game)
      (http-err 403 "Forbidden")))

(defroute :post "/game/new"
  (if-let (player (find-user-session *req*))
    (progn
      (apply #'create-new-game player *body*)
      (page/player-dash player))
    (http-err 403 "Forbidden")))

(defroute :get "/game/:gameid/view"
  (let* ((player (find-user-session *req*))
         (game (and player (store:store-object-with-id (parse-integer gameid)))))
    (cond ((and player game)
          (page/game-view player game))
          (player
           (http-err 404 "Game Not Found"))
          (t
           (http-err 403 "Forbidden")))))

(defroute :post "/goal/add/:gameid"
  (let* ((player
           (find-user-session *req*))
         (game-id
           (parse-integer gameid))
         (game
          (and player
               (store:store-object-with-id game-id))))

    (cond ((and game (member player (game-players game)))
           (create-goal player game (getf *body* :title))
           (page/game-view player game))
          (t
           (http-err 403 "Forbidden")))))

(defroute :get "/goal/:goalid/unvote"
  (let* ((player
           (find-user-session *req*))
         (goal
           (and player (store:store-object-with-id (parse-integer goalid)))))
    (cond ((and goal (member player (game-players (goal-game goal))))
           (player-retracts-vote-for-goal goal player)
           (page/game-view player (goal-game goal)))

          (t
           (http-err 403 "Forbidden")))))

(defroute :get "/goal/:goalid/vote"
  (let* ((player
           (find-user-session *req*))
         (goal
           (and player (store:store-object-with-id (parse-integer goalid)))))
    (cond ((and goal (member player (game-players (goal-game goal))))
           (player-votes-for-goal goal player)
           (page/game-view player (goal-game goal)))

          (t
           (http-err 403 "Forbidden")))))

(defroute :post "/goal/:goalid/evidence"
  (let* ((player
           (find-user-session *req*))
         (goal
           (and player (store:store-object-with-id (parse-integer goalid)))))
    (cond ((and goal (eql player (goal-player goal)))
           (add-evidence-text goal (getf *body* :evidence))
           (page/game-view player (goal-game goal)))
          (t
           (http-err 403 "Forbidden")))))

(defroute :post "/goal/:goalid/evidence-file"
  (let* ((player
           (find-user-session *req*))
         (goal
           (and player (store:store-object-with-id (parse-integer goalid)))))
    (cond ((and goal (eql player (goal-player goal)))
           (let ((upload-data (first *body*)))
             (add-evidence-blob goal
                                (getf upload-data :filename)
                                (getf upload-data :content-type)
                                (getf upload-data :body)))
           (page/game-view player (goal-game goal)))
          (t
           (http-err 403 "Forbidden")))))

(defroute :post "/game/:gameid/invite"
  (let* ((player
           (find-user-session *req*))
         (game
           (and player (store:store-object-with-id (parse-integer gameid))))
         (other-player
           (store:store-object-with-id (parse-integer (getf *body* :playerid)))))
    (cond ((and game other-player (member player (game-players game)))
           (add-player-to-game game other-player)
           (page/game-view player game))
          (t
           (http-err 403 "Forbidden")))))


(defun able-to-change-password-p (player &key oldpw newpw repeatnewpw)
  (and (equal newpw repeatnewpw)
       (plusp (length newpw))
       (equal (pw-hash player)
              (pw-digest oldpw))))

(defroute :post "/player/password"
  (let ((player (find-user-session *req*)) )
    (if (apply #'able-to-change-password-p player *body*)
        (progn
          (set-password player (getf *body* :newpw))
          (page/account player :message "Password updated!"))
        (page/account player :message "Failed to update password - try again?"))))

(defroute :get "/goal/:goalid/delete"
  (let ((player (find-user-session *req*))
        (goal (store:store-object-with-id (parse-integer goalid))))
    (cond ((and player goal)
           (let ((game (goal-game goal)))
             (delete-goal goal)
             (page/game-view player game)))
          (player
           (http-err 404 "Goal not found"))
          (t
           (http-err 403 "Forbidden")))))

(defroute :post "/game/:gameid/prize"
  (let ((player (find-user-session *req*))
        (new-prize (getf *body* :prize))
        (game (store:store-object-with-id (parse-integer gameid))))
    (cond
      ((and player game new-prize)
       (store:with-transaction ()
         (setf (game-prize game) new-prize))
       (page/game-view player game))

      (player
       (http-err 404 "Game not found"))

      (t
       (http-err 403 "Forbidden")))))

(defroute :get "/files/:imageid"
  (let ((player (find-user-session *req*))
        (blob (store:store-object-with-id (parse-integer imageid))))
    (if (and player blob)
        (http-ok
         (store:blob-mime-type blob)
         (alexandria:read-file-into-byte-vector
          (store:blob-pathname blob)))
        (http-err 403 "Forbidden"))))

(defroute :get "/goal/:goalid/drop-evidence/:idx"
  (let ((player (find-user-session *req*))
        (goal (store:store-object-with-id (parse-integer goalid))))
    (if (and player goal (eql player (goal-player goal)))
        (progn (drop-evidence-from-goal goal (parse-integer idx))
               
               (page/game-view player (goal-game goal)))
        (http-err 403 "Forbidden"))))
