;;;; goalkeeper.lisp

(in-package #:goalkeeper)

(defparameter +data-store-directory-name+
  "goalkeeper-store")

(defparameter +session-cookie-key+
  "sesson")

(defun data-store-path ()
  (make-pathname
   :directory (append (pathname-directory (user-homedir-pathname))
                      (list +data-store-directory-name+))))

(defun initialize-datastore ()
  (ensure-directories-exist (data-store-path))
  (make-instance 'store:mp-store
                 :directory (data-store-path)
                 :subsystems (list (make-instance 'store:store-object-subsystem)
                                   (make-instance 'store:blob-subsystem))))

(defclass player (store:store-object)
  ((username
    :accessor username
    :initarg :username
    :initform (error "USERs must have a name")
    :index-type idx:string-unique-index 
    :index-reader player-by-name
    :index-values player-users)
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

(defun make-session (player)
  (make-instance 'session
                 :player player
                 :cookie (make-session-cookie (username player))))

(defun cookie-header-value (session)
  (format nil "~a=~a" +session-cookie-key+ (session-cookie session)))

(define-condition player-exists-error (error)
  ((username :reader username :initarg :username)))

(defun make-session-cookie (username)
  (base64:string-to-base64-string 
   (format nil "~a~a"
           (gensym username)
           (get-universal-time))))

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

(defun make-game (player)
  (make-instance 'game :players (list player)))

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
   (votes
    :accessor goal-votes
    :initform (list)
    :documentation "A list of players who have agreed that this goal
    has been met. Counts as met if a majority of players in this game
    have approved of this goal. (> 50%)"))
  (:metaclass store:persistent-class))

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


;;; transactions 

(defun login-player (username password)
  "Looks up a PLAYER by username and password."
  (store:with-transaction () 
    (when-let (player (player-by-name (string-downcase username)))
      (when (equal (pw-digest password) (pw-hash player))
        (make-session player)))))


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

;;; pages

(defun main-css ()
  (lass:compile-and-write
   '(:let ((bg-color "#232366")
           (tab-color "#121244")
           (secondary-bg-color "#343477"))
     (body
      :background-color #(bg-color)
      :color white)

     (h1
      :color white)

     (form
      :margin 12px
      ((:or input button label)
       :padding 2px
       :margin 6px))

     (.card
      :background-color #(secondary-bg-color)
      :padding 10px
      :border-radius 5px
      :margin 5px)
     
     (nav
      :background-color #(tab-color)
      :width 100%
      (a
       :color white
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
     (:a :href "/games" "Games")
     " "
     (:a :href "/game/new" "Add a Game"))))

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
             (:p "Greetings " (username player))))))))

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
              (:button :type "submit" "Create Game")))))))

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
              (:button :type "submit" "Login")))))))

(defun all-games ()
  (store:store-objects-with-class 'game))

(defun listing/game (game)
  (html:with-html
    (:div :class "card"
     (:h3 (start-time  game) " - " (end-time game))
     (:p (:strong  "Prize: ") (game-prize game))
     (:p (:strong "Participants"))
     (:ul
      (dolist (player (game-players game))
        (:li (username player)))))))

(defun page/games-list ()
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
       (:div :id "games-list"
        (dolist (game (all-games))
          (listing/game game)))
       )))))

;;; routes

(defroute :get "/" 
  (if-let (player (find-user-session *req*))
    (page/player-dash player)
    (page/login)))

(defroute :get "/games"
  (if-let (session (find-user-session *req*))
    (page/games-list)
    (http-err 403 "Forbidden")))

(defroute :post "/login"
  (if-let (session (login-player (getf *body* :username)
                                 (getf *body* :password)))
    (progn 
      (lzb:add-header :set-cookie (cookie-header-value session))
      (page/player-dash (session-player session)))
    (page/login)))

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
