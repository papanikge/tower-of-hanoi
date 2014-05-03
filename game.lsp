;;
;; George 'papanikge' Papanikolaou 2014
;; Tower of Hanoi game description and functions for CEID
;;

; We are going to keep the game-configuration in the global var stacks.
; Using a hash-table so we can have names for each rod/peg.
(defvar stacks)
(setf stacks (make-hash-table))

; self-explanatory explanation message
(defun print-hello-msg ()
  (format t "Welcome to Tower of Hanoi emulation!!!~%")
  (format t "    Press 1 if you want to enter the game configuration from the keyboard.~%")
  (format t "       or 2 if you want to enter a file name to read it from.~%"))

; our abort function in case of wrong game configuration
(defun die ()
  (format t "The game configuration format is wrong.~%")
  (format t "Smaller discs go on top of the bigger ones. Aborting and reinitializing...~%")
  (init))

; ask the user for a filename
(defun get-filename ()
  (format t "Please type a file name... ")
  (finish-output)
  (read-line))

; print the game configuration
(defun print-conf ()
  (format t "~%=-=-=-= Game configuration =-=-=-= ~%")
  (if (not (null (gethash :A stacks))) (format t "A: ~S~%" (gethash :A stacks)) (format t "A: ()~%"))
  (if (not (null (gethash :B stacks))) (format t "B: ~S~%" (gethash :B stacks)) (format t "B: ()~%"))
  (if (not (null (gethash :C stacks))) (format t "C: ~S~%" (gethash :C stacks)) (format t "C: ()~%")))

; check if list is sorted
(defun is-sorted (ls)
  (equal ls (sort (copy-list ls) #'<)))

; auxiliary function to translate number of rods to symbols
(defun translate-to-symbols (n)
  (cond
    ((= n 1) :A)
    ((= n 2) :B)
    ((= n 3) :C)
    (t (die))))

; load a game-conf from the provided file
(defun read-from-file ()
  (let ((filename (get-filename)) (temp '()) (lines 1))
    (if (probe-file filename)
      (with-open-file (stream filename)
        ; parsing the file character-by-character
        (do ((char (read-char stream nil)
                   (read-char stream nil)))
          ((null char))
          (if (<= lines 3)
            (if (eql char #\Newline)
              (progn
                ; this is where all the magic with the actual lists is happening
                (setf (gethash (translate-to-symbols lines) stacks) (reverse temp))
                ; re-initialize the temp one for the next round
                (setf temp '())
                (setf lines (1+ lines)))
              ; if we get an actual character:
              (push (digit-char-p char) temp)))))
      (format t "File does not exist.~%"))))

; save a game-conf to a provided file
(defun save-to-file ()
  (let ()
    (with-open-file (str (get-filename)
                         :direction :output
                         :if-exists :supersede  ; overwriting old files
                         :if-does-not-exist :create)
      (dolist (ch (gethash :A stacks))
        (format str "~D" ch))
      (format str "~%")
      (dolist (ch (gethash :B stacks))
        (format str "~D" ch))
      (format str "~%")
      (dolist (ch (gethash :C stacks))
        (format str "~D" ch))
      (format str "~%"))))

; asks and saves the game-conf from user input
(defun get-conf-keyboard ()
  (format t "Rods: 3 (A, B, C) --- Discs: 5 (sizes: 1,2,3,4,5)~%")
  (loop for i from 5 downto 1 do
    (format t "Disc: ~D - Please type the corresponding key of the rod you want the disc placed.~%" i)
    (let ((r))
      (setf r (read-line))
      (cond
        ((string-equal "a" r) (push i (gethash :A stacks)))
        ((string-equal "b" r) (push i (gethash :B stacks)))
        ((string-equal "c" r) (push i (gethash :C stacks)))
        (t (die)))))
  ; test lists, so they following the rules. Smaller on top of bigger.
  (if (not (is-sorted (gethash :A stacks))) (die))
  (if (not (is-sorted (gethash :B stacks))) (die))
  (if (not (is-sorted (gethash :C stacks))) (die)))

; this is an auxiliary function to avoid multiple if-s in the move-disc macro
; it is used to check the second ('to') list
(defun myfirst (lis)
  (if (null lis)
    6 ; returns a value larger than the larger disc
    (first lis)))

; the move macro that moves from list 'from' to 'to' if possible
(defmacro move-disc (from to)
  `(if (and
         (not (null ,from))
         (< (first ,from) (myfirst ,to)))
     (push (pop ,from) ,to)
     (format t "Your move was illegal. ~%")))

; main (wrapper) function for one step of the game. Moves and prints.
; accepts symbols (:A :B :C or lower case)
(defun move (from to)
  (move-disc (gethash from stacks) (gethash to stacks))
  (print-conf)
  (abort)) ; this is just to avoid printing the returned NIL

; function to init them all. main?
(defun init ()
  (setf (gethash :A stacks) '())
  (setf (gethash :B stacks) '())
  (setf (gethash :C stacks) '())
  (let ((x))
    (print-hello-msg)
    (setf x (read-line))
    (cond
      ((= (parse-integer x) 1) (get-conf-keyboard))
      ((= (parse-integer x) 2) (read-from-file))
      (t (progn
           (format t "Please enter only 1 or 2.~%")
           (init))))))
