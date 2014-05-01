;;
;; George 'papanikge' Papanikolaou 2014
;; Tower of Hanoi game description and functions for CEID
;;

; We are going to keep the game-configuration in the global var stacks.
; Using a hash-table so we can have names for each rod/peg.
(defvar stacks)
(setf stacks (make-hash-table))
(setf (gethash :A stacks) '())
(setf (gethash :B stacks) '())
(setf (gethash :C stacks) '())

; self-explanatory explanation message
(defun print-hello-msg ()
  (format t "Welcome to Tower of Hanoi emulation!!!~%")
  (format t "    Press 1 if you want to enter the game configuration in the keyboard.~%")
  (format t "       or 2 if you want to enter a file name.~%"))

; our abort function in case of wrong game configuration
(defun die ()
  (format t "The game configuration format is wrong.~%")
  (format t "Smaller discs go on top of the bigger ones. Aborting.~%")
  (abort))

; ask the user for a filename
(defun get-filename ()
  (format t "Please type a file name... ")
  (finish-output)
  (read-line))

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
  (let ((filename) (lines))
    (setf lines 1)
    (setf filename (get-filename))
    (with-open-file (stream filename)
      (loop
        ; our file should only have 3 lines (as many as the rods)
        (when (>= lines 3) (return))
        (let ((x) (where))
          ; reading & testing char-by-char
          (setf x (read-char stream))
          (if (eql x #\Newline)
            (+ lines 1)
            (do
              (setf where (translate-to-symbols lines))
              (push (parse-integer x) (gethash where stacks)))))))))

; save a game-conf to a provided file
(defun save-to-file ()
  (let ((one))
    (with-open-file (str (get-filename)
                         :direction :output
                         :if-exists :supersede
                         :if-does-not-exist :create)
      (dolist (one (gethash :A stacks))
        (format str "~D" one))
      (format str "~%")
      (dolist (one (gethash :B stacks))
        (format str "~D" one))
      (format str "~%")
      (dolist (one (gethash :C stacks))
        (format str "~D" one))
      (format str "~%"))))

; asks and saves the game-conf from user input
(defun get-conf-keyboard ()
  (format t "Rods: 3 (A, B, C)~%Discs: 5 (sizes: 1,2,3,4,5)~%")
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

; wrapper function to ease the movement (without using hashes)
; and to print the game configuration. Accepts symbols (:A :B :C or lower case)
(defun move (from to)
  (move-disc (gethash from stacks) (gethash to stacks))
  ; TODO print configuration
  )

; the main move function. Moves from list a to b if possible
(defun move-disc (from to)
  (if (and
        (not (null from))
        (< (first from) (first to)))
    (push (pop from) to)
    (format t "Your move was illegal. ~%")))

; function to start them all. main?
(defun start ()
  (let ((x))
    (print-hello-msg)
    (setf x (read-line))
    (cond
      ((= x 1) (get-conf-keyboard))
      ((= x 2) (read-from-file))
      (t (do
           (format t "Please enter only 1 or 2.~%")
           (abort))))))
