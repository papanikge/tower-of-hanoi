;;
;; George 'papanikge' Papanikolaou 2014
;; Tower of Hanoi game description and functions for CEID
;;

; Format identifiers:
; ~A: string
; ~D: number
; ~%: new-line

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
  (format t "Smaller discs go on top of the bigger ones.~%")
  (abort)) ; TODO: is this abort correct???

; ask the user for a filename
(defun get-filename ()
  (format t "Please type a file name... ")
  (read))

; check if list is sorted
(defun is-sorted (ls)
  (equal ls (sort (copy-list ls) #'<)))

; load a game-conf from the provided file
(defun read-from-file ()
  (let ((filename))
    (setf filename (get-filename))
    (with-open-file (stream filename)
      (push (split-sequence (read-line stream)) (gethash :A stacks)))))
  ;;; TODO: need to put to correct peg and convert them to numbers (parse-integer). not strings

; save a game-conf to a provided file
(defun save-to-file (filename)
    ; TODO. write to file?
  )

; asks and saves the game-conf from user input
(defun get-conf-keyboard ()
  (format t "Rods: 3 (A, B, C)~%Discs: 5 (sizes: 1,2,3,4,5)~%")
  (format t "Please type the number of the corresponding discs you want in rod A. Type 0 when done.~%")
  (loop
    (let ((x))
      (setf x (read))
      (if (= x 0) break ; probably wrong. TODO.
        (push x (gethash :A stacks)))))
  (format t "Same for rod B~%")
  (loop
    (let ((x))
      (setf x (read))
      (if (= x 0) break ; probably wrong. TODO.
        (push x (gethash :B stacks)))))
  (format t "Same for rod C~%")
  (loop
    (let ((x))
      (setf x (read))
      (if (= x 0) break ; probably wrong. TODO.
        (push x (gethash :C stacks)))))
  ; test lists, so they following the rules. Smaller on top of bigger.
  (if (not (is-sorted (gethash :A stacks))) (die))
  (if (not (is-sorted (gethash :B stacks))) (die))
  (if (not (is-sorted (gethash :C stacks))) (die)))

; function to start them all. main?
(defun start ()
  (let ((x))
    (print-hello-msg)
    (setf x (read))
    (cond
      ((= x 1) (get-conf-keyboard))
      ((= x 2) (read-from-file (read)))
      (t return))))
