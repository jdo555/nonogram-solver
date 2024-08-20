(defpackage #:nonogram-solver/cli-demo
  (:use #:common-lisp
        #:nonogram-solver
        #:nonogram-solver/tests/test-nonograms)
  (:export #:main-menu))

(in-package #:nonogram-solver/cli-demo)

(defun split-by-char (original-string character)
  "Splits a string according to the passed CHARACTER, returning a list of strings. Note that the resulting list does not include empty strings."
  (let ((string-list nil) ;; this is the final return value, which is a list of strings
        (cur-str (make-array 0 :element-type 'character :fill-pointer 0))) ;; this holds subsequent characters until a "splitting" character is found, at which point it is reset again
    (dotimes (i (length original-string))
      (if (char= (char original-string i) character) ;; if the searched for CHARACTER is found at current index
          ;; THEN, when CUR-STR is not an empty string, push it onto STRING-LIST and then reset CUR-STR
          (when (> (fill-pointer cur-str) 0)
            (push cur-str string-list)
            (setq cur-str (make-array 0 :element-type 'character :fill-pointer 0)))
          ;; ELSE, save the character into CUR-STR
          (vector-push-extend (char original-string i) cur-str)))
    ;; when CUR-STR is not empty after completion of loop, add it to list
    (when (> (fill-pointer cur-str) 0)
      (push cur-str string-list))
    ;; reversing the list to maintain proper ordering of substrings
    (reverse string-list)))

(defun show-and-solve-created-nonogram (side-clues top-clues)
  "Creates an instance of a nonogram, and then prompts the user to press enter, after which the nonogram-solver is run against the created nonogram."
  (let ((inst (make-instance 'nonogram :side-clues side-clues :top-clues top-clues)))
    (print-grid inst nil nil nil :border nil)
    (format t "~%Press enter to continue...")
    (read-line *query-io*)
    (format t "~%")
    (solve-nonogram inst)))

;;; !!! include detailed error-checking later...
(defun manual-input-menu ()
  (let ((side-clues (make-array 1 :adjustable t :fill-pointer 0))
        (top-clues (make-array 1 :adjustable t :fill-pointer 0)))
    (format t "~%Enter the side-clues one row at a time, separating numbers with spaces only. Enter an asterisk (*) when finished.~%~%")
    (do ((user-input nil nil)
         (clue-seq nil nil)
         (row-count 1 (1+ row-count)))
        (nil)
      (setq clue-seq (make-array 1 :element-type 'integer :adjustable t :fill-pointer 0))
      (format t "Row ~d clues: " row-count)
      (setq user-input (read-line *query-io*))
      (when (string= user-input "*")
        (return))
      (dolist (num-as-str (split-by-char user-input #\space))
        (vector-push-extend (parse-integer num-as-str) clue-seq))
      (vector-push-extend (copy-seq clue-seq) side-clues))
    (format t "~%Enter the top-clues one column at a time, separating numbers with spaces only. Enter an asterisk (*) when finished.~%~%")
    (do ((user-input nil nil)
         (clue-seq nil nil)
         (column-count 1 (1+ column-count)))
        (nil)
      (setq clue-seq (make-array 1 :element-type 'integer :adjustable t :fill-pointer 0))
      (format t "Column ~d clues: " column-count)
      (setq user-input (read-line *query-io*))
      (when (string= user-input "*")
        (return))
      (dolist (num-as-str (split-by-char user-input #\space))
        (vector-push-extend (parse-integer num-as-str) clue-seq))
      (vector-push-extend (copy-seq clue-seq) top-clues))
    (show-and-solve-created-nonogram side-clues top-clues)))

(defun split-by-str (original-string searched-for-string)
  "Splits a string according to the passed SEARCHED-FOR-STRING, returning a list of strings. Note that the resulting list does not include empty strings. Also, note that the search occurs from left-to-right so that if overlapping subsequences exist, only the leftmost one will be matched (e.g. (SPLIT-BY-STRING \"eee\" \"ee\") results in '(\"e\"); here the first two Es are the match [indexes 0 and 1], whereas the second subsequence of Es [indexes 1 and 2] does not get considered because, after the initial match, the next comparison starts at index 2, looking further right)."
  (let* ((original-string-len (length original-string))
         (sfs-len (length searched-for-string)) ;; sfs -> Searched-For-String
         (index-exclusive-end (- original-string-len (1- sfs-len))) ;; ! this is used to control the iteration count for the following DO loop...
         (end-indexes nil)
         (start-indexes (list 0))) ;; ! notice that this list, unlike END-INDEXES, does not start empty, holding the first index of the string
    (when (zerop sfs-len)
      (error "SEARCHED-FOR-STRING must not be an empty string!"))
    ;; searching ORIGINAL-STRING one step at a time (via subsequence comparisons of width exactly equal to length of SEARCHED-FOR-STRING)
    (do ((i 0)) ;; ! note that i is only changed within loop body
        ((>= i index-exclusive-end))
      (if (string= searched-for-string (subseq original-string i (+ i sfs-len)))
          (progn ;; THEN, save appropriate values into lists, and adjust i so as to step past the found substring
            (push i end-indexes)
            (push (+ i sfs-len) start-indexes)
            (incf i sfs-len))
          (progn ;; ELSE, adjust i so as to move on to the next character in string
            (incf i))))
    ;; finally, making both lists of equal length, by adding one more element to END-INDEXES (with this element indicating the full length of ORIGINAL-STRING)
    (push original-string-len end-indexes)
    (let ((string-list nil)) ;; note that this is the returned list
      ;; pushing, onto returned list, each non-empty subsequence as derived from ORIGINAL-STRING via the orderly pairing of elements from START-INDEXES and END-INDEXES ...
      ;; ... such that those subsequences that were previously matched (if any) define the breaks between individual strings
      (mapc #'(lambda (start-i end-i) (when (> (- end-i start-i) 0) (push (subseq original-string start-i end-i) string-list))) (reverse start-indexes) (reverse end-indexes))
      (reverse string-list))))

;;; to make this function more flexible, spaces would need to be removed from original string
;;; ...also, adding in the zero character makes this very specific to what it is doing here
(defun 2d-bracketed-list-to-seq (2d-bracketed-list-str)
  (let ((bl-str 2d-bracketed-list-str))
    (do ((index (search "[]" bl-str :test 'equal) (search "[]" bl-str :test 'equal)))
        ((not index))
      (setq bl-str (concatenate 'string (subseq bl-str 0 (1+ index)) "0" (subseq bl-str (1+ index)))))
    (setq bl-str (subseq bl-str 2 (- (length bl-str) 2)))
    (let ((seq (make-array 1 :adjustable t :fill-pointer 0))
          (split-bl (split-by-str bl-str "],[")))
      (dolist (string-elt split-bl)
        (let ((inner-seq (make-array 1 :adjustable t :fill-pointer 0)))
          (dolist (x (split-by-char string-elt #\,))
            (vector-push-extend (parse-integer x) inner-seq))
          (vector-push-extend inner-seq seq)))
      (values seq))))

(defun input-website-format ()
  (let ((side-clues nil)
        (top-clues nil))
    (format t "~%Copy and paste the row clues:~%")
    (setq side-clues (2d-bracketed-list-to-seq (read-line *query-io*)))
    (format t "~%Copy and paste the column clues:~%")
    (setq top-clues (2d-bracketed-list-to-seq (read-line *query-io*)))
    (show-and-solve-created-nonogram side-clues top-clues)))

(defun input-menu ()
  (let ((chosen-option nil))
    (format t "~%Choose from the below input options.~%")
    (format t "1) Row-by-row, column-by-column input~%")
    (format t "2) copy/paste picross website format~%~%")
    (do ((user-input nil)
         (input-menu-options (list "1" "2")))
        (user-input (setq chosen-option user-input))
      (format t "Choose an option: ")
      (setq user-input (read-line *query-io*))
      (unless (member user-input input-menu-options :test 'equal)
        (setq user-input nil)))
    (cond
      ((string= chosen-option "1") (manual-input-menu))
      ((string= chosen-option "2") (input-website-format)))))

(defun test-menu ()
  (do ((user-input "")
       (num -1)
       (nonogram-count (length *test-nonograms*)))
      (nil)
    (format t "~%Enter a number from 0 to ~d to select a nonogram: " (- nonogram-count 1))
    (setq user-input (read-line *query-io*))
    (setq num (parse-integer user-input :junk-allowed t))
    (when (and num (>= num 0) (<= num (- nonogram-count 1)))
      (let* ((test-data (elt *test-nonograms* num))
             (inst (make-instance 'nonogram :side-clues (elt (car test-data) 0) :top-clues (elt (car test-data) 1) :answer-as-vector-of-vectors (cdr test-data))))
        (print-grid inst nil nil nil :border nil)
        (format t "Proceed with this nonogram? <y/n> ") ;; !!! should y/n query be in a loop?
        (setq user-input (read-line *query-io*))
        (when (string= user-input "")
          (setq user-input "n"))
        (when (char= (char-downcase (elt user-input 0)) #\y)
          (format t "~%") ;; move later???
          (solve-nonogram inst)
          (return))))))

(defun test-init ()
  (let ((ng (make-instance 'nonogram :side-clues (elt (car (elt *test-nonograms* 0)) 0) :top-clues (elt (car (elt *test-nonograms* 0)) 1) :answer-as-vector-of-vectors (cdr (elt *test-nonograms* 0)))))
    (format t "grid height: ~a~%" (grid-height ng))
    (format t "grid width: ~a~%~%" (grid-width ng))
    (format t "~a~%~%" (grid ng))
    (dotimes (i (line-count ng))
      (format t "line-name: ~a~%orientation: ~a~%index-in-lines: ~a~%line-clues: ~a~%~%" (line-name (elt (lines ng) i)) (orientation (elt (lines ng) i)) (index-in-lines (elt (lines ng) i)) (line-clues (elt (lines ng) i))))
    (format t "printed grid...~%~%")
    (print-grid ng (elt (lines ng) 0) (cons 0 0) (cons 'fake-function "no-line"))))

(defun main-menu ()
  (let ((chosen-option nil))
    (format t "~%Welcome to Jacob Olson's Nonogram Solver!~%~%Choose an option below to begin solving nonograms!~%")
    (format t "1) Manually input clues~%")
    (format t "2) Demonstration/Testing mode~%~%")
    (format t "9) Troubleshooting~%~%")
    (format t "q) End program~%~%")
    (do ((user-input nil)
         (main-menu-options (list "1" "2" "9" "q")))
        (user-input (setq chosen-option user-input))
      (format t "Choose an option: ")
      (setq user-input (read-line *query-io*))
      (unless (member (string-downcase user-input) main-menu-options :test 'equal)
        (setq user-input nil)))
    (cond
      ((string= chosen-option "1") (input-menu))
      ((string= chosen-option "2") (test-menu))
      ((string= chosen-option "9") (test-init)))
    (format t "~%Closing program...~%")))
