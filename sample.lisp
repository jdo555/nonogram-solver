;;;; IMPORTANT: This is only a sample of the entire project.
;;;; The following is one of the programmed "techniques" as used in the full nonogram-solver code.

;;; assesses a line-segment from left-to-right and then from right-to-left, marking any "empties" based on clues whose proximity to an edge implies empty locations
;;; this method looks inward from the edge coordinate x spaces PAST the start point, where x is the considered (first or last) clue's value
;;; it then iterates such that the "edge" coordinate and the "checked" coordinate both shift 1 space in the same direction towards the center...
;;; ... marking all "edge" locations as "empty" until out of bounds OR the "edge" is not empty OR a "fill" is NOT found at the checked location
;;; e.g. |4 2| ----111-----1-- yields |4 2| 000-111-----1-0
(defmethod mark-as-empty-based-on-edge-proximity ((ng nonogram) (line-obj line) current-clues clue-count line-range function-name)
  (do ((iteration-count 0 (1+ iteration-count))
       (clue (elt current-clues 0) (elt current-clues (1- clue-count)))
       (direction 1 -1)
       (edge-index (car line-range) (1- (cdr line-range))))
      ((>= iteration-count 2))
    (do ((i 0 (1+ i)))
        (nil)
      (let* ((current-index (+ edge-index (* i direction))) ;; this starts at edge and moves inward 1 step at a time
             (checked-index (+ current-index (* clue direction)))) ;; this is at "clue" spaces PAST current-index
        (unless (within-constraints-of-line-range-p line-obj checked-index line-range) ;; only need to verify checked-index
          (return))
        (let ((current-coord (elt (line-coords line-obj) current-index))
              (checked-coord (elt (line-coords line-obj) checked-index)))
          (if (and (= (unknown-value ng) (aref (grid ng) (car current-coord) (cdr current-coord)))
                   (= (filled-value ng) (aref (grid ng) (car checked-coord) (cdr checked-coord))))
              (mark-as-x-at-coord ng line-obj (empty-value ng) current-coord function-name)
              (return)))))))