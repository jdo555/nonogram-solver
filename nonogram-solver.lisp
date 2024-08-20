(defpackage #:nonogram-solver
  (:use #:common-lisp)
  (:export ;; all workable-subsection class stuff
           #:workable-subsection
           #:wss-range
           #:wss-clues
           #:wss-finished-p
           ;; all line class stuff
           #:line
           #:orientation
           #:index-from-edge
           #:index-in-lines
           #:line-name
           #:line-clues
           #:line-complete-p
           #:use-primary-p
           #:use-secondary-p
           #:use-gen1-p
           #:use-gen2-p
           #:update-shortened-line-p
           #:shortened-line-range
           #:shortened-line-clues
           #:shortened-line-finished-clues-by-index
           #:update-line-subsections-p
           #:line-subsections
           ;; all nonogram class stuff
           #:nonogram
           #:top-clues
           #:side-clues
           #:grid-width
           #:grid-height
           #:grid
           #:lines
           #:line-count
           #:empty-value
           #:filled-value
           #:unknown-value
           #:horizontal-char
           #:vertical-char
           #:prepare-printing-data-p
           #:print-each-step-p
           #:side-clues-print-data
           #:top-clues-print-data
           #:grid-value-to-printable-string-ht
           #:step-count
           #:grid-was-changed-p
           #:collect-move-data-p
           #:all-moves-array
           #:pause-for-function
           #:answer-as-vector-of-vectors
           ;; basic functions and methods having to do primarily with counts, length, range, and index
           #:make-line-name
           #:get-clue-count-for-line
           #:get-clue-count-for-shortened-line
           #:get-clue-count-for-workable-subsection
           #:get-subsection-count-for-line
           #:every-workable-subsection-is-finished-p
           #:orientation-is-horizontal-p
           #:orientation-is-vertical-p
           #:orientation-is-valid-p
           #:get-opposite-orientation
           #:line-has-orientation-p
           #:line-is-horizontal-p
           #:line-is-vertical-p
           #:check-range
           #:get-line-length-via-range
           #:get-line-length
           #:get-shortened-line-length
           #:get-length-of-workable-subsection
           #:within-constraints-of-index-range-p
           #:index-is-valid-for-line-p
           #:index-is-valid-for-shortened-line-range-p
           #:index-is-valid-for-array-line-subsections-p
           #:index-is-valid-for-array-shortened-line-clues-p
           #:index-is-valid-for-workable-subsection-range-p
           #:check-index-for-line
           #:get-coord-from-index-on-line
           ;; printing functionality
           #:prepare-all-printing-data
           #:press-enter-to-continue
           #:print-grid
           ;; more basic functions and methods that have primarily to do with coordinates, indexes, and lines
           #:check-coord
           #:get-value-at-coord-in-grid
           #:get-value-at-line-index-in-grid
           #:is-x-at-coord-p
           #:is-x-at-index-p
           #:coord-is-filled-p
           #:index-is-filled-p
           #:every-index-in-range-is-filled-p
           #:coord-is-empty-p
           #:index-is-empty-p
           #:coord-is-unknown-p
           #:index-is-unknown-p
           #:get-line-by-orientation-and-index-from-edge
           #:get-intersecting-line-from-index
           #:get-intersecting-line-from-coord
           ;; the methods that handle the marking of the nonogram
           #:update-nonogram-and-line-data
           #:mark-as-x-at-coord
           #:mark-as-x-at-coord-when-unknown
           #:mark-as-x-at-i-when-unknown
           #:mark-as-x-at-i
           #:mark-as-x-at-i-when-unknown-for-range
           ;; methods related to preparing a shortened-line or line-subsection
           #:find-completed-clues-from-extreme-edges
           #:set-shortened-line-clues
           #:set-shortened-line-range
           #:mark-all-remaining-unknowns-as-empty
           #:update-shortened-line
           #:set-line-subsections
           #:relate-clues-to-line-subsections
           #:update-line-subsections
           ;; some parameters and their associated macro
           #:+full-line+
           #:+shortened-line+
           #:+line-subsections+
           #:proceed-according-to-line-assessment-type
           ;; other utility methods
           #:get-zone-data-for-line-segment-by-clue-count-in-direction
           #:get-largest-or-smallest-remaining-clue-by-line-subsections
           ;; line techniques and some more utility methods
           #:mark-for-lines-with-no-clues
           #:basic-check-for-finished-line
           #:find-overlap
           #:find-initial-overlap
           #:mark-as-filled-based-on-edge-proximity
           #:mark-as-empty-based-on-edge-proximity
           #:mark-for-edgemost-clue-when-delimited-maximally-close-to-edge
           #:bridge-empties-from-edge
           #:empty-before-or-at-line-coord-p ;; !!!
           #:mark-for-repeating-clues-at-edge
           #:get-index-of-first-filled-from-left-or-right ;; !!!
           #:get-count-of-consecutive-filled-moving-right-or-left-from-index ;; !!!
           #:mark-around-singular-clue
           #:mark-around-unjoinable-clues
           #:mark-empties-around-largest-represented-clues
           #:mark-small-unfillable-line-subsections-as-empty
           #:mark-out-empty-workable-subsections
           ;; line generation technique and its associated methods
           #:copy-adjustable-sequence ;; !!!
           #:generate-line-variations-as-bit-vectors
           #:line-segment-has-necessary-overlap-with-generated-bit-vector-p
           #:mark-according-to-consistency-of-generated-lines
           ;; nonogram solving methods
           #:apply-initial-nonogram-techniques
           #:do-unfinished-lines-according-to-slot-value
           #:apply-techniques-to-all-nonogram-lines-single-iteration
           #:apply-techniques-to-all-nonogram-lines
           #:create-move-list-text-file
           #:solve-nonogram
           ;; convenience methods etc. !!!
           #:with-single-line-nonogram))

(in-package #:nonogram-solver)

(defclass workable-subsection ()
  ((wss-range
    :initarg :wss-range
    :reader wss-range
    :documentation "This is a CONS of two integers that follows the specification set by the function CHECK-RANGE. This range indicates a subsection of a shortened-line that has any collection of exclusively FILLED-VALUE(s) or UNKNOWN-VALUES(s) in any order such that this collection is delimited on each end within the original shortened-line by either an EMPTY-VALUE or line-edge. See the method SET-LINE-SUBSECTIONS for more information.")
   (wss-clues
    :initform nil
    :reader wss-clues
    :documentation "This is an integer-only adjustable array with a fill-pointer, or is otherwise NIL. When an array, it consists of the clues values that relate to this WORKABLE SUBSECTION according to the order of the associated line, as with LINE-CLUES of the LINE class. When NIL, it indicates that no clue grouping has yet been definitively associated with this subsection.")
   (wss-finished-p
    :initform nil
    :accessor wss-finished-p
    :documentation "This is a boolean. When T, indicates that the WORKABLE-SUBSECTION is already complete, meaning that it consists of only FILLED-VALUE(s), and need not be assessed further with any subsection-techniques; when NIL, indicates that this subsection should still be assessed by any relevant subsection-techniques.")))

(defclass line ()
  ((orientation
    :initarg :orientation
    :reader orientation
    :documentation "This is a character (either HORIZONTAL-CHAR or VERTICAL-CHAR [see NONOGRAM class]) that indicates whether the line is oriented horizontally or vertically. Note that horizontal line positions are indexed from left to right; and vertical line positions are indexed from top to bottom.")
   (index-from-edge
    :initarg :index-from-edge
    :reader index-from-edge
    :documentation "This is an integer that indicates the index of this line from the top or left edge of the nonogram according to the line's orientation of horizontal or vertical respectively.")
   (index-in-lines
    :initarg :index-in-lines
    :reader index-in-lines
    :documentation "This is an integer that indicates this line's index in the collection of all lines in the NONOGRAM array LINES. Note that LINES is ordered such that horizontal lines come first (proceeding from top to bottom), with vertical lines following after (proceeding from left to right).")
   (line-name
    :initarg :line-name
    :reader line-name
    :documentation "This is a string that uniquely and human-readably identifies this line. It follows a format that is dictated by the function MAKE-LINE-NAME as used in the initialization of the NONOGRAM class, such that it is a combination of the HORIZONTAL-CHAR or VERTICAL-CHAR (as appropriate) and the line's index from edge. This string is used exclusively for output.")
   (line-clues
    :initarg :line-clues
    :reader line-clues
    :documentation "This is an integer-only adjustable array with a fill-pointer, where each integer represents a clue for this line. Note that for horizontal lines the clue index aligns with the left-to-right orientation of the line; and for vertical lines the clue index aligns with the top-to-bottom orientation of the line. Note that because this array has a fill-pointer, the LENGTH function can be used against it to efficiently get the count of clues for the line. Also note that this slot directly references a fill-pointer array from within the NONOGRAM class slot TOP-CLUES or SIDE-CLUES, depending on line orientation.")
   (line-complete-p
    :initform nil
    :accessor line-complete-p
    :documentation "This is a boolean. When T, it indicates that the line is complete, such that there is no UNKNOWN-VALUE left anywhere on the line; when NIL, it indicates that the line is incomplete such that there are one or more UNKNOWN-VALUEs left on the line, implying that the line should still be assessed with techniques in order to reach completion.")
   (use-primary-p
    :initform t
    :accessor use-primary-p
    :documentation "This is a boolean. When T, it indicates that the \"primary\" techniques should be used against this line; when NIL, it implies that \"primary\" techniques were already used against this line without making any changes. Note that this slot is strongly related to the method UPDATE-NONOGRAM-AND-LINE-DATA and the main solving methods.")
   (use-secondary-p
    :initform t
    :accessor use-secondary-p
    :documentation "This is a boolean. When T, it indicates that the \"secondary\" techniques should be used against this line; when NIL, it implies that \"secondary\" techniques were already used against this line without making any changes. Note that this slot is strongly related to the method UPDATE-NONOGRAM-AND-LINE-DATA and the main solving methods.")
   (use-gen1-p
    :initform t
    :accessor use-gen1-p
    :documentation "This is a boolean. When T, it indicates that the restricted line-generation techniques should be used against this line; when NIL, it implies that the restricted line-generation techniques were already used against this line without making any changes. Note that this slot is strongly related to the method UPDATE-NONOGRAM-AND-LINE-DATA and the main solving methods.")
   (use-gen2-p
    :initform t
    :accessor use-gen2-p
    :documentation "This is a boolean. When T, it indicates that the unrestricted line-generation techniques should be used against this line; when NIL, it implies that the unrestricted line-generation techniques were already used against this line without making any changes. Note that this slot is strongly related to the method UPDATE-NONOGRAM-AND-LINE-DATA and the main solving methods.")
   (update-shortened-line-p
    :initform t
    :accessor update-shortened-line-p
    :documentation "This is a boolean. When T, it indicates that the line has been changed in some way and that an attempt should be made to update the shortened-line data, which includes the LINE class slots SHORTENED-LINE-RANGE and SHORTENED-LINE-CLUES; when NIL, it indicates that no changes have been made to the line recently, and thus no attempt need be made to update the shortened-line data. Note that this variable starts out with the value T, because the related shortened-line data is not set during initialization (because that would be pointless) and is only set (and should only be set) by calling the method UPDATE-SHORTENED-LINE. Also note that this boolean is set to T by the method UPDATE-NONOGRAM-AND-LINE-DATA whenever the associated line is changed.")
   (shortened-line-range
    :initform nil
    :accessor shortened-line-range
    :documentation "This is a CONS of two integers that follows the specification set by the function CHECK-RANGE. This range indicates the unfinished subsection of a line, excluding the fully finished parts (if any) on the left or right ends. Note that such subsections are only considered \"fully finished\", and thereby excluded, when they consist of only EMPTY-VALUE(s) and zero or more FILLED-VALUE(s) such that the side-most value nearest to the unfinished section is an EMPTY-VALUE. See the method SET-SHORTENED-LINE-RANGE for more information.")
   (shortened-line-clues
    :initform nil
    :accessor shortened-line-clues
    :documentation "This is an integer-only adjustable array with a fill-pointer that is very similar to the above slot LINE-CLUES, with two major differences: the array's clues align with the shortened-line instead of the full line; and the array DOES NOT have referential integrity with any other clue array, being a freshly generated copy of some subsection of LINE-CLUES. Note that this slot is set by the method SET-SHORTENED-LINE-CLUES.")
   (shortened-line-finished-clues-by-index
    :initform (make-hash-table)
    :accessor shortened-line-finished-clues-by-index
    :documentation "This is a hash-table that simulates a set, such that the hash-table key is an integer and the hash-table value is T for every entry. The key is to be the index of a clue -- the clue's index in the LINE class slot LINE-CLUES -- that is completed within the line; completed, as in, the full clue value is present within the line, in its proper order, as a group of FILLED-VALUEs of a count equal to clue, surrounded by either line edges or EMPTY-VALUE(s). Note that this hash-table is set up within the method FIND-COMPLETED-CLUES-FROM-EXTREME-EDGES, which is part of every call to UPDATE-SHORTENED-LINE.")
   (update-line-subsections-p
    :initform t
    :accessor update-line-subsections-p
    :documentation "This is a boolean. When T, it indicates that the line has been changed in some way and that an attempt should be made to update the line-subsection data, which only includes the LINE class slot LINE-SUBSECTIONS; when NIL, it indicates that no changes have been made to the line recently, and thus no attempt need be made to update the line-subsection data. Note that this variable starts out with the value T, because the related line-subsection data is not set during initialization (because that would be pointless) and is only set (and should only be set) by calling the method UPDATE-LINE-SUBSECTIONS. Also note that this boolean is set to T by the method UPDATE-NONOGRAM-AND-LINE-DATA whenever the associated line is changed.")
   (line-subsections
    :initform nil
    :accessor line-subsections
    :documentation "This is an adjustable array with a fill-pointer. It consists exclusively of WORKABLE-SUBSECTION class objects, such that LINE-SUBSECTIONS is an ordered collection of all WORKABLE-SUBSECTIONs found within the shortened-line from left-to-right (when line is horizontal) or top-to-bottom (when line is vertical). Note that this slot is set with the method SET-LINE-SUBSECTIONS; see that method for more details.")))

(defclass nonogram ()
  ((top-clues
    :initarg :top-clues
    :reader top-clues
    :documentation "Note that this slot value does not retain its original passed INITARG state, and is updated during initialization. As an INITARG, this is expected to be an array of arrays, where each array of the secondary arrays relates by its own index to the left-to-right ordering of the columns of GRID such that each secondary array gives the clues for the same column in a top-to-bottom ordering; note that each secondary array is expected to consist of only integers (if any) where each integer is a clue. On the other hand, as a post-initialization slot value, TOP-CLUES is a simple-array of integer-only FILL-POINTER arrays, following the same ordering as described above, and is created (non-destructively) from the array of arrays that was passed as an INITARG during initialization.")
   (side-clues
    :initarg :side-clues
    :reader side-clues
    :documentation "Note that this slot value does not retain its original passed INITARG state, and is updated during initialization. As an INITARG, this is expected to be an array of arrays, where each array of the secondary arrays relates by its own index to the top-to-bottom ordering of the rows of GRID such that each secondary array gives the clues for the same row in a left-to-right ordering; note that each secondary array is expected to consist of only integers (if any) where each integer is a clue. On the other hand, as a post-initialization slot value, SIDE-CLUES is a simple-array of integer-only FILL-POINTER arrays, following the same ordering as described above, and is created (non-destructively) from the array of arrays that was passed as an INITARG during initialization.")
   (grid-width
    :reader grid-width
    :documentation "This is an integer that indicates the width of GRID.")
   (grid-height
    :reader grid-height
    :documentation "This is an integer that indicates the height of GRID.")
   (grid
    :accessor grid
    :documentation "This is a 2-dimensional array that represents the actual nonogram; it can only hold integers and is initialized such that every element has the value (UNKNOWN-VALUE NONOGRAM). Note that because the grid is 2-dimensional, any call to update the grid or any line in the grid necessarily requires 2-part coordinates.")
   (lines
    :reader lines
    :documentation "This is a simple array that holds all LINE objects. Note that LINES is ordered such that horizontal lines come first (proceeding from top to bottom), with vertical lines following after (proceeding from left to right). Note that the method GET-LINE-BY-INDEX-FROM-EDGE-AND-ORIENTATION depends of the previously described ordering.")
   (line-count
    :reader line-count
    :documentation "This is an integer that indicates the size of the simple array LINES.")
   (empty-value
    :initform 0
    :reader empty-value
    :documentation "This integer is a value that is placed into a coordinate within GRID. This value in GRID indicates an \"empty\" mark on the line.")
   (filled-value
    :initform 1
    :reader filled-value
    :documentation "This integer is a value that is placed into a coordinate within GRID. This value in GRID indicates a \"filled\" space that relates directly to a numerical clue.")
   (unknown-value
    :initform 8
    :reader unknown-value
    :documentation "This integer is a value that is placed into a coordinate within GRID. This value in GRID indicates a space whose proper value has not yet been deduced.")
   (horizontal-char
    :initform #\h
    :reader horizontal-char
    :documentation "This is a character that is used to indicate that a line has a horizontal orientation; this value must be distinct from VERTICAL-CHAR. Note that the ORIENTATION slot of the LINE class makes use of this character.")
   (vertical-char
    :initform #\v
    :reader vertical-char
    :documentation "This is a character that is used to indicate that a line has a vertical orientation; this value must be distinct from HORIZONTAL-CHAR. Note that the ORIENTATION slot of the LINE class makes use of this character.")
   (prepare-printing-data-p
    :initform t
    :initarg :prepare-printing-data-p
    :reader prepare-printing-data-p
    :documentation "This is a boolean. When T, all necessary printing data will be prepared at the initialization of the NONOGRAM instance, so that the method PRINT-GRID will be usable. Note that this slot will be set to T if the method PREPARE-ALL-PRINTING-DATA is manually called.")
   (print-each-step-p
    :initform t
    :initarg :print-each-step-p
    :reader print-each-step-p
    :documentation "This is a boolean. When T, a textual representation of the grid will be printed to standard-output every time the GRID was changed via the application of some technique (so long as the technique uses one of the MARK-AS-X-AT-* methods -- as all should); when NIL, there will be no such output. Note that printing cannot occur unless the NONOGRAM slot class PREPARE-PRINTING-DATA-P is set to T, thus an error will be triggered when these two slots are set in contradiction.")
   (side-clues-print-data
    :reader side-clues-print-data
    :initform nil
    :documentation "This is a fill-pointer array where each element is a fill-pointer string that represents all clues of the row as text followed by a separator with such spacing as ensures that the clue-parts of this string gravitate rightward so as to touch against the grid-separator. This slot must be prepared via the method PREPARE-ALL-PRINTING-DATA in order for the method PRINT-GRID to work. Note that the NONOGRAM class slot PREPARE-PRINTING-DATA-P must be T for this slot to be prepared at initialization of a NONOGRAM.")
   (top-clues-print-data
    :reader top-clues-print-data
    :initform nil
    :documentation "This is a fill-pointer array where each element is a fill-pointer string, such that each string represents a row of text in the full representation of all top-clues, with the last string being the separator between clues and grid; this collection of strings is prepared so that the clue-parts gravitate downward so as to touch against the grid-separator. This slot must be prepared via the method PREPARE-ALL-PRINTING-DATA in order for the method PRINT-GRID to work. Note that the NONOGRAM class slot PREPARE-PRINTING-DATA-P must be T for this slot to be prepared at initialization of a NONOGRAM.")
   (grid-value-to-printable-string-ht
    :reader grid-value-to-printable-string-ht
    :documentation "This is a hash-table that yields the printable string value for each associated *-VALUE (such as FILLED-VALUE, EMPTY-VALUE, or UNKNOWN-VALUE). All entries in this hash-table are manually set within the method PREPARE-ALL-PRINTING-DATA. Note that the NONOGRAM class slot PREPARE-PRINTING-DATA-P must be T for this slot to be prepared at initialization of a NONOGRAM.")
   (step-count
    :initform 0
    :accessor step-count
    :documentation "This is an integer representing the total number of marks made against the GRID since the initialization of the NONOGRAM. Note that STEP-COUNT is only ever changed within the method UPDATE-NONOGRAM-AND-LINE-DATA.")
   (grid-was-changed-p
    :initform nil
    :accessor grid-was-changed-p
    :documentation "This is a boolean. When T, it indicates that GRID was changed, meaning that some element was changed from UNKNOWN-VALUE to either EMPTY-VALUE or FILLED-VALUE; when NIL, it indicates that GRID has not been changed by any of the used techniques. This slot is used especially in the main solving method to help to determine whether there is a need to move onto more complex (and less efficient) techniques, or otherwise, to determine when no more progress can be made on the current nonogram.")
   (collect-move-data-p
    :initarg :collect-move-data-p
    :initform nil
    :reader collect-move-data-p
    :documentation "This is a boolean. When T, it indicates that all markings made to GRID should be saved to the NONOGRAM slot ALL-MOVES-ARRAY, thereby also enabling the creation of a text file (via the method CREATE-MOVE-LIST-TEXT-FILE) that lists all such moves; when NIL, no such data will be collected.")
   (all-moves-array
    :initform nil
    :accessor all-moves-array
    :documentation "When COLLECT-MOVE-DATA-P is NIL, this is also NIL. Otherwise, it is a fill-pointer array that holds lists in the following format: (list (cons <integer> <integer>) <integer>), or, represented differently, (list <COORD> <value-at-COORD>). Thus, the full array contains data about each mark made against the GRID, in order from start to finish, where each element holds data about the location of the marking and mark-type.")
   (pause-for-function
    :initform nil
    :initarg :pause-for-function
    :reader pause-for-function
    :documentation "This should be either NIL or a symbol. When a symbol, it must be the quoted name of a nonogram technique (such as 'find-overlap); and when it is correctly set as such a symbol, all solving processes will halt or pause as soon as the technique of the same name has made any mark (according to any line-assessment-type), thereafter waiting for the user to press the <enter> key.")
   (answer-as-vector-of-vectors
    :initarg :answer-as-vector-of-vectors
    :initform nil
    :reader answer-as-vector-of-vectors
    :documentation "This is either NIL or else a vector where every element is another vector that consists exclusively of FILLED-VALUEs and EMPTY-VALUEs, but can also include UNKNOWN-VALUEs thereby indicating that a fully complete answer is unachievable without guessing. When this slot is NIL, nothing additional happens; but when this slot is properly set, printing functionalities will be expanded such that each marking made by any technique will be checked for correctness against this slot with additional output elaborating on this correctness or lack thereof. Note that the passed vector of vectors is assumed to be correctly formulated, using only said *-VALUES, and is also assumed to exactly match the dimensions of GRID; note that no checks are done to verify the correctness of this slot.")))

(defun make-line-name (line-orientation index)
  "Returns a string that consists of the character LINE-ORIENTATION and the integer INDEX, separated by a hyphen. See the slot LINE-NAME of the LINE class for more information about how it is used."
  (format nil "~a-~a" line-orientation index))

(defmethod get-clue-count-for-line ((line-obj line))
  "Returns the length of the LINE class slot LINE-CLUES yielding the count of clues for the line. Note that LINE-CLUES is an array with a fill-pointer."
  (fill-pointer (line-clues line-obj)))

(defmethod get-clue-count-for-shortened-line ((line-obj line))
  "Returns the length of the LINE class slot SHORTENED-LINE-CLUES yielding the count of clues for the shortened-line. Note that SHORTENED-LINE-CLUES is an array with a fill-pointer."
  (fill-pointer (shortened-line-clues line-obj)))

(defmethod get-clue-count-for-workable-subsection ((wss-obj workable-subsection))
  "Returns the length of the WORKABLE-SUBSECTION class slot WSS-CLUES yielding the count of clues for the considered subsection; however, if the subsection has not been related to clues, NIL will be returned."
  (if (wss-clues wss-obj)
      (fill-pointer (wss-clues wss-obj))
      nil))

(defmethod get-subsection-count-for-line ((line-obj line))
  "Returns the length of the LINE class slot LINE-SUBSECTIONS yielding the count of WORKABLE-SUBSECTIONs for the associated shortened-line. Note that LINE-SUBSECTIONS is an array with a fill-pointer."
  (fill-pointer (line-subsections line-obj)))

(defmethod every-workable-subsection-is-finished-p ((line-obj line))
  "Returns T when every WORKABLE-SECTION in the LINE class slot LINE-SUBSECTIONS is finished (meaning WSS-FINISHED-P is set to T for the object); otherwise returns NIL."
  (let ((all-finished-p t))
    (dotimes (i (get-subsection-count-for-line line-obj))
      (let ((wss-obj (elt (line-subsections line-obj) i)))
        (unless (wss-finished-p wss-obj)
          ;; when one subsection is NOT finished, change boolean and stop iterating
          (setq all-finished-p nil)
          (return))))
    all-finished-p))

(defmethod orientation-is-horizontal-p ((ng nonogram) orientation)
  "Returns T when ORIENTATION is HORIZONTAL-CHAR; otherwise returns NIL."
  (char= orientation (horizontal-char ng)))

(defmethod orientation-is-vertical-p ((ng nonogram) orientation)
  "Returns T when ORIENTATION is VERTICAL-CHAR; otherwise returns NIL."
  (char= orientation (vertical-char ng)))

(defmethod orientation-is-valid-p ((ng nonogram) orientation)
  "Returns T when ORIENTATION is a character equal to either HORIZONTAL-CHAR or VERTICAL-CHAR; otherwise returns NIL."
  (cond
    ((not (characterp orientation))
     nil)
    ((orientation-is-horizontal-p ng orientation)
     t)
    ((orientation-is-vertical-p ng orientation)
     t)
    (t
     nil)))

(defmethod get-opposite-orientation ((ng nonogram) orientation)
  "Returns the orientation that is opposite to the passed ORIENTATION. ORIENTATION must be either HORIZONTAL-CHAR or VERTICAL-CHAR, otherwise an error will be triggered."
  (cond
    ((orientation-is-horizontal-p ng orientation)
     (vertical-char ng))
    ((orientation-is-vertical-p ng orientation)
     (horizontal-char ng))
    (t
     (error "ORIENTATION is not a valid orientation!"))))

(defmethod line-has-orientation-p ((line-obj line) orientation)
  "Returns T when the LINE object has an orientation matching ORIENTATION; otherwise returns NIL. Note that ORIENTATION is expected to be a single character, and is expected to be either HORIZONTAL-CHAR or VERTICAL-CHAR; also, no checks are done verify the correctness of ORIENTATION."
  (char= (orientation line-obj) orientation))

(defmethod line-is-horizontal-p ((ng nonogram) (line-obj line))
  "Returns T when the LINE object has a horizontal orientation; otherwise returns NIL."
  (line-has-orientation-p line-obj (horizontal-char ng)))

(defmethod line-is-vertical-p ((ng nonogram) (line-obj line))
  "Returns T when the LINE object has a vertical orientation; otherwise returns NIL."
  (line-has-orientation-p line-obj (vertical-char ng)))

(defun check-range (range)
  "Triggers an error when RANGE is not a valid range. Note that RANGE should be a CONS of two integers where the first integer is the inclusive start and the second is the exclusive end such that the first integer is less than or equal to the second."
  (unless (consp range)
    (error "RANGE must be a CONS of two integers!"))
  (when (or (not (integerp (car range))) (not (integerp (cdr range))))
    (error "Both the CAR element and the CDR element of RANGE must be integers!"))
  (when (> (car range) (cdr range))
    (error "The integer in the CAR position must be less than or equal to the integer in the CDR position.")))

(defun get-line-length-via-range (range &key (check-range nil))
  "Returns the length of the line-segment based on the passed RANGE. Set CHECK-RANGE to T to verify the correctness of range using the function CHECK-RANGE."
  (when check-range
    (check-range range))
  (- (cdr range) (car range)))

(defmethod get-line-length ((ng nonogram) (line-obj line))
  "Returns the length of the line according to the line's orientation. Note that every horizontal line has length equal to the width of GRID; and every vertical line has length equal to the height of GRID."
  (if (line-is-horizontal-p ng line-obj)
      (grid-width ng)
      (grid-height ng)))

(defmethod get-shortened-line-length ((line-obj line))
  "Returns the length of the shortened-line for LINE when SHORTENED-LINE-RANGE has already been set; otherwise returns NIL."
  (when (shortened-line-range line-obj)
    (get-line-length-via-range (shortened-line-range line-obj))))

(defmethod get-length-of-workable-subsection ((wss-obj workable-subsection))
  "Returns the length of the passed WORKABLE-SUBSECTION object."
  (get-line-length-via-range (wss-range wss-obj)))

(defun within-constraints-of-index-range-p (checked-index range &key (check-range nil))
  "Returns T if CHECKED-INDEX is a valid index for the passed index RANGE; otherwise returns NIL. Set CHECK-RANGE to T to verify the correctness of range using the function CHECK-RANGE."
  (when check-range
    (check-range range))
  (if (and (>= checked-index (car range)) (< checked-index (cdr range))) t nil))

(defmethod index-is-valid-for-line-p ((ng nonogram) (line-obj line) checked-index)
  "Returns T when CHECKED-INDEX is a valid index for the full line range of the passed LINE object; otherwise returns NIL."
  (within-constraints-of-index-range-p checked-index (cons 0 (get-line-length ng line-obj))))

(defmethod index-is-valid-for-shortened-line-range-p ((line-obj line) checked-index)
  "Returns T when CHECKED-INDEX is valid according to the range-based slot, SHORTENED-LINE-RANGE, of the passed LINE object; otherwise returns NIL."
  (within-constraints-of-index-range-p checked-index (shortened-line-range line-obj)))

(defmethod index-is-valid-for-array-line-subsections-p ((line-obj line) checked-index)
  "Returns T when CHECKED-INDEX is valid for the array and LINE class slot LINE-SUBSECTIONS; otherwise returns NIL."
  (within-constraints-of-index-range-p checked-index (cons 0 (get-subsection-count-for-line line-obj))))

(defmethod index-is-valid-for-array-shortened-line-clues-p ((line-obj line) checked-index)
  "Returns T when CHECKED-INDEX is valid for the array and LINE class slot SHORTENED-LINE-CLUES; otherwise returns NIL."
  (within-constraints-of-index-range-p checked-index (cons 0 (get-clue-count-for-shortened-line line-obj))))

(defmethod index-is-valid-for-workable-subsection-range-p ((wss-obj workable-subsection) checked-index)
  "Returns T when CHECKED-INDEX is valid according to the range-based slot, WSS-RANGE, of the passed WORKABLE-SUBSECTION object; otherwise returns NIL."
  (within-constraints-of-index-range-p checked-index (wss-range wss-obj)))

(defmethod check-index-for-line ((ng nonogram) (line-obj line) index)
  "Triggers an error when INDEX is out of bounds for the given line, or is otherwise invalid."
  (unless (integerp index)
    (error "INDEX must be an integer!"))
  (unless (index-is-valid-for-line-p ng line-obj index)
    (error "INDEX is invalid for the given LINE!")))

(defmethod get-coord-from-index-on-line ((ng nonogram) (line-obj line) index &key (check-index nil))
  "Returns the coord CONS for the given INDEX on line. INDEX must be an integer and should be valid for the given line; note that passing an invalid index will result in an invalid coord CONS. Set CHECK-INDEX to T to trigger an error when INDEX is invalid for the given line."
  (when check-index
    (check-index-for-line ng line-obj index))
  (if (line-is-horizontal-p ng line-obj)
      (cons (index-from-edge line-obj) index)
      (cons index (index-from-edge line-obj))))

(defmethod prepare-all-printing-data ((ng nonogram))
  "Prepares the NONOGRAM class slots SIDE-CLUES-PRINT-DATA, TOP-CLUES-PRINT-DATA, and GRID-VALUE-TO-PRINTABLE-STRING-HT so as to enable the printing of the grid via the method PRINT-GRID, which depends on these slots. Note that this method will only be utilized at initialization when the NONOGRAM class slot PREPARE-PRINTING-DATA-P is T; however, this method may be called manually if the nonogram already exists in an interactive environment where the user forgot to prepare printing data via initialization arguments, but this method should only ever be called once per nonogram because the resulting data should always be the same per nonogram."
  ;; setting the appropriate slot value, on the off-chance that the user is calling this method manually
  (unless (prepare-printing-data-p ng)
    (setf (slot-value ng 'prepare-printing-data-p) t))
  (let ((side-clue-char-counts (make-array 0 :element-type 'integer :fill-pointer 0)) ;; this is an array where each element is the length of the string-representation of side-clues for that row
        (greatest-of-side-clue-char-counts nil) ;; this will be an integer derived from SIDE-CLUE-CHAR-COUNTS
        (top-clue-counts (make-array 0 :element-type 'integer :fill-pointer 0)) ;; this is an array that has GRID-WIDTH size where each element is the count of clues for that column
        (greatest-top-clue-group-height nil)) ;; this will be an integer derived from TOP-CLUE-COUNTS
    ;; collecting "character counts" for each group of clues by row
    ;; ! note that the longest (via textual representation) group of side clues will dictate the needed spacing before for all other horizontal clues to ensure ...
    ;; ... that the printed grid will line up properly
    (dotimes (i (grid-height ng))
      (let* ((clues-by-row (elt (side-clues ng) i))
             (clue-count (fill-pointer clues-by-row))
             (char-count 0))
        ;; getting cumulative count of characters across all clues in CLUES-BY-ROW
        (dotimes (j clue-count)
          (incf char-count (length (format nil "~a" (elt clues-by-row j)))))
        ;; adding count of spaces needed to separate each clue string
        (when (> clue-count 0)
          (incf char-count (1- clue-count)))
        ;; saving CHAR-COUNT to appropriate array
        (vector-push-extend char-count side-clue-char-counts)))
    ;; setting GREATEST-OF-SIDE-CLUE-CHAR-COUNTS according to the maximum value held in SIDE-CLUE-CHAR-COUNTS
    (setq greatest-of-side-clue-char-counts (reduce #'max side-clue-char-counts))
    ;; collecting clue-counts for each group of clues by column
    ;; ! note that the largest (in terms of clue-count) group of top clues will dictate the number of "empty-spots" needed above other vertical-clue-groups ...
    ;; ... of a lesser count to ensure that the printed grid will line up properly
    (dotimes (i (grid-width ng))
      (vector-push-extend (fill-pointer (elt (top-clues ng) i)) top-clue-counts))
    ;; setting GREATEST-TOP-CLUE-GROUP-HEIGHT according to the maximum value held in TOP-CLUE-COUNTS
    (setq greatest-top-clue-group-height (reduce #'max top-clue-counts))
    ;; preparing SIDE-CLUES-PRINT-DATA...
    (setf (slot-value ng 'side-clues-print-data) (make-array (grid-height ng) :fill-pointer (grid-height ng)))
    ;; creating a specialized string for each row of clues and saving it
    (do ((i 0 (1+ i))
         (clue-string nil))
        ((>= i (grid-height ng)))
      ;; preparing initial CLUE-STRING such that this is one space on left (so that text is not up against screen edge)
      (setq clue-string (make-array 1 :element-type 'character :initial-element #\space :fill-pointer 1))
      ;; appending spaces to CLUE-STRING based on widest clue group
      (dotimes (iteration-count (- greatest-of-side-clue-char-counts (elt side-clue-char-counts i)))
        (vector-push-extend #\space clue-string))
      ;; when there is no clue, simply append a space; otherwise append each clue onto CLUE-STRING with an additional space afterwards
      (if (zerop (elt side-clue-char-counts i))
          (vector-push-extend #\space clue-string)
          (dotimes (j (fill-pointer (elt (side-clues ng) i)))
            (let ((clue-as-string (format nil "~a" (elt (elt (side-clues ng) i) j))))
              (dotimes (k (length clue-as-string))
                (vector-push-extend (char clue-as-string k) clue-string))
              (vector-push-extend #\space clue-string))))
      ;; appending the separator, that separates clues from GRID, to the CLUE-STRING
      (vector-push-extend #\# clue-string)
      ;; saving completed CLUE-STRING to NONOGRAM class slot
      (setf (elt (slot-value ng 'side-clues-print-data) i) clue-string))
    ;; preparing TOP-CLUES-PRINT-DATA...
    ;; ! notice the 1+ in the size sections of the below call to MAKE-ARRAY; this is necessary because TOP-CLUES-PRINT-DATA also holds the row of text ...
    ;; ... that separates the top-clues from the grid
    (setf (slot-value ng 'top-clues-print-data) (make-array (1+ greatest-top-clue-group-height) :element-type 'string :initial-element "" :fill-pointer (1+ greatest-top-clue-group-height)))
    (let ((left-spacing (+ greatest-of-side-clue-char-counts 3))) ;; this is the left-spacing for each row of text representing the top-clues; note that it matches up with the size of side-clue text with its spacing and separator
      ;; creating a specialized string (as CURRENT-ROW-AS-TEXT) for each row of text that will in total represent all columns of clues
      ;; ! note that each newly created CURRENT-ROW-AS-TEXT is saved as an element of TOP-CLUES-PRINT-DATA
      (do ((nth-row-above-grid greatest-top-clue-group-height (1- nth-row-above-grid))
           (arr-index 0 (1+ arr-index))
           (current-row-of-clues nil)
           (current-row-as-text nil))
          ((<= nth-row-above-grid 0))
        (setq current-row-of-clues (make-array (grid-width ng) :element-type 'integer))
        (setq current-row-as-text (make-array left-spacing :element-type 'character :initial-element #\space :fill-pointer left-spacing))
        ;; filling out CURRENT-ROW-OF-CLUES with appropriate integers that represent clues at the given row above grid; where a 0 is present, that implies there is no clue
        (dotimes (i (grid-width ng))
          (when (>= (elt top-clue-counts i) nth-row-above-grid)
            (setf (elt current-row-of-clues i) (elt (elt (top-clues ng) i) (- (elt top-clue-counts i) nth-row-above-grid)))))
        ;; filling out CURRENT-ROW-AS-TEXT such that it fully represents the current row of text above the grid, including spacing and clues
        (dotimes (i (grid-width ng))
          (if (zerop (elt current-row-of-clues i))
              (progn ;; THEN, append the appropriate amount of spaces
                (vector-push-extend #\space current-row-as-text)
                (vector-push-extend #\space current-row-as-text))
              (progn ;; ELSE, append the clue with spacing according to clue size
                (let* ((single-clue (format nil "~a" (elt current-row-of-clues i)))
                       (single-clue-length (length single-clue)))
                  (cond
                    ;; ! note that the above IF accounts for the possibility of zero, via its use of ZEROP
                    ((= 1 single-clue-length)
                     (vector-push-extend #\space current-row-as-text)
                     (vector-push-extend (char single-clue 0) current-row-as-text))
                    ((= 2 single-clue-length)
                     (vector-push-extend (char single-clue 0) current-row-as-text)
                     (vector-push-extend (char single-clue 1) current-row-as-text))
                    ((>= 3 single-clue-length)
                     (error "Some clue is too large for the full grid to be represented properly via text!")))))))
        ;; finally, saving CURRENT-ROW-AS-TEXT as an element to the NONOGRAM class slot TOP-CLUES-PRINT-DATA
        (setf (elt (slot-value ng 'top-clues-print-data) arr-index) (copy-seq current-row-as-text)))
      ;; lastly, creating the row of text (again as CURRENT-ROW-AS-TEXT) that separates the top-clues from the actual grid
      (let ((current-row-as-text (make-array left-spacing :element-type 'character :initial-element #\space :fill-pointer left-spacing)))
        (dotimes (x (grid-width ng))
          (vector-push-extend #\space current-row-as-text)
          (vector-push-extend #\# current-row-as-text))
        ;; saving CURRENT-ROW-AS-TEXT as the final element of the NONOGRAM class slot TOP-CLUES-PRINT-DATA
        (setf (elt (slot-value ng 'top-clues-print-data) greatest-top-clue-group-height) current-row-as-text))))
  ;; preparing the other slot necessary for printing
  (setf (slot-value ng 'grid-value-to-printable-string-ht) (make-hash-table))
  (setf (gethash (empty-value ng) (slot-value ng 'grid-value-to-printable-string-ht)) "0")
  (setf (gethash (filled-value ng) (slot-value ng 'grid-value-to-printable-string-ht)) "1")
  (setf (gethash (unknown-value ng) (slot-value ng 'grid-value-to-printable-string-ht)) "-"))

(defmethod initialize-instance :after ((ng nonogram) &key)
  ;; checking for an invalid combination of print-related predicates
  (when (and (not (prepare-printing-data-p ng)) (print-each-step-p ng))
    (error "The slot PRINT-EACH-STEP-P cannot be T when the slot PREPARE-PRINTING-DATA-P is NIL!"))
  ;; preparing  some basic grid data
  (setf (slot-value ng 'grid-height) (length (side-clues ng)))
  (setf (slot-value ng 'grid-width) (length (top-clues ng)))
  ;; setting up grid and some line data...
  (setf (grid ng) (make-array `(,(grid-height ng) ,(grid-width ng)) :element-type 'integer :initial-element (unknown-value ng)))
  (setf (slot-value ng 'line-count) (+ (grid-height ng) (grid-width ng)))
  (setf (slot-value ng 'lines) (make-array (line-count ng)))
  ;; creating line data below...
  (let ((index 0))
    ;; dealing with lines by row...
    (let ((new-side-clues (make-array (grid-height ng)))) ;; ! recreating SIDE-CLUES to ensure referential integrity between it and LINE class's LINE-CLUES
      (dotimes (d (grid-height ng))
        (let ((line-clues (make-array 0 :element-type 'integer :adjustable t :fill-pointer 0)))
          ;; recreating each LINE-CLUES array as an adjustable array 
          (dotimes (i (length (elt (side-clues ng) d)))
            (let ((clue (elt (elt (side-clues ng) d) i)))
              (vector-push-extend clue line-clues)))
          ;; saving recreated LINE-CLUES into NEW-SIDE-CLUES
          (setf (elt new-side-clues d) line-clues)
          ;; creating LINE object and placing it in LINES array
          (setf (elt (slot-value ng 'lines) index) (make-instance 'line :line-name (make-line-name (horizontal-char ng) d) :line-clues line-clues :index-from-edge d :index-in-lines index :orientation (horizontal-char ng))))
        (incf index))
      (setf (slot-value ng 'side-clues) new-side-clues))
    ;; dealing with lines by column...
    (let ((new-top-clues (make-array (grid-width ng)))) ;; ! recreating TOP-CLUES to ensure referential integrity between it and LINE class's LINE-CLUES
      (dotimes (r (grid-width ng))
        (let ((line-clues (make-array 0 :element-type 'integer :adjustable t :fill-pointer 0)))
          ;; recreating each LINE-CLUES array as an adjustable array
          (dotimes (i (length (elt (top-clues ng) r)))
            (let ((clue (elt (elt (top-clues ng) r) i)))
              (vector-push-extend clue line-clues)))
          ;; saving recreated LINE-CLUES into NEW-TOP-CLUES
          (setf (elt new-top-clues r) line-clues)
          ;; creating LINE object and placing it in LINES array
          (setf (elt (slot-value ng 'lines) index) (make-instance 'line :line-name (make-line-name (vertical-char ng) r) :line-clues line-clues :index-from-edge r :index-in-lines index :orientation (vertical-char ng))))
        (incf index))
      (setf (slot-value ng 'top-clues) new-top-clues)))
  ;; preparing to collect move-data, if necessary
  (when (collect-move-data-p ng)
    (setf (all-moves-array ng) (make-array 0 :element-type 'list :fill-pointer 0)))
  ;; preparing data in order to enable textual representation of grid (via PRINT-GRID), if necessary
  (when (prepare-printing-data-p ng)
    (prepare-all-printing-data ng)))

(defun press-enter-to-continue ()
  "Pauses execution until the user presses the enter-key."
  (format t "<Press ENTER to continue>")
  (read-line *query-io*)
  (format t "~%"))

(defmethod print-grid ((ng nonogram) line-obj coord calling-function &key (border t))
  "Prints the textual representation of GRID to standard-output. LINE-OBJ is expected to be a LINE class object or NIL, and this is why it is not parameterized in the usual manner of a DEFMETHOD; note that LINE-OBJ is not checked for validity. COORD must be NIL or is expected to be valid coordinate CONS according to the method CHECK-COORD; note, however, that it is not checked for validity. CALLING-FUNCTION must be NIL or else a CONS where the CAR element should be the quoted name of the calling function and the CDR element should be a string indicating the line-assessment type. Notice, then, that each required argument besides NG may be NIL; this will affect what is shown in the textual output. BORDER, when T, causes a border to be displayed above and below the textual representation of the nonogram; note that this border (at top) also includes the numerical value of the NONOGRAM class slot STEP-COUNT. This method seldom needs to be called directly and, instead, will usually only be called indirectly through the (also indirect) calling of the method UPDATE-NONOGRAM-AND-LINE-DATA; note that this method can only be reached indirectly when the NONOGRAM class slot PRINT-EACH-STEP-P is T. Lastly, note that this method is only usable when the NONOGRAM class slot PREPARE-PRINTING-DATA-P is set to T."
  (unless (prepare-printing-data-p ng)
    (error (format nil "Print data must be prepared for this NONOGRAM in order to use the method PRINT-GRID!~%When initializing the NONOGRAM, set PREPARE-PRINTING-DATA-P to T, or if the NONOGRAM object already exists in an interactive environment you may manually execute the method PREPARE-ALL-PRINTING-DATA.")))
  ;; printing upper border (with step-count)
  (when border
    (format t "###########################~10:@<~a~>###########################~%~%" (step-count ng)))
  ;; printing details about the function or method that made changes to the line, if called from function
  (when calling-function
    (format t "~a (for ~a) made changes to ~a~%" (car calling-function) (cdr calling-function) (line-name line-obj))
    (format t "~a was marked as ~a~%" coord (gethash (aref (grid ng) (car coord) (cdr coord)) (grid-value-to-printable-string-ht ng))))
  (let ((pause-due-to-incorrect-mark nil)) ;; ! note that this variable is only relevant when ANSWER-AS-VECTOR-OF-VECTORS is set
    ;; testing the changes to GRID against a specially prepared answer array, if one was set at initialization
    (when (and coord (answer-as-vector-of-vectors ng))
      (if (= (aref (grid ng) (car coord) (cdr coord)) (elt (elt (answer-as-vector-of-vectors ng) (car coord)) (cdr coord)))
          (format t "Marking is correct!~%")
          (progn
            (format t "Marking is INCORRECT!~%According to ANSWER-AS-VECTOR-OF-VECTORS, the correct marking is ~a~%" (elt (elt (answer-as-vector-of-vectors ng) (car coord)) (cdr coord)))
            (setq pause-due-to-incorrect-mark t))))
    ;; adding additional spacing
    (format t "~%")
    ;; printing the top clues (as relate to the vertical lines)
    (dotimes (i (fill-pointer (top-clues-print-data ng)))
      (format t "~a~%" (elt (top-clues-print-data ng) i)))
    ;; setting COORD to a invalid coordinate that will never be used (while yet giving it the correct format to prevent errors), when COORD was not passed
    (unless coord
      (setq coord (cons -1 -1)))
    ;; printing one row at a time
    (dotimes (d (grid-height ng))
      ;; printing side clues first
      (format t "~a" (elt (side-clues-print-data ng) d))
      ;; if the recently changed coordinate is in this row...
      (if (= d (car coord))
          (progn ;; THEN, print each value of row, adding either a space before, or a specialized character when the row value was just changed (based on COORD)
            (let ((markers-to-add 0)
                  (marker (if (line-is-horizontal-p ng line-obj) #\~ #\|)))
              (dotimes (r (grid-width ng))
                (when (= r (cdr coord))
                  (setq markers-to-add 2)) ;; ! a marker will surround the newly placed value, as in ~1~ or |1|, hence the 2
                (let ((space-or-marker #\space))
                  (when (> markers-to-add 0)
                    (decf markers-to-add)
                    (setq space-or-marker marker))
                  ;; print each row-wise GRID value, preceded by a space or MARKER when appropriate
                  (format t "~a~a" space-or-marker (gethash (aref (grid ng) d r) (grid-value-to-printable-string-ht ng)))))
              ;; when a closing mark still had not been printed (since it must have been at the end of the row), print it
              (when (> markers-to-add 0)
                (format t "~a" marker))))
          (progn ;; ELSE, simply print each value of row, preceded by a space
            (dotimes (r (grid-width ng))
              (format t " ~a" (gethash (aref (grid ng) d r) (grid-value-to-printable-string-ht ng))))))
      ;; printing so as to separate GRID rows
      (format t "~%"))
    ;; when the current mark is incorrect, pause until user presses enter
    (when pause-due-to-incorrect-mark
      (format t "~%Pausing due to incorrect marking!~%")
      (press-enter-to-continue)))
  ;; printing additional space below grid
  (format t "~%")
  ;; printing border when appropriate
  (when border
    (format t "################################################################~%~%")))

(defmethod check-coord ((ng nonogram) (line-obj line) coord)
  "Triggers an error when the passed COORD is invalid. Note that throughout the entire program, every variable named COORD should be a CONS with the following format: (INTEGER . INTEGER). The first integer is the DOWN (or D) position and the second integer is the RIGHT (or R) position as used in looking up values in GRID (following a down-x-spaces-then-right-x-spaces scheme, hence D-R coordinates); variables are frequently named according to this convention through the code."
  (unless (consp coord)
    (error "COORD must be a CONS of two integers!"))
  (let ((d (car coord))
        (r (cdr coord)))
    (when (or (not (integerp d)) (not (integerp r)))
      (error "COORD must be a CONS of two integers!"))
    (unless (within-constraints-of-index-range-p d (cons 0 (grid-height ng)))
      (error "The first integer of COORD is invalid in relation to the height of GRID!"))
    (unless (within-constraints-of-index-range-p r (cons 0 (grid-width ng)))
      (error "The second integer of COORD is invalid in relation to the width of GRID!"))
    (if (line-is-horizontal-p ng line-obj)
        (when (/= d (index-from-edge line-obj))
          (error "The first integer of COORD does not refer to the passed horizontal line!"))
        (when (/= r (index-from-edge line-obj))
          (error "The second integer of COORD does not refer to the passed vertical line!")))))

(defmethod get-value-at-coord-in-grid ((ng nonogram) coord)
  "Returns the value in GRID at position COORD. COORD is expected to be valid according to the method CHECK-COORD; note, however, that it is not checked for validity."
  (aref (grid ng) (car coord) (cdr coord)))

(defmethod get-value-at-line-index-in-grid ((ng nonogram) (line-obj line) index)
  "Returns the value in GRID at the index of the given line. INDEX is expected to be valid according to the passed line; note, however, that it is not checked for validity."
  (let ((coord (get-coord-from-index-on-line ng line-obj index)))
    (get-value-at-coord-in-grid ng coord)))

(defmethod is-x-at-coord-p ((ng nonogram) x coord)
  "Returns T when the value in GRID at position COORD is numerically equal to X; otherwise returns NIL. X must be an integer, and is expected to be one of FILLED-VALUE, EMPTY-VALUE, or UNKNOWN-VALUE."
  (= x (get-value-at-coord-in-grid ng coord)))

(defmethod is-x-at-index-p ((ng nonogram) (line-obj line) x index)
  "Returns T when the value in GRID at INDEX in line is numerically equal to X; otherwise returns NIL. X must be an integer, and is expected to be one of FILLED-VALUE, EMPTY-VALUE, or UNKNOWN-VALUE. INDEX is expected to be valid according to the passed line; note, however, that it is not checked for validity."
  (= x (get-value-at-line-index-in-grid ng line-obj index)))

(defmethod coord-is-filled-p ((ng nonogram) coord)
  "Returns T when the value in GRID at position COORD is (FILLED-VALUE NG); otherwise returns NIL."
  (is-x-at-coord-p ng (filled-value ng) coord))

(defmethod index-is-filled-p ((ng nonogram) (line-obj line) index)
  "Returns T when the value at INDEX on line is (FILLED-VALUE NG); otherwise returns NIL."
  (is-x-at-index-p ng line-obj (filled-value ng) index))

(defmethod every-index-in-range-is-filled-p ((ng nonogram) (line-obj line) index-range)
  "Returns T if every index on line has the value (FILLED-VALUE NG); but returns NIL immediately upon finding any value that is not (FILLED-VALUE NG). INDEX-RANGE must be a CONS of two integers where the first is a valid inclusive index and the second is a valid exclusive index; note that INDEX-RANGE is assumed to be valid and is not checked."
  (do ((x (car index-range) (1+ x)))
      ((>= x (cdr index-range)) t) ;; return T when all indexes are FILLED-VALUE
    (unless (index-is-filled-p ng line-obj x)
      (return nil)))) ;; return NIL when any index is not FILLED-VALUE

(defmethod coord-is-empty-p ((ng nonogram) coord)
  "Returns T when the value in GRID at position COORD is (EMPTY-VALUE NG); otherwise returns NIL."
  (is-x-at-coord-p ng (empty-value ng) coord))

(defmethod index-is-empty-p ((ng nonogram) (line-obj line) index)
  "Returns T when the value at INDEX on line is (EMPTY-VALUE NG); otherwise returns NIL."
  (is-x-at-index-p ng line-obj (empty-value ng) index))

(defmethod coord-is-unknown-p ((ng nonogram) coord)
  "Returns T when the value in GRID at position COORD is (UNKNOWN-VALUE NG); otherwise returns NIL."
  (is-x-at-coord-p ng (unknown-value ng) coord))

(defmethod index-is-unknown-p ((ng nonogram) (line-obj line) index)
  "Returns T when the value at INDEX on line is (UNKNOWN-VALUE NG); otherwise returns NIL."
  (is-x-at-index-p ng line-obj (unknown-value ng) index))

(defmethod get-line-by-orientation-and-index-from-edge ((ng nonogram) orientation index)
  "Returns the LINE object that is at INDEX from edge according to ORIENTATION. This method simply gets the LINE object from the NONOGRAM class slot LINES, according to the ordering of LINES; note that, in this ordering, horizontal lines come first, progressing from top horizontal to bottom horizontal, and then vertical lines come next, progressing from left-most vertical to right-most vertical. INDEX must be an integer. ORIENTATION must be either HORIZONTAL-CHAR or VERTICAL-CHAR."
  (let* ((horizontal-p (orientation-is-horizontal-p ng orientation))
         (index-range (cons 0 (if horizontal-p (grid-height ng) (grid-width ng)))))
    (if (not (within-constraints-of-index-range-p index index-range))
        (error "INDEX is not within bounds of the GRID for this ORIENTATION!")
        (if horizontal-p
            (elt (lines ng) index)
            (elt (lines ng) (+ index (grid-height ng)))))))

(defmethod get-intersecting-line-from-index ((ng nonogram) (line-obj line) index &optional (check-index nil))
  "Returns the LINE object that intersects with INDEX on the passed line. Set CHECK-INDEX to T, to confirm that INDEX is valid for the passed line."
  (when check-index
    (check-index-for-line ng line-obj index))
  (get-line-by-orientation-and-index-from-edge ng (get-opposite-orientation ng (orientation line-obj)) index))

(defmethod get-intersecting-line-from-coord ((ng nonogram) (line-obj line) coord &optional (check-coord nil))
  "Returns the LINE object that intersects with the relevant coordinate (of COORD) of the passed LINE-OBJ. Note that the \"relevant coordinate\" is determined by the orientation of LINE-OBJ."
  (when check-coord
    (check-coord ng line-obj coord))
  (if (line-is-horizontal-p ng line-obj)
      (get-line-by-orientation-and-index-from-edge ng (vertical-char ng) (cdr coord)) ;; then use the R coordinate to get the intersecting horizontal
      (get-line-by-orientation-and-index-from-edge ng (horizontal-char ng) (car coord)))) ;; else use the D coordinate to get the intersecting vertical

(defmethod update-nonogram-and-line-data ((ng nonogram) (line-obj line) coord calling-function)
  "Updates various important slots of the NONOGRAM class and LINE class (including the line that intersects with the LINE-OBJ line), while also handling the printing of the grid (depending on the value of NONOGRAM slot PRINT-EACH-STEP-P) and the preparation of the associated NONOGRAM slot ALL-MOVES-ARRAY (depending on the value of the NONOGRAM slot COLLECT-MOVE-DATA-P). This method is meant to be called whenever the grid has been updated by a technique in order to keep proper track of things like whether the grid was changed, the count of steps in solving a nonogram, and which groups of techniques should be applied to this line. This method should only ever be called indirectly by calling one of the methods like MARK-AS-X-AT-*. COORD is the coordinate where the recent change has occurred; note that COORD is not checked for validity in any sense. CALLING-FUNCTION, if provided, must be a CONS where the CAR element should be the quoted name of the calling function and the CDR element should be a string indicating the line-assessment type."
  (unless (grid-was-changed-p ng)
    (setf (grid-was-changed-p ng) t))
  ;; incrementing the number of steps used to solve the nonogram
  (incf (step-count ng))
  ;; update LINE-OBJ and the appropriate intersecting line so as to ensure that these updated lines are reassessed
  (dolist (considered-line (list line-obj (get-intersecting-line-from-coord ng line-obj coord nil)))
    (unless (use-primary-p considered-line)
      (setf (use-primary-p considered-line) t))
    (unless (use-secondary-p considered-line)
      (setf (use-secondary-p considered-line) t))
    (unless (use-gen1-p considered-line)
      (setf (use-gen1-p considered-line) t))
    (unless (use-gen2-p considered-line)
      (setf (use-gen2-p considered-line) t))
    (unless (update-shortened-line-p considered-line)
      (setf (update-shortened-line-p considered-line) t))
    (unless (update-line-subsections-p considered-line)
      (setf (update-line-subsections-p considered-line) t)))
  ;; collecting data about recent "mark" if appropriate
  (when (collect-move-data-p ng)
    (vector-push-extend (list coord (get-value-at-coord-in-grid ng coord)) (all-moves-array ng)))
  ;; printing textual representation of GRID, if appropriate
  (when (print-each-step-p ng)
    (print-grid ng line-obj coord calling-function))
  ;; pausing for a function, if specified
  (when (and (pause-for-function ng)
             (symbolp (pause-for-function ng))
             (equal (pause-for-function ng) (car calling-function)))
    (format t "Pausing because the method ~a was called....~%" (symbol-name (car calling-function)))
    (press-enter-to-continue)))

(defmethod mark-as-x-at-coord ((ng nonogram) (line-obj line) x coord &optional calling-function)
  "Sets the value at position COORD in the NONOGRAM multi-dimensional array (GRID) to be X. X must be an integer and is expected to be either EMPTY-VALUE or FILLED-VALUE; note that the validity of X is not checked. COORD must be a valid CONS of integers that form a coordinate pair, and is expected to be a coordinate that exists on LINE-OBJ; note that the validity of COORD is not checked. CALLING-FUNCTION, if provided, must be a CONS where the CAR element should be the quoted name of the calling function and the CDR element should be a string indicating the line-assessment type. Because this method does not check the value at COORD, it should be used cautiously, and ideally only when the value at COORD has already been checked or is otherwise certain to be of UNKNOWN-VALUE."
  (setf (aref (grid ng) (car coord) (cdr coord)) x)
  (update-nonogram-and-line-data ng line-obj coord calling-function))

(defmethod mark-as-x-at-coord-when-unknown ((ng nonogram) (line-obj line) x coord &optional calling-function)
  "Sets the value at position COORD in the NONOGRAM multi-dimensional array (GRID) to be X, but only when the value at COORD is UNKNOWN-VALUE. X must be an integer and is expected to be either EMPTY-VALUE or FILLED-VALUE; note that the validity of X is not checked. COORD must be a valid CONS of integers that form a coordinate pair, and is expected to be a coordinate that exists on LINE-OBJ; note that the validity of COORD is not checked. CALLING-FUNCTION, if provided, must be a CONS where the CAR element should be the quoted name of the calling function and the CDR element should be a string indicating the line-assessment type."
  (when (coord-is-unknown-p ng coord)
    (mark-as-x-at-coord ng line-obj x coord calling-function)))

(defmethod mark-as-x-at-i-when-unknown ((ng nonogram) (line-obj line) x i &optional calling-function)
  "Sets the value at position I in LINE-OBJ to be X, such that the appropriate coordinate in the NONOGRAM multi-dimensional array (GRID) is changed, but only when the value at I is UNKNOWN-VALUE. X must be an integer and is expected to be either EMPTY-VALUE or FILLED-VALUE; note that the validity of X is not checked. The parameter I must be an integer, and is expected to be valid according to the length of line; note that the validity of I is not checked. CALLING-FUNCTION, if provided, must be a CONS where the CAR element should be the quoted name of the calling function and the CDR element should be a string indicating the line-assessment type."
  (let ((coord (get-coord-from-index-on-line ng line-obj i)))
    (mark-as-x-at-coord-when-unknown ng line-obj x coord calling-function)))

(defmethod mark-as-x-at-i ((ng nonogram) (line-obj line) x i &optional calling-function)
  "Sets the value at position I in LINE-OBJ to be X, such that the appropriate coordinate in the NONOGRAM multi-dimensional array (GRID) is changed. X must be an integer and is expected to be either EMPTY-VALUE or FILLED-VALUE; note that the validity of X is not checked. The parameter I must be an integer, and is expected to be valid according to the length of line; note that the validity of I is not checked. CALLING-FUNCTION, if provided, must be a CONS where the CAR element should be the quoted name of the calling function and the CDR element should be a string indicating the line-assessment type. Because this method does not check the value at I, it should be used cautiously, and ideally only when the value at I has already been checked or is otherwise certain to be of UNKNOWN-VALUE."
  (let ((coord (get-coord-from-index-on-line ng line-obj i)))
    (mark-as-x-at-coord ng line-obj x coord calling-function)))

(defmethod mark-as-x-at-i-when-unknown-for-range ((ng nonogram) (line-obj line) x i line-range &optional calling-function)
  "Sets the value at position I in LINE-OBJ to be X, such that the appropriate coordinate in the NONOGRAM multi-dimensional array (GRID) is changed, but only when the value at I is UNKNOWN-VALUE, and I is valid according to LINE-RANGE. X must be an integer and is expected to be either EMPTY-VALUE or FILLED-VALUE; note that the validity of X is not checked. The parameter I must be an integer, and is checked against LINE-RANGE, such that if I is invalid nothing will happen. LINE-RANGE is expected to be a CONS of two integers that follows the specification set by the function CHECK-RANGE; note, however, that the validity of LINE-RANGE is also not checked. CALLING-FUNCTION, if provided, must be a CONS where the CAR element should be the quoted name of the calling function and the CDR element should be a string indicating the line-assessment type."
  (when (within-constraints-of-index-range-p i line-range)
    (mark-as-x-at-i-when-unknown ng line-obj x i calling-function)))

(defmethod find-completed-clues-from-extreme-edges ((ng nonogram) (line-obj line))
  "Marks GRID by considering the passed line from each edge such that any edge-adjacent FILL-VALUE(s) are extended (if needed) according to the related clue, and then afterwards capped off with an EMPTY-VALUE; note that \"edge-adjacent\" includes the idea of shortened-line edges. If it has been determined that all clues have been marked and capped, then any remaining UNKNOWN-VALUEs will be changed to EMPTY-VALUE. This method will set the LINE class slot LINE-COMPLETE-P to T if the line is determined to be complete. Note that this method is meant to be used immediately before setting shortened-line slots (such as with SET-SHORTENED-LINE-RANGE and SET-SHORTENED-LINE-CLUES), and in the main solver is only used indirectly in preparing shortened-lines through the method UPDATE-SHORTENED-LINE. This method also specifically sets the LINE class slot SHORTENED-LINE-FINISHED-CLUES-BY-INDEX, and is the only method that sets that slot.
e.g. |2 1| 1---- becomes 110--
e.g. |3| ----1 becomes 00111
e.g. |2 2| 1---- becomes 110--
e.g. |2 1| 1------1 becomes 110---01
e.g. |1 1 2 1| -----101 becomes ---01101"
  (let ((calling-function (cons 'find-completed-clues-from-extreme-edges "full-line")))
    ;; assesses line from left-to-right and then right-to-left, keeping track of consecutive FILLED-VALUES while relating them to clues ...
    ;; ... extending any obvious FILLED-VALUE elements according to related clue, capping any completed clues, and continuing until finding an unknown that cannot be changed
    (do ((iteration-count 0 (1+ iteration-count)) ;; will iterate twice; once for each direction
         (direction 1 -1) ;; left-to-right, and then right-to-left
         (start-index 0 (1- (get-line-length ng line-obj)))
         (clue-index 0 (1- (get-clue-count-for-line line-obj)))
         (cur-filled-count 0 0) ;; is the count of FILLED-VALUEs encountered since start or last EMPTY-VALUE
         (next-is-empty-p nil nil)) ;; indicates that the next element should be EMPTY-VALUE; will be set to T after counting out x FILLED-VALUEs where x is clue value
        ((or (line-complete-p line-obj) (>= iteration-count 2)))
      ;; iterating over indexes of line in one direction
      (do ((next-iteration-count 0 (1+ next-iteration-count)) ;; will iterate, at maximum, a number of times equal to the length of the line
           (i start-index (+ i direction))
           (coord nil nil))
          ((>= next-iteration-count (get-line-length ng line-obj)) (setf (line-complete-p line-obj) t)) ;; if this finally-clause is reached, the line must have no UNKNOWN-VALUEs and must be complete
        ;; when CLUE-INDEX is out-of-bounds, meaning all clues have been found and any remaining UNKNOWN-VALUEs (if any) must be EMPTY-VALUE
        (when (or (< clue-index 0) (>= clue-index (get-clue-count-for-line line-obj))) ;; !!!!!!!!!!!!! could make a specialized method for this...
          ;; iterate across line, changing every found UNKNOWN-VALUE to EMPTY-VALUE
          (do ((j i (+ j direction)))
              ((or (< j 0) (>= j (get-line-length ng line-obj))))
            (mark-as-x-at-i-when-unknown ng line-obj (empty-value ng) j calling-function))
          ;; mark line as complete and move onto next directional iteration, and subsequently end that iteration too
          (setf (line-complete-p line-obj) t)
          (return))
        (setq coord (get-coord-from-index-on-line ng line-obj i))
        ;; set variables and possibly update GRID according to what *-VALUE is found at GRID coordinate
        ;; ! note that if an unknown cannot be changed, then the directional loop will terminate this iteration
        (cond
          ((coord-is-filled-p ng coord)
           (incf cur-filled-count)
           (setq next-is-empty-p nil)
           ;; when clue value has been reached
           (when (= cur-filled-count (elt (line-clues line-obj) clue-index)) ;; !!! generalize this ???
             (setq cur-filled-count 0)
             (setf (gethash clue-index (shortened-line-finished-clues-by-index line-obj)) t)
             (setq next-is-empty-p t)
             (setq clue-index (+ clue-index direction))))
          ((coord-is-empty-p ng coord)
           (setq cur-filled-count 0)
           (setq next-is-empty-p nil))
          ((coord-is-unknown-p ng coord)
           ;; once an UNKNOWN-VALUE is encountered it is set to either FILLED-VALUE or EMPTY-VALUE as appropriate, or else the directional loop terminates this iteration
           (cond
             (next-is-empty-p
              ;; mark as empty since clue value was just reached
              (mark-as-x-at-coord ng line-obj (empty-value ng) coord calling-function)
              (setq cur-filled-count 0)
              (setq next-is-empty-p nil))
             ((and (> cur-filled-count 0) (< cur-filled-count (elt (line-clues line-obj) clue-index)))
              ;; mark as filled since clue value had not (and may still have not) yet been reached
              (mark-as-x-at-coord ng line-obj (filled-value ng) coord calling-function)
              (incf cur-filled-count)
              (setq next-is-empty-p nil)
              ;; when clue value has been reached
              (when (= cur-filled-count (elt (line-clues line-obj) clue-index)) ;; !!! generalize this ???
                (setq cur-filled-count 0)
                (setf (gethash clue-index (shortened-line-finished-clues-by-index line-obj)) t)
                (setq next-is-empty-p t)
                (setq clue-index (+ clue-index direction))))
             (t
              ;; end directional loop since an unknown was encountered and not changed
              (return))))))))
  (if (line-complete-p line-obj)
      nil
      t))

(defmethod set-shortened-line-clues ((ng nonogram) (line-obj line))
  "Attempts to set the LINE class slot SHORTENED-LINE-CLUES, returning T when this slot has been set or updated, but returning NIL if the line was already complete, without updating SHORTENED-LINE-CLUES. Note that this method depends on having the method FIND-COMPLETED-CLUES-FROM-EXTREME-EDGES run against LINE-OBJ immediately before, in order to have an up-to-date and relevant value for SHORTENED-LINE-FINISHED-CLUES-BY-INDEX. Also note that this method is only meant to be called from within the method UPDATE-SHORTENED-LINE.
e.g. |1 3 2| 010-11--1- has SHORTENED-LINE-CLUES of #(3 2)
e.g. |2 4 1| 110-111-01 has SHORTENED-LINE-CLUES of #(4)
e.g. |1 1| ----0 has SHORTENED-LINE-CLUES of #(1 1)
e.g. |1 1| ----1 has SHORTENED-LINE-CLUES of #(1 1) (but through UPDATE-SHORTENED-LINE method the line changes to ---01 so that SHORTENED-LINE-CLUES is #(1))"
  (unless (line-complete-p line-obj) ;; do nothing and return NIL if line already complete
    (let ((remaining-clues (make-array 0 :element-type 'integer :adjustable t :fill-pointer 0)))
      (dotimes (i (get-clue-count-for-line line-obj))
        (unless (gethash i (shortened-line-finished-clues-by-index line-obj))
          (vector-push-extend (elt (line-clues line-obj) i) remaining-clues)))
      (setf (shortened-line-clues line-obj) remaining-clues))
    t))

(defmethod set-shortened-line-range ((ng nonogram) (line-obj line))
  "Attempts to set the LINE class slot SHORTENED-LINE-RANGE, returning T when this slot has been set or updated, but returning NIL when the line has been determined complete (in which case the LINE class slot LINE-COMPLETE-P is set to T, without updating SHORTENED-LINE-RANGE). Note that subsections of the line are only excluded when they consist of only EMPTY-VALUE(s) and zero or more FILLED-VALUE(s) such that the side-most value nearest to the unfinished section is an EMPTY-VALUE. Also note that this method is only meant to be called from within the method UPDATE-SHORTENED-LINE.
e.g. |1 3 2| 010-11--1- has SHORTENED-LINE-RANGE of (3 . 10), giving this subsequence: -11--1-
e.g. |2 4 1| 11---1--01 has SHORTENED-LINE-RANGE of (0 . 8), giving this subsequence: 11---1-- (by UPDATE method has range of (3 . 8), becoming 110--1--01, giving --1--)
e.g. |2| 0-1-0 has SHORTENED-LINE-RANGE of (1 . 4), giving this subsequence: -1-
e.g. |1| --1-- has SHORTENED-LINE-RANGE of (0 . 5), giving this subsequence: --1--"
  (unless (line-complete-p line-obj) ;; do nothing and return NIL if line already complete
    (let ((start -1)
          (end -1)) ;; note that END is an exclusive index
      ;; assesses line from left-to-right, setting START to the index that is one index past the last EMPTY-VALUE encountered upon finding the first UNKNOWN-VALUE
      ;; then assesses line right-to-left, setting END to the index of the last EMPTY-VALUE encountered upon finding the first UNKNOWN-VALUE
      ;; if no UNKNOWN-VALUE is found during iteration across line, then the line is complete, and the second directional iteration will be skipped
      (do ((iteration-count 0 (1+ iteration-count)) ;; will iterate twice; once for each direction
           (direction 1 -1) ;; left-to-right, and then right-to-left
           (start-index 0 (1- (get-line-length ng line-obj)))
           (index-of-last-empty -1 (get-line-length ng line-obj))) ;; holds the index of the last encountered EMPTY-VALUE
          ((or (line-complete-p line-obj) (>= iteration-count 2)))
        ;; iterating over indexes of line in one direction
        (do ((next-iteration-count 0 (1+ next-iteration-count)) ;; will iterate, at maximum, a number of times equal to the length of the line
             (i start-index (+ i direction))
             (coord nil nil))
            ((>= next-iteration-count (get-line-length ng line-obj)) (setf (line-complete-p line-obj) t)) ;; if this finally-clause is reached, the line must have no UNKNOWN-VALUEs and must be complete
          (setq coord (get-coord-from-index-on-line ng line-obj i))
          (if (coord-is-unknown-p ng coord)
              (progn ;; when UNKNOWN-VALUE was encountered
                (if (= 1 direction)
                    (setq start (1+ index-of-last-empty)) ;; when left-to-right, set START to INDEX-OF-LAST-EMPTY +1
                    (setq end index-of-last-empty)) ;; when right-to-left, set END to INDEX-OF-LAST-EMPTY
                (return)) ;; stop iterating across line, and move onto next directional iteration
              (progn ;; when other value was encountered
                (when (coord-is-empty-p ng coord)
                  (setq index-of-last-empty i))))))
      (if (line-complete-p line-obj)
          nil
          (progn ;; setting LINE class slot based on START and END
            (setf (shortened-line-range line-obj) (cons start end))
            t)))))

(defmethod mark-all-remaining-unknowns-as-empty ((ng nonogram) (line-obj line))
  "Marks GRID such that every UNKNOWN-VALUE of the passed line is indiscriminately changed to EMPTY-VALUE. This method uses shortened-line data because by the rules of shortened-line creation, only finished sections of the line will be excluded from consideration; this increases efficiency by reducing the number of checked positions in the line. Note that this method is dangerous (because it simply makes changes without considering correctness) and is only meant to be used when other methods or sequences of code have determined that it is correct to change all unknowns to empties; incautious usage will result in invalid nonogram solutions!
e.g. this is a proper usage scenario, as both 2 and 1 are clearly already represented within the line: |2 1| 110---01 becomes 11000001"
  (do ((i (car (shortened-line-range line-obj)) (1+ i)))
      ((>= i (cdr (shortened-line-range line-obj))))
    (mark-as-x-at-i-when-unknown ng line-obj (empty-value ng) i (cons 'mark-all-remaining-unknowns-as-empty "shortened-line")))
  (setf (line-complete-p line-obj) t))

(defmethod update-shortened-line ((ng nonogram) (line-obj line))
  "Updates shortened-line data for the passed LINE-OBJ so long as changes have been made to it since last update (based on the value of the LINE class slot UPDATE-SHORTENED-LINE-P). The updated LINE class slots include SHORTENED-LINE-CLUES, SHORTENED-LINE-RANGE, and UPDATE-SHORTENED-LINE-P. This method returns NIL when the line it is assessing has been completed; it returns T when the line is still incomplete (regardless of whether the shortened-line was updated), indicating that this line should still be further analyzed. Note that this method makes use of FIND-COMPLETED-CLUES-FROM-EXTREME-EDGES and hence may change the passed line significantly; see that method for more details."
  (if (line-complete-p line-obj)
      nil ;; return NIL because the line is done and should not be assessed further by other methods
      (if (not (update-shortened-line-p line-obj))
          t ;; returning T since shortened-line does not need to be updated, although the line should still be assessed by other methods (hence the T)
          (if (not (find-completed-clues-from-extreme-edges ng line-obj)) ;; ! note that this method returns NIL when it has completed the line
              nil ;; return NIL when line has been completed by the previous method
              (progn ;; when line is not complete, update shortened-line variables based on the marks done (if any) by FIND-COMPLETED-CLUES-FROM-EXTREME-EDGES
                (set-shortened-line-clues ng line-obj)
                (set-shortened-line-range ng line-obj)
                (setf (update-shortened-line-p line-obj) nil) ;; ! since shortened-line was just updated, changing this predicate accordingly
                (if (> (get-clue-count-for-shortened-line line-obj) 0) ;; if clues still remain to be marked on line...
                    t ;; return T because the line is still incomplete and should be assessed further by other methods
                    (progn ;; when the line still has UNKNOWN-VALUE(s) but all clues are complete...
                      (mark-all-remaining-unknowns-as-empty ng line-obj)
                      nil))))))) ;; return NIL because the line was just completed

(defmethod set-line-subsections ((ng nonogram) (line-obj line))
  "Sets the LINE class slot LINE-SUBSECTIONS such that it is reset according to the current shortened-line. Note that LINE-SUBSECTIONS consists exclusively of instances of the class WORKABLE-SUBSECTION. A single \"workable-subsection\" is any collection of exclusively FILLED-VALUE(s) or UNKNOWN-VALUES(s) in any order such that this collection is delimited on each end within the original shortened-line by either an EMPTY-VALUE or line-edge. Note that all workable-subsections of the shortened-line are parsed out and kept, even those subsections that consist of only FILLED-VALUE. Note that this method is only meant to be called indirectly through the method UPDATE-LINE-SUBSECTIONS, which organizes all methods that set workable-subsection-data in a way that ensures correctness and consistency.
e.g. |1 4 1| 10-111-0-- yields two workable-subsections: -111- and --, in that order
e.g. |1 3 1| --0-1-1-0-- yields three workable-subsections: --, -1-1-, and --, in that order
e.g. |1 2 1| --011000-- yields three workable-subsections: --, 11, and --, in that order, notice that the finished 2-section is included"
  (setf (line-subsections line-obj) (make-array 0 :adjustable t :fill-pointer 0)) ;; ! note that LINE-SUBSECTIONS is reset each time this method is called
  (let ((start -1)) ;; ! a START value of -1 implies that a workable-subsection starting point is being looked for; but a value other than -1 implies that a workable-subsection starting point was already found and the index is START
    ;; iterating across the shortened-line left-to-right finding distinct workable-subsections and setting LINE-SUBSECTIONS accordingly
    ;; ! note that the delimiter for a workable-subsection is an empty or edge
    (do ((iteration-count 0 (1+ iteration-count))
         (i (car (shortened-line-range line-obj)) (1+ i)))
        ((>= iteration-count (get-shortened-line-length line-obj)))
      (if (index-is-empty-p ng line-obj i)
          (when (/= start -1) ;; if index is empty and START is not -1
            (vector-push-extend (make-instance 'workable-subsection :wss-range (cons start i)) (line-subsections line-obj))
            (setq start -1))
          (when (= start -1) ;; if index is filled or unknown and START is -1
            (setq start i))))
    ;; when a workable-subsection starting point was found, but the end was not (because iteration reached the shortened-line edge)... set one last workable-subsection
    (when (/= start -1)
      (vector-push-extend (make-instance 'workable-subsection :wss-range (cons start (cdr (shortened-line-range line-obj)))) (line-subsections line-obj)))))

;; !!!!! |1 3 1| --0-1-1-0-- (what happens with this???) !!! this yields three workable-subsections with no clue relations!!!!!
;; ............. 1-0111-10-- from left
;; ............. --01-1110-1 from right
;; ... note that this stacking ignores and even contradicts already placed FILLED-VALUEs
;; ... although 3 is certainly in central section, it is not certain, when ignoring already placed FILLED-VALUES, that it is only the 3 or the 3 with left-1 or the 3 with right-1
;; ... but given already placed FILLED-VALUES it is certain that 1s are on sides and 3 is in center !!!!!!!!!!!!!!!!
;; !!!!! also, for |2| -1-0--, it should be obvious that the right subsection must be empty... but the method does not do this yet!!!!!!!!!!!!!!!!!!!!!!!!!!
(defmethod relate-clues-to-line-subsections ((ng nonogram) (line-obj line))
  "Examines all WORKABLE-SUBSECTIONs of the given line, determining which of the shortened-line clues must be related to which subsections (where it is possible to know [via the simple method used, as in the example below]), setting the WORKABLE-SUBSECTION class slot WSS-CLUES accordingly; and also setting and WSS-FINISHED-P to T when appropriate. Note that this method must immediately follow a call to the method SET-LINE-SUBSECTIONS, but keep in mind that this method is only meant to be called indirectly through the method UPDATE-LINE-SUBSECTIONS, which organizes all methods that set workable-subsection-data in a way that ensures correctness and consistency.
e.g. the workable-subsections -111- and 11 of line |1 4 1| 10-111-0-- are related thus: |4| -111- and |1| --
e.g. for |2 1 1| -----0--, all clues can be related because of the two simple stacking variations: 11-1- 1- and -11-1 -1, yielding |2 1| ----- and |1| --
e.g. note that the stacking process disregards the presence of FILLED-VALUEs such that the line |1 3 1| --0-1-1-0-- yields no clue relations."
  ;; preparing directional hash-tables
  (let ((from-left-ht (make-hash-table))   ;; these hash-tables are used in the below loop, being used according to the current direction (left-to-right or right-to-left)...
        (from-right-ht (make-hash-table))) ;; ... each hash-table has, as key, the value of INDEX-IN-LINE-SUBSECTIONS, and its associated value in the key-value pair is ...
                                           ;; ... in this format: (cons (list CLUE-INDEX...) <size-of-list>); note that the list size is only kept for efficiency's sake
    ;; steps through line-subsections and shortened-line-clues from left-to-right and then right-to-left, in order to relate clues to line-subsections ...
    ;; ... filling out the appropriate hash-table as referenced by TABLE with the preliminary data that will be used to make the relations below (after this loop)
    (do ((iteration-count 0 (1+ iteration-count)) ;; will iterate twice; once for each direction
         (direction 1 -1) ;; left-to-right, and then right-to-left
         (table from-left-ht from-right-ht) ;; selecting the appropriate hash-table to modify per direction
         (index-in-line-subsections 0 (1- (get-subsection-count-for-line line-obj)))
         (clue-index 0 (1- (get-clue-count-for-shortened-line line-obj)))
         (additional-empty 0 0) ;; an integer that is ether 0 or 1, indicating at the current iteration whether or not to simulate the inclusion of one more EMPTY-VALUE
         (clue-sum 0 0)) ;; an integer that is the sum of all clues that are trying to be fit into the current workable-subsection plus any necessary EMPTY-VALUEs
        ((>= iteration-count 2))
      ;; the below loop (figuratively) lays out all clues in order across the workable-subsections of the line starting from a single side, providing minimal spacing ...
      ;; ... between each pair of clues, until all have been placed and thereby associated, with each clue being associated to a single workable-subsection
      ;; ! once this has been done in both directions (via above loop), overlaps in the patterning of both directions will tell which clues must relate to which subsections
      (do ()
          (nil) ;; ! note that this loop will end when CLUE-INDEX is out of bounds, meaning that all clues have been stacked across in single direction
        (incf clue-sum (+ (elt (shortened-line-clues line-obj) clue-index) additional-empty))
        (if (<= clue-sum (get-length-of-workable-subsection (elt (line-subsections line-obj) index-in-line-subsections)))
            (progn ;; when the clue can fit within the subsection
              (if (gethash index-in-line-subsections table) ;; if hash-table already has an entry (meaning another clue has already fit into this subsection)
                  ;; THEN modify the existing hash-table entry
                  (let ((entry (gethash index-in-line-subsections table)))
                    (push clue-index (car entry))
                    (incf (cdr entry)))
                  ;; ELSE create a new hash-table entry
                  (setf (gethash index-in-line-subsections table) (cons (list clue-index) 1)))
              ;; updating and verifying CLUE-INDEX
              (incf clue-index direction) ;; ! note that CLUE-INDEX is always incremented after hash-table update thereby guaranteeing it is unique in LIST part of table
              (unless (index-is-valid-for-array-shortened-line-clues-p line-obj clue-index)
                (return)) ;; move onto next directional iteration
              (setq additional-empty 1))
            (progn ;; when the clue CANNOT fit within the subsection
              ;; move onto next workable-subsection (if any)
              (incf index-in-line-subsections direction)
              (unless (index-is-valid-for-array-line-subsections-p line-obj index-in-line-subsections)
                (if (< index-in-line-subsections 0)
                    (error "INDEX-IN-LINE-SUBSECTIONS is somehow negative!") ;; ! this error should NEVER happen unless incorrect changes are made to the code
                    (error "Attempted to move onto next WORKABLE-SUBSECTION when none exists; this indicates that remaining clues could not be fit into existing WORKABLE-SUBSECTIONS, implying that the calling of this method was preceded (somewhere) by incorrect markings!")))
              ;; reset variables before moving on to next workable-subsection
              (setq clue-sum 0)
              (setq additional-empty 0)))))
    ;; setting the WORKABLE-SUBSECTION slot RELATED-CLUE-INDEX for every subsection whose entire clue relation can be known from both left-to-right and right-to-left
    ;; ! e.g. for |2 1 1| -----0--, all clues can be related because of the two simple stacking variations: 11-1- 1- and -11-1 -1, yielding |2 1| ----- and |1| -- ...
    ;; ! ... but for |2 1 1| -----0--- no definite relations are made because of variations: 11-1- 1-- and ---11 1-1; these variations tell that a 2 definitely is ...
    ;; ! ... in the left and a 1 is definitely in the right section, but another 1 may be in the left or right; this kind of information is not kept because this ...
    ;; ! ... method is only seeking those situations where a WORKABLE-SUBSECTION's clues can be _known_ from both a left-to-right consideration AND a right-to-left one
    ;; note that this loop also checks for finished workable-subsections
    (dotimes (i (get-subsection-count-for-line line-obj))
      ;; considering one subsection at a time, and its related assessments from different directions
      (let ((from-left-entry (gethash i from-left-ht))
            (from-right-entry (gethash i from-right-ht))
            (cur-wss (elt (line-subsections line-obj) i)))
        (when (and from-left-entry from-right-entry
                   (= (cdr from-left-entry) (cdr from-right-entry))
                   (every #'(lambda (x) (member x (car from-right-entry))) (car from-left-entry)))
          ;; when left and right entries are not NIL, sizes of sets are the same, and contents of FROM-RIGHT-ENTRY set are found in FROM-LEFT-ENTRY set
          ;; preparing to set WSS-CLUES
          (let* ((subsection-clue-indexes (coerce (car from-right-entry) 'vector)) ;; using FROM-RIGHT-ENTRY because of its ordering (as a pushed list)
                 (subsection-clue-count (cdr from-right-entry))
                 (subsection-clues (make-array subsection-clue-count :element-type 'integer :fill-pointer 0)))
            ;; filling out SUBSECTION-CLUES so that it has all actual clue values according to previously collected clue-indexes
            (dotimes (i subsection-clue-count)
              (vector-push-extend (elt (shortened-line-clues line-obj) (elt subsection-clue-indexes i)) subsection-clues))
            ;; finally setting the WSS-CLUES slot
            (setf (slot-value cur-wss 'wss-clues) subsection-clues)))
        ;; checks whether every element in workable-subsection is a FILLED-VALUE, and, if so, marks subsection as complete...
        (when (every-index-in-range-is-filled-p ng line-obj (wss-range cur-wss))
          (setf (wss-finished-p cur-wss) t))))))

(defmethod update-line-subsections ((ng nonogram) (line-obj line))
  "Updates shortened-line data and line workable-subsection data for the passed LINE-OBJ so long as changes have been made to it since last update (based on the value of the LINE class slot UPDATE-SHORTENED-LINE-P and UPDATE-LINE-SUBSECTIONS-P). The updated LINE class slots include SHORTENED-LINE-CLUES, SHORTENED-LINE-RANGE, UPDATE-SHORTENED-LINE-P, LINE-SUBSECTIONS, and UPDATE-LINE-SUBSECTIONS-P. This method returns NIL when the line it is assessing has been completed; it returns T when the line is still incomplete (regardless of whether the shortened-line or line-subsections were updated), indicating that this line should still be further analyzed."
  (if (not (update-shortened-line ng line-obj)) ;; ! note that WORKABLE-SUBSECTION data is based on shortened-line
      nil ;; return NIL because the line must have been completed via updating the shortened-line
      (if (not (update-line-subsections-p line-obj))
          t ;; returning T since LINE-SUBSECTIONS does not need to be updated, although the line should still be assessed by other methods (hence the T)
          (progn ;; when LINE-SUBSECTIONS should be updated
            (setf (update-line-subsections-p line-obj) nil) ;; don't update LINE-SUBSECTIONS again, unless changes have been made to the related line
            (set-line-subsections ng line-obj)
            (relate-clues-to-line-subsections ng line-obj)
            (if (not (every-workable-subsection-is-finished-p line-obj))
                t ;; returning T because LINE-SUBSECTIONS was updated and line should be assessed further
                (progn ;; when all clues are complete, but the line is not
                  (mark-all-remaining-unknowns-as-empty ng line-obj)
                  nil)))))) ;; returning NIL because the line was just completed

(defparameter +full-line+ 1 "This is to be passed to a nonogram technique, and indicates that the full-line data is to be used.")
(defparameter +shortened-line+ 2 "This is to be passed to a nonogram technique, and indicates that the line's shortened-line data is to be used.")
(defparameter +line-subsections+ 3 "This is to be passed to a nonogram technique, and indicates that the line's line-subsection data is to be used.")

(defmacro proceed-according-to-line-assessment-type ((clues-var clue-count-var line-range-var calling-function-var quoted-function-name) &body body)
  "This macro prepares the appropriate variables for use within every line-based technique's main code according to LINE-ASSESSMENT-TYPE, while also, according to the same, updating LINE data beforehand, where necessary. Note that this macro expects the following local variables to be bound in the context of where it is used: the NONOGRAM class object NG, the LINE class object LINE-OBJ, and the integer LINE-ASSESSMENT-TYPE which has the value of +FULL-LINE+, +SHORTENED-LINE+, or +LINE-SUBSECTIONS+. This macro also determines whether the technique gets used at all: if the considered subsequence of the line is already complete or if necessary data for the technique has not be set, the technique will not be used."
  (let ((quoted-function-name-gs (gensym))
        (arr-of-lists-gs (gensym))
        (index-gs (gensym))
        (cur-wss-gs (gensym))
        (x-gs (gensym)))
    `(let ((,clues-var nil)
           (,clue-count-var nil)
           (,line-range-var nil)
           (,calling-function-var nil)
           (,quoted-function-name-gs ,quoted-function-name)
           (,arr-of-lists-gs (make-array 0 :fill-pointer 0))) ;; this array holds at each element a list in this format: (CLUES CLUE-COUNT LINE-RANGE)
       ;; preparing variables (with most being placed orderly into ARR-OF-LISTS-GS) according to the line type, doing any necessary line preparation beforehand
       (cond
         ((= line-assessment-type +full-line+)
          (unless (line-complete-p line-obj)
            (vector-push-extend (list (line-clues line-obj) (get-clue-count-for-line line-obj) (cons 0 (get-line-length ng line-obj))) ,arr-of-lists-gs)
            (setq ,calling-function-var (cons ,quoted-function-name-gs, "full-line"))))
         ((= line-assessment-type +shortened-line+)
          (when (update-shortened-line ng line-obj)
            (vector-push-extend (list (shortened-line-clues line-obj) (get-clue-count-for-shortened-line line-obj) (shortened-line-range line-obj)) ,arr-of-lists-gs)
            (setq ,calling-function-var (cons ,quoted-function-name-gs, "shortened-line"))))
         ((= line-assessment-type +line-subsections+)
          (when (update-line-subsections ng line-obj)
            (dotimes (,index-gs (get-subsection-count-for-line line-obj))
              (let ((,cur-wss-gs (elt (line-subsections line-obj) ,index-gs)))
                (when (and (not (wss-finished-p ,cur-wss-gs)) (wss-clues ,cur-wss-gs)) ;; when subsection is not finished and clues are set
                  (vector-push-extend (list (wss-clues ,cur-wss-gs) (get-clue-count-for-workable-subsection ,cur-wss-gs) (wss-range ,cur-wss-gs)) ,arr-of-lists-gs)
                  (setq ,calling-function-var (cons ,quoted-function-name-gs, "line-subsections")))))))
         (t
          (error "LINE-ASSESSMENT-TYPE must be one of the following: +FULL-LINE+, +SHORTENED-LINE+, or +LINE-SUBSECTIONS+!")))
       ;; iterating through ARR-OF-LISTS such that all remaining variables are set correctly for each iteration (where each iteration is a single running of the technique against a single line subsequence)
       ;; ! note that the following loop will only iterate once at most, unless line-subsections are being worked with, in which case there may be more than one iteration
       (dotimes (,index-gs (fill-pointer ,arr-of-lists-gs))
         (let ((,x-gs (elt ,arr-of-lists-gs ,index-gs)))
           (setq ,clues-var (first ,x-gs)
                 ,clue-count-var (second ,x-gs)
                 ,line-range-var (third ,x-gs))
           ,@body)))))

;; !!!!! could track whether there is a capping EMPTY-VALUE at end of zone (or extended-zone)
;; !!!!! could track for completed clues and mark around largest completed clue...
(defmethod get-zone-data-for-line-segment-by-clue-count-in-direction ((ng nonogram) (line-obj line) clues clue-count line-range considered-clue-count &key (direction 1) (check-basic-args nil))
  "Returns the size of the zone and the extended zone for the line-segment, according to the number of considered clues and DIRECTION. A zone is an area where, if all FILLED-VALUEs were placed maximally edge adjacent, then any FILLED-VALUE found there must be part of the same collection of clues by CONSIDERED-CLUE-COUNT; the extended zone is a zone that had a FILLED-VALUE at its furthest zone-reach that extends outside the zone, thereby indicating that this final group of \"extended\" FILLED-VALUEs is necessarily part of the same zone although reaching beyond it. CLUES, CLUE-COUNT, and LINE-RANGE should be variables that are derived from a usage of the macro PROCEED-ACCORDING-TO-LINE-ASSESSMENT-TYPE; note that they are not checked for validity. CONSIDERED-CLUE-COUNT is an integer that indicates how many clues of CLUES should be considered in calculating the zone. DIRECTION must be either 1 or -1, with 1 indicating a left-to-right assessment, and -1 indicating a right-to-left assessment. Set CHECK-BASIC-ARGS to T to trigger an error when any of the basic arguments are invalid.
e.g. with DIRECTION 1 and CONSIDERED-CLUE-COUNT of 2, |2 1| ----- has zone of *****-, or ZONE-SIZE of 5 (2+1+1+1), with extended size being the same
e.g. with DIRECTION 1 and CONSIDERED-CLUE-COUNT of 1, |2 1| ----- has zone of ***---, or ZONE-SIZE of 3 (2+1), with extended size being the same
e.g. with DIRECTION 1 and CONSIDERED-CLUE-COUNT of 2, |2 3 1| ------111-- has zone of *******11--, or ZONE-SIZE of 7 (2+3+1+1), with extended size of 9
e.g. with DIRECTION -1 and CONSIDERED-CLUE-COUNT of 2, |2 3 1| ---11------ has zone of ---1*******, or ZONE-SIZE of 7 (2+3+1+1), with extended size of 8"
  (when check-basic-args
    (unless (integerp direction)
      (error "DIRECTION must be equal to 1 or -1."))
    (when (and (/= direction 1) (/= direction -1))
      (error "DIRECTION must be equal to 1 or -1."))
    (when (> considered-clue-count clue-count)
      (error "CONSIDERED-CLUE-COUNT cannot be greater than the count of clues for the shortened-line!"))
    (when (<= considered-clue-count 0)
      (error "CONSIDERED-CLUE-COUNT must be greater than zero!")))
  ;; assessing line-segment from left-to-right or right-to-left according to value of DIRECTION
  (let* ((left-to-right (= 1 direction))
         (clue-index (if left-to-right 0 (1- clue-count)))
         (zone-size 0)) ;; this is an integer representing the width of edge-zone where, if any filled occurs, it must be part of a clue by CONSIDERED-CLUE-COUNT
    ;; iterating across clues according to CONSIDERED-CLUE-COUNT, calculating RESTRICTED-ZONE
    ;; e.g. considering the 2 clues from left for |2 3 1| ----------- zone is |2 3 1| *******----; however, this does not mean that the 2 and 3 will be in the zone ...
    ;; ... but only that if a FILLED-VALUE is found within that zone, then it must be part of either the 2-clue grouping or the 3-clue grouping by the simple fact ...
    ;; ... of edge proximity, since the 2 and 3 must fill out this whole zone if, and only if, they are to be found within it at all (as in, 1101110 or 1100111 or 0110111)
    ;; ! the zone size can be numerically calculated thus: considered-clues + considered-clue-count... yielding (as in the above example) a zone of size 2+3+2 = 7
    (do* ((iteration-count 0 (1+ iteration-count))
          (ci clue-index (+ clue-index direction))
          (clue (elt clues ci) (elt clues ci)))
         ((>= iteration-count considered-clue-count))
      (incf zone-size (1+ clue)))
    ;; setting EXTENDED-ZONE, if appropriate
    ;; e.g. considering the 2 clues from left for |2 3 1| ------11---, the zone is |2 3 1| *******1---, and, because there is a filled at far end of zone and a filled ...
    ;; ... just outside the zone, the extended zone is of size 8; in other words, if the final position of the zone is filled, then maximally left-filling the zone ...
    ;; ... results in an invalid line: |2 3 1| 1101111; thereby indicating that any FILLED-VALUEs extending from the final zone position must necessarily also be ...
    ;; ... part of the 2-clue or 3-clue group
    ;; ... note that left-heavy variations for the 2-3 zone additionally include the following: 110000111, 011000111, 001100111, 000110111, 11000111, 01100111, 00110111
    ;; ! keep in mind that for large enough lines, it is very possible that there are no FILLED-VALUEs anywhere within the zone.
    (let* ((extended-zone-size zone-size) ;; this is like ZONE-SIZE and equal to it unless any FILLED-VALUE(s) within restricted-zone form a group that extends beyond it
           (start-index (if left-to-right (car line-range) (1- (cdr line-range)))) ;; this is the first index in line-segment by DIRECTION
           (last-index-in-zone (if left-to-right (+ start-index (1- zone-size)) (- start-index (1- zone-size)))))
      (when (index-is-filled-p ng line-obj last-index-in-zone)
        ;; incrementing EXTENDED-ZONE-SIZE for each filled found that extends from zone, stopping at first non-filled value
        (do* ((i (+ last-index-in-zone direction) (+ i direction)))
             (nil)
          (if (index-is-filled-p ng line-obj i)
              (incf extended-zone-size)
              (return))))
      (values zone-size extended-zone-size))))

(defmethod get-largest-or-smallest-remaining-clue-by-line-subsections ((ng nonogram) (line-obj line) &key (largest t))
  "Returns the largest or smallest remaining clue of the line, by comparing SHORTENED-LINE-CLUES to any centrally finished clues from any WORKABLE-SUBSECTIONs. When LARGEST is T, returns the largest clue value; when LARGEST is NIL, returns the smallest clue value.
e.g. when looking for largest with line |1 3 2| ----------, 3 is returned
e.g. when looking for largest with line |1 3 2| --01110---, 2 is returned
e.g. when looking for smallest with line |2 1 3| ----------, 1 is returned
e.g. when looking for smallest with line |2 1 3| ---010----, 2 is returned"
  (let ((clues (copy-seq (shortened-line-clues line-obj)))) ;; ! note that CLUES is a copy
    (dotimes (i (get-subsection-count-for-line line-obj))
      (let ((wss-obj (elt (line-subsections line-obj) i)))
        (when (wss-finished-p wss-obj)
          ;; when this subsection is finished, remove clue of same size as finished subsection from CLUES
          (setq clues (remove (get-length-of-workable-subsection wss-obj) clues :test #'= :count 1)))))
    (let ((f (if largest #'max #'min)))
      (reduce f clues))))

(defmethod mark-for-lines-with-no-clues ((ng nonogram))
  "Completes every line that has no clue; note that any line with no clue must consist entirely of EMPTY-VALUEs. This technique is an \"initial\" technique, and is only meant to be used once at the initial assessment of the nonogram.
e.g. || ----- becomes 00000"
  (let ((calling-function (cons 'mark-for-lines-with-no-clues "full-line")))
    (dotimes (i (line-count ng))
      (let ((cur-line (elt (lines ng) i)))
        (when (zerop (get-clue-count-for-line cur-line))
          (dotimes (j (get-line-length ng cur-line))
            (mark-as-x-at-i-when-unknown ng cur-line (empty-value ng) j calling-function))
          (setf (line-complete-p cur-line) t))))))

(defmethod basic-check-for-finished-line ((ng nonogram) (line-obj line))
  "Sets the LINE class slot value LINE-COMPLETE-P to T when a line has been completely finished, meaning every index in line has value is FILLED-VALUE or EMPTY-VALUE, such that the line has no UNKNOWN-VALUE remaining. Note that this method does not verify correctness of the line in any way, and merely verifies the absence of UNKNOWN-VALUEs."
  (let ((no-unknowns-p t)) ;; ! it is assumed that no unknowns exist in line until one is encountered
    (dotimes (i (get-line-length ng line-obj))
      (when (index-is-unknown-p ng line-obj i)
        (setq no-unknowns-p nil)
        (return))) ;; end loop early when UNKNOWN-VALUE is encountered
    (when no-unknowns-p
      (setf (line-complete-p line-obj) t))))

(defmethod find-overlap ((ng nonogram) (line-obj line) line-assessment-type)
  "Marks the appropriate subsequence of the line, placing only FILLED-VALUEs, according to the minimal necessary overlap of the related clues. This method takes a mathematical approach to finding this overlap; it uses an integer called OVERLAP-THRESHOLD, which can be calculated by adding all clues of the line and the sum of the count of the same clues minus one, and then subtracting that from the overall length of the line; every clue greater than this threshold value must have an overlap of clue-value minus OVERLAP-THRESHOLD, whereas any clue equal to this threshold has a full overlap. When a clue has an overlap, the method proceeds to find where the overlap occurs by index; it does so by first calculating the \"furthest-index\", which is the sum of the starting index and cumulative value of all clue-values up to and including current-clue from the same edge minus 1 (because of number vs index), while also adding 1 for each empty that is needed to provide minimal separation between clues; then the method steps back from this furthest-index, according to the size of the overlap, marking all unknowns as filled. LINE-ASSESSMENT-TYPE determines how the line is considered, and must be one of the following: +FULL-LINE+, +SHORTENED-LINE+, or +LINE-SUBSECTIONS+.
e.g. with +FULL-LINE+, |2 2| ----- becomes 11-11
e.g. with +FULL-LINE+, |2 1| ----- becomes -1---
e.g. with +FULL-LINE+, |3| ----- becomes --1--
e.g. with +FULL-LINE+, |4| ----- becomes -111-
e.g. with +FULL-LINE+, |1 2 4| ---------- becomes ---1--111-
e.g. with +SHORTENED-LINE+, |1 2 3| 100---0111 is considered as |2| --- and is updated to |2| -1-, yielding |1 2 3| 100-1-0111
e.g. with +LINE-SUBSECTIONS+, |1 2 2| 10--0-0--- is considered as |2| -- and |2| --- (ignoring the central group), and are updated to |2| 11 and |2| -1-, yielding |1 2 2| 10110-0-1-"
  ;; preparing appropriate variables so as to apply the technique to the appropriate part of the line
  (proceed-according-to-line-assessment-type (clues clue-count line-range calling-function 'find-overlap)
    (let (;;                                 sequence-size                     fills       empties-between
          (overlap-threshold (- (- (cdr line-range) (car line-range)) (+ (reduce '+ clues) (1- clue-count)))) ;; ! every clue greater than OVERLAP-THRESHOLD has overlap of: clue MINUS overlap-threshold
          (running-sum 0)) ;; holds the current running-sum of clues for the loop below
      ;; attempting to mark for each clue sequentially
      (dotimes (i clue-count)
        (let ((cur-clue (elt clues i)))
          (incf running-sum cur-clue)
          (let ((cur-clue-overlap (- cur-clue overlap-threshold)))
            (when (>= cur-clue-overlap 1) ;; when current clue has an overlap
              (let ((furthest-overlap-index (+ (car line-range) running-sum i -1))) ;; combining line-starting-position with total-fills-so-far and number-of-empties-so-far, removing 1 to correct index
                ;; the below DO loop iteratively steps back one position from FURTHEST-OVERLAP-INDEX, CUR-CLUE-OVERLAP times, marking each as filled (if not already filled)
                (do ((index furthest-overlap-index (- index 1))
                     (end-index (- furthest-overlap-index cur-clue-overlap)))
                    ((<= index end-index))
                  (let ((coord nil)) ;; ! COORD must be set according to the line's orientation
                    (if (line-is-vertical-p ng line-obj)
                        (setq coord (cons index (index-from-edge line-obj)))
                        (setq coord (cons (index-from-edge line-obj) index)))
                    (mark-as-x-at-coord-when-unknown ng line-obj (filled-value ng) coord calling-function)))))))))))

(defmethod find-initial-overlap ((ng nonogram))
  "Calls the method FIND-OVERLAP-FOR-LINE against every line. Note that this is an \"initial\" technique, and is only meant to be used once at the initial assessment of the nonogram."
  (dotimes (i (line-count ng))
    (let ((cur-line (elt (lines ng) i)))
      (find-overlap ng cur-line +full-line+))))

(defmethod mark-as-filled-based-on-edge-proximity ((ng nonogram) (line-obj line) line-assessment-type)
  "Marks the appropriate subsequence of the line, placing only FILLED-VALUEs, when there is a clue so close to an edge that its FILLED-VALUEs must necessarily extend towards the opposite edge, due to the fact that extending only towards the first edge would not allow for the full clue-length; note that such an assessment can only yield marks when the edge-adjacent clue is greater than 1. LINE-ASSESSMENT-TYPE determines how the line is considered, and must be one of the following: +FULL-LINE+, +SHORTENED-LINE+, or +LINE-SUBSECTIONS+.
e.g. with +FULL-LINE+, |1 3| ------1- becomes -----11-
e.g. with +FULL-LINE+, |2 5| 1-------1-- becomes 11----111--
e.g. with +SHORTENED-LINE+, |1 3 1| ------1-01 is considered as |1 3| ------1- and is updated to |1 3| -----11-, yielding |1 3 1| -----11-01
e.g. with +LINE-SUBSECTIONS+, |3 3| -----10-1---- is considered as |3| -----1 and |3| -1----, and are updated to |3| ---111  and |3| -11---, yielding |3 3| ---1110-11---"
  ;; preparing appropriate variables so as to apply the technique to the appropriate part of the line
  (proceed-according-to-line-assessment-type (clues clue-count line-range calling-function 'mark-as-filled-based-on-edge-proximity)
    ;; assesses line-segment from left-to-right and then right-to-left, filling for clues whose proximity to edge gives away fill locations
    (do ((iteration-count 0 (1+ iteration-count)) ;; will iterate twice; once for each direction
         (direction 1 -1) ;; left-to-right, and then right-to-left
         (clue (elt clues 0) (elt clues (1- clue-count)))
         (start-index (car line-range) (1- (cdr line-range)))
         (filled-found-p nil nil)) ;; this is a boolean that indicates, when T that a FILLED-VALUE was found, and when NIL that one has not yet been encountered
        ((>= iteration-count 2))
      ;; moves toward center from edge of line-segment i spaces, where i is the considered clue's value, and upon encountering first fill continues stepping the remaining i spaces (if any) marking all encountered unknowns as fills
      (dotimes (i clue)
        (let ((coord (get-coord-from-index-on-line ng line-obj (+ start-index (* i direction)))))
          (if filled-found-p
              (mark-as-x-at-coord-when-unknown ng line-obj (filled-value ng) coord calling-function)
              (when (coord-is-filled-p ng coord) ;; when COORD has FILLED-VALUE (and a filled was not previously found)
                (setq filled-found-p t))))))))

(defmethod mark-as-empty-based-on-edge-proximity ((ng nonogram) (line-obj line) line-assessment-type)
  "Marks the passed line-segment, placing only EMPTY-VALUEs, when there is a group of one or more FILLED-VALUEs already placed in the segment such that a quantity of FILLED-VALUE(s) equal to the related edge-adjacent clue could not (with an empty added) fit between the group of FILLED-VALUE(s) and the associated edge, meaning that one or more UNKNOWN-VALUEs from the far end of same edge must be EMPTY-VALUE(s), depending on the size of the same group of FILLED-VALUE(s) already present; in other words, determines whether the group of FILLED-VALUEs found near to an edge are associated with the clue on the same edge, marking as empty necessarily empty positions as indicated by the size of the edge-clue. LINE-ASSESSMENT-TYPE determines how the line is considered, and must be one of the following: +FULL-LINE+, +SHORTENED-LINE+, or +LINE-SUBSECTIONS+.
e.g. with +FULL-LINE+, |2 1| --1--1- yields |2 1| 0-1--10
e.g. with +FULL-LINE+, |4 2| ----111-----1-- yields |4 2| 000-111-----1-0
e.g. with +FULL-LINE+, |5| ---1111-- yields |5| 00-1111-0
e.g. with +FULL-LINE+, |5 1| -0---1111---1 yields |5 1| 0000-1111---1
e.g. with +SHORTENED-LINE+, |3 1| ---11--01 is considered as |3 1| ---11-- and is updated to |3 1| 00-11-0, yielding |3 1| 00-11-001
e.g. with +LINE-SUBSECTIONS+, |3 4| -11--0---111- is considered as |3| -11-- and |4| ---111-, and are updated to |3| -11-0 and |4| 00-111-, yielding |3 4| -11-0000-111-"
  ;; preparing appropriate variables so as to apply the technique to the appropriate part of the line
  (proceed-according-to-line-assessment-type (clues clue-count line-range calling-function 'mark-as-empty-based-on-edge-proximity)
    ;; assesses a line-segment from left-to-right and then from right-to-left, marking any "empties" based on clues whose proximity to an edge implies empty locations
    (do ((iteration-count 0 (1+ iteration-count)) ;; will iterate twice; once for each direction
         (direction 1 -1) ;; left-to-right, and then right-to-left
         (clue (elt clues 0) (elt clues (1- clue-count)))
         (edge-index (car line-range) (1- (cdr line-range))))
        ((>= iteration-count 2))
      ;; iterates from the edge towards its opposite side, tracking "current" position, and looking ahead CLUE spaces, tracking "checked" position ...
      ;; ... stopping as soon as current position is filled or checked position is not filled; otherwise, the looping continues and the current position is marked as empty
      (do ((i 0 (1+ i)))
          (nil)
        (let* ((current-index (+ edge-index (* i direction))) ;; this starts at edge related to EDGE-INDEX and moves toward opposite side, 1 step at a time
               (checked-index (+ current-index (* clue direction)))) ;; this is at CLUE-value spaces beyond current-index towards opposite side
          (unless (within-constraints-of-index-range-p checked-index line-range) ;; ! only CHECKED-INDEX needs to be verified since it is further along than CURRENT-INDEX
            (return))
          (let ((current-coord (get-coord-from-index-on-line ng line-obj current-index))
                (checked-coord (get-coord-from-index-on-line ng line-obj checked-index)))
            (if (or (coord-is-filled-p ng current-coord)
                    (not (coord-is-filled-p ng checked-coord)))
                (return)
                (mark-as-x-at-coord-when-unknown ng line-obj (empty-value ng) current-coord calling-function))))))))

(defmethod mark-for-edgemost-clue-when-delimited-maximally-close-to-edge ((ng nonogram) (line-obj line) line-assessment-type)
  "Marks the passed line-segment, placing only FILLED-VALUES, when an edgemost clue is represented by at least one FILLED-VALUE such that the furthest filled is at a count from edge that is equal to CLUE, and the filled is capped at its furthest end by an empty. LINE-ASSESSMENT-TYPE determines how the line is considered, and must be one of the following: +FULL-LINE+ or +SHORTENED-LINE+ (note that +LINE-SUBSECTIONS+ is not allowed for this method). Note that this method is closely related to the MARK-AS-*-BASED-ON-EDGE-PROXIMITY methods, and should typically follow them for best utility.
e.g. with +FULL-LINE+, |2 1 1| -10------- becomes |2 1 1| 110-------
e.g. with +FULL-LINE+, |3 5| --10---01-1-- becomes |3 5| 1110---011111
e.g. note that MARK-AS-FILLED-BASED-ON-EDGE-PROXIMITY should be used to prep the following line for this method: |3 1| -1-0---- giving |3 1| -110---- ...
... thus, with +FULL-LINE+ |3 1| -110---- becomes |3 1| 1110----
e.g. note that MARK-AS-EMPTY-BASED-ON-EDGE-PROXIMITY should be used to prep the following line for this method: |2 1 1| --10-------, yielding |2 1 1| 0-10------- ...
... thus, with +SHORTENED-LINE+, |2 1 1| 0-10------- is considered as |2 1 1| -10------- and is updated to |2 1 1| 110-------, yielding |2 1 1| 0110-------"
  (when (= line-assessment-type +line-subsections+)
    (error "MARK-FOR-EDGEMOST-CLUE-WHEN-DELIMITED-MAXIMALLY-CLOSE-TO-EDGE is disallowed from working with line-subsections, because this technique relies on the presence of EMPTY-VALUEs in the line."))
  ;; preparing appropriate variables so as to apply the technique to the appropriate part of the line
  (proceed-according-to-line-assessment-type (clues clue-count line-range calling-function 'mark-for-edgemost-clue-when-delimited-maximally-close-to-edge)
    ;; assesses the line-segment from left-to-right and then right-to-left, looking for a specific circumstance where a clue is represented by at least one FILLED-VALUE ...
    ;; ... such that the furthest filled is at a count from edge that is equal to CLUE, and the filled is capped at its furthest end by an empty, like the 2 in |2 1| -10---
    (do ((iteration-count 0 (1+ iteration-count)) ;; will iterate twice; once for each direction
         (direction 1 -1) ;; left-to-right, and then right-to-left
         (clue (elt clues 0) (elt clues (1- clue-count)))
         (edge-index (car line-range) (1- (cdr line-range))))
        ((>= iteration-count 2))
      (when (> clue 1) ;; ! the clue must necessarily be 2 or higher for anything to happen
        (let* ((empty-index (+ edge-index (* direction clue)))
               (filled-index (- empty-index direction)))
          (when (and (within-constraints-of-index-range-p empty-index line-range)
                     (index-is-empty-p ng line-obj empty-index)
                     (index-is-filled-p ng line-obj filled-index))
            ;; stepping back towards edge, marking any unknowns as filled
            (let ((reversed-direction (* direction -1)))
              (do ((step-count 1 (1+ step-count))
                   (i (+ filled-index reversed-direction) (+ i reversed-direction)))
                  ((>= step-count clue))
                (mark-as-x-at-i-when-unknown ng line-obj (filled-value ng) i calling-function)))))))))

(defmethod bridge-empties-from-edge ((ng nonogram) (line-obj line) line-assessment-type)
  "Marks the passed line-segment, placing only EMPTY-VALUES, when there is a grouping of consecutive UNKNOWN-VALUEs near an edge where the related edge-adjacent clue cannot fit into that same section. LINE-ASSESSMENT-TYPE determines how the line is considered, and must be one of the following: +FULL-LINE+ or +SHORTENED-LINE+ (note that +LINE-SUBSECTIONS+ is not allowed for this method).
e.g. with +FULL-LINE+, |2 3| -0--------0-- yields 00--------000
e.g. with +FULL-LINE+, |3| --0-0------ yields 000-0------; note that the next application of this technique would make further changes
e.g. with +SHORTENED-LINE+, |2 1| ----0-01 is considered as |2| ----0- and is updated to |2| ----00 yielding |2 1| ----0001"
  (when (= line-assessment-type +line-subsections+)
    (error "BRIDGE-EMPTIES-FROM-EDGE is disallowed from working with line-subsections, because this technique relies on the presence of EMPTY-VALUEs in the line."))
  ;; preparing appropriate variables so as to apply the technique to the appropriate part of the line
  (proceed-according-to-line-assessment-type (clues clue-count line-range calling-function 'bridge-empties-from-edge)
    ;; assesses the line-segment from left-to-right and then right-to-left, marking as empty any edge-adjacent unknown groupings not large enough to accommodate edge-clue
    (do ((iteration-count 0 (1+ iteration-count)) ;; will iterate twice; once for each direction
         (direction 1 -1) ;; left-to-right, and then right-to-left
         (clue (elt clues 0) (elt clues (1- clue-count)))
         (edge-index (car line-range) (1- (cdr line-range)))
         (unknown-count 0 0))
        ((>= iteration-count 2))
      ;; steps 1 step at a time from edge tracking consecutive unknowns so as to mark them as empty when the edge clue won't fit
      ;; ! note that this loop only iterates as long as unknowns are found at COORD; as soon as anything else is found, then iteration will end
      (do ((i edge-index (+ i direction))
           (coord nil nil))
          (nil)
        (setq coord (get-coord-from-index-on-line ng line-obj i))
        (cond
          ((coord-is-unknown-p ng coord)
           (incf unknown-count)
           (when (>= unknown-count clue)
             (return))) ;; stop assessing in this direction because clue could fit in this section
          ((coord-is-empty-p ng coord)
           ;; ! note that here unknown-count may be zero; in which case, the below loop body will not execute
           (do ((second-counter 0 (1+ second-counter))
                (j edge-index (+ j direction)))
               ((>= second-counter unknown-count))
             (mark-as-x-at-i-when-unknown ng line-obj (empty-value ng) j calling-function)) ;; ! using the *-WHEN-UNKNOWN method here is needless, but done for caution's sake
           (return)) ;; stop assessing in this direction because an empty was encountered
          (t ;; if it is filled-value or (potentially) something else...
           (return)))))))

(defmethod empty-before-or-at-line-coord-p ((ng nonogram) (line-obj line) i direction line-range &key (at nil))
  "Returns T when there is an EMPTY-VALUE (or edge) at or before (depending on the key AT) the passed index (I), based on DIRECTION; otherwise returns NIL. I is expected to be an integer and index for the passed line. DIRECTION is expected to be an integer, and should be 1 for left-to-right and -1 for right-to-left consideration. LINE-RANGE is the range of the line to be considered, and is expected to follow the specification set by the function CHECK-RANGE. When AT is T, the position at I is checked; when AT is NIL, the position before I (by DIRECTION) is checked. Note that this method is used exclusively by the method MARK-FOR-REPEATING-CLUES-AT-EDGE."
  (let ((index (if at i (- i direction))))
    (if (or (= index (1- (car line-range)))
            (= index (cdr line-range)))
        t ;; returning T because an edge was found
        (if (index-is-empty-p ng line-obj index)
            t ;; returning T because an empty was found
            nil)))) ;; returning NIL because neither an empty nor an edge was found

;; !!!!!!!!!!!!! make a variation that does things with zone "separation"
;; ... such that it could resolve the following
;; |2 2 1| -1---1----
;; |2 2 1| ******----
;; |2 2 1| -1-0-1----
;; !!! and even the following
;; |2 3 1| -1----1-----
;; |2 3 1| *******-----
;; |2 3 1| -1-0--1-----
;; |3 3 1| --1----11----
;;         ********
;;         --1--0-11----
;; |3 4 1| --1-----11-----
;;         *********
;;         --1--0--11-----
;; |3 4 1| --1-----111----
;;         *********
;;         --1--00-111----
(defmethod mark-for-repeating-clues-at-edge ((ng nonogram) (line-obj line) line-assessment-type)
  "Where there are clues that occur repeatedly from either edge of the appropriate line subsequence, marks as appropriate around any FILLED-VALUE(s) that are already present (if any) within a specified range from same edge when a EMPTY-VALUE stands directly adjacent to one such FILLED-VALUE; note that this range is the smallest range in which all repeating clues could fit if they were to be maximally edge adjacent; also note that because only repeating clues are considered, then any FILLED-VALUE(s) found in that specified range must be part of the same clue-size. LINE-ASSESSMENT-TYPE determines how the line is considered, and must be one of the following: +FULL-LINE+ or +SHORTENED-LINE+ (note that +LINE-SUBSECTIONS+ is not allowed for this method).
e.g. with +FULL-LINE+, |2 2 1| -1--01---- the zone is ******---- and the result is |2 2 1| -1--0110--
e.g. with +FULL-LINE+, |2 2 1| -1---10--- the zone is ******---- and the result is |2 2 1| -1-0110---
e.g. with +FULL-LINE+, |1 3 3| ----------01--- the zone is -------******** and the result is |1 3 3| ----------01110
e.g. with +FULL-LINE+, |3 3 1| -----111------- the zone is ********------- and the result is |3 3 1| ----01110------
e.g. with +FULL-LINE+, |3 3 1| -------111----- the zone is ********------- and the result is |3 3 1| ------01110---- (note that here only one FILLED-VALUE is in the zone)
e.g. with +SHORTENED-LINE+, |1 2 2 1| ---01----01 is considered as |1 2 2| ---01---- (zone is --******) and is updated to |1 2 2| ---0110-- yielding |1 2 2 1| ---0110--01"
  (when (= line-assessment-type +line-subsections+)
    (error "MARK-FOR-REPEATING-CLUES-AT-EDGE is disallowed from working with line-subsections, because this technique relies on the presence of EMPTY-VALUEs in the line."))
  ;; preparing appropriate variables so as to apply the technique to the appropriate part of the line
  (proceed-according-to-line-assessment-type (clues clue-count line-range calling-function 'mark-for-repeating-clues-at-edge)
    (when (> clue-count 1)
      ;; assesses a line-segment from left-to-right and then right-to-left, keeping track of a "zone" where, if any FILLED-VALUEs occur, then such FILLED-VALUES must be ...
      ;; ... certainly related to the repeating clues from the edge; however, if there are no repeating clues from the edge then this method will do nothing
      ;; e.g. for the line |2 2 1| ---------- the 2s are the repeating clues and the zone is marked with *s |2 2 1| ******----; however, this does not mean that the 2s ...
      ;; ... will be there, but only means that if a FILLED-VALUE is found within that zone, then it must be part of a 2-clue by the simple fact of edge proximity ...
      ;; ... since the 2s must fill out this whole zone if, and only if, they are to be found within it at all (as in, 110110 or 110011 or 011011)
      ;; ! the zone size can be numerically calculated thus: repeat-count * edge-clue + repeat-count... yielding (as in the above example) 2*2+2 = 6
      (do ((iteration-count 0 (1+ iteration-count)) ;; will iterate twice; once for each direction
           (direction 1 -1) ;; left-to-right, and then right-to-left
           (edge-clue (elt clues 0) (elt clues (1- clue-count)))
           (start-index (car line-range) (1- (cdr line-range)))
           (unknown-count 0 0)
           (second-clue-index 1 (- clue-count 2))
           (repeat-count 1 1) ;; this indicates the number of repeating clues (that have exact same value) from edge
           (zone-size nil nil) ;; this will be set to an integer according to the updated value of REPEAT-COUNT (unless there are no repeating clues)
           (filled-group-ranges nil nil)) ;; this is a list of ranges, where each range indicates the placement of consecutive FILLED-VALUEs
          ((>= iteration-count 2))
        ;; collecting appropriate zone data first (setting REPEAT-COUNT and ZONE-SIZE to their proper values)
        (do ((i second-clue-index (+ i direction)))
            ((or (< i 0) (>= i clue-count) zone-size)) ;; end loop if i is out of bounds OR if ZONE-SIZE is set to non-nil value
          (if (= edge-clue (elt clues i))
              (incf repeat-count)
              (setq zone-size (+ (* repeat-count edge-clue) repeat-count))))
        (when (and zone-size (>= repeat-count 2)) ;; ! zone size must be set and there must be at least 2 repeating clues from edge
          ;; moving through zone, one index at a time, finding all start- and end-indexes of separate FILLED-VALUE groupings, saving each range to FILLED-GROUP-RANGES
          ;; ! note that this loop's iteration count is not solely restricted by the "zone" and its iteration will step outside the zone when any FILLED-VALUE found within the zone extends outside of the zone
          (do ((zone-size-counter 0 (1+ zone-size-counter))
               (i start-index (+ i direction))
               (start-i nil)
               (coord nil nil)
               (last-was-filled-p nil))
              ((or (not (within-constraints-of-index-range-p i line-range)) (and (not last-was-filled-p) (>= zone-size-counter zone-size))) (when start-i (push (cons start-i i) filled-group-ranges)))
            (setq coord (get-coord-from-index-on-line ng line-obj i))
            (setq last-was-filled-p (coord-is-filled-p ng coord))
            (if last-was-filled-p
                (unless start-i ;; THEN, when START-I is not already set, set it
                  (setq start-i i))
                (when start-i ;; ELSE, when START-I is set, save the range
                  (push (cons start-i i) filled-group-ranges)
                  (setq start-i nil))))
          ;; checking around groups of FILLED-VALUEs for empties, checking one group at a time (in the reversed order), making marks as appropriate
          (dolist (filled-range filled-group-ranges)
            (let* ((start-i (car filled-range))
                   (end-i (cdr filled-range))
                   (filled-count (abs (- end-i start-i)))
                   (remaining-filled (- edge-clue filled-count)))
              (cond
                ;; if there is an empty (or edge) before START-I
                ((empty-before-or-at-line-coord-p ng line-obj start-i direction line-range)
                 ;; moving one step at a time, REMAINING-FILLED times, towards END-I, starting after last FILLED-VALUE by DIRECTION, marking all unknowns as filled ...
                 ;; ... and placing an empty afterwards if one is not already placed
                 (do ((iter-count 0 (1+ iter-count))
                      (i (+ start-i (* filled-count direction)) (+ i direction)))
                     ((>= iter-count remaining-filled) (mark-as-x-at-i-when-unknown-for-range ng line-obj (empty-value ng) i line-range calling-function))
                   (mark-as-x-at-i-when-unknown-for-range ng line-obj (filled-value ng) i line-range calling-function)))
                ;; if there is an empty (or edge) at END-I (because END-I is part of a non-inclusive range)
                ((empty-before-or-at-line-coord-p ng line-obj end-i direction line-range :at t)
                 ;; moving one step at a time, REMAINING-FILLED times, towards START-I, starting after last FILLED-VALUE by DIRECTION, marking all unknowns as filled ...
                 ;; ... and placing an empty afterwards if one is not already placed
                 (do ((iter-count 0 (1+ iter-count))
                      (i (- end-i (* (1+ filled-count) direction)) (- i direction)))
                     ((>= iter-count remaining-filled) (mark-as-x-at-i-when-unknown-for-range ng line-obj (empty-value ng) i line-range calling-function))
                   (mark-as-x-at-i-when-unknown-for-range ng line-obj (filled-value ng) i line-range calling-function)))
                ;; if the clue is finished, but has not been surrounded by empties on either side...
                ((= remaining-filled 0)
                 ;; marking as empty the positions at sides of FILLED-VALUEs that fully represent the clue within "zone"
                 (do ((iter-count 0 (1+ iter-count))
                      (i (- start-i direction) end-i)
                      (coord nil nil))
                     ((>= iter-count 2))
                   (mark-as-x-at-i-when-unknown-for-range ng line-obj (empty-value ng) i line-range calling-function)))))))))))

(defmethod get-index-of-first-filled-from-left-or-right ((ng nonogram) (line-obj line) line-range &key (from-left t))
  "Returns the index of the first FILLED-VALUE encountered in the line, within the bounds set by LINE-RANGE, from left or from right (depending on value of key FROM-LEFT); however, when no FILLED-VALUE is encountered, NIL is returned. LINE-RANGE is the range of the line to be considered, and is expected to follow the specification set by the function CHECK-RANGE. FROM-LEFT, when T, causes the method to assess the line from left-to-right; but, when NIL, from right-to-left."
  (let ((direction (if from-left 1 -1))
        (starting-index (if from-left (car line-range) (1- (cdr line-range))))
        (line-subsequence-length (- (cdr line-range) (car line-range)))
        (return-value nil))
    (do ((iteration-count 0 (1+ iteration-count))
         (i starting-index (+ i direction)))
        ((>= iteration-count line-subsequence-length))
      (when (index-is-filled-p ng line-obj i)
        (setq return-value i)
        (return))) ;; ending loop early upon finding a FILLED-VALUE
    return-value))

(defmethod get-count-of-consecutive-filled-moving-right-or-left-from-index ((ng nonogram) (line-obj line) index line-range &key (moving-right t))
  "Returns the count of consecutive FILLED-VALUEs encountered starting at index I and moving right or left (depending on key MOVING-RIGHT) while staying within the bounds of LINE-RANGE. I must be an integer, and is expected to be valid for the line and within the bounds of LINE-RANGE. LINE-RANGE is the range of the line to be considered, and is expected to follow the specification set by the function CHECK-RANGE. MOVING-RIGHT, when T, causes the method to check positions incrementally to the right of the starting point; when NIL, causes the method to check positions incrementally to the left of the starting point."
  (let ((direction (if moving-right 1 -1))
        (filled-count 0))
    (do ((i index (+ i direction)))
        ((not (within-constraints-of-index-range-p i line-range)))
      (if (index-is-filled-p ng line-obj i)
          (incf filled-count)
          (return))) ;; stop iterating when index is not filled
    filled-count))

;; !!! could this possibly be generalized while somehow combined with mark-around-unjoinable-clues to make a single method that might be called mark-around-clues !!??
;; ... thereby creating a new method that flexibly looks at the clue count of the line to address all possible clue-counts
(defmethod mark-around-singular-clue ((ng nonogram) (line-obj line) line-assessment-type)
  "Marks a line-segment that has only one remaining clue so long as at least one FILLED-VALUE is already present in segment, bridging FILLED-VALUEs when possible, and placing EMPTY-VALUEs according to the reach of the clue. LINE-ASSESSMENT-TYPE determines how the line is considered, and must be one of the following: +FULL-LINE+, +SHORTENED-LINE+, or +LINE-SUBSECTIONS+. Note that although this technique may essentially complete a line, it does not mark the line as complete except through UPDATE-* methods.
e.g. with +FULL-LINE+, |2| ---1---- becomes 00-1-000
e.g. with +FULL-LINE+, |1| -0-1-- becomes 000100
e.g. with +FULL-LINE+, |5| ------1111------- becomes |5| 00000-1111-000000
e.g. with +FULL-LINE+, |6| --------1--1------- becomes |6| 000000--1111--00000
e.g. with +SHORTENED-LINE+, |2 3| 0110--1----- is considered as |3| --1----- and is updated to |3| --1--000 yielding |2 3| 0110--1--000
e.g. with +LINE-SUBSECTIONS+, |5 4| --1-1---0----- is considered as |5| --1-1--- and |4| ----- and only the former is updated, to |5| --111--0 yielding |5 4| --111--00-----"
  ;; preparing appropriate variables so as to apply the technique to the appropriate part of the line
  (proceed-according-to-line-assessment-type (clues clue-count line-range calling-function 'mark-around-singular-clue)
    (when (= clue-count 1) ;; ! this technique will only do things when there is 1 clue in the line subsequence
      (let ((left-i (get-index-of-first-filled-from-left-or-right ng line-obj line-range :from-left t)))
        (when left-i ;; ! when there is at least one FILLED-VALUE in line
          (let ((right-i (get-index-of-first-filled-from-left-or-right ng line-obj line-range :from-left nil)))
            (when (/= left-i right-i)
              ;; bridge the FILLED-VALUEs if necessary
              (do ((i (1+ left-i) (1+ i)))
                  ((>= i right-i))
                (mark-as-x-at-i-when-unknown ng line-obj (filled-value ng) i calling-function)))
            (let* ((filled-group-size (1+ (- right-i left-i)))
                   (filled-needed (- (elt clues 0) filled-group-size)))
              ;; looking left of LEFT-I to mark empties according to reach of clue
              (do ((i (- left-i 1 filled-needed) (1- i)))
                  ((< i (car line-range)))
                (mark-as-x-at-i-when-unknown ng line-obj (empty-value ng) i calling-function))
              ;; looking right of RIGHT-I to mark empties according to reach of clue
              (do ((i (+ right-i 1 filled-needed) (1+ i)))
                  ((>= i (cdr line-range)))
                (mark-as-x-at-i-when-unknown ng line-obj (empty-value ng) i calling-function)))))))))

;; !!!!!!!!!! this technique could probably be generalized to work with zone groupings or something like that
;; ... such that |2 3 4 5| ---------------------------- could be considered 3 times as |2 3| then |3 4| then |4 5|, or at least twice from edges as |2 3| and then |4 5|
;; ... as another possibility, some variation on this technique could be combined somehow with a broader variation on the method mark-for-repeating-clues-at-edge
(defmethod mark-around-unjoinable-clues ((ng nonogram) (line-obj line) line-assessment-type)
  "Marks a line-segment that has only two remaining clues where each clue is represented in the line by a group of one or more FILLED-VALUEs such that the proximity of the different groupings in relation to each other indicates the presence of FILLED-VALUEs and/or EMPTY-VALUEs in the line-segment. LINE-ASSESSMENT-TYPE determines how the line is considered, and must be one of the following: +FULL-LINE+, +SHORTENED-LINE+, or +LINE-SUBSECTIONS+. Note that although this technique may essentially complete a line, it does not mark the line as complete except through UPDATE-* methods.
e.g. with +FULL-LINE+, |3 2| ----1--1--- becomes |3 2| 00-11--1-00
e.g. with +FULL-LINE+, |2 3| ---1----1--- becomes |2 3| 00-1-0--1--0
e.g. with +FULL-LINE+, |2 2| ---1-1--- becomes |2 2| 001101100
e.g. with +FULL-LINE+, |3 2| ---11-1--- becomes |3 2| 0011101100
e.g. with +FULL-LINE+, |6 3| ----1---1-----1-1--- becomes |6 3| ----1---1-00001-1--0
e.g. with +SHORTENED-LINE+, |3 1 1| ----1--1--01 is considered as |3 1| ----1--1-- and is updated to |3 1| 00-11-0100 yielding |3 1 1| 00-11-010001
e.g. with +LINE-SUBSECTIONS+, |2 2 3| 0-1--1--0----- is considered as |2 2| -1--1-- and |3| ----- and only the former is updated, to |2 2| -1--1-0 yielding |2 2 3| 0-1--1-00-----"
  ;; preparing appropriate variables so as to apply the technique to the appropriate part of the line
  (proceed-according-to-line-assessment-type (clues clue-count line-range calling-function 'mark-around-unjoinable-clues)
    (when (= clue-count 2) ;; ! this technique will only do things when there are 2 clues in the line subsequence
      (let ((left-i (get-index-of-first-filled-from-left-or-right ng line-obj line-range :from-left t)))
        (when left-i ;; ! when there is at least one FILLED-VALUE in line
          (let ((right-i (get-index-of-first-filled-from-left-or-right ng line-obj line-range :from-left nil))
                (left-clue (elt clues 0))
                (right-clue (elt clues 1))
                (max-clue (reduce 'max clues)))
            ;; ! note that if LEFT-I and RIGHT-I have the same value, then their difference must be 0, whereas a clue cannot be of value 0
            (when (>= (- right-i left-i) max-clue) ;; when both indexes are set and distinct, and the difference of indexes is >= the largest clue
              ;; ! when RIGHT-I minus LEFT-I is >= MAX-CLUE then the edge-most FILLED-VALUEs must be unjoinable because connecting them would cause the line to represent a clue that is not part of the line's clues
              ;; ... for example, |2 3| -1--1---- has index 1 at left and index 4 at right, giving 4-1=3. 3 is >= the largest clue (3), thus they are unjoinable ...
              ;; ... but if they were joined, this is the result: |2 3| -1111----, which is clearly an invalid line; thus, these clue groups must have at least one ...
              ;; ... EMPTY-VALUE between them, although in this example it cannot be determine how many or where (e.g. -10-1---- or -1-01---- or -1001----)
              (let* ((left-group-size (get-count-of-consecutive-filled-moving-right-or-left-from-index ng line-obj left-i line-range :moving-right t))
                     (inner-left-i (+ left-i (1- left-group-size)))
                     (needed-in-left-group (- left-clue left-group-size))
                     (right-group-size (get-count-of-consecutive-filled-moving-right-or-left-from-index ng line-obj right-i line-range :moving-right nil))
                     (inner-right-i (- right-i (1- right-group-size)))
                     (needed-in-right-group (- right-clue right-group-size))
                     (inner-i-diff (- inner-right-i inner-left-i))
                     (usable-space-in-center (- inner-i-diff 2))) ;; ! note that this number assumes a necessary EMPTY-VALUE between
                ;; extending the FILLED-VALUE groups if possible when there is maximal closeness for clues
                ;; ! when there is maximal closeness, a FILLED can be marked on side of largest clue (or both clues if they are same) although center may be uncertain
                ;; ... e.g. the following lines have maximal closeness: |2 2| -*1-1*-, |2 2| -*1-11-, and |2 4| --1---1*---, such that the asterisks must be FILLED-VALUE
                (when (and (<= inner-i-diff max-clue)) ;; ! when still only possibly maximally close... 
                  ;; attending to left side
                  (when (and (or (= left-clue max-clue) (= 2 (- inner-right-i inner-left-i))) ;; when LEFT-CLUE is largest or clue-groups are only separated by 1 space
                             (< left-group-size left-clue)                                    ;; and LEFT-CLUE is not fully represented
                             (> needed-in-left-group usable-space-in-center))                 ;; and there is not enough space in center to accommodate full clue
                    (let ((further-index (1- left-i)))
                      (when (and (within-constraints-of-index-range-p further-index line-range) (index-is-unknown-p ng line-obj further-index))
                        (mark-as-x-at-i ng line-obj (filled-value ng) further-index calling-function)
                        (decf left-i) ;; moving LEFT-I further to left via decrementing
                        (incf left-group-size))))
                  ;; attending to right side
                  (when (and (or (= right-clue max-clue) (= 2 (- inner-right-i inner-left-i))) ;; when RIGHT-CLUE is largest or clue-groups are only separated by 1 space
                             (< right-group-size right-clue)                                   ;; and RIGHT-CLUE is not fully represented
                             (> needed-in-right-group usable-space-in-center))                 ;; and there is not enough space in center to accommodate full clue
                    (let ((further-index (1+ right-i)))
                      (when (and (within-constraints-of-index-range-p further-index line-range) (index-is-unknown-p ng line-obj further-index))
                        (mark-as-x-at-i ng line-obj (filled-value ng) further-index calling-function)
                        (incf right-i) ;; moving RIGHT-I further to right via incrementing
                        (incf right-group-size)))))
                ;; looking left of left-clue-group to mark empties according to reach of left-clue
                ;; e.g. |2 2| ---1-1--- becomes |2 2| 00-1-1---
                (do ((i (- left-i 1 (- left-clue left-group-size)) (1- i)))
                    ((< i (car line-range)))
                  (mark-as-x-at-i-when-unknown ng line-obj (empty-value ng) i calling-function))
                ;; looking right of right-clue-group to mark empties according to reach of right-clue
                ;; e.g. |2 2| ---1-1--- becomes |2 2| ---1-1-00
                (do ((i (+ right-i 1 (- right-clue right-group-size)) (1+ i)))
                    ((>= i (cdr line-range)))
                  (mark-as-x-at-i-when-unknown ng line-obj (empty-value ng) i calling-function))
                ;; assessing the section between clues
                (let ((inner-range (cons (1+ (+ inner-left-i (- left-clue left-group-size))) (- inner-right-i (- right-clue right-group-size)))))
                  (do ((i (car inner-range) (1+ i)))
                      ((not (within-constraints-of-index-range-p i inner-range)))
                    (mark-as-x-at-i-when-unknown ng line-obj (empty-value ng) i calling-function)))))))))))

;;; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! shouldn't this simply be a functionality of line-subsections??? !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;;; !!!  notes for unfinished method...
;;; move through line from each side checking to see if first clue is finished (or essentially finished) then mark all passed unkowns as empty...
;;; uses line-subsections... but will not be applied to all line-subsections... the shortened line will be assessed with the help of workable-subsection data...
;;; watch out for duplicate clues!!!
;;; |1 4 1 1| -010-,1----,1-010
;;;           0                 ;this method will only get that much; other methods will get the rest...
;;; first clue (or clues if duplicated) must be finished (or nearly finished, as in (2| -1-) or (3| -11-)) or otherwise related to later line-subsections
;;; clues do not have to be distinct, but duplicates make the checks much more involved... will probably need to do recursion or something similar
;;; with |3 3 3 3 1 2| four threes would have to be accounted for in order to be certain that unknowns found before the first 3 could be safely marked as empty
;;; a zone might need to be involved to handle more difficult cases...
(defmethod mark-as-empty-when-edgemost-clues-are-central ((ng nonogram) (line-obj line))
  )

;; ! note that for this method, and similar methods below, I opted not to use the usual macro, choosing to leave the method fairly specialized, in order to avoid a redundant call to UPDATE-LINE-SUBSECTIONS, and to avoid the creation of some variables that would not be needed or used
(defmethod mark-empties-around-largest-represented-clues ((ng nonogram) (line-obj line))
  "Marks the workable-subsections of a line with EMPTY-VALUE(s) when, within any of workable-subsection, there is a clue -- which must be the largest \"uncapped\" clue-value in the entire line -- that is fully represented by FILLED-VALUES but has not yet been capped off with an EMPTY-VALUE (or edge) on both ends. Note that this is a more specialized method that only works with line-subsections.
e.g. |2 1| ---11--- becomes |2 1| --0110--
e.g. |2 2 1| -1--11---- becomes |2 2 1| -1-0110---
e.g. |1 1 1 1 1| ---1----1--1---- becomes |1 1 1 1 1| --010--010010---
e.g. |2 3 2| --11---1110---- becomes |2 3 2| --11--01110---- 
e.g. |2 3 2| --11--01110---- becomes |2 3 2| -0110-01110----"
  (when (update-line-subsections ng line-obj) ;; ! note that line-subsections MUST be updated before the setting of CLUE-VALUE
    (let ((calling-function (cons 'mark-empties-around-largest-represented-clues "line-subsections only"))
          (clue-value (get-largest-or-smallest-remaining-clue-by-line-subsections ng line-obj)))
      ;; checking each wss (workable-subsection)
      (dotimes (wss-index (get-subsection-count-for-line line-obj))
        (let ((wss-obj (elt (line-subsections line-obj) wss-index))
              (first-i nil) ;; first index of consecutive fills
              (filled-count 0)
              (index-list nil)) ;; contains every FIRST-I that is part of a finished clue
          ;; iterating left-to-right across the workable-subsection, finding and saving the first-index of any grouping of FILLED-VALUE(s) whose count is equal ...
          ;; ... to CLUE-VALUE, saving this value in the list INDEX-LIST for later usage
          (do ((i (car (wss-range wss-obj)) (1+ i)))
              ((>= i (cdr (wss-range wss-obj))) (when (= filled-count clue-value) ;; ! this finally clause will only trigger when the largest-clue is adjacent to wss-edge
                                                  (push first-i index-list)))
            (if (index-is-filled-p ng line-obj i)
                (progn ;; THEN set FIRST-I if not already set and increment FILLED-COUNT
                  (unless first-i ;; if first-i is not yet set...
                    (setq first-i i))
                  (incf filled-count))
                (progn ;; ELSE save index data when FILLED-COUNT was equal to clue, and then reset FILLED-COUNT and FIRST-I
                  (when (= filled-count clue-value)
                    (push first-i index-list))
                  (setq filled-count 0)
                  (setq first-i nil))))
          ;; attempting to mark the sides of the clue-groupings as delineated by the previously collected FIRST-I(s), if any, marking with empties
          (dolist (index index-list)
            (do ((iteration-count 0 (1+ iteration-count)) ;; will iterate 2 times, once for each side of finished clue
                 (i (1- index) (+ index clue-value))) ;; i is the index for the left, and then right, empty at boundary of FILLED-VALUE(s) of clue (or else a wss-edge)
                ((>= iteration-count 2))
              (when (index-is-valid-for-workable-subsection-range-p wss-obj i) ;; ! note that i can indicate a wss-edge, hence this check
                (mark-as-x-at-i-when-unknown ng line-obj (empty-value ng) i calling-function)))))))))

;; ! at the marking section of this method, it should not be necessary to check whether the coordinate is already unknown because empties are workable-subsection delimiters, and a fill could not be there if the workable-subsection is not finished AND the workable-subsection-size is less than smallest remaining clue... the check is performed anyway however
(defmethod mark-small-unfillable-line-subsections-as-empty ((ng nonogram) (line-obj line))
  "Marks the workable-subsections of a line with EMPTY-VALUE(s) for any subsection whose size is so small that it cannot accommodate even the smallest clue of the entire line. Note that this is a more specialized method that only works with line-subsections.
e.g. |3| ---0--0--- becomes |3| ---0000---
e.g. |2| -0---0-0--- becomes |2| 00---000---
e.g. |3 2| ---0-0-- becomes |3 2| ---000--
e.g. |1 2 3 1| -010-0--0----010- becomes |1 2 3 1| 001000--0----0100"
  (when (update-line-subsections ng line-obj) ;; ! note that line-subsections MUST be updated before the setting of CLUE-VALUE
    (let ((calling-function (cons 'mark-small-unfillable-line-subsections-as-empty "line-subsections only"))
          (clue-value (get-largest-or-smallest-remaining-clue-by-line-subsections ng line-obj :largest nil)))
      ;; iterating through workable-subsections, looking for any unfinished subsection that is to small to accommodate the smallest clue in the full-line ...
      (dotimes (wss-index (get-subsection-count-for-line line-obj))
        (let ((cur-wss (elt (line-subsections line-obj) wss-index)))
          (when (and (not (wss-finished-p cur-wss))                              ;; when wss not finished
                     (< (get-length-of-workable-subsection cur-wss) clue-value)) ;; and wss too small for smallest clue
            ;; marking the entire workable-subsection empty
            (do ((i (car (wss-range cur-wss)) (1+ i)))
                ((>= i (cdr (wss-range cur-wss))))
              (mark-as-x-at-i-when-unknown ng line-obj (empty-value ng) i calling-function))))))))

;; !!!!!!!! this method could probably be simpler and better if line-subsections were improved...
;; !!!!!!!! actually, this method should simply be redundant after a proper upgrade of UPDATE-LINE-SUBSECTIONS
(defmethod mark-out-empty-workable-subsections ((ng nonogram) (line-obj line))
  "Marks the entirety of one or more workable-subsections of a line with EMPTY-VALUE(s) when the subsection certainly has no clues associated with it. Note that this is a more specialized method that only works with line-subsections.
e.g. |2| -1-0- becomes |2| -1-00 ;; !!!!! note that currently the presence of a FILLED-VALUE is irrelevant!!!!!
e.g. |3| --0----0-- becomes |3| 000----000"
  (when (update-line-subsections ng line-obj) ;; ! note that line-subsections MUST be updated before the setting of CLUE-COUNT, etc.
    (let ((calling-function (cons 'mark-out-empty-workable-subsections "line-subsections only"))
          (clue-count (get-clue-count-for-shortened-line line-obj)) ;; ! note that this is the clue-count for the shortened-line
          (potentially-empty-wss-indexes nil))
      ;; finding all subsections by index that might possible be fully empty while tracking which sections have definite clue relations
      (dotimes (i (get-subsection-count-for-line line-obj))
        (let* ((cur-wss (elt (line-subsections line-obj) i))
               (wss-clue-count (get-clue-count-for-workable-subsection cur-wss)))
          (if (or (eql nil wss-clue-count) (= 0 wss-clue-count)) ;; !!!!!!!!!! note that when WSS-CLUE-COUNT is 0, that should mean that this wss is definitely empty...
              (push i potentially-empty-wss-indexes) ;; keeping track of a subsection with no definite clue relations
              (decf clue-count wss-clue-count)))) ;; keeping track of associated clues
      (when (= 0 clue-count) ;; when all clues have been represented within shortened line
        ;; mark any workable-subsections as completely empty (if any were saved by index into POTENTIALLY-EMPTY-WSS-INDEXES)
        (dolist (wss-i potentially-empty-wss-indexes)
          (let ((cur-wss (elt (line-subsections line-obj) wss-i)))
            ;; marking the entire workable-subsection empty
            (do ((i (car (wss-range cur-wss)) (1+ i)))
                ((>= i (cdr (wss-range cur-wss))))
              (mark-as-x-at-i-when-unknown ng line-obj (empty-value ng) i calling-function))))))))

(defun copy-adjustable-sequence (seq &key (element-type nil))
  "Creates a copy of the passed adjustable sequence so as to keep a usable FILL-POINTER, without modifying the original sequence. SEQ is the passed adjustable sequence. ELEMENT-TYPE should be NIL or a quoted type (such as 'CHARACTER) and it should match the :ELEMENT-TYPE of the passed sequence."
  (let ((new-seq (make-array (fill-pointer seq) :element-type element-type :fill-pointer (fill-pointer seq))))
    (dotimes (i (fill-pointer seq))
      (setf (elt new-seq i) (elt seq i)))
    new-seq))

(defun generate-line-variations-as-bit-vectors (clues clue-count line-range)
  "Creates and returns a list of bit-vectors where each bit-vector represents a single unique arrangement of FILLED-VALUEs (as 1s) and EMPTY-VALUEs (as 0s) in a completely marked line according to the passed LINE-RANGE; thus the totality of all bit-vectors in the list represent all valid arrangements of the line according to the provided clues. Note that this function does not account for markings already present in a line; it simply creates all valid permutations assuming that the line was entirely unmarked. CLUES, CLUE-COUNT, and LINE-RANGE should be variables that are derived from a usage of the macro PROCEED-ACCORDING-TO-LINE-ASSESSMENT-TYPE; note that they are not checked for validity. Note that this function is fairly expensive, although it aims to be efficient, using a custom insertion-permutation algorithm that prevents insertions that result in duplicates.
e.g. providing #(2 1), 2, and (cons 0 5) yields (#*01101 #*11001 #*11010)
e.g. providing #(2 1), 2, and (cons 0 6) yields (#*011010 #*110010 #*110100 #*011001 #*110001 #*001101)
e.g. providing #(1 2 1), 3, and (cons 0 7) yields (#*0101101 #*1001101 #*1011001 #*1011010)"
  (let* ((line-len (get-line-length-via-range line-range))
         (insertion-locations (1+ clue-count)) ;; this number represents the number of options for placing an additional EMPTY-VALUE into the custom array
         (array-of-custom-clue-strings (make-array insertion-locations)) ;; ! see comments below (right above DOTIMES loop) for details about this array
         (character-count-of-custom-array 0)) ;; the count of all characters across all strings in the array ARRAY-OF-CUSTOM-CLUE-STRINGS
    ;; adding the empty string to the custom array first...
    (setf (elt array-of-custom-clue-strings 0) (make-array 0 :element-type 'bit :fill-pointer 0))
    ;; preparing ARRAY-OF-CUSTOM-CLUE-STRINGS such that each element is a string that represents the associated clue with an EMPTY-VALUE appended to each non-final clue ...
    ;; ... with an additional empty string up front where EMPTY-VALUE(s) may be added; note that this section also updates CHARACTER-COUNT-OF-CUSTOM-ARRAY accordingly
    ;; e.g. for a line with clues |2 3 1|, ARRAY-OF-CUSTOM-CLUE-STRINGS will be #("" "110" "1110" "1"), with CHARACTER-COUNT-OF-CUSTOM-ARRAY being finally set to 8
    (dotimes (i clue-count)
      (let* ((clue-value (elt clues i))
             (clue-represented-as-string (make-array clue-value :element-type 'bit :initial-element 1 :fill-pointer clue-value)))
        (incf character-count-of-custom-array clue-value)
        (when (> (- clue-count i) 1) ;; ! when NOT the last clue
          ;; append a 0 to the grouping of FILLED-VALUEs since there must certainly be a 0 following this clue because another clue exists after it
          (vector-push-extend 0 clue-represented-as-string)
          ;; account for the added 0
          (incf character-count-of-custom-array 1))
        (setf (elt array-of-custom-clue-strings (1+ i)) clue-represented-as-string))) ;; ! notice the increment on i, accounting for the empty string at front
    (let ((insertion-based-result nil))
      ;; placing one EMPTY-VALUE at a time onto end of each clue-string such that by final iteration every possible variation in empty placement ...
      ;; ... has been generated; note that this generative process continues only so long at to create those variations that are exactly equal to LINE-LEN
      ;; e.g. the STARTING-LIST (#("" "10" "11")) becomes (#("0" "10" "11") #("" "100" "11") #("" "10" "110")) by next iteration, with this result being expanded ...
      ;; ... as many times as necessary until character count (from all strings of the same array combined) is exactly LINE-LEN
      (do ((i character-count-of-custom-array (1+ i)) ;; ! starting with the count in order to simply continue by counting up to LINE-LEN
           (starting-list (list array-of-custom-clue-strings) list-after-insertion) ;; ! note that STARTING-LIST becomes LIST-AFTER-INSERTION after each iteration
           (list-after-insertion nil nil) ;; ! notice that LIST-AFTER-INSERTION is reset to NIL after each iteration
           (unique-arrays-ht (make-hash-table :test 'equal) (make-hash-table :test 'equal))) ;; is used as set to check uniqueness of each NEW-SEQ below
          ((>= i line-len) (setq insertion-based-result starting-list)) ;; ! notice the SETQ here !
        ;; creating LIST-AFTER-INSERTION from STARTING-LIST
        ;; first, selecting each sequence to be expanded upon
        (dolist (considered-seq starting-list)
          ;; preparing to place an EMPTY-VALUE at each insertion-index
          (dotimes (insertion-index insertion-locations)
            ;; creating a copy of the sequence which can then be modified safely
            (let ((new-seq (make-array insertion-locations)))
              ;; preparing NEW-SEQ so that it is a copy of CONSIDERED-SEQ with fill-pointers intact
              (dotimes (seq-i insertion-locations)
                (setf (elt new-seq seq-i) (copy-adjustable-sequence (elt considered-seq seq-i) :element-type 'bit)))
              ;; appending an EMPTY-VALUE (as 0) onto the end of the string at INSERTION-INDEX in NEW-SEQ
              (vector-push-extend 0 (elt new-seq insertion-index))
              ;; writing NEW-SEQ as string to check its uniqueness against the hash-table UNIQUE-ARRAYS-HT
              (let ((new-seq-as-string (format nil "~a" new-seq)))
                (unless (gethash new-seq-as-string unique-arrays-ht) ;; when string representing NEW-SEQ has not already been saved...
                  ;; tracking that this array was saved, and pushing NEW-SEQ onto LIST-AFTER-INSERTION
                  (setf (gethash new-seq-as-string unique-arrays-ht) t)
                  (push new-seq list-after-insertion)))))))
      (let ((simplified-result nil))
        ;; joining the separate bit-vectors of each array in the list INSERTION-BASED-RESULT (for example, #(#*0 #*1100 #*1110 #*1) becomes #*0110011101)
        (dolist (array-of-bit-vectors insertion-based-result)
          (let ((temp-bit-vector (make-array line-len :element-type 'bit :fill-pointer line-len))
                (temp-index 0))
            ;; setting TEMP-BIT-VECTOR according to the individual "insertion" bit-vectors
            (dotimes (insertion-index insertion-locations)
              (let ((cur-bit-vector (elt array-of-bit-vectors insertion-index)))
                (dotimes (index (fill-pointer cur-bit-vector))
                  (setf (elt temp-bit-vector temp-index) (elt cur-bit-vector index))
                  (incf temp-index))))
            (push temp-bit-vector simplified-result))) ;; ! note that this push will cause a reversal of the results
        simplified-result))))

(defmethod line-segment-has-necessary-overlap-with-generated-bit-vector-p ((ng nonogram) (line-obj line) line-range generated-bit-vector)
  "Returns T when the line in GRID and the passed GENERATED-BIT-VECTOR are consistent in terms of the already marked positions on the line in GRID; returns NIL otherwise. LINE-RANGE should be valid according to the actual line, GENERATED-BIT-VECTOR length, and the function CHECK-RANGE; note, however, that it is not checked for validity. GENERATED-BIT-VECTOR should be one of the bit-vectors as generated by the function GENERATE-LINE-VARIATIONS-AS-BIT-VECTORS.
e.g. if line is |1 2| ----- and bit array is #*01011, T is returned (note that when line is fully unmarked T is always returned)
e.g. if line is |1 2| 1---- and bit array is #*01011, NIL is returned (because the 1 in the line conflicts with bit-vector)
e.g. if line is |1 2| -0--- and bit array is #*01011, NIL is returned (because the 0 in the line conflicts with bit-vector)
e.g. if line is |1 2| -101- and bit array is #*01011, T is returned (because all existing marks match up)"
  (do ((line-index (car line-range) (1+ line-index))
       (bit-vector-index 0 (1+ bit-vector-index)))
      ((>= line-index (cdr line-range)) t) ;; ! returning T because the necessary overlap exists
    (let ((grid-value (get-value-at-line-index-in-grid ng line-obj line-index))
          (bit-value (elt generated-bit-vector bit-vector-index)))
      (when (or (and (= grid-value (filled-value ng)) ;; when grid position is filled AND bit-vector position is empty ...
                     (= bit-value 0))
                (and (= grid-value (empty-value ng))  ;; ... OR grid position is empty AND bit-vector position is filled
                     (= bit-value 1)))
        (return nil))))) ;; returning NIL because necessary overlap does NOT exist

(defmethod mark-according-to-consistency-of-generated-lines ((ng nonogram) (line-obj line) line-assessment-type &key (restrict-usage nil))
  "Marks a line-segment for which there is complete consistency by column between all generated possibilities indicating the necessary placement of either a FILLED-VALUE or EMPTY-VALUE at the corresponding index in passed line. LINE-ASSESSMENT-TYPE determines how the line is considered, and must be one of the following: +FULL-LINE+, +SHORTENED-LINE+, or +LINE-SUBSECTIONS+. Note that the primary reason for using a different LINE-ASSESSMENT-TYPE is efficiency.
e.g. with +FULL-LINE+, |1 2| ----- yields |1 2| ---1- because variations are 10110, 10011, and 01011, with ---*- being consistent between all as a 1
e.g. with +FULL-LINE+, |1 2| 1---- yields |1 2| 10-1- because variations are 10110 and 10011, with 1*-*- being consistent as 0 at first * and 1 at second *
e.g. with +FULL-LINE+, |1 2| --0-- yields |1 2| --011 because variations are 10011 and 01011, with --0** being consistent as 1 at both *s
e.g. with +FULL-LINE+, |1| --- is not updated, because variations are not consistent (100, 010, and 001)
e.g. with +FULL-LINE+, |1 2 2 1| --0------- yields |1 2 2 1| --01101101 because variations are 10... and 01..., with the \"...\" sections being consistent
e.g. with +SHORTENED-LINE+, |2 2| ---011 is considered as |2| --- and is updated to |2| -1- (because of variations 011 and 110), yielding |2 2| -1-011 
e.g. with +LINE-SUBSECTIONS+, |3 4| --11-0---1-1- is considered as |3| --11- and |4| ---1-1-, and are updated to |3| 0-11- and |4| 00-111-, yielding |3 4| 0-11-000-111-"
  ;; preparing appropriate variables so as to apply the technique to the appropriate part of the line
  (proceed-according-to-line-assessment-type (clues clue-count line-range calling-function 'mark-according-to-consistency-of-generated-lines)
    (when (or (not restrict-usage) (>= (reduce #'+ clues) (/ (get-line-length-via-range line-range) 3))) ;; !!!!!! refine this...
      (let ((generated-bit-vectors (generate-line-variations-as-bit-vectors clues clue-count line-range))
            (bit-vectors-with-necessary-overlap (list)))
        ;; collecting only those bit-vectors that are consistent with the line (in terms of the values already present in the line)
        (dolist (bit-vector generated-bit-vectors)
          (when (line-segment-has-necessary-overlap-with-generated-bit-vector-p ng line-obj line-range bit-vector)
            (push bit-vector bit-vectors-with-necessary-overlap)))
        ;; checking for consistency between the generated bit-vectors, making marks accordingly
        ;; e.g. when all bit vectors have FILLED-VALUEs in same spot (as in 01011, 10110, and 10011) then a fill is marked in actual line at appropriate position (---*-)
        ;; e.g. ... with EMPTY-VALUEs in same spot (for line |2 2| ---0-0--, with 11000011 and 01100011) them an empty is marked accordingly (---0*0--)
        (do ((iteration-count 0 (1+ iteration-count)) ;; will iterate twice, once for each SOUGHT-VALUE
             (sought-value 1 0) ;; ! note that in bit-vectors, FILLED-VALUEs are 1 and EMPTY-VALUEs are 0
             (sought-value-ng (filled-value ng) (empty-value ng))) ;; checking the consistency of FILLED-VALUEs first, and then EMPTY-VALUEs
            ((>= iteration-count 2))
          ;; walking across the bit-vector width while tracking related index in line
          (do ((line-index (car line-range) (1+ line-index))
               (bit-vector-index 0 (1+ bit-vector-index))
               (all-consistent t t))
              ((>= line-index (cdr line-range)))
            ;; running vertically down all bit-vectors at same horizontal index checking that all values are SOUGHT-VALUE, marking if so, and ending loop early otherwise
            (dolist (bit-vector bit-vectors-with-necessary-overlap)
              (unless (= sought-value (elt bit-vector bit-vector-index))
                (setq all-consistent nil)
                (return)))
            (when all-consistent
              (mark-as-x-at-i-when-unknown ng line-obj sought-value-ng line-index calling-function))))))))

(defmethod apply-initial-nonogram-techniques ((ng nonogram))
  "Applies the specialized \"initial\" techniques to the entire nonogram."
  (mark-for-lines-with-no-clues ng)
  (find-initial-overlap ng))

(defmacro do-unfinished-lines-according-to-slot-value ((current-line-var quoted-slot-name) &body body)
  "This macro loops through the NONOGRAM class slot LINES, one line at a time, with CURRENT-LINE-VAR acting as symbol for the current line and remaining available for use throughout the macro BODY. Lines that are completed will be ignored; and any lines whose QUOTED-SLOT-NAME is NIL will also be ignored. QUOTED-SLOT-NAME is expected to be a quoted LINE class slot name, and should be one of the few boolean slots such as 'USE-PRIMARY-P, 'USE-SECONDARY-P, etc.; if the line was not ignored (implying that this boolean was necessarily T), then this boolean will be set to NIL immediately before the BODY code; note that the validity of QUOTED-SLOT-NAME is not checked. This macro expects the NONOGRAM class object NG to be bound in the context of where it is used. Lastly, this macro is specifically designed to simplify and factor the method SOLVE-NONOGRAM-SINGLE-ITERATION."
  (let ((i-gs (gensym))
        (quoted-slot-name-gs (gensym)))
    `(let ((,quoted-slot-name-gs ,quoted-slot-name))
       (dotimes (,i-gs (line-count ng))
         (let ((,current-line-var (elt (lines ng) ,i-gs)))
           (when (and (not (line-complete-p ,current-line-var))
                      (slot-value ,current-line-var ,quoted-slot-name-gs))
             (setf (slot-value ,current-line-var ,quoted-slot-name-gs) nil)
             ,@body))))))

(defmethod apply-techniques-to-all-nonogram-lines-single-iteration ((ng nonogram))
  "Completes one full cycle of technique application against the passed nonogram. This method organizes different techniques according to their complexity or efficiency, and enforces a strict application of techniques such that only the simplest techniques are applied first, and more complex techniques are only ever applied after the preceding grouping of simpler techniques has failed to make any changes; thus, in any single calling of this method, only one grouping of techniques will be applied. Note that this method is used in the method APPLY-TECHNIQUES-TO-ALL-NONOGRAM-LINES as the body-code of its main loop; hence the method name."
  (setf (grid-was-changed-p ng) nil)
  (do-unfinished-lines-according-to-slot-value (current-line 'use-primary-p)
    ;; doing primary techniques...
    (mark-as-filled-based-on-edge-proximity ng current-line +shortened-line+)
    (mark-as-empty-based-on-edge-proximity ng current-line +shortened-line+)
    (mark-for-edgemost-clue-when-delimited-maximally-close-to-edge ng current-line +shortened-line+)
    (bridge-empties-from-edge ng current-line +shortened-line+)
    (mark-around-singular-clue ng current-line +shortened-line+)
    (mark-around-unjoinable-clues ng current-line +shortened-line+)
    (find-overlap ng current-line +shortened-line+)
    (basic-check-for-finished-line ng current-line))
  (unless (grid-was-changed-p ng)
    (do-unfinished-lines-according-to-slot-value (current-line 'use-secondary-p)
      ;; secondary techniques...
      (mark-as-filled-based-on-edge-proximity ng current-line +line-subsections+)
      (mark-as-empty-based-on-edge-proximity ng current-line +line-subsections+)
      (mark-around-singular-clue ng current-line +line-subsections+)
      (mark-around-unjoinable-clues ng current-line +line-subsections+)
      (mark-for-repeating-clues-at-edge ng current-line +shortened-line+)
      (mark-empties-around-largest-represented-clues ng current-line)
      (mark-small-unfillable-line-subsections-as-empty ng current-line)
      (mark-out-empty-workable-subsections ng current-line)
      (find-overlap ng current-line +line-subsections+)))
  (unless (grid-was-changed-p ng)
    (do-unfinished-lines-according-to-slot-value (current-line 'use-gen1-p)
      ;; line-generation-1
      (mark-according-to-consistency-of-generated-lines ng current-line +line-subsections+ :restrict-usage t)))
  (unless (grid-was-changed-p ng)
    (do-unfinished-lines-according-to-slot-value (current-line 'use-gen2-p)
      ;; line-generation-2
      (mark-according-to-consistency-of-generated-lines ng current-line +line-subsections+))))

(defmethod apply-techniques-to-all-nonogram-lines ((ng nonogram))
  "Repeatedly applies an assortment of techniques against the nonogram until no progress can be made, such that the nonogram was completed or else remains incomplete with all techniques failing to bring further progress. See the method APPLY-TECHNIQUES-TO-ALL-NONOGRAM-LINES-SINGLE-ITERATION for more details."
  (setf (grid-was-changed-p ng) t) ;; ! ensuring that technique application will always occur when this method is called
  (do ()
      ((not (grid-was-changed-p ng))) ;; ! looping until the GRID is unchanged...
    (apply-techniques-to-all-nonogram-lines-single-iteration ng)))

(defmethod create-move-list-text-file ((ng nonogram))
  "Creates a text file that contains simple data about all marks made against the current NONOGRAM such that the first line is information about the width and height, and each subsequent line follows the exact order of the NONOGRAM class slot ALL-MOVES-ARRAY and indicates the mark's location and type; note that each line in this file consists of numbers separated by single spaces. The resulting text file can then be parsed to automate the placement of marks on an actual GUI nonogram application."
  ;; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! address issues with the path string !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  (with-open-file (fout "c:/users/pc/documents/programming/common-lisp/nonogram-solver/nonogram-move-list.txt" :direction :output :if-exists :supersede)
    (write-line (format nil "~d ~d" (grid-width ng) (grid-height ng)) fout)
    (dotimes (i (fill-pointer (all-moves-array ng)))
      (let ((x (elt (all-moves-array ng) i)))
        (write-line (format nil "~d ~d ~d" (car (first x)) (cdr (first x)) (second x)) fout)))))

(defmethod solve-nonogram ((ng nonogram))
  "Attempts to solve the nonogram represented by the NONOGRAM object NG, applying all techniques to every line according to an optimized ordering of techniques until no further progress can be made. This method will also trigger the creation of the move-list text file if the NONOGRAM slot COLLECT-MOVE-DATA-P is T."
  ;; printing GRID at start before application of any technique, if appropriate
  (when (print-each-step-p ng)
    (print-grid ng nil nil nil))
  ;; applying all initial techniques to the nonogram grid
  (apply-initial-nonogram-techniques ng)
  ;; applying all techniques repeatedly until no further progress can be made, implying completion of the nonogram or else that current techniques can reveal nothing else
  (apply-techniques-to-all-nonogram-lines ng)
  ;; printing GRID at end after application of all techniques, if appropriate
  (unless (print-each-step-p ng)
    (print-grid ng nil nil nil))
  ;; creating the move-list text file, when the data has been collected
  (when (collect-move-data-p ng)
    (create-move-list-text-file ng)))

(defmacro with-single-line-nonogram (nonogram-var line-var line-length clue-array &body body)
  "This macro creates a specialized NONOGRAM class object, named according to NONOGRAM-VAR, that is meant to be used, not as a full nonogram, but as a single line, which is named according to LINE-VAR. This macro is to be used primarily to test individual techniques against specific lines, and has been used extensively in testing each individual technique. Note that trying to use SOLVE-NONOGRAM or the like on a nonogram created with this macro should trigger an error. LINE-LENGTH is the length of the line under consideration. CLUE-ARRAY is an array of integers where each integer represents a clue according to the line orientation (left-to-right for horizontal clue-sets, and top-to-bottom for vertical clue-sets). Lastly, note that the line is always represented visibly as horizontal."
  (let ((line-length-gs (gensym))
        (clue-array-gs (gensym))
        (top-clues-gs (gensym))
        (i-gs (gensym)))
    `(let* ((,line-length-gs ,line-length)
            (,clue-array-gs ,clue-array)
            (,top-clues-gs (make-array ,line-length-gs)))
       ;; preparing top-clues such that they are all empty, and thereby, as a whole, inaccurate (because they are not meant to be used)
       (dotimes (,i-gs ,line-length-gs)
         (setf (aref ,top-clues-gs ,i-gs) (vector)))
       (let* ((,nonogram-var (make-instance 'nonogram :side-clues (vector ,clue-array-gs) :top-clues ,top-clues-gs))
              (,line-var (get-line-by-orientation-and-index-from-edge ,nonogram-var (horizontal-char ,nonogram-var) 0)))
         ,@body))))
