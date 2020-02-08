;;;;;;
;;;;;; Mazeout
;;;;;;

;;; Types

;; CLtL pg.63
(defun equidimensional (a)
  (or (< (array-rank a) 2)
      (apply #'= (array-dimensions a))))

;; CLtL pg.63
(deftype square-matrix (&optional type size)
  `(and (array ,type (,size ,size))
        (satisfies equidimensional)))

(deftype adjacency-matrix (&optional size)
  `(square-matrix bit ,size))

(defun make-adjacency-matrix (size)
  (make-array `(,size ,size)
              :element-type 'bit
              :initial-element 0))

;; How to enforce list of single type?
(defun int-cons (pair)
  (and (integerp (car pair))
       (integerp (cdr pair))))

(deftype int-pair ()
  '(and cons
    (satisfies int-cons)))

;;; Actions on Types

(defun add-edge (a b mat)
  (setf (aref mat a b) 1)
  (setf (aref mat b a) 1))

;;; Translations from razimantv's "mazegenerator"

;; mg-graph: a node is an index into the adjacency
;;           list array, it can be the index into a
;;           node metadata array as well

(deftype mg-graph (&optional size)
  `(array list ,size))

(defun make-mg-graph (length)
  (make-array length
              :element-type 'list
              :initial-element '()))

(defun insert-edge (tree a b)
  (push (cons a b)
        tree))

(defun shuffle (array)
  "The Knuth Shuffle"
  (labels ((shuffle (i)
                  (cond ((<= i 0) array)
                        (t
                         (rotatef (aref array (1- i))
                                  (aref array (random i)))
                         (shuffle (1- i))))))
    (shuffle (length array))))

(defun depth-first-search (vertex-count mg-graph)
  "First-shot transcription, someone pls test"
  (let ((min-spanning-tree (make-list))
        (visited (make-array vertex-count
                             :element-type bit
                             :initial-element 0))
        (first-vertex (random vertex-count)))
    (labels ((dfs (vertex mg-graph)
               (let* ((adjacencies (aref mg-graph vertex))
                      (nodeorder (make-array :initial-contents
                                             (iota (length adjacencies)))))
                 (setf (aref visited vertex) 1)
                 (shuffle nodeorder)
                 (mapc (lambda (index)
                         (let ((nextvertex (nth adjacencies index)))
                           (unless (or (< nextvertex 0)
                                       (= (aref visited nextvertex) 1))
                             (push (cons vertex nextvertex) min-spanning-tree)
                             (dfs nextvertex mg-graph)))
                         nodeorder)))))
      (dfs first-vertex mg-graph))
    min-spanning-tree))
