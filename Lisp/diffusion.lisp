#!/usr/bin/sbcl --script

;;; Simplified 3D Diffusion Model
(let ((maxsize) ; number of blocks per dimension
      (p-flag)  ; partition flag
      (diffusion_coefficient 0.175)
      (room_dimension 5) ; 5 Meters
      (speed_of_gas_molecules 250.0) ; Base on 100 g/mol gas at RT
      (timestep)
      (distance_between_blocks)
      (DTerm)
      (cube) ; the 3D-array
      (change)
      (acctime 0.0l0)   ; accumulated time (Note: "time" is a Lisp keyword)
      (eqratio 0.0l0)   ; (Note: "ratio" is also a Lisp keyword)
      (minval 0.0l0)    ; (Note: "0.0l0" asserts double precision data type)
      (maxval 0.0l0)
      (sumval 0.0l0)
      (px) ; partition x-value 
      (py)) ; partition y-value

      (format t "Msize?: ")
      (finish-output)
      (setf maxsize (read))
      (clear-input)

      (format t "Add partition? (y/n): ")
      (finish-output)
      (setf p-flag (read-char))
      (clear-input)

      (setf timestep (/ (/ room_dimension speed_of_gas_molecules) maxsize)) ; h in seconds
      (setf distance_between_blocks (/ room_dimension maxsize))
      (setf DTerm (/ (* diffusion_coefficient timestep) (* distance_between_blocks distance_between_blocks)))

      (setf cube (make-array (list maxsize maxsize maxsize):initial-element 0.0l0)) ; create 3D-array with zeroes

      ;; Add partition if user inputs 'y'
      (when (char-equal p-flag #\y)
          (setf px (- (ceiling (* maxsize 0.5)) 1))
          (setf py (- (ceiling (* maxsize (- 1 0.75))) 1)) ; partition height (- 1 percent-heigh) where percent-height = 0.75
          (loop for j from py to (- maxsize 1) do
              (loop for k from 0 to (- maxsize 1) do
                  (setf (aref cube px j k) -1)))) ; fill partition spaces with -1

      (setf (aref cube 0 0 0) 1.0e21) ; Initialize first cell

      (loop while (<= eqratio 0.99) do 
          (loop for i from 0 to (- maxsize 1) do
              (loop for j from 0 to (- maxsize 1) do
                  (loop for k from 0 to (- maxsize 1) do
                      (loop for l from 0 to (- maxsize 1) do
                          (loop for m from 0 to (- maxsize 1) do
                              (loop for n from 0 to (- maxsize 1) do
                                  (when (or (or (or (and (and (= i l) (= j m)) (= k (+ n 1)))
                                        (and (and (= i l) (= j m)) (= k (- n 1))))
                                        (or (and (and (= i l) (= j (+ m 1))) (= k n))
                                        (and (and (= i l) (= j (- m 1))) (= k n))))
                                        (or (and (and (= i (+ l 1)) (= j m)) (= k n))
                                        (and (and (= i (- l 1)) (= j m)) (= k n))))
                                            (setf change (* (- (aref cube i j k) (aref cube l m n)) DTerm))
                                            (when (or (= (aref cube i j k) -1) (= (aref cube l m n) -1))
                                                (setf change 0.0)) ; when partition encountered
                                            (decf (aref cube i j k) change)
                                            (incf (aref cube l m n) change))))))))
      
      (incf acctime timestep)

      (setf sumval 0.0)
      (setf maxval (aref cube 0 0 0))
      (setf minval (aref cube 0 0 0))

      (loop for i from 0 to (- maxsize 1) do
          (loop for j from 0 to (- maxsize 1) do
              (loop for k from 0 to (- maxsize 1) do
                  (unless (= (aref cube i j k) -1)
                      (setf maxval (max (aref cube i j k) maxval))
                      (setf minval (min (aref cube i j k) minval))
                      (incf sumval (aref cube i j k))))))
    
      (setf eqratio (/ minval maxval))) ; while loop ends here when not printing values

      ;(format t "~20a" eqratio)    
      ;(format t "~20a~20a" acctime (aref cube 0 0 0))
      ;(format t "~20a" (aref cube (- maxsize 1) 0 0))
      ;(format t "~20a" (aref cube (- maxsize 1) (- maxsize 1) 0))
      ;(format t "~20a" (aref cube (- maxsize 1) (- maxsize 1) (- maxsize 1)))
      ;(format t "~20a~%" sumval))                                             ; while loop ends here

      (format t "Box equilibrated in ~a seconds of time.~%" acctime)) 



