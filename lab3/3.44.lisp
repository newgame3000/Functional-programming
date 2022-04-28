;Обёртка
(defun matrix-t1-t2(n)
    (main (make-array (list n n)) 1 NIL n))

;Главная функция, происходит проход по диагоналям
(defun main(matrix num up n)
    (loop for k from 0 to ( - ( * 2 n) 2)
        do 
        (fill_diag k num matrix up n)
        (if (< k n)
            (setf num (+ num (+ k 1)))
            (setf num (+ num (- (* 2 n) k 1))))
        (setq up (not up)))
matrix)


;Заполняет соответствующую диагональ
(defun fill_diag(k num matrix up n) 
    (if (< k n)
        (loop for i from 0 to k
            do  (let ((j (- k i)))
                (if up
                    (setf (aref matrix i j) (+ num i))
                    (setf (aref matrix j i) (+ num i)))))
        (loop for i from ( + (- k n) 1) to (- n 1)
            do  (let ((j (- k i)))
                (if up
                    (setf (aref matrix i j) (- (+ num i n) k 1))
                    (setf (aref matrix j i) (- (+ num i n) k 1)))))))