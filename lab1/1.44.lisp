;Считает элемент по двум числам(координатам в треугольнике) 
(defun elem (x y)
(cond
    ((= x 1) 1)
    ((= y 1) 1)
    ((= y x) 1)
    (T (+ (elem (- x 1) (- y 1)) (elem (- x 1) y)))))

;Печать строки по номеру
(defun printstr (x y)
(cond ((> y 0)
    (cond ((= x y)
    (print (elem x y)))
    (T (princ (elem x y)) (princ " ")))
    (printstr x (- y 1)))))
    
;Функция печати для правильного порядка
(defun printriangle (x n)
    (cond ((> x 0) 
        (printstr n n)
        (printriangle (- x 1) (+ n 1)))))

(defun pascal-triangle (x)
    (printriangle x 1) T)