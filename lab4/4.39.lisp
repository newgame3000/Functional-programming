
;Проход по всем параметрам текущего оператора
(defun operator (a op flag s)
    (cond ((and (eq (lastop op) '-) (null (rest (rest a))))
        (setq s (concatenate 'string s "( "))
        (setq s (concatenate 'string s "- "))
        (let ((lst (multiple-value-list (main (first (rest a)) op s))))
            (setq op (first lst))
            (setq s (second lst)))
        (setq s (concatenate 'string s ") ")))

    ((and (eq (lastop op) '/) (null (rest (rest a))))
        (setq s (concatenate 'string s "( "))
        (setq s (concatenate 'string s "1 / "))
        (let ((lst (multiple-value-list (main (first (rest a)) op s))))
            (setq op (first lst))
            (setq s (second lst)))
        (setq s (concatenate 'string s ") ")))

    (T
    (if flag
        (setq s (concatenate 'string s "( ")))

    (let ((l (- (length a) 2)) (k 0) (curop (lastop op)))
        (loop for elem in (rest a)
            do
            (let ((lst (multiple-value-list (main elem op s))))
                (setq op (first lst))
                (setq s (second lst)))

            (cond ((not (= k l))
                (setq s (concatenate 'string s (princ-to-string curop)))
                (setq s (concatenate 'string s " "))))
            (setq k (+ k 1))))

    (if flag
        (setq s (concatenate 'string s ") ")))))
    (values (rest op) s)
)


;Главная функция, обход по операторам
(defun main (a op s)
    (cond ((listp a)
        (cond ((eq (first a) '+)
            (cond ((or (eq (lastop op) '*) (eq (lastop op) '/) (eq (lastop op) '-))
                (setq op (cons (first a) op))
                (let ((lst (multiple-value-list (operator a op T s))))
                    (setq op (first lst))
                    (setq s (second lst))
                ))

                (T 
                (setq op (cons (first a) op))
                (let ((lst (multiple-value-list (operator a op nil s))))
                    (setq op (first lst))
                    (setq s (second lst))
                ))))


        ((eq (first a) '*)
            (cond (( eq (lastop op) '/)
                (setq op (cons (first a) op))
                (let ((lst (multiple-value-list (operator a op T s))))
                    (setq op (first lst))
                    (setq s (second lst))
                ))

                (T 
                (setq op (cons (first a) op))
                (let ((lst (multiple-value-list (operator a op nil s))))
                    (setq op (first lst))
                    (setq s (second lst))
                ))))


        ((eq (first a) '/)
            (cond (( eq (lastop op) '*)
                (setq op (cons (first a) op))
                (let ((lst (multiple-value-list (operator a op T s))))
                    (setq op (first lst))
                    (setq s (second lst))
                ))

                (T 
                (setq op (cons (first a) op))
                (let ((lst (multiple-value-list (operator a op nil s))))
                    (setq op (first lst))
                    (setq s (second lst))
                ))))


        ((eq (first a) '-)
            (cond ((or ( eq (lastop op) '*) (eq (lastop op) '/))
                (setq op (cons (first a) op))
                (let ((lst (multiple-value-list (operator a op T s))))
                    (setq op (first lst))
                    (setq s (second lst))
                ))

                (T 
                (setq op (cons (first a) op))
                (let ((lst (multiple-value-list (operator a op nil s))))
                    (setq op (first lst))
                    (setq s (second lst))
                ))))))
    (T 
    (setq s (concatenate 'string s (string-downcase (princ-to-string a))))
    (setq s (concatenate 'string s " "))))
    (values op s)
)

;Оператор на вершине стека, если стек пустой возвращается темринальный символ _
(defun lastop(op)
    (if (null op)
        '_
        (first op)
    ) 
) 

;Обертка
(defun form-to-infix (v)
    (let ((lst (multiple-value-list (main v nil nil))))
         (string-right-trim " " (second lst))))