
;Обёртка
(defun map-set (f x)
    (main f x `())
)

;Проходит по списку, применяет к каждому элементу функцию, проверяет наличие
;добавляет, если такого не оказалось
(defun main (f x res)
    (if (null x)
        res
        (let((a (funcall f (first x))))
            (if (member a res)
                (main f (rest x) res )
                (main f (rest x) (append res (list a) ))))))


