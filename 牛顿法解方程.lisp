;;应用牛顿法解方程

;f 欲求解函数
;start-value 迭代初始值
;differential 微分差值
;interval 迭代初始值范围,传入一个双目CONS
;start-d 迭代初始值步长

;求导函数
(defun df (f x differential)
   (/ (- (funcall f (+ x differential)) (funcall f x)) differential)) 

;递推函数
(defun next-x (f x differential)
  (let* ((y (funcall f x))
	 (k (funcall #'df f x differential)))
    (if (= k 0)
	(cons (- x 0.01) nil)
	(cons (- x (/ y k)) t))))

;main
(defun newton (f &key (start-value 0) (differential 0.0001)
	       (interval '(-10 . 10) start-interval-supplied-p) (start-d 1))

  (let* ((ans start-value)
	 anslist)

    (if (not start-interval-supplied-p)
	(dotimes (i 1000000 ans)
	  (setq ans (car (next-x f ans differential))))
	(dotimes (i (floor (/ (- (cdr interval) (car interval)) start-d)) 
		  (remove-if (lambda (x) (or (> x (cdr interval)) (< x (car interval)))) 
			     (remove-duplicates anslist :test (lambda (x y) (< (abs (- x y)) differential)))))
	  (setq ans (+ (car interval) (* (1- i) start-d)))
	  (push (dotimes (j 10000 ans)
		  (setq ans (car (next-x f ans differential)))) anslist)))))


    