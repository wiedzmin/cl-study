;; 1
(+ (- 5 1) (+ 3 7))
(list 1 (+ 2 3))
(if (listp 1) (+ 1 2) (+ 3 4))
(list (and (listp 3) t) (+ 1 2))

;; 2
(cons 'a (cons 'b (cons 'c nil)))
(cons 'a '(b c))
(cons 'a (cons 'b '(c . nil)))          ;maybe there is one more way

;; 3
(defun get-4th (lst)
  (car (cdr (cdr (cdr lst)))))

(defun get-4th-abbrev (lst)
  (cadddr lst))

(get-4th '(1 2 3 4 5))
(get-4th-abbrev '(1 2 3 4 5))

;; 4
(defun my/max (x y)
  (if (> x y)
      x
      y))

(my/max 10 20)

;; 5
(defun enigma (x)
  (and (not (null x))
       (or (null (car x))
           (enigma (cdr x)))))

(enigma '(t t t t))
(enigma '(t nil t nil))
(enigma '(nil t t nil))

(defun mystery (x y)
  (if (null y)
      nil
      (if (eql (car y) x)
          0
          (let ((z (mystery x (cdr y))))
            (and z (+ z 1))))))

(mystery 1 nil)
(and 1 2)
(and 2 1)
(mystery 1 '(nil t nil t nil))
(mystery 4 '(1 2 3 4 5 6))

;; 6
; > (car (x (cdr '(a (b c) d))))
; B
(car (car (cdr '(a (b c) d))))
; > (x 13 (/ 1 0))
; 13
(or 13 (/ 1 0))
; > (x #'list 1 nil)
; (1)
(apply #'list 1 nil)

;; 7
(defun any-nested-list-p (lst)
  (if (null lst)
      nil
      (or (listp (car lst))
          (any-nested-list-p (cdr lst)))))

(any-nested-list-p '(1 2 3 4))
(any-nested-list-p '((1 2) 3 4))
(any-nested-list-p '(1 2 (3 4)))

;; 8
(defun num-to-points-iter (num)
  (dotimes (x num)
    (princ ".")))

(defun count-a-symbols-iter (lst)
  (let ((a-sum 0))
    (dolist (symb lst)
      (if (eql symb 'a)
          (incf a-sum)))
    a-sum))

(defun num-to-points-recur (num)
  (if (<= num 0)
      0
      (progn
        (princ ".")
        (num-to-points-recur (decf num)))))

(defun count-a-symbols-recur (lst)
  (if (null lst)
      0
      (if (eql (car lst) 'a)
          (+ 1 (count-a-symbols-recur (cdr lst)))
          (count-a-symbols-recur (cdr lst)))))

(num-to-points-iter 10)
(count-a-symbols-iter nil)
(count-a-symbols-iter '(a a a b b))
(num-to-points-recur 10)
(count-a-symbols-recur nil)
(count-a-symbols-recur '(a a a b b))
(count-a-symbols-recur '(b b))

;; 9
(defun summit-a (lst)
  (remove nil lst)
  (apply #'+ lst))

(defun summit-b (lst)
  (let ((x (car lst)))
    (if (null lst)
        (summit-b (cdr lst))
        (+ x (summit-b (cdr lst))))))

(defun summit (lst)
  (apply #'+ (remove nil lst)))

(summit '(1 2 3 4 nil 5))
(summit-a '(1 2 3 4 nil 5))
(summit-b '(1 2 3 4 nil 5))
