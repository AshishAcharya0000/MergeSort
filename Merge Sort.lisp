(defun left (list n)
  (if (<= n 0)
      '()
      (cons (car list) (left (cdr list) (- n 1)))))

(defun right (list n)
  (if (<= n 0) list
      (right (cdr list) (- n 1))))

(defun merge-sort (list predicate)
  (labels((merge_ (a b)
            (cond
              ((= (length a) 0) b)
              ((= (length b) 0) a)
              ((funcall predicate (first a) (first b)) (cons (first a) (merge_ (cdr a) b)))
              ((funcall predicate (first b) (first a)) (cons (first b) (merge_ a (cdr b))))
              ((= (first a) (first b)) (cons (first a) (cons (first b) (merge_ (cdr a) (cdr b))))))))

    (let ((len_list (length list)))
      (if (<= len_list 1)
          list(merge_
           (merge-sort (left list (/ len_list 2)) predicate)
           (merge-sort (right list (/ len_list 2)) predicate))))))

 
          
    
    
    
             


      

