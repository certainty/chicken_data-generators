(use test)

(use data-generators numbers)

(define (in? x ls) (not (null? (member x ls))))
(define (between? a x y) (and (>= a x) (<= a y)))

(define-syntax test-fixed-range
  (syntax-rules ()
    ((_  gen lower upper)
     (test-group (symbol->string (quote gen))
       (test-assert
        (between? (<- (gen)) lower upper))))))

(test-group "gen-for-each"
    (let ((runs 1))
      (test "applies proc amount times"
            3
            (begin
              (gen-for-each 3 (lambda (value) (set! runs (add1 runs))) (gen-list-of (gen-fixnum)))
              runs))))

(test-group "<-"
            (test "it takes one element from gen"
                  3
                  (<- (gen-sample (list 3)))))

(test-group "<-*"
            (test "it takes n elements form gen"
                  (list 3 3 3)
                  (<-* 3 (gen-sample (list 3)))))

(test-group "gen-constant"
            (test "it constantly returns the given value"
                  42
                  (<- (gen-constant 42))))

(test-group "gen-bool"
  (test-assert
   (in? (<- (gen-bool)) (list #t #f))))

(test-group "gen-fixnum"
    (test-assert
     (between? (<- (gen-fixnum)) (gen-current-fixnum-min) (gen-current-fixnum-max)))
    (test-assert
     (between? (<- (gen-fixnum 4)) (gen-current-fixnum-min) 4))
    (test-assert
     (between? (<- (gen-fixnum 2 4)) 2 4))
    (test-error "lower bound <= upper bound"
     (gen-fixnum 4 2)))

(test-fixed-range gen-int8 -127 127)
(test-fixed-range gen-uint8 0 255)
(test-fixed-range gen-int16 -32767 32767)
(test-fixed-range gen-uint16 0 65535)
(test-fixed-range gen-int32 -2147483647 2147483647)
(test-fixed-range gen-uint32 0 4294967295)
(test-fixed-range gen-int64 -9223372036854775807 9223372036854775807)
(test-fixed-range gen-uint64 0 18446744073709551615)

(test-group "gen-real"
    (test-assert
     (between? (<- (gen-real)) 0.0 1.0))
    (test-assert
     (between? (<- (gen-real 1.0)) 0.0 1.0))
    (test-assert
     (between? (<- (gen-real 1.0 2.0)) 1.0 2.0))
    (test-error "lower bound <= upper bound"
     (gen-real 2.0 1.0)))

(test-group "gen-char"
    (test-assert
     (char-set-contains? char-set:graphic (<- (gen-char))))
    (test-assert
     (char-set-contains? char-set:digit (<- (gen-char char-set:digit))))
    (test-assert
     (char-set-contains? (char-set #\a #\b #\c) (<- (gen-char #\a #\c))))
    (test-error "lower bound <= upper bound"
     (gen-char #\z #\a)))

(test-group "gen-sample"
    (test-assert (between? (<- (gen-sample (iota 10))) 0 9))
    (test-assert (in? (<- (gen-sample (list #\a #\b #\c))) (list #\a #\b #\c))))

(test-group "range syntax"
	    (test-group "exclusive"
			(test "neginf to upper"
			      (cons (gen-current-fixnum-min)  4)
			      (range .. 5))
			(test "lower to posinf"
			      (cons 3  (sub1 (gen-current-fixnum-max)))
			      (range 3 .. ))
			(test "lower .. upper"
			      (cons 1  9)
			      (range 0 .. 10))))

(test-group "gen-sample-of"
            (test-assert ((lambda (e) (or (fixnum? e) (char? e)))
			  (<- (gen-sample-of (gen-fixnum) (gen-char))))))

(test-group "gen-pair-of"
    (test-assert "produces a pair"
                 (pair? (<- (gen-pair-of (gen-fixnum) (gen-fixnum)))))
    (test-assert "car is element of expected set"
                 (fixnum? (car (<- (gen-pair-of (gen-fixnum) (gen-fixnum))))))
    (test-assert "cdr is element of expected set"
                 (fixnum? (cdr (<- (gen-pair-of (gen-fixnum) (gen-fixnum)))))))

(test-group "gen-tuple-of"
    (test "produces tuple of given size"
          3 (length (<- (gen-tuple-of (gen-fixnum) (gen-fixnum) (gen-fixnum)))))
    (test "each element is element of expected set"
          #t
          (every fixnum? (<- (gen-tuple-of (gen-fixnum) (gen-fixnum))))))


(define-syntax test-size-spec-support
  (syntax-rules ()
    ((_ ?length (?gen ?args ...))
     (let ((length-matches (lambda (from to ls)
			     (every (lambda (e) (between? e from to)) (map ?length ls)))))
       (test-group "supports size-spec"
		   (test-assert "range"
				(length-matches 0 10  (<-* 10 (?gen ?args ... (make-range 0 10)))))
		   (test-assert "fixed size"
				(length-matches 10 10 (<-* 10 (?gen ?args ... 10))))
		   (test-assert "generator"
				(length-matches 0 10  (<-* 10 (?gen ?args ... (gen-fixnum 0 10)))))
		   (test-assert "it generates a list with different lengths"
				(let ((ls (map length (<-* 10 (?gen ?args ... (gen-fixnum 0 100))))))
				  (not (every (lambda (e) (= e (car ls))) ls)))))))))


(test-group "gen-list-of"
    (test-assert "produces a list"
                 (list? (<- (gen-list-of (gen-fixnum)))))
    (test-assert "each element is part of expected set"
                 (every fixnum? (<- (gen-list-of (gen-fixnum)))))
    (test-assert "accepts ranges"
		 (between? (length (<- (gen-list-of (gen-fixnum) (range 1 .. 10)))) 2 9))

    (test-size-spec-support length (gen-list-of (gen-fixnum))))

(test-group "gen-alist-of"
   (test-assert "produces a list"
                (list? (<- (gen-alist-of (gen-char) (gen-fixnum)))))
   (test-assert "every key is of expected set"
                (every (compose char? car) (<- (gen-alist-of (gen-char) (gen-fixnum)))))
   (test-assert "every value is of expected set"
                (every (compose fixnum? cdr) (<- (gen-alist-of (gen-char) (gen-fixnum)))))
   (test "it generates alist of given length"
       4
       (with-size 4 (length (<- (gen-alist-of (gen-char) (gen-fixnum)))))))

(test-group "gen-vector-of"
   (test-assert "produces a vector"
                (vector? (<- (gen-vector-of (gen-fixnum)))))
   (test-assert "every element is element of expected set"
                (every fixnum?  (vector->list (<- (gen-vector-of (gen-fixnum))))))
   (test "it generates vector of given length"
      4
      (with-size 4
                 (vector-length (<- (gen-vector-of (gen-fixnum)))))))

(test-group "gen-string-of"
    (test-assert "produces a string"
                 (string? (<- (gen-string-of (gen-char)))))
    (test-assert "every element is within the given char-set"
                 (every (cut char-set-contains? char-set:graphic <>)
                        (string->list
                         (<- (gen-string-of (gen-char char-set:graphic))))))
    (test "it generates a string of the given length"
       10
       (with-size 10 (string-length (<- (gen-string-of (gen-char)))))))

(test-group "gen-hash-table-of"
    (test-assert "produces a hash-table"
                 (hash-table? (<- (gen-hash-table-of (gen-fixnum) (gen-fixnum)))))
    (test-assert "every key is element of expected set"
                 (every fixnum? (hash-table-keys (<- (gen-hash-table-of (gen-fixnum) (gen-fixnum))))))
    (test-assert "every value is element of expected set"
                 (every fixnum? (hash-table-values (<- (gen-hash-table-of (gen-fixnum) (gen-fixnum))))))
    (test "it generates a hash-table of given size"
      10
      (with-size 10 (hash-table-size (<- (gen-hash-table-of (gen-fixnum) (gen-fixnum)))))))


(define-record test-record x y)

(test-group "gen-record"
            (let ((record (<- (gen-record make-test-record (gen-fixnum) (gen-fixnum)))))
              (test-assert
               (test-record? record))
              (test-assert
               (fixnum? (test-record-x record)))
              (test-assert
               (fixnum? (test-record-y record)))))

(test-group "with-size"
            (test-assert "fixed size" (= 2 (length  (with-size 2 (<- (gen-list-of (gen-fixnum)))))))
            (test-assert "range" (between? (length  (with-size (2 . 4) (<- (gen-list-of (gen-fixnum))))) 2 4)))
