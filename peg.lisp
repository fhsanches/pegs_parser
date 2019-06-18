;; usage example
(with-peg-parse "calc" "(1+1-4)")

(defvar spaces-list '(#\Space #\Newline #\Backspace #\Tab #\Linefeed #\Page #\Return #\Rubout))
(defparameter *pos* 0)
(defparameter *text* "")

;;TODO: Parse classes & ranges (e.g.: [a-z])
(defparameter *rules* (make-hash-table :test 'equal))

(defun file-string (path)
  (with-open-file (stream path)
    (let ((data (make-string (file-length stream))))
      (read-sequence data stream)
      data)))

;; for quick testing
;; (defparameter calcpeg (file-string "pegs/port.peg"))

(defstruct parsed-value
  type
  value
  children
  action)

(defstruct peg
  root
  rules)

(defstruct rule
  name
  definition)


(defun with-peg-parse (peg-name text-input)
  (let* ((*text* (file-string (concatenate 'string "pegs/" peg-name ".peg")))
	 (*pos* 0)
	 (*rules* (generate-parser *text*))
	 (root (peg-root (grammar))))
    (let ((*text* text-input)
	  (*pos* 0))
      (try (parsed-value-action root)))))

(defun generate-parser (text)
  (let* ((*pos* 0)
	 (*text* text)
	 (rules (make-hash-table :test 'equal)))
    (loop for rule in (peg-rules (grammar)) do
	 (setf (gethash (parsed-value-value rule) rules) (parsed-value-action rule)))
    rules))

(defun dd ()
  (subseq *text* *pos*))

(defun grammar ()
  (let ((result (seq '((spacing) (many-plus (definition)) (endoffile)))))
    (when result
      (let ((definitions (valid-values (cdr result))))
	(make-peg
	 :root (car definitions) ;FIXME single rule grammars
	 :rules (loop for rule in definitions if (not (eql rule t)) 
		     collect rule))))))
;		   (setf (gethash (parsed-value-value rule) *rules*) (parsed-value-action rule))))))))

(defun generate-definition-rules (defs)
  (loop for def in defs do
       (format t "leftside: ~a -> rightside: ~a" (car (parsed-value-children def)) (cdr (parsed-value-children def)))))

(defun definition ()
  (let* ((init-pos *pos*)
	 (def-name (car (seq '((identifier) (cLEFT-ARROW)))))	 
	 (def-form (when def-name
		     (expression))))
    (unless def-form
      (setf *pos* init-pos)
      nil)    
    (when def-form (make-parsed-value :type 'definition :value (parsed-value-value def-name) :children def-form :action (parsed-value-action def-form)))))
    
    ;; (when def-form
    ;;   (setf (gethash (parsed-value-value def-name) *rules*) (parsed-value-action def-form))
    ;;   (make-parsed-value :type 'definition :value (parsed-value-value def-name) :children def-form :action (parsed-value-action def-form)))))


;;Expression <- Sequence (SLASH Sequence)*
;; seq1 / seq2 / seq3 / ... / seq4
;; try seq1, if not; expression(cdr)
;; (try '(lambda1)) retorna uma lista com um lambda de g-sequence
(defun expression () ;FIXME?
  ;;  (break)
  (let* ((sub-sequences (seq '((g-sequence) (many-star (seq ((cSLASH) (g-sequence)))))))
	 (valid-values (valid-values sub-sequences)) ;filter empty symbols from * and such
	 (useful-values (remove-if-not (lambda (p) (not (eql (parsed-value-value p) #\/))) valid-values))
	 (action-values (mapcar (lambda (v) (parsed-value-action v)) useful-values)))
    (when sub-sequences
      (make-parsed-value
       :type 'expression
       :action (lambda ()
		 (if
		  (> (length action-values) 1)
		  (choice action-values)
		  (applyf (car action-values))))))))

;; Sequence <- Prefix*
(defun g-sequence ()
  (let ((res (try '(many-star (prefix)))))
    (when (and res (not (eql t res)))
      (make-parsed-value
       :type 'sequence
       :value nil
       :children res
       :action (lambda ()
		 (if (> (length res) 1)
		     (filter-trues (seq (mapcar #'parsed-value-action res)))
		     (applyf (parsed-value-action (car res)))))))))


;;Prefix     <- (AND / NOT)? Suffix
;; &suffix / !suffix / suffix
(defun prefix ()
  (let* ((pre-value (seq '((zero-or-one (choice ((cAND) (cNOT)))) (suffix))))
	 (value (filter-trues pre-value)))  
    (when value
      (make-parsed-value
       :type 'prefix
       :children value
       :action (if (eql (length value) 1)
		   (parsed-value-action (car value)) ;sem predicado
		   (case (parsed-value-value (car value)) ;com predicado
		     (#\& (lambda () (predicate (parsed-value-action (cadr value)))))
		     (#\! (lambda () (not (predicate (parsed-value-action (cadr value))))))
		     (otherwise (error "invalid prefix"))))))))
	  


       
;;Suffix     <- Primary (QUESTION / STAR / PLUS)?
;; Primary? / Primary* / Primary+
(defun suffix ()
  (let* ((pre-value (seq '((primary) (zero-or-one (choice ((cquestion) (cstar) (cplus)))))))
	 (value (filter-trues pre-value)))
    (when value
      (make-parsed-value
       :type 'suffix
       :children value
       :action (case (length value)
		 (1 (parsed-value-action (car value)))
		 (otherwise (case (parsed-value-value (cadr value))
			      (#\? (lambda ()
				     (zero-or-one (parsed-value-action (car value)))))
			      (#\* (lambda () 
				     (many-star (parsed-value-action (car value)))))
			      (#\+ (lambda ()
				     (many-plus (parsed-value-action (car value))))))))))))

;;Primary    <- Identifier !LEFTARROW
;;		/ OPEN Expression CLOSE
;;		/ Literal / Class / DOT				       
(defun primary ()
  (let* ((parsed (choice '((seq ((identifier) (not-a (cLEFT-ARROW))))
			   (seq ((cOPEN) (expression) (cCLOSE)))
			   (literal)
			   (g-class)
			   (cDOT))))
	 (value (case (type-of parsed)
		  (cons (if (eql (parsed-value-value (car parsed)) #\( )
			    (cadr parsed)
			    (car parsed)))
       		  (otherwise parsed))))
    (when value
      (make-parsed-value
       :type 'primary
       :value parsed
       :action (case (parsed-value-type value)
		 (identifier (lambda ()
			       (let ((exp (try (fetch-rule-definition (parsed-value-value value)))))
				 (when exp
				   (list (parsed-value-value value) exp)))))
		 (expression (parsed-value-action value))
		 (match-string (parsed-value-value value))
		 (g-class 'g-class-action-here) ;TODO
		 (dot (lambda () (match (lambda (any) t))))
		 (literal (lambda () (match-string (parsed-value-value value))))
		 (t (format t "primary value type: ~a~%" (parsed-value-type value))))))))

(defun g-class ()
  (make-node
   '(seq ((match-char #\[)
	  (many-star (seq ((not-a (match-char #\]))
			   (range))))
	  (match-char #\])
	  (spacing)))
   'class))

;;lexical
(defun identifier ()
  (let* ((pre-this (seq '((ident-start) (many-star (ident-cont)) (spacing))))
	 (list-this (filter-trues (flatten pre-this)))
	 (this 
	  (unless (null list-this)
	    (string-trim spaces-list (coerce list-this 'string)))))
    (when this
      (make-parsed-value
       :type 'identifier
       :value this
       :action (lambda () ))))) ;;fixme empty lambda?

(defun ident-start ()
  (match #'alpha-char-p))

(defun ident-cont ()
  (choice '((ident-start) (digit))))

  ;;;  Literal    <- ['] (!['] Char)+ ['] Spacing                              ;;;                / ["] (!["] Char)+ ["] Spacing   
(defun literal ()
  (let* ((lit (choice
	       '((seq
		  ((optional (match-char #\'))
		   (many-plus
		    (seq
		     ((not-a (optional (match-char #\')))
		      (my-char))))		  
		   (optional (match-char #\'))
		   (spacing)))
		 (seq
		  ((optional (match-char #\"))
		   (many-plus
		    (seq
		     ((not-a (optional (match-char #\")))
		      (my-char))))
		   (optional (match-char #\"))
		   (spacing))))))

	 (lit-valids (when lit
		       (valid-values lit))) ;remove Ts
	 (lit-value (when lit-valids
		      (remove-if-not (lambda (c) (eql (type-of c) 'standard-char)) lit-valids))) ;remove non-chars
	 (lit-string (when lit-value
		       (coerce lit-value 'string))) ;list->string
	 (lit-nonspace (when lit-string (string-trim spaces-list lit-string)))
	 (unquoted-string (when lit-nonspace
			    (subseq lit-nonspace 1 (-(length lit-nonspace) 1))))) ;remove quotes
    (when unquoted-string
      (make-parsed-value :type 'literal :value unquoted-string :children lit-valids
			 :action (lambda () (match-string unquoted-string))))))
     
(defun range ()
  (choice '((seq ((my-char) (match-char #'\)) (my-char)))
	    (my-char))))

(defun my-char ()
  ;; Char       <- '\\' [nrt'"\[\]\\]
  ;;               / '\\u' HexPair HexPair                                                  ;;               / '\\' [0-2][0-7][0-7]
  ;;               / '\\' [0-7][0-7]?                                                     
  ;;               / !'\\' .      
  (choice `((seq ((match-char #\\) (match-char #\\) (match-char-list (nrt'\"\[\]\\))) 2)
	    (seq ((match-char #\\) (match-char #\\) (zero-to-two) (octal) (octal)) 2)
	    (seq ((match-char #\\) (match-char #\\) (octal) (predicate (octal))) 2)
	    (seq ((not-a (seq ((match-char #\\) (match-char #\\)))) (match ,(lambda (c) t))) 1))))

(defun cLEFT-ARROW ()
  (seq '((match-char #\<) (match-char #\-) (spacing))))
      
(defun cSLASH()
  (match-char-spacing #\/))

(defun cAND()
  (match-char-spacing #\&))

(defun cNOT()
  (match-char-spacing #\!))

(defun cQUESTION()
  (match-char-spacing #\?))

(defun cSTAR()
  (match-char-spacing #\*))

(defun cPLUS()
  (match-char-spacing #\+))

(defun cOPEN()
  (match-char-spacing #\())

(defun cCLOSE()
  (match-char-spacing #\)))

(defun cDOT()
  (match-char-spacing #\.))

(defun seq (funlist &optional pos)
  (let*
      ((init-pos *pos*)
       (this (try (car funlist)))
       (return-value
	(if
	 (null (cdr funlist))
	 (if this ;if the I'm the only element and I exist
	     (list this) ;make a 1-element list
	     nil)
	 (when this ;if the first element exists and there're more after
	   (let ((next (seq (cdr funlist))))
	     (if next ;next elt exists,recursively checked for whole list
		 (cons this next)
		 (progn
		   (setf *pos* init-pos)
		   nil)))))))
    (if (and pos return-value)
	(elt return-value pos) ;elt = element, takes the pos'th element
	return-value)))


(defun predicate (funform)
  (let ((init-pos *pos*)
	(res (applyf funform)))
    (setf *pos* init-pos)
    res))

(defun not-a (funform)
  (not (predicate funform)))

(defun choice (funlist)
  "prioritized choice, recursive - returns the first parsed-value matched"
  (if (null funlist)
      nil ;recursion end
      (let ((this (try (car funlist))))
	(if this
	    this
	    (choice (cdr funlist))))))

(defun many-star (funform)
  (let ((this (try funform)))
    (if this ;eu existo
	(let ((next (many-star funform)))
	  (cond
	    ((eql (car next) t) ;;proximo retornou "t", fim da estrela
	     (cons this '()))
	    (next ;;proximo existe e != t
	     (cons this next))))
	(cons t '())))) ;;nao existo, retorno t numa lista

(defun many-plus (funform)
  (let ((this (try funform)))
    (when this
      (let ((next (many-star funform)))
	(cond
	  ((eql (car next) t)
	   (cons this '()))
	  (next
	   (cons this next))
	  (t
	   (cons this '())))))))

(defun zero-or-one (funform)
  (let ((v (try funform)))
    (or v t)))

(defun digit ()
  (match #'digit-char-p))

(defun g-space ()
  (choice '((match-char-list (#\space #\tab)) (endofline))))

(defun zero-to-two ()
  (choice '((match-char #\0) (match-char #\1) (match-char #\2))))

(defun octal ()
  (choice '((match-char #\0)
	    (match-char #\1)
	    (match-char #\2)
	    (match-char #\3)
	    (match-char #\4)
	    (match-char #\5)
	    (match-char #\6)
	    (match-char #\7)
	    (match-char #\8))))

(defun spacing ()
  (many-star '(choice ((g-space) (comment)))))

(defun comment ()
  (when
      (seq `((match-char #\#)
	     (many-star (seq ((not-a (endofline))
			      (match ,(lambda (r) t)))))
	     (endofline)))
      (make-parsed-value :type 'comment :action (lambda () t))))

(defun endofline ()
  (choice
   '((seq ((match-char #\return) (match-char #\linefeed)))
     (match-char #\linefeed)
     (match-char #\return))))

(defun endoffile ()
  (>= *pos* (length *text*)))

(defun match (f)
  "calls a function f with the  the character at the current position as an argument. increases *pos* on success"
  (when
      (< *pos* (length *text*))
    (let ((character (char *text* *pos*)))
      (when
	  (funcall f character)
	(incf *pos*)
	character))))

(defun match-char (c)
  "matches a character at the current position"
  (when
      (< *pos* (length *text*))
    (let ((character (char *text* *pos*)))
      (when
	  (eql c character)
	(incf *pos*)
	character))))

(defun match-char-spacing (c)
  "matches a character followed by spacing"
  (let ((parsed-char (seq `((match-char ,c) (spacing)))))
    (when parsed-char
      (make-parsed-value
       :type 'char-spacing
       :value (car parsed-char)
       :action (lambda () (match-char-spacing c))))))

(defun match-char-list (cs)
  "matches any character on list cs, prioritized"
  (if
   (null cs)
   nil
   (let ((c (match-char (car cs))))
     (if c
	 c
	 (match-char-list (cdr cs))))))


(defun applyf (funform)
;;  (format t "funform: ~a~%" (type-of funform))
  (when funform
    (if
     (eql (type-of funform) 'cons)
     (apply (car funform) (cdr funform)) ;list with parameters
     (funcall funform)))) ;single lambda, just call


(defun mapply (funlist)
  (mapcar #'applyf funlist))

(defun try (funform)
  "applies a function with its arguments, resets pos if function returns nil
returns que function's return value (nil if it fails)"
  (let ((init-pos *pos*)
	(res (applyf funform)))
    (if res
	res
	(progn
	  (setf *pos* init-pos)
	  nil))))

(defun optional (funform)
  (try funform))

(defun match-string (str)
  (let* ((init-pos *pos*)
	 (loop-return
	    (loop for c across str
	       if (null (match-char c))
	       return nil
	       collect c)))
    (if loop-return
	loop-return
	(progn
	  (setf *pos* init-pos)
	  nil))))

(defun make-node (funform type &optional action)
  "tries a function, if succeeds, return a parsed-value with its results and type"
  (let ((res (try funform)))
    (when (and res (not (eql t res)))
      (make-parsed-value :type type :children res :action action))))

(defun flatten (L)
"Converts a list to single level."
    (if (null L)
        nil
        (if (atom (first L))
            (cons (first L) (flatten (rest L)))
            (append (flatten (first L)) (flatten (rest L))))))


(defun filter-trues (ls)
  (remove-if-not (lambda (c) (not (eql t c))) ls))

(defun valid-values (values)
  (remove-if-not (lambda (c) (not (eql t c))) (flatten values)))

(defun fetch-rule-definition (rule-name)
  (gethash rule-name *rules*))

;;(setf *rules* (parse calcpeg))
;; (defparameter formnumber (gethash "Number" *rules*))
(defun list-rules () (loop for r being the hash-keys in *rules* collect r))
