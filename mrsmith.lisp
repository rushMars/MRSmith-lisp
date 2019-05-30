
;;; About this program
;;;
;;; This program should in the end be able to generate new sentences.
;;; The idea is that the program reads a full text and generates from this a table of which word could map to which, with a weight of how often those words follow each other.
;;; From this information it will then try to generate new sentences.
;;; I tried to work with a very rudimental scoring and generation system, as i would need a lot more time and research to make it more advanced.
;;; As the program is started it first generates a kind of a lexicon, the bigger and more versatile the text, the more it knows.
;;; It then uses the score to increase or decrease the weight of the follow up words, the higher (worse) the score, the more it randomizes the values.
;;; It uses a greedy search to generate a sentence, then prompts the user for an evaluation. There is no limit as to how good or bad the user judges the sentence.
;;; Just know that the higher the score, the wilder the randomizing function goes.
;;; A positive rating will make the program keep the randomized lexicon. A negative rating will make the program reuse the old version of the lexicon.
;;; I hoped that this way, it will slowly raise the weight of the good connections between words.
;;; To exit the program write 'exit' when prompted.

;;; There is a lot that can be improved on. First, the search could work with the weight of the word itself, not only of its 'follow up' words. It could also need some kind of loop prevention
;;; The chaos function could change more than only weights, it could add new words as leafs to lexicon entries or remove some.
;;; Also i wish, there was rather some merging between lexicon generations, rather than total replacement.
;;; There are many things more i could add, for example implement conversation, where the program tries to reuse knowledge optained from sentences the user wrote to him.
;;; For now i am somewhat happy with the code and glad i learned a bit of lisp on the way. It is an amazing language.


;; load gui library
(load "/home/mars/ltk/ltk")
(use-package :ltk)
(load "~/quicklisp/setup.lisp")
(ql:quickload "cl-utilities")
(use-package :cl-utilities)
 
;;; variable that later holds the full set of relations between words
;;; i call it lexicon
;;; layout: ( word count is-initial ((next-word weigth) (other-word weight)))
(defvar word-map '())
;;; variable that keeps track of an acceptable sentence length
(defvar avg-sentence-length 8)
;;; a score that should vary depending on the programs performance
(defvar score 5)

(defun find-in-map (word map occurence)
  "finds place in list where item occurred, either returns -1 or the index of the item"
  (let ((x (+ 1 occurence)))
    (cond
     ((NULL map) -1)
     ((null word) -1)
     ((string-equal word (car (car map))) x)
     (t (find-in-map word (cdr map) x))
     )
    )
  )

(defun return-node (lexicon word)
  "finds and returns the lexicon entry of a word, returns nil if not found"
  (cond
   ((string-equal word (car(car lexicon))) (car lexicon))
   ((null (cdr lexicon)) nil)
   (T (return-node (cdr lexicon) word))
   )
  )

(defun nodep (lexicon word)
  (cond
   ((null (return-node lexicon word)) nil)
   (T T)
   )
  )

(defun return-node-leafs (lexicon word)
  "returns the children of a node, if not in lexicon, return nil"
  (cond
   ((null (return-node lexicon word)) nil)
   (T  (nth 3 (return-node lexicon word)))
   )
  )

(defun print-node (lexicon node)
  "prints a node prettily"
  (print (mapcar #'car (nth 3 node)))
  (print " |")
  (print (car node))
  (return-from print-node lexicon)
  )

(defun print-lex (lexicon)
  "prints the lexicon nicely"
  (cond
   ((null lexicon) (print "----------------------------------"))
   ((null (return-node-leafs lexicon (car (car lexicon)))) (print (car (car lexicon))))
   (T (print-lex (cdr (print-node (car lexicon) lexicon))))
   )
  )

(defun update-leafs (leafs word-after nu-leafs)
  "updates the children of a lexicon entry"
  (cond
   ((null leafs) nu-leafs)
   ((string-equal word-after (car (car leafs))) (update-leafs (cdr leafs) word-after (append nu-leafs (list (list word-after (+ 1 (nth 1 (car leafs)))))) ))
   (T (update-leafs (cdr leafs) word-after (if (null (cdr leafs)) (append nu-leafs (list (car leafs)) (list (list word-after 1))) (append nu-leafs (list (car leafs))))))
   )
  )

(defun update-lex (word next-word lexicon new-lex initial count)
  "updates entries of the lexicon"
  (cond
   ((null lexicon) new-lex)
   ((string-equal word (car (car lexicon))) (update-lex word next-word (cdr lexicon) (append new-lex (list (list word (+ count (nth 1 (car lexicon))) (if (nth 2 (car lexicon)) t initial) (update-leafs (nth 3 (car lexicon)) next-word '() )))) initial count))
   (T (update-lex word next-word (cdr lexicon) (append new-lex (list (car lexicon))) initial count))
   )
  )

(defun eval-sentence (sentence lexicon iter)
  "takes one sentence in the form as a list and adds the words to lexicon"
  (let ((occ (nodep lexicon (car sentence)))
	(initial (if (= iter 0) t nil))
	(word (car sentence))
	(next-word (nth 1 sentence)))
    (cond
     ((NULL sentence) lexicon)
     ((null occ) (eval-sentence (cdr sentence) (append (list (list word 1 initial (if (not (null next-word)) (list (list next-word 1)) '() ))) lexicon) (+ 1 iter)))
     (T (eval-sentence (cdr sentence) (update-lex word next-word lexicon '() initial 1) (+ 1 iter)))
     )
    )
  )

(defun random-pick (list)
  "return random element from a list"
  (nth (random (list-length list) (make-random-state t)) list)
  )

(defun random-sentence (lex)
  "creates a new sentence from lexicon"
  (let ((sent '()) (current-word) (next-word))
    (loop
     (setq current-word (random-pick lex))
     (when (nth 2 current-word) (return current-word))
     )
    (setq sent (list (car current-word)))
    (loop
     (if (not (null (nth 3 current-word))) (setq next-word  (car (random-pick (nth 3 current-word)))) (setq next-word nil) )
     
     (when (or (> (list-length sent) (- avg-sentence-length 1)) (null next-word))
       (return sent)
       )
     
     (setq sent (append sent (list next-word)))
     (if (not(null next-word)) (setq current-word (nth (find-in-map next-word lex -1) lex)))  
     )
    (return-from random-sentence sent)
    )
  )

(defun pick-by-weight (lex entry order)
  (cond
   ((null lex) entry)
   ((< 0 order) (if (< (nth 1 entry)(nth 1 (car lex))) (pick-by-weight (cdr lex) (car lex) order) (pick-by-weight (cdr lex) entry order)))
   ((> 0 order) (if (> (nth 1 entry)(nth 1 (car lex))) (pick-by-weight (cdr lex) (car lex) order) (pick-by-weight (cdr lex) entry order)))
   )
  )

(defun reduce-lex (lex parent leafs new-lex)
  (cond
   ((null leafs) new-lex)
   (T (reduce-lex lex parent (cdr leafs) (append new-lex (list (return-node lex (car (car leafs)))))))
   )
  )

(defun informed-sentence (oldlex lex inter-result last-entry iter)
  (let ((cur-entry)(next-word))
    (setq cur-entry (pick-by-weight lex (car lex) 1))
    (if (not (null last-entry))(setq next-word (car (pick-by-weight (nth 3 last-entry) (car (nth 3 last-entry)) 1))))
    (cond
     ((or (= avg-sentence-length (list-length inter-result))
	  (and (null (nth 3 cur-entry)) (< 0 iter)))
      (append inter-result (list (car cur-entry))))
     
     ((and (null inter-result) (not (nth 2 cur-entry)))
      (informed-sentence oldlex (update-lex (car cur-entry) (car (car (nth 3 cur-entry))) lex '() nil -1) inter-result cur-entry iter))
     
     (T (informed-sentence oldlex (reduce-lex oldlex (car cur-entry) (nth 3 cur-entry) '()) (append inter-result (list (car cur-entry))) (return-node lex next-word) (+ 1 iter)))
     )
    )
  )

(defun change-weights (list strength)
  (cond
   ((null list) nil)
   (t (append (list (list (car (car list)) (+ (- (random strength (make-random-state t)) (floor (/ strength 2))) (nth 1 (car list))))) (change-weights (cdr list) strength) ))
   )
  )

(defun chaos (lex scor)
  "wreaks more havoc the worse the score"
  (cond
   ((null lex) nil)
   (t (append (list (list (nth 0 (car lex)) (nth 1 (car lex)) (nth 2 (car lex)) (change-weights (nth 3 (car lex)) scor )))  (chaos (cdr lex) scor)))
   )
  )

(defun subroutine(lexicon interlex)
  (let ((input)(lex lexicon)(ilex interlex))
    (loop
     (print '(this is the subroutine))
     (return)
     (when (numberp input)
       (if (> score -1) (- score input))
       (cond
	((< 0 input) (setq lex ilex))
	(t nil)
	)
       )
     )
    )
  )

(defun send-button (input lex)
  
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                        main program                                           ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(trace eval-sentence update-lex update-leafs)
(with-ltk ()
	  (let* ((result)(input "test")(inputs)(lex word-map)(interlex)(line-height 5)(scroll-height 400)
		(f (make-instance 'frame))
		
		(sc (make-instance 'scrolled-canvas))
		(c (canvas sc))
		(texto (create-text c 5 line-height "Be nice to baby bot Smithy"))
		(e (make-instance 'entry
				  :master f))
		(b (make-instance 'button 
				  :master f
				  :text "Send"
				  :command (lambda ()
					     (when T
					       (setq input (text e))
					       (setq line-height (+ line-height 15))
					       (create-text c 5 line-height input)
					       (setq lex (eval-sentence (split-sequence #\space input) lex 0))
					       (setq result (informed-sentence lex lex '() nil 0))
					       (setq line-height (+ line-height 15))
					       (create-text c 5 line-height result)
					       (if (> (+ 30 line-height) scroll-height) (setq scroll-height (+ scroll-height 35)))
					       (scrollregion c 0 0 0 scroll-height)
					       
						   )
					     )))
		)
	    (pack f)
	    (pack e :side :left)
	    (pack b :side :left)
	    (pack sc :expand 1 :fill :both)
	    (configure c :background 'white) 
	    (scrollregion c 0 0 0 scroll-height)
	    ;(loop
	     ;(setq input (read-line))
	     ;(setq inputs (split-sequence #\space input))
	     ;(cond
	     ; ((string-equal input "") )
	     ; ((string-equal input "print") (print-lex lex))
	     ; ((string-equal input "exit") (return nil))
	     ; ((string-equal input "op") (subroutine lex interlex))
	     ; (T (when T 
		;   (setq lex (eval-sentence inputs lex 0))
		 ;  (print (random-sentence lex)) 
		  ; )
		 ;)
	      ;)
	     ;)
	    )
	  )
