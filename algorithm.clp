(defglobal ?*low-priority* = -5)

(deftemplate land
  (slot name (default ?NONE) (type STRING))
  (slot water (default ?NONE) (type SYMBOL))
  (slot soil (default ?NONE) (type SYMBOL))
  (slot surface (default ?NONE) (type SYMBOL))
  (slot humus (default ?NONE) (type SYMBOL))
  (slot square (default ?NONE) (type FLOAT))
  (slot underlayment (default ?NONE) (type SYMBOL)))

(deftemplate land-digits
  (slot name (default ?NONE) (type STRING))
  (slot water (default ?NONE) (type FLOAT))
  (slot soil (default ?NONE) (type FLOAT))
  (slot surface (default ?NONE) (type FLOAT))
  (slot humus (default ?NONE) (type FLOAT))
  (slot underlayment (default ?NONE) (type FLOAT)))

(deftemplate land-coef
  (slot name (default ?NONE) (type STRING))
  (slot coef (default ?NONE) (type FLOAT)))
  
(deftemplate land-maximum-candidate
  (slot name (default ?NONE) (type STRING))
  (slot berry-of-choice (default ?NONE) (type STRING))
  (slot profit (default ?NONE) (type FLOAT))
  (slot checked (default FALSE) (type SYMBOL)))
  
(deftemplate berry 
  (slot name (default ?NONE) (type STRING))
  (slot raw-cost (default ?NONE) (type FLOAT))
  (slot jam-cost (default ?NONE) (type FLOAT))
  (slot fruit-coef (default ?NONE) (type FLOAT))
  (slot leaves-coef (default ?NONE) (type FLOAT))
  (slot raw-pdk (default ?NONE) (type FLOAT))
  (slot leaves-pdk (default ?NONE) (type FLOAT))
  (slot leaves-pdk-reduction (default ?NONE) (type FLOAT)))

(deftemplate berry-land
  (slot name (default ?NONE) (type STRING))
  (slot land-name (default ?NONE) (type STRING))
  (slot leaves-pollution (default ?NONE) (type FLOAT))
  (slot fruit-pollution (default ?NONE) (type FLOAT))
  (slot amount (default -1.0) (type FLOAT)))

(deftemplate berry-profit
  (slot name (default ?NONE) (type STRING))
  (slot land-name (default ?NONE) (type STRING))
  (slot profit (default ?NONE) (type FLOAT)))



;; basic formulas
(deffunction pollution-coefficient
  (?water ?soil ?surface ?humus ?underlayment)
  (+ (/ (+ ?water ?soil ?surface) 3) 
                 (/ (+ ?humus ?underlayment) 2)))
                 
(deffunction guard
  (?condition ?value)
  (if (eq ?condition TRUE)
   then ?value
   else 0.0))

;; generic mapping rules
(defrule land-metric-defs
  (land 
    (name ?name)
    (water ?water)
    (soil ?soil)
    (surface ?surface)
    (humus ?humus)
    (underlayment ?underlayment))
  =>
  (assert (metric ?name water ?water))
  (assert (metric ?name soil ?soil))
  (assert (metric ?name surface ?surface))
  (assert (metric ?name humus ?humus))
  (assert (metric ?name underlayment ?underlayment)))

(defrule reduce-metrics
  ?namem <- (metric ?name&:(stringp ?name) water ?water&:(floatp ?water))
  ?soilm <- (metric ?name soil ?soil&:(floatp ?soil))
  ?surfacem <- (metric ?name surface ?surface&:(floatp ?surface))
  ?humusm <- (metric ?name humus ?humus&:(floatp ?humus))
  ?underlaymentm <- (metric ?name underlayment ?underlayment&:(floatp ?underlayment))
  =>
  (retract ?namem ?soilm ?surfacem ?humusm ?underlaymentm)
  (assert 
    (land-digits
      (name ?name)
      (water ?water)
      (soil ?soil)
      (surface ?surface)
      (humus ?humus)
      (underlayment ?underlayment))))

(defrule land-reduce-metrics
  ?digits <- (land-digits
    (name ?name)
    (water ?water)
    (soil ?soil)
    (surface ?surface)
    (humus ?humus)
    (underlayment ?underlayment))
  =>
  (retract ?digits)
  (assert 
    (land-coef 
      (name ?name)
      (coef (pollution-coefficient ?water ?soil ?surface ?humus ?underlayment)))))

(defrule constant-replace
   ?m <- (metric ?name&:(stringp ?name) ?category ?type) 
   (mapping ?category&:(symbolp ?category) ?type&:(symbolp ?type) ?value&:(floatp ?value))
   => (retract ?m) (assert (metric ?name ?category ?value)))

(defrule bind-berry-land
  (land-coef (name ?name) (coef ?coef))
  (berry (name ?bname)
              (fruit-coef ?fcoef) (leaves-coef ?lcoef))
  => (assert (berry-land 
               (name ?bname) (land-name ?name) 
               (leaves-pollution (* ?fcoef ?coef))
               (fruit-pollution (* ?lcoef ?coef)))))


;;
;; rules to detect berry perspectives
;;
(defrule berry-perspective-amount-reduction
  ?binding <- (berry-land (name ?bname) (land-name ?lname) (leaves-pollution ?lpollut) (amount -1.0))
  (berry (name ?bname) (leaves-pdk ?lpdk) (leaves-pdk-reduction ?reduction))
  (land (name ?lname) (square ?square))
  (test (> ?lpollut ?lpdk))
  =>
  (modify ?binding (amount (* ?square ?reduction))))

(defrule berry-perspective-amount-all
  ?binding <- (berry-land (name ?bname) (land-name ?lname) (leaves-pollution ?lpollut) (amount -1.0))
  (berry (name ?bname) (leaves-pdk ?lpdk) (leaves-pdk-reduction ?reduction))
  (land (name ?lname) (square ?square))
  (test (<= ?lpollut ?lpdk))
  =>
  (modify ?binding (amount (* ?square 1))))
                      
(defrule berry-perspective-all
  ?binding <- (berry-land 
    (name ?bname) (land-name ?lname) (fruit-pollution ?fpollut) (amount ?amount))
  (berry (name ?bname) 
    (raw-cost ?rcost) (jam-cost ?jcost) (raw-pdk ?pdk))
  (land (name ?lname) (square ?square))
  (test (< ?fpollut ?pdk))
  (test (neq ?amount -1.0))
  =>
  (retract ?binding)
  (assert (berry-profit 
            (name ?bname) (land-name ?lname)
            (profit (max (* ?rcost ?amount) (* ?jcost ?amount))))))                     

(defrule berry-perspective-jam-only
  ?binding <- (berry-land 
    (name ?bname) (land-name ?lname) (fruit-pollution ?fpollut) (amount ?amount))
  (berry (name ?bname) 
    (raw-cost ?rcost) (jam-cost ?jcost) (raw-pdk ?pdk))
  (land (name ?lname) (square ?square))
  (test (neq ?amount -1.0))
  (test (> ?fpollut ?pdk))
  (test (<= (/ ?fpollut 2.0) ?pdk))
  =>
  (retract ?binding)
  (assert (berry-profit 
            (name ?bname) (land-name ?lname)
            (profit (* ?jcost ?amount)))))                      

(defrule berry-perspective-no
  ?binding <- (berry-land 
    (name ?bname) (land-name ?lname) (fruit-pollution ?fpollut) (amount ?amount))
  (berry (name ?bname) 
    (raw-cost ?rcost) (jam-cost ?jcost) (raw-pdk ?pdk))
  (land (name ?lname) (square ?square))
  (test (neq ?amount -1.0))
  (test (> (* ?fpollut 2.0) ?pdk))
  =>
  (retract ?binding)
  (assert (berry-profit 
            (name ?bname) (land-name ?lname)
            (profit 0.0))))


(defrule choose-berries
  ?b1 <- (berry-profit (name ?name1) (land-name ?lname) (profit ?profit1))
  ?b2 <-(berry-profit (name ?name2) (land-name ?lname) (profit ?profit2))
  (test(>= ?profit1 ?profit2))
  =>
  (retract ?b1 ?b2)
  (assert (land-maximum-candidate (name ?lname) (berry-of-choice ?name1) (profit ?profit1))))

(defrule reduce-maximum-candidate
  ?land1 <- (land-maximum-candidate (profit ?p1) (name ?name) (berry-of-choice ?berry))
  ?land2 <- (land-maximum-candidate (profit ?p2) (name ?name))
  (test(neq ?land1 ?land2))
  (test(< ?p1 ?p2))
  =>
  (retract ?land1))

(defrule select-maximum-candidate
  (declare (salience ?*low-priority*))
  ?land1 <- (land-maximum-candidate (profit ?p1) (checked FALSE) (name ?name1) (berry-of-choice ?berry))
  ?land2 <- (land-maximum-candidate (profit ?p2) (checked FALSE) (name ?name2))
  (test(neq ?land1 ?land2))
  (test(neq ?name1 ?name2))
  (test(< ?p1 ?p2))
  =>
  (retract ?land1)
  (assert (land-maximum-candidate (profit ?p1) (checked TRUE) (name ?name1) (berry-of-choice ?berry))))


;; ruleset for characteristics:
;;   water, soil, surface, humus, underlayment
(deffacts type-mappings
  (mapping water throughout 0.75)
  (mapping water nothroughout 0.65)
  (mapping water dry 0.2)
  (mapping soil big 0.3)
  (mapping soil nut 0.25)
  (mapping soil tiles 0.75)
  (mapping surface granular 0.15)
  (mapping surface corticial 0.7)
  (mapping surface cracked 0.8)
  (mapping humus thick 0.95)
  (mapping humus mthick 0.8)
  (mapping humus mthin 0.5)
  (mapping humus thin 0.15)
  (mapping underlayment thick 0.85)
  (mapping underlayment medium 0.5)
  (mapping underlayment thin 0.1))
  

(deffacts berries
  (berry 
  	(name "strawberry")
	(raw-cost 500.0) (jam-cost 250.0)
	(fruit-coef 0.3) (leaves-coef 0.35)
	(raw-pdk 0.2) (leaves-pdk 0.25)
	(leaves-pdk-reduction 0.0))
  (berry 
  	(name "currant")
	(raw-cost 400.0) (jam-cost 300.0)
	(fruit-coef 0.15) (leaves-coef 0.45)
	(raw-pdk 0.5) (leaves-pdk 0.4)
	(leaves-pdk-reduction 0.5)))
