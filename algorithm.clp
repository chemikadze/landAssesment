(deftemplate land
  (slot name (default ?NONE) (type STRING))
  (slot water (default ?NONE) (type SYMBOL))
  (slot soil (default ?NONE) (type SYMBOL))
  (slot surface (default ?NONE) (type SYMBOL))
  (slot humus (default ?NONE) (type SYMBOL))
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
  
(deftemplate land-minimum-candidate
  (slot name (default ?NONE) (type STRING))
  (slot coef (default ?NONE) (type FLOAT)))



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
        (coef (+ (/ (+ ?water ?soil ?surface) 3) 
                 (/ (+ ?humus ?underlayment) 2)) ))))


(defrule constant-replace
   ?m <- (metric ?name&:(stringp ?name) ?category ?type) 
   (mapping ?category&:(symbolp ?category) ?type&:(symbolp ?type) ?value&:(floatp ?value))
   => (retract ?m) (assert (metric ?name ?category ?value)))

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


(defrule gen-minimum-candidate
  ?land <- (land-coef (name ?name) (coef ?coef))
  => (assert (land-minimum-candidate (name ?name) (coef ?coef))))

(defrule reduce-minimum
  ?land1 <- (land-minimum-candidate (coef ?c1))
  ?land2 <- (land-minimum-candidate (coef ?c2))
  (test(neq ?land1 ?land2))
  (test(>= ?c1 ?c2))
  =>
  (retract ?land1))







