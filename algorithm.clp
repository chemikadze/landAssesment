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
  (metric ?name&:(stringp ?name) water ?water&:(floatp ?water))
  (metric ?name soil ?soil&:(floatp ?soil))
  (metric ?name surface ?surface&:(floatp ?surface))
  (metric ?name humus ?humus&:(floatp ?humus))
  (metric ?name underlayment ?underlayment&:(floatp ?underlayment))
  =>
  (assert 
    (land-digits
      (name ?name)
      (water ?water)
      (soil ?soil)
      (surface ?surface)
      (humus ?humus)
      (underlayment ?underlayment))))

(defrule land-reduce-metrics
  (land-digits
    (name ?name)
    (water ?water)
    (soil ?soil)
    (surface ?surface)
    (humus ?humus)
    (underlayment ?underlayment))
  =>
  (assert 
    (land-coef 
      (name ?name)
        (coef (+ (/ (+ ?water ?soil ?surface) 3) 
                 (/ (+ ?humus ?underlayment) 2)) ))))

;; ruleset for characteristics:
;;   water, soil, surface, humus, underlayment

;; water rules
(defrule water-throughout
  (metric ?name&:(stringp ?name) water throughout) 
  => (assert (metric ?name water 0.75)))

(defrule water-nothoughout
  (metric ?name&:(stringp ?name) water nothroughout) 
  => (assert (metric ?name water 0.65)))

(defrule water-dry
  (metric ?name&:(stringp ?name) water dry) 
  => (assert (metric ?name water 0.2)))

;; soil rules
(defrule soil-big
  (metric ?name&:(stringp ?name) soil big) 
  => (assert (metric ?name soil 0.3)))

(defrule soil-nut
  (metric ?name&:(stringp ?name) soil nut) 
  => (assert (metric ?name soil 0.25)))

(defrule soil-tiles
  (metric ?name&:(stringp ?name) soil tiles) 
  => (assert (metric ?name soil 0.75)))

;; surface
(defrule surface-granular
  (metric ?name&:(stringp ?name) surface granular) 
  => (assert (metric ?name surface 0.15)))

(defrule surface-corticial
  (metric ?name&:(stringp ?name) surface corticial) 
  => (assert (metric ?name surface 0.7)))

(defrule surface-cracked
  (metric ?name&:(stringp ?name) surface cracked) 
  => (assert (metric ?name surface 0.8)))

;; humus
(defrule humus-thick
  (metric ?name&:(stringp ?name) humus thick) 
  => (assert (metric ?name humus 0.95)))

(defrule humus-mthick
  (metric ?name&:(stringp ?name) humus mthick) 
  => (assert (metric ?name humus 0.8)))

(defrule humus-mthin
  (metric ?name&:(stringp ?name) humus mthin) 
  => (assert (metric ?name humus 0.5)))

(defrule humus-thin
  (metric ?name&:(stringp ?name) humus thin) 
  => (assert (metric ?name humus 0.15)))

;; underlayment
(defrule underlayment-thick
  (metric ?name&:(stringp ?name) underlayment thick) 
  => (assert (metric ?name underlayment 0.85)))

(defrule underlayment-medium
  (metric ?name&:(stringp ?name) underlayment medium) 
  => (assert (metric ?name underlayment 0.5)))

(defrule underlayment-thin
  (metric ?name&:(stringp ?name) underlayment thin) 
  => (assert (metric ?name underlayment 0.1)))








