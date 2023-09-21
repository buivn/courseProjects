(define
(domain cook)
(:requirements :strips :typing :action-costs)
(:types location supply - object
	ingredient equipment manipulator - supply
        food drink - ingredient
	rice pasta - food
	tea instance-coffee water - drink
        cooking-spot drink-spot rice-holder tea-holder coffee-holder faucet initial-position - location
        pot kettle cup empty - equipment)

(:predicates
  (at ?s - supply ?l - location)
  (hold ?r - manipulator ?e - equipment)
  (inside ?ing - ingredient ?e - equipment)
  (food-ready ?f - food ?l - location)
  (hot ?w - water ?e - equipment)
  (drink-ready ?dr - drink ?l - location)
)

(:functions
  (total-cost)
)

(:action get-ingredient
  :parameters (?r - manipulator ?e - equipment ?l - location ?ing - ingredient ?emp - empty)
  :precondition (and 
    (hold ?r ?e) 
    (at ?r ?l)
    (at ?ing ?l)
    (not (hold ?r ?emp))
    )
  :effect (and 
		(inside ?ing ?e)
		(increase (total-cost) 1)
	  )
)

;(:action move-with-equipment
;  :parameters (?r - manipulator ?e - equipment ?from - location ?to - location)
;  :precondition (and 
;    			(at ?r ?from) 
;			(at ?e ?from)
;			(hold ?r ?e)
;      		)
;  :effect (and
;    (not (at ?r ?from))
;    (at ?r ?to)
;    (not (at ?e ?from))
;    (at ?e ?tp)
;    (increase (total-cost) 1)
;  )
;)

(:action move
  :parameters (?r - manipulator ?from - location ?to - location)
  :precondition (and
	(at ?r ?from)
	)
  :effect (and
	(not (at ?r ?from))
	(at ?r ?to) 
	)
)

(:action get-equipment
  :parameters (?r - manipulator ?e - equipment ?l - location ?emp - empty)
  :precondition (and 
    			(hold ?r ?emp) 
    			(at ?r ?l)
    			(at ?e ?l)
   		 )
  :effect ( and
    (not (hold ?r ?emp))
    (hold ?r ?e)
    (increase (total-cost) 3)
  )
)

(:action release-equipment
  :parameters (?r - manipulator ?e - equipment ?l - location ?emp - empty)
  :precondition (and 
    	(hold ?r ?e) 
    	(at ?r ?l)
    )
  :effect ( and
    (not (hold ?r ?e))
    (hold ?r ?emp)
    (increase (total-cost) 1)
  )
)

(:action pour-in
  :parameters (?r - manipulator ?e - equipment ?fau - faucet ?w - water)
  :precondition (and
	(hold ?r ?e)
	(at ?r ?fau)
	)
  :effect (and
	(inside ?w ?e)
	(increase (total-cost) 1)
	)
)

(:action cook
  :parameters (?r - manipulator ?p - pot ?cp - cooking-spot  ?ing - ingredient ?emp - empty ?w - water)
  :precondition (and 
    (hold ?r ?emp)
    (at ?r ?cp)
    (inside ?ing ?p)
    (inside ?w ?p)
    )
  :effect ( and
    (food-ready ?ing ?cp)
    (increase (total-cost) 5)
  )
)

(:action boil-water
  :parameters (?ke - kettle ?r - manipulator ?cp - cooking-spot ?w - water ?emp - empty)
  :precondition (and
	(at ?r ?cp)
	(at ?ke ?cp)
	(hold ?r ?emp)
	(inside ?w ?ke)
     )
  :effect (and
	(hot ?w ?ke)
	(increase (total-cost) 2)
    )
)

(:action pour-hot-water-for-tea
  :parameters (?r - manipulator ?ke - kettle ?cu - cup ?te - tea ?w - water ?ds - drink-spot)
  :precondition (and
	(at ?r ?ds)
	(at ?ke ?ds)
	(at ?cu ?ds)
	(hot ?w ?ke)
	(hold ?r ?ke)
	;(inside ?w ?ke)
	(inside ?te ?cu)
	) 
  :effect (and
	(drink-ready ?te ?ds)
	(increase (total-cost) 3)
	)
)



)


