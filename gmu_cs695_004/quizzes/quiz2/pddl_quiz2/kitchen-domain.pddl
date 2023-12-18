(define
(domain kitchen)
(:requirements :strips :typing :action-costs)
(:types location supply - object
        cooking-facility manipulator - supply
        ingredient equipment - cooking-facility 
        left-manip right-manip - manipulator
        food drink - ingredient
        rice pasta - food
        tea coffee water - drink
        cooking-spot drink-spot rice-holder tea-holder coffee-holder faucet cooking-shelf res-location table - location
        pot coffee-mud kettle bowl - equipment)

(:predicates
  (at ?s - supply ?l - location)
  (on ?cf - cooking-facility ?l - location)
  (hold ?r - manipulator ?cf - cooking-facility)
  (inside ?ing - ingredient ?e - equipment)
  (fd-ready ?ing - ingredient)
  (hot ?w - water)
  (empty-hand ?r - manipulator)
  (occupied ?l - location ?e - equipment))

(:functions
  (total-cost)
)

(:action move
  :parameters (?r - manipulator ?from - location ?to - location)
  :precondition (and
        (at ?r ?from)
        )
  :effect (and
        (not (at ?r ?from))
        (at ?r ?to)
        (increase (total-cost) 3)
        )
)

(:action get-equipment
  :parameters (?r - manipulator ?e - equipment ?l - location)
  :precondition (and
                        (empty-hand ?r)
                        (at ?r ?l)
                        (on ?e ?l)
                 )
  :effect ( and
    (hold ?r ?e)
    (not (empty-hand ?r))
    (not (on ?e ?l))
    (increase (total-cost) 1)
  )
)

(:action release-equipment
  :parameters (?r - manipulator ?e - equipment ?l - location)
  :precondition (and
        (hold ?r ?e)
        (at ?r ?l)
    )
  :effect ( and
    (not (hold ?r ?e))
    (empty-hand ?r)
    (on ?e ?l)
    (increase (total-cost) 1)
  )
)

(:action put-ingredient-in
  :parameters (?r - manipulator ?e - equipment ?ing - ingredient ?l - location)
  :precondition (and
        (empty-hand ?r)
        (at ?r ?l)
        (on ?ing ?l)
        (on ?e ?l)
        )
  :effect (and
        (inside ?ing ?e)
        (empty-hand ?r)
        (not (at ?ing ?l))
        (increase (total-cost) 1)
        )
)

(:action pour-water-in
  :parameters (?r - manipulator ?e - equipment ?fau - faucet ?w - water)
  :precondition (and
        (hold ?r ?e)
        (at ?r ?fau)
        (at ?w ?fau)
        )
  :effect (and
        (inside ?w ?e)
        (not (at ?w ?fau))
        (increase (total-cost) 1)
        )
)

(:action cook-rice
  :parameters (?p - pot ?cp - cooking-spot  ?ing - ingredient ?w - water)
  :precondition (and
    (on ?p ?cp)
    (inside ?ing ?p)
    (inside ?w ?p)
    )
  :effect ( and
    (fd-ready ?ing)
    (increase (total-cost) 3)
  )
)


(:action stirring-coffee-done
  :parameters (?cu - coffee-mud  ?coff - coffee ?w - water)
  :precondition (and
    (and (hot ?w) (inside ?w ?cu))
    (inside ?coff ?cu)
    )
  :effect ( and
    (fd-ready ?coff)
    (increase (total-cost) 0)
  )
)


(:action pour-hotwater-into
  :parameters (?r - manipulator ?ke - kettle ?cu - coffee-mud ?l - location ?w - water)
  :precondition (and
        (hold ?r ?ke)
        (at ?r ?l)
        (on ?cu ?l)
        (inside ?w ?ke)
        (hot ?w)
        )
  :effect (and
        (inside ?w ?cu)
        (not (inside ?w ?ke))
        (increase (total-cost) 2)
        )
)

(:action boil-water
  :parameters (?ke - kettle ?cp - cooking-spot ?w - water)
  :precondition (and
        (on ?ke ?cp)
        (inside ?w ?ke)
     )
  :effect (and
        (hot ?w)
        (inside ?w ?ke)
        (increase (total-cost) 2)
    )
)

)