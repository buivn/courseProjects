(define
(domain space)
(:requirements :strips :typing :action-costs)
(:types location locatable - object
	ship key vault - locatable
	red-key blue-key purple-key - key
	red-vault blue-vault purple-vault - vault)

(:predicates
  (at ?l - locatable ?p - location)
  (on-ship ?k - key ?s - ship)
  (adjacent ?a - location ?b - location)
  )

(:functions
  (distance ?a - location ?b - location)
  (total-cost))

(:action travel
  :parameters (?s - ship ?from - location ?to - location)
  :precondition (and
    (at ?s ?from)
    (adjacent ?from ?to))
  :effect (and
    (not (at ?s ?from))
    (at ?s ?to)
    (increase (total-cost) (distance ?from ?to))
    ))
)
