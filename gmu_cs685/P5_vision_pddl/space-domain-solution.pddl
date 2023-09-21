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

(:action beam-up-key
  :parameters (?s - ship ?l - location ?k - key)
  :precondition (and
    (at ?s ?l)
    (at ?k ?l))
  :effect (and
    (not (at ?k ?l))
    (on-ship ?k ?s)
    (increase (total-cost) 1)
    ))

(:action unlock-red-vault
  :parameters (?s - ship ?l - location ?rv - vault ?rk - red-key)
  :precondition (and
    (at ?s ?l)
    (at ?rv ?l)
    (on-ship ?rk ?s))
  :effect (and
    (at ?rk ?l)
    (not (on-ship ?rk ?s))
    (increase (total-cost) 1)
    ))

(:action unlock-blue-vault
  :parameters (?s - ship ?l - location ?bv - vault ?bk - blue-key)
  :precondition (and
    (at ?s ?l)
    (at ?bv ?l)
    (on-ship ?bk ?s))
  :effect (and
    (at ?bk ?l)
    (not (on-ship ?bk ?s))
    (increase (total-cost) 1)
    ))
(:action unlock-purple-vault
  :parameters (?s - ship ?l - location ?pv - vault ?pk - purple-key)
  :precondition (and
    (at ?s ?l)
    (at ?pv ?l)
    (on-ship ?pk ?s))
  :effect (and
    (at ?pk ?l)
    (not (on-ship ?pk ?s))
    (increase (total-cost) 1)
    ))

)
