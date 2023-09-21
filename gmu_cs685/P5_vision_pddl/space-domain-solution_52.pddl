(define
(domain space)
(:requirements :strips :typing :action-costs)
(:types location locatable - object
	ship key vault - locatable
	red-key blue-key purple-key - key
	red-vault blue-vault purple-vault - vault)

(:predicates
  (at ?l - locatable ?px - location ?py - location)
  (on-ship ?k - key ?s - ship)
  (adjacent ?ax - location ?ay - location ?bx - location ?by - location)
  )

(:functions
  (distance ?ax - location ?ay - location ?bx - location ?by - location)
  (total-cost))

(:action travel
  :parameters (?s - ship ?from-x - location ?from-y - location ?to-x - location ?to-y - location)
  :precondition (and
    (at ?s ?from-x ?from-y)
    (adjacent ?from-x ?from-y ?to-x ?to-y))
  :effect (and
    (not (at ?s ?from-x ?from-y))
    (at ?s ?to-x ?to-y)
    (increase (total-cost) (distance ?from-x ?from-y ?to-x ?to-y))
    ))

(:action beam-up-key
  :parameters (?s - ship ?l-x - location ?l-y - location ?k - key)
  :precondition (and
    (at ?s ?l-x ?l-y)
    (at ?k ?l-x ?l-y))
  :effect (and
    (not (at ?k ?l-x ?l-y))
    (on-ship ?k ?s)
    (increase (total-cost) 1)
    ))

(:action unlock-red-vault
  :parameters (?s - ship ?l-x - location ?l-y - location ?rv - vault ?rk - red-key)
  :precondition (and
    (at ?s ?l-x ?l-y)
    (at ?rv ?l-x ?l-y)
    (on-ship ?rk ?s))
  :effect (and
    (at ?rk ?l-x ?l-y)
    (not (on-ship ?rk ?s))
    (increase (total-cost) 1)
    ))

(:action unlock-blue-vault
  :parameters (?s - ship ?l-x - location ?l-y - location ?bv - vault ?bk - blue-key)
  :precondition (and
    (at ?s ?l-x ?l-y)
    (at ?bv ?l-x ?l-y)
    (on-ship ?bk ?s))
  :effect (and
    (at ?bk ?l-x ?l-y)
    (not (on-ship ?bk ?s))
    (increase (total-cost) 1)
    ))
(:action unlock-purple-vault
  :parameters (?s - ship ?l-x - location ?l-y - location ?pv - vault ?pk - purple-key)
  :precondition (and
    (at ?s ?l-x ?l-y)
    (at ?pv ?l-x ?l-y)
    (on-ship ?pk ?s))
  :effect (and
    (at ?pk ?l-x ?l-y)
    (not (on-ship ?pk ?s))
    (increase (total-cost) 1)
    ))

)
