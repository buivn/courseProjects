(define
(domain space)
(:requirements :strips :typing :action-costs)
(:types location locatable - object
        ship supply - locatable
        plasmaconduit plasmainjector warpcoil dilithium medicalsupply warpdrive - supply)

(:predicates
  (at ?l - locatable ?p - location)
  (adjacent ?a - location ?b - location)
  (on-ship ?l - locatable ?pc - supply)
  (warp-drive-ready ?s - ship ?wd - warpdrive))

(:functions
  (distance ?a - location ?b - location)
  (total-cost)
  (warp-drive-active))

(:action travel-impulse-speed
  :parameters (?s - ship ?from - location ?to - location)
  :precondition (and
    (at ?s ?from)
    (adjacent ?from ?to))
  :effect (and
    (not (at ?s ?from))
    (at ?s ?to)
    (increase (total-cost) (distance ?from ?to))
    ))

(:action travel-warp-speed
  :parameters (?s - ship ?from - location ?to - location ?wd - warpdrive)
  :precondition (and
    (at ?s ?from)
    (adjacent ?from ?to)
    (warp-drive-ready ?s ?wd)
    )
  :effect (and
    (not (at ?s ?from))
    (at ?s ?to)
    (increase (total-cost) (warp-distance ?from ?to))
    ))

(:action beam-up-supplies
  :parameters (?s - ship ?l - location ?p - supply ?wd - warpdrive)
  :precondition (and
        (at ?s ?l)
        (not (on-ship ?s ?p))
        (at ?p ?l)
        )
  :effect (and
    (at ?s ?l)
    (on-ship ?s ?p)
    (increase (total-cost) 1)
    (not (warp-drive-ready ?s ?wd))
    ))

(:action enable-warp-drive
  :parameters (?s - ship ?pc - plasmaconduit ?pi - plasmainjector ?wc - warpcoil ?di - dilithium ?wd - warpdrive)
  :precondition (and
    (on-ship ?s ?pc)
    (on-ship ?s ?pi)
    (on-ship ?s ?wc)
    (on-ship ?s ?di)
    (not (warp-drive-ready ?s ?wd))
   )

  :effect (and
        (increase (total-cost) 3)
        (warp-drive-ready ?s ?wd)
   ))
)
