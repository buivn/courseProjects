(define
(domain space)
(:requirements :strips :typing :action-costs)
(:types location locatable - object
	ship supply - locatable
	plasmaconduit plasmainjector warpcoil dilithium medicalsupply - supply)

(:predicates
  (at ?l - locatable ?p - location)
  (adjacent ?a - location ?b - location)
  (on-ship ?s - ship  ?p - supply)
  (warp-drive-active ?s - ship ?pc - plasmaconduit ?pli - plasmainjector ?wc - warpcoil ?di - dilithium))

(:functions
  (distance ?a - location ?b - location)
  (total-cost))

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
  :parameters (?s - ship ?from - location ?to - location ?pc - plasmaconduit 
        ?pli - plasmainjector ?wc - warpcoil ?di - dilithium)
  :precondition (and
    (at ?s ?from)
    (adjacent ?from ?to)
    (warp-drive-active ?s ?pc ?pli ?wc ?di)
    )
  :effect (and
    (not (at ?s ?from))
    (at ?s ?to)
    (increase (total-cost) (warp-distance ?from ?to))
    ))

(:action beam-up-supplies
  :parameters (?s - ship ?l - location ?p - supply)
  :precondition (and
    (at ?s ?l)
    (at ?p ?l)
    )
  :effect (and
    (not (at ?p ?l))
    (on-ship ?s ?p)
    (increase (total-cost) 1))
  )

(:action enable-warp-drive
  :parameters (?s - ship ?pc - plasmaconduit ?pli - plasmainjector 
        ?wc - warpcoil ?di - dilithium)
  :precondition (and
    (on-ship ?s ?pc)
    (on-ship ?s ?pli)
    (on-ship ?s ?wc)
    (on-ship ?s ?di))
  :effect (and
    (warp-drive-active ?s ?pc ?pli ?wc ?di)
    (not (on-ship ?s ?pc))
    (not (on-ship ?s ?pli))
    (not (on-ship ?s ?wc))
    (not (on-ship ?s ?di))
    (increase (total-cost) 3)))
)



