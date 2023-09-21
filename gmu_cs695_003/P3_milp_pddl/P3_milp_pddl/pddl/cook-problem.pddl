; from the problem definition

(define (problem cook-problem)
(:domain cook)
(:objects
        rice1 - rice
        water1 - water 
        cooking-spot1 - cooking-spot
        rice-holder1 - rice-holder
        faucet1 - faucet
        pot1 - pot 
        empty1 - empty
        robot - manipulator
        initial-pos - initial-position 
        )
(:init
        (at robot initial-pos)
        (at rice1 rice-holder1)
        (at pot1 cooking-spot1)
        (at water1 faucet1)
	(hold robot empty1)
	(= (total-cost) 0)
)
;(:goal (food-ready rice1 cooking-spot1))

;(:goal (at robot rice-holder1))

;(:goal (hold robot pot1))


(:goal (and 
;    (inside water1 pot1)
;    ;(at pot1 cooking-spot1)
     (hold robot pot1)
    )
  )
(:metric minimize (total-cost))
)
