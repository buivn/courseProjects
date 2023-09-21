; from the problem definition

(define (problem cook-problem2)
(:domain cook)
(:objects
        tea1 - tea
	;rice1 - rice
        water1 - water
        cooking-spot1 - cooking-spot
        tea-holder1 - tea-holder
        faucet1 - faucet
        ;pot1 - pot
        ket1 - kettle
	cup1 - cup
	drink-spot1 - drink-spot
	empty1 - empty
        robot - manipulator
        initial-pos - initial-position
        )
(:init
        (at robot initial-pos)
      
	(at ket1 cooking-spot1)
        (at water1 faucet1)
	(at cup1 drink-spot1 )
	(at tea1 tea-holder1)
        (hold robot empty1)
        (= (total-cost) 0)
)
;(:goal (food-ready rice1 cooking-spot1))

;(:goal (at robot rice-holder1))

;(:goal (hold robot cup1))


(:goal (and  
     ;(drink-ready tea1 drink-spot1)
     (inside tea1 cup1)
     ;(hold robot empty1)
     (at robot drink-spot1)
     (at cup1 drink-spot1)
     (hot water1 ket1)
     (hold robot ket1)
     ;(at ket1 drink-spot1) 
    )
  )
(:metric minimize (total-cost))
)

