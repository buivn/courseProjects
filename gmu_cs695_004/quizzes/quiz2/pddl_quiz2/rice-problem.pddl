(define (problem rice-problem)
(:domain kitchen)

(:objects
        rice1 - rice
        water1 - water
        cooking-spot1 - cooking-spot
        rice-holder1 - rice-holder
        faucet1 - faucet
        pot1 - pot
        robot - manipulator
        res-loc1 - res-location
        cooking-shelf1 - cooking-shelf
        )
(:init
        (at robot res-loc1)
        (on rice1 rice-holder1)
        (on pot1 cooking-shelf1)
        (at water1 faucet1)
        (empty-hand robot)
        (not (hot water1))        
        (= (total-cost) 0)
)
(:goal (fd-ready rice1))
(:metric minimize (total-cost))

)
  