(define (problem coffee-problem)
(:domain kitchen)

(:objects
        coffee1 - coffee
        water1 - water
        cooking-spot1 - cooking-spot
        drink-spot1 - drink-spot
        coffee-holder1 - coffee-holder
        faucet1 - faucet
        kettle1 - kettle
        robot - left-manip
        res-loc1 - res-location
        cooking-shelf1 - cooking-shelf
        cup1 - coffee-mud
        )
(:init
        (at robot res-loc1)
        (on coffee1 coffee-holder1)
        (on kettle1 cooking-shelf1)
        (on cup1 drink-spot1)
        (at water1 faucet1)
        (empty-hand robot)
        (not (hot water1))        
        (= (total-cost) 0)
)
(:goal (and (fd-ready coffee1)
        (on cup1 drink-spot1)))

(:metric minimize (total-cost))

)
  