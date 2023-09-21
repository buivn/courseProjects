 from the problem definition

(define (problem space-problem)
(:domain space)

(:objects
        earth sb128 vulcan betazed cardassia qonos ferenginar levinia - location
        enterprise - ship
        plasmaconduit1 - plasmaconduit
        plasmainjector1 - plasmainjector
        warpcoil1 - warpcoil
        dilithium1 - dilithium
        medicalsupply1 - medicalsupply
        warpdrive1 - warpdrive
        )


(:init
        (at enterprise earth)
        (at plasmaconduit1 vulcan)
        (at plasmainjector1 betazed)
        (at warpcoil1 qonos)
        (at dilithium1 ferenginar)
        ; Locations and travel
        (adjacent earth sb128) (= (distance earth sb128) 1) (= (warp-distance earth sb128) 0)
        (adjacent sb128 earth) (= (distance sb128 earth) 1) (= (warp-distance sb128 earth) 0)
        (adjacent earth vulcan) (= (distance earth vulcan) 10) (= (warp-distance earth vulcan) 2)
        (adjacent vulcan earth) (= (distance vulcan earth) 10) (= (warp-distance vulcan earth) 2)
        (adjacent earth betazed) (= (distance earth betazed) 15) (= (warp-distance earth betazed) 3)
        (adjacent betazed earth) (= (distance betazed earth) 15) (= (warp-distance betazed earth) 3)
        (adjacent vulcan cardassia) (= (distance vulcan cardassia) 7) (= (warp-distance vulcan cardassia) 1)
        (adjacent cardassia vulcan) (= (distance cardassia vulcan) 7) (= (warp-distance cardassia vulcan) 1)
        (adjacent betazed cardassia) (= (distance betazed cardassia) 9) (= (warp-distance betazed cardassia) 2)
        (adjacent cardassia betazed) (= (distance cardassia betazed) 9) (= (warp-distance cardassia betazed) 2)
        (adjacent betazed ferenginar) (= (distance betazed ferenginar) 10) (= (warp-distance betazed ferenginar) 2)
        (adjacent ferenginar betazed) (= (distance ferenginar betazed) 10) (= (warp-distance ferenginar betazed) 2)
        (adjacent betazed qonos) (= (distance betazed qonos) 10) (= (warp-distance betazed qonos) 2)
        (adjacent qonos betazed) (= (distance qonos betazed) 10) (= (warp-distance qonos betazed) 2)
        (adjacent qonos vulcan) (= (distance qonos vulcan) 6) (= (warp-distance qonos vulcan) 1)
        (adjacent vulcan qonos) (= (distance vulcan qonos) 6) (= (warp-distance vulcan qonos) 1)
        (adjacent levinia qonos) (= (distance levinia qonos) 500) (= (warp-distance levinia qonos) 100)
        (adjacent qonos levinia) (= (distance qonos levinia) 500) (= (warp-distance qonos levinia) 100)

        (= (total-cost) 0)
        (= (warp-drive-active) 0)
        (not (warp-drive-ready enterprise warpdrive1))
)

(:goal (at enterprise levinia))
(:metric minimize (total-cost))

)
        