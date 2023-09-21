(define (problem space-problem)
(:domain space)

(:objects
	earth sb128 vulcan betazed cardassia qonos ferenginar levinia - location
	cerritos - ship
	redkey1 - red-key
	redvault1 - red-vault
	bluekey1 - blue-key
	bluevault1 - blue-vault
	purplekey1 - purple-key
	purplevault1 - purple-vault
	)

(:init
	; Enterprise
	(at cerritos earth)

	; Locations and travel
	(adjacent earth sb128) (= (distance earth sb128) 1)
	(adjacent sb128 earth) (= (distance sb128 earth) 1)
	(adjacent earth vulcan) (= (distance earth vulcan) 10)
	(adjacent vulcan earth) (= (distance vulcan earth) 10)
	(adjacent earth betazed) (= (distance earth betazed) 15)
	(adjacent betazed earth) (= (distance betazed earth) 15)
	(adjacent vulcan cardassia) (= (distance vulcan cardassia) 7)
	(adjacent cardassia vulcan) (= (distance cardassia vulcan) 7)
	(adjacent betazed cardassia) (= (distance betazed cardassia) 9)
	(adjacent cardassia betazed) (= (distance cardassia betazed) 9)
	(adjacent betazed ferenginar) (= (distance betazed ferenginar) 10)
	(adjacent ferenginar betazed) (= (distance ferenginar betazed) 10)
	(adjacent betazed qonos) (= (distance betazed qonos) 10)
	(adjacent qonos betazed) (= (distance qonos betazed) 10)
	(adjacent qonos vulcan) (= (distance qonos vulcan) 6)
	(adjacent vulcan qonos) (= (distance vulcan qonos) 6)
	(adjacent levinia qonos) (= (distance levinia qonos) 500)
	(adjacent qonos levinia) (= (distance qonos levinia) 500)
	; Cost function
	(= (total-cost) 0)
	)

(:goal (at cerritos levinia))
(:metric minimize (total-cost))

)

