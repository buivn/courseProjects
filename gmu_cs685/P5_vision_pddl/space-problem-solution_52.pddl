(define (problem space-problem)
(:domain space)

(:objects
	; earth sb128 vulcan betazed cardassia qonos ferenginar levinia - location
	start-x start-y rkey-x rkey-y bkey-x bkey-y pkey-x pkey-y rvault-x rvault-y bvault-x bvault-y pvault-x pvault-y - location
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
	(at cerritos start-x start-y)
	(at purplekey1 pkey-x pkey-y)
	(at bluekey1 bkey-x bkey-y)
	(at redkey1 rkey-x rkey-y)
	(at bluevault1 bvault-x bvault-y)
	(at redvault1 rvault-x rvault-y)
	(at purplevault1 pvault-x pvault-y)

	; Locations and travel
	(adjacent start-x start-y rkey-x rkey-y) (= (distance start-x start-y rkey-x rkey-y) 959)
	(adjacent rkey-x rkey-y start-x start-y) (= (distance rkey-x rkey-y start-x start-y) 959)
	(adjacent start-x start-y bkey-x bkey-y) (= (distance start-x start-y bkey-x bkey-y) 738)
	(adjacent bkey-x bkey-y start-x start-y) (= (distance bkey-x bkey-y start-x start-y) 738)
	(adjacent start-x start-y pkey-x pkey-y) (= (distance start-x start-y pkey-x pkey-y) 776)
	(adjacent pkey-x pkey-y start-x start-y) (= (distance pkey-x pkey-y start-x start-y) 776)	
	(adjacent start-x start-y pvault-x pvault-y) (= (distance start-x start-y pvault-x pvault-y) 615)
	(adjacent pvault-x pvault-y start-x start-y) (= (distance pvault-x pvault-y start-x start-y) 615)	
	(adjacent start-x start-y bvault-x bvault-y) (= (distance start-x start-y bvault-x bvault-y) 231)
	(adjacent bvault-x bvault-y start-x start-y) (= (distance bvault-x bvault-y start-x start-y) 231)	
	(adjacent start-x start-y rvault-x rvault-y) (= (distance start-x start-y rvault-x rvault-y) 801)
	(adjacent rvault-x rvault-y start-x start-y) (= (distance rvault-x rvault-y start-x start-y) 801)	
	(adjacent rkey-x rkey-y bkey-x bkey-y) (= (distance rkey-x rkey-y bkey-x bkey-y) 1482)
	(adjacent bkey-x bkey-y rkey-x rkey-y) (= (distance bkey-x bkey-y rkey-x rkey-y) 1482)
	(adjacent rkey-x rkey-y pkey-x pkey-y) (= (distance rkey-x rkey-y pkey-x pkey-y) 385)
	(adjacent pkey-x pkey-y rkey-x rkey-y) (= (distance pkey-x pkey-y rkey-x rkey-y) 385)
	(adjacent rkey-x rkey-y bvault-x bvault-y) (= (distance rkey-x rkey-y bvault-x bvault-y) 839)
	(adjacent bvault-x bvault-y rkey-x rkey-y) (= (distance bvault-x bvault-y rkey-x rkey-y) 839)
	(adjacent rkey-x rkey-y rvault-x rvault-y) (= (distance rkey-x rkey-y rvault-x rvault-y) 221)
	(adjacent rvault-x rvault-y rkey-x rkey-y) (= (distance rvault-x rvault-y rkey-x rkey-y) 221)
	(adjacent rkey-x rkey-y pvault-x pvault-y) (= (distance rkey-x rkey-y pvault-x pvault-y) 1370)
	(adjacent pvault-x pvault-y rkey-x rkey-y) (= (distance pvault-x pvault-y rkey-x rkey-y) 1370)
	(adjacent pkey-x pkey-y bkey-x bkey-y) (= (distance pkey-x pkey-y bkey-x bkey-y) 1299)
	(adjacent bkey-x bkey-y pkey-x pkey-y) (= (distance bkey-x bkey-y pkey-x pkey-y) 1299)
	(adjacent pkey-x pkey-y bvault-x bvault-y) (= (distance pkey-x pkey-y bvault-x bvault-y) 656)
	(adjacent bvault-x bvault-y pkey-x pkey-y) (= (distance bvault-x bvault-y pkey-x pkey-y) 656)
	(adjacent pkey-x pkey-y rvault-x rvault-y) (= (distance pkey-x pkey-y rvault-x rvault-y) 227)
	(adjacent rvault-x rvault-y pkey-x pkey-y) (= (distance rvault-x rvault-y pkey-x pkey-y) 227)
	(adjacent pkey-x pkey-y pvault-x pvault-y) (= (distance pkey-x pkey-y pvault-x pvault-y) 1187)
	(adjacent pvault-x pvault-y pkey-x pkey-y) (= (distance pvault-x pvault-y pkey-x pkey-y) 1187)
	(adjacent bkey-x bkey-y bvault-x bvault-y) (= (distance bkey-x bkey-y bvault-x bvault-y) 642)
	(adjacent bvault-x bvault-y bkey-x bkey-y ) (= (distance bvault-x bvault-y bkey-x bkey-y) 642)
	(adjacent bkey-x bkey-y rvault-x rvault-y) (= (distance bkey-x bkey-y rvault-x rvault-y) 1325)
	(adjacent rvault-x rvault-y bkey-x bkey-y) (= (distance rvault-x rvault-y bkey-x bkey-y) 1325)
	(adjacent bkey-x bkey-y pvault-x pvault-y) (= (distance bkey-x bkey-y pvault-x pvault-y) 693)
	(adjacent pvault-x pvault-y bkey-x bkey-y) (= (distance pvault-x pvault-y bkey-x bkey-y) 693)
	(adjacent pvault-x pvault-y rvault-x rvault-y) (= (distance pvault-x pvault-y rvault-x rvault-y) 1212)
	(adjacent rvault-x rvault-y pvault-x pvault-y) (= (distance rvault-x rvault-y pvault-x pvault-y) 1212)
	(adjacent pvault-x pvault-y bvault-x bvault-y) (= (distance pvault-x pvault-y bvault-x bvault-y) 530)
	(adjacent bvault-x bvault-y pvault-x pvault-y) (= (distance bvault-x bvault-y pvault-x pvault-y) 530)
	(adjacent bvault-x bvault-y rvault-x rvault-y) (= (distance bvault-x bvault-y rvault-x rvault-y) 681)
	(adjacent rvault-x rvault-y bvault-x bvault-y) (= (distance rvault-x rvault-y bvault-x bvault-y) 681)

	; (adjacent earth vulcan) (= (distance earth vulcan) 10)
	; (adjacent vulcan earth) (= (distance vulcan earth) 10)
	; (adjacent earth betazed) (= (distance earth betazed) 15)
	; (adjacent betazed earth) (= (distance betazed earth) 15)
	; (adjacent vulcan cardassia) (= (distance vulcan cardassia) 7)
	; (adjacent cardassia vulcan) (= (distance cardassia vulcan) 7)
	; (adjacent betazed cardassia) (= (distance betazed cardassia) 9)
	; (adjacent cardassia betazed) (= (distance cardassia betazed) 9)
	; (adjacent betazed ferenginar) (= (distance betazed ferenginar) 10)
	; (adjacent ferenginar betazed) (= (distance ferenginar betazed) 10)
	; (adjacent betazed qonos) (= (distance betazed qonos) 10)
	; (adjacent qonos betazed) (= (distance qonos betazed) 10)
	; (adjacent qonos vulcan) (= (distance qonos vulcan) 6)
	; (adjacent vulcan qonos) (= (distance vulcan qonos) 6)
	; (adjacent levinia qonos) (= (distance levinia qonos) 500)
	; (adjacent qonos levinia) (= (distance qonos levinia) 500)
	

	; Cost function
	(= (total-cost) 0)
	)

(:goal (and 
	(at cerritos start-x start-y)
	(not (on-ship redkey1 cerritos))
	; (on-ship redkey1 cerritos)
	
	(not (on-ship bluekey1 cerritos))
	(not (on-ship purplekey1 cerritos))
	; (on-ship bluekey1 cerritos)
	; (on-ship purplekey1 cerritos)
	(at redkey1 rvault-x rvault-y)
	(at bluekey1 bvault-x bvault-y)
	(at purplekey1 pvault-x pvault-y)
	))
(:metric minimize (total-cost))

)

