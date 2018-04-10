##DEBUG
- bug detected in SUBDAG.to\_dot : leafs are not dumped (probably from UDAG.to\_dot)
- bug detected in CPX.AllSat : unexpected output : no entry point for the problem
	(check for solved)

##TODO

##GroBdd
- implement existential/universal closure
- implement the closure+conjunction operator

#WUBdag [WIP]
- re-implementing Ubdag with GC-friendly storing/caching data structures

#BITV
- integrate Bitv into ocaml-tools by re-coding this library
- make Bitv immutable (maybe Bitv -> BitA (Bit Array : mutable) and BitS (Bit String : immutable ) )
- check : https://github.com/mirage/ocaml-openflow/blob/master/lib/bitv.mli for extra interfacing
- check : https://github.com/backtracking/bitv/blob/master/bitv\_string.ml
- create an index accessor such as ( .%[] )

#CONV
- refactor the conversion system
- complete the conversion system

#UDAG
- add a binary dump to UDAG
- NEXT: implement a version of UDAG which uses Ephemeron

#(S)UBDAG.ml
- add a binary dump to UBDAG
- NEXT: implement SUBDAG as an upgraded special case of taggedSUBDAG
	-- NB: ( taggedSUBDAG --> SUBDAG ) and ( SUBDAG --> notagSUBDAG )


#GroBdd
- TO THINK : NU-X+ (NU-X + left factorisation) or NU-X2 (NU-X + 2-predictions)
- TO THINK : NUA-X (non-significant variables + anti-variables + 1-predictions) version of GroBdd
	-- TO THINK: NNI-X (polarity-phase invariant + 1-predictions)
	-- TO THINK: NU-X+ (NU-X + left fatctorisation), NNI+ (NNI + left factorisation), NNI-X+ (NNI-X + left factorisation), etc.
- implement reordering, exhaustive & heuristics
- TO THINK: last variables packing
	-- profile: hit count vs arity in AND/XOR.manager

##Yann
#Example
- Find a better example than n-queens to illustrate the X (1-prediction) case.
#Graphviz
(1) - clusterize depending on edges' arity
(2) - replace bleu/red edges by 1/0 at the beginning of descriptors
(3) - make nodes' shape = rectangle
#Structure
Change the DAG structure in order to have:
(1) - unification of descriptors
(2) - unification of Pair : pair of Node's id (current nodes)
(3) - unification of Node : descriptor's id x Pair's id (current edges)
Furthermore, improve the unification system by creating a hashconsing table per arity.
We may resolve the arity of a Node's Pair from its descriptor (n -> m), as the co-arity n.
co-arity (d) = number of significant variable in the descriptor variables.
At a more formal level, a Node is a triplet n = (d, n0, n1)
with sem n = sem d (sem n0 * sem n1).
#Fixpoint
- solve recursive fixpoint equation
	-- fix R = fix (00 R\_00 + 01 R\_01 + 10 R\_ 10 + 11 R\_11 ) = 00 Z\_00 + 01 Z\_01 + 10 Z\10 + 11 Z\_11
	with
	-- X\_00 = fix R\_00 and X\_11 = fix R\_11
	-- X\_01 = X\_00 o R\_01 and X\_10 = X\_11 o R\_10
	-- Y\_0 = fix (X\_01 o X\_10)
	-- Y\_1 = [sol 1] fix (X\_10 o X\_10)
          = [sol 2] Id + (X\_10 o Y\_0 o X\_O1)
	-- Z\_00 = Y\_0  o X\_00 and Z\_11 = Y\_1  o X\_11
	-- Z\_01 = X\_01 o Z\_11 and Z\_10 = X\_10 o Z\_00
- cost :
	-- sol 1 : 4 x rec + 8 x comp
	-- sol 2 : 3 x rec + 9 x comp + 1 x conj
- terminal cases:
	-- fix Id\_n = Id\_n
	-- fix O\_n = Id\_n
	-- fix 1\_n = 1\_n
	-- fix ( R\_1 (x) R\2 ) = (fix R\_1) (x) (fix R\_2)
- special cases:
	-- if R\_10 = 0\_n
		 then fix R = 00 X\_00 + 01 Z\_01 + 11X\_11
		 with Z\_01 = X\_00 o R\_01 o X\_11
	-- if R\_01 = 0\_n (symmetric case to R\_10 = 0\_n) 
		 then fix R = 00 X\_00 + 10 Z\_10 + 11X\_11
		 with Z\_10 = X\_11 o R\_10 o X\_00
#Presentation
- make a presentation of binary operators (AND and XOR) with GroBdd
	-- constrast out the factorization operator as a difference between ROBDD and GroBdd
- make a presentation of NU-X


##Timothee
#Example
- Explicit a case reconstitution in the n-queen example of doc/2017-10-19-odp-trg
- Make a presentation : modele NU-X
- Explicit existential closure


##Khalil
#WAP
- make a presentation
#Example
- FIXME in doc/2017-10-19-odp-trg


##NEXT NEXT
- separate versions which uses Ephemeron from version which do not, e.g. hsrc vs esrc
- implement trees & rename src -> dag
- implement an iterable version of each data structure ?
-- list -> iter (done)
-- tree -> iter-tree (doable)
-- dag  -> iter-dag (thinking in progress)
-- rdag -> iter-rdag
-- graph -> iter-graph
-- digraph -> iter-digraph

##Related
# WAP
- wap-paper-SAT2018 -> doc/2018-02-12
# P-invariants
- caracterazation of sub-groups of the symmetric group
 -- caracterization of NPN-invariants
- application to DAG-isomorphism
