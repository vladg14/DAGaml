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


##DEBUG
- bug detected in SUBDAG.to\_dot : leafs are not dumped (probably from UDAG.to\_dot)
- bug detected in CPX.AllSat : unexpected output : no entry point for the problem

##TODO

#UDAG
- add a binary dump to UDAG
- NEXT: implement a version of UDAG which uses Ephemeron

#(S)UBDAG.ml
- add a binary dump to UBDAG
- NEXT: implement SUBDAG as an upgraded special case of taggedSUBDAG
	-- NB: ( taggedSUBDAG --> SUBDAG ) and ( SUBDAG --> notagSUBDAG )

#Tools
- add binary tree
- add tree iterator

#GroBdd
- implement the NUA-X (non-significant variables + anti-variables + 1-predictions) version of GroBdd
	-- TO THINK: NNI-X (polarity-phase invariant + 1-predictions)
	-- TO THINK: NU-X+ (NU-X + left fatctorisation), NNI+ (NNI + left factorisation), NNI-X+ (NNI-X + left factorisation), etc.
- implement reordering, exhaustive & heuristics
- TO THINK: last variables packing
	-- profile: hit count vs arity in AND/XOR.manager
