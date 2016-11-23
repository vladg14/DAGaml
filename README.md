# DAGaml
Abstract manipulations of Directed Acyclic Graph (DAG) using OCaml.

https://en.wikipedia.org/wiki/Directed_acyclic_graph

https://ocaml.org/

# Implementation
DAGaml implements various kinds of DAG such as UDAG, URDAG, UBDAG

## UDAG(leaf, edge, node)
Implements syntactically canonical DAG: each node (edges' order included) is unique in the data structure

## URDAG(leaf, edge, node)
Implements syntactically canonical Recursive DAG, i.e. nodes are either regular nodes or a rooted DAGs themselves. Thus, we add a special kind of leaf called Output

## UBDAG(leaf, node)
Implements Binary DAG using ephemeron to weakly record nodes in the unique table.
Implements memoization facilities using weakly referenced inputs


# Why ?
I'm currently developping a variant of BDDs and I got tired of dealing with manipulation of DAG. Furthermore at some point I decided that I will have to develop a way of manipulating circuits (such as AIG) which also are DAGs. Hence, I decided to design an abstract way of efficiently dealing with them.
Feel free to use it and contribute !
If you do so, please send me a message so I can keep track of who is using it and for which purpose.
Also, feel free to send me links to articles/reports that could help design this library.

If you want to help me design this tool but you have no idea how : contact me.

# Installation
you should have installed ocaml, then clone this repository
