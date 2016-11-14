# DAGaml
Abstract manipulations of Directed Acyclic Graph (DAG) using OCaml
https://en.wikipedia.org/wiki/Directed_acyclic_graph
https://ocaml.org/

# Why ?
I'm currently developping a variant of BDDs and I got tired of dealing with manipulation of DAG. Furthermore at some point I decided that I will have to develop a way of manipulating circuits (such as AIG) which also are DAGs. Hence, I decided to design an abstract way of efficiently dealing with them.
Feel free to use it and contribute !
If you do so, please send me a message so I can keep track of who is using it and for which purpose.
Also, feel free to send me links to articles/reports that could help designing this library.

If you want to help me design this tool but you have no idea how : contact me.
If you want to make a contribution remember only one thing : keep it simple, I don't want to have performance optimal code that I can't understand in a reasonnable amount of time.

# Installation
you should have installed ocaml and set the variable NPROC to the number of core you want to be used by the make file
for example add this line at the end of your .bashrc
'''
export NPROC=8 (* if you have 8 cores*)
'''
