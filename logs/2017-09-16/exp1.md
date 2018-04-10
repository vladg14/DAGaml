commit: f38c6ea07d0c313bdaf728bd534d6dac04ab5780
date: 2017-09-16 saturday 18h05 (Paris)
description:
comparing runtime of .cp.A.tacx to .cpx.B.pure when preprocessing without/with FORCE heuristic
runtime does not include the application of the heuristic itself as the code could be hugely improved
benchmarks launched on uf20-91/\*.cnf (exp1.csv)
benchmarks launched on uf50-218/\*.cnf (exp2.csv)
data:
 - exp{1-2}.csv:
benchmark name | #sat (v3) | #sat (v4) | runtime (v3)      | runtime (v4)      | #node (v3) | #node (v4)
               |           |           | real | user | sys | real | user | sys |            |
analysis:

data summary:
 - exp1.csv
			| v3   | v4   | v4 - v3 | (v4-v3)/v3
real (ms)	| 79.0 | 73.9 | -5.09   | -6.44%
user (ms)	| 71.9 | 67.5 | -4.35   | -6.05%
sys  (ms)	| 4.72 | 4.10 | -0.62   | -13.2%
tacx-\#node	| 93.6 | 93.7 | +0.16   | +0.17%

a positive (non negligeable) effect seems observable

- exp2.csv
 [+ 11.417E 0  ,+ 11.368E 0  ,+ 39.383E-3.0, +206.140E 0  ,
  + 11.81 E 0  ,+ 11.34 E 0  ,+ 38.655E-3.0, +206.338E 0  ]
[-335.698E-3.0,-333.411E-3.0,-  0.727E-3.0,+197.802E-3.0]
ratios
[-  2.940E 0  ,-  2.933E 0  ,-  0.2  E+3.0,+ 95.955E-3.0]
			| v3      | v4   | v4 - v3 | (v4-v3)/v3
real (s)	|  11.417 | 73.9 | -5.09   | -6.44%
user (s)	|  11.368 | 67.5 | -4.35   | -6.05%
sys  (s)	|   0.039 | 4.10 | -0.62   | -13.2%
tacx-\#node	| 206.140 | 93.6 | 93.7 | +0.16   | +0.17%
 - exp3.csv
[490.17128125000005, 486.8749375, 0.30821875, 488.38471875, 482.86693749999995, 0.29643749999999996, 287.34375, 287.0625]
[-1.7865625000000005, -4.007999999999997, -0.01178125, -0.28125]
[-0.36447718753412794, -0.8232093482938824, -3.822366419953361, -0.09787928221859706]
[+490.171E 0  ,+486.875E 0  ,+308.219E-3.0,+488.385E 0  ,+482.867E 0  ,+296.437E-3.0,+287.344E 0  ,+287.63 E 0  ]
[-  0.2  E+3.0,-  4.8  E 0  ,- 11.781E-3.0,-281.250E-3.0]
ratios
[-364.477E-3.0,-823209348293.882E-12.0,-  3.822E 0  ,- 97.879E-3.0]

 -exp4.csv
 [408.495776119403, 402.06395522388055, 0.2871492537313433, 432.33329850746253, 425.9645373134328, 0.29635820895522386, 288.0149253731343, 286.7910447761194]
[23.837522388059696, 23.900582089552238, 0.0092089552238806, -1.2238805970149254]
[5.835439135872976, 5.94447270863754, 3.2070273922761063, -0.4249365186298389]
[+408.496E 0  ,+402.64 E 0  ,+287.149E-3.0,+432.333E 0  ,+425.965E 0  ,+296.358E-3.0,+288.15 E 0  ,+286.791E 0  ]
[+ 23.838E 0  ,+ 23.901E 0  ,+  9.209E-3.0,-  0.0  E+9.0]
ratios
[+  5.835E 0  ,+  5.944E 0  ,+  3.207E 0  ,-424.937E-3.0]




