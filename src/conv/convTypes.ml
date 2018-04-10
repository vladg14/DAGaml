type extern_tag = unit
type input_tag =
| Verilog
| Cnf
| Pla
type output_tag =
| Dot
| Check
| Stats
| CntSat
| AllSat
type modele_tag =
| Bryant
| Zdd
| Cp
| Nni
| Cpx
type tacx_pure =
| Tacx
| Pure
type version_tag =
| VA
| VB
type file_tag =
| Ext  of extern_tag
| ExtI of input_tag
| ExtO of output_tag
| Int  of (tacx_pure * modele_tag * version_tag)
