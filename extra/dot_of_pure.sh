./test_eval_pure_to_dot.native $1 $2 $2.dot
dot -Tsvg -O $2.dot && viewnior $2.dot.svg

