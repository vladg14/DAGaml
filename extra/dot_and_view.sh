if [ $1 = "--pdf" ]
then
	dot -Tpdf -O $2 && xpdf $2.pdf
elif [ $1 = "--svg" ]
then
	dot -Tsvg -O $2 && viewnior $2.svg
else
	dot -Tsvg -O $1 && viewnior $1.svg
fi
