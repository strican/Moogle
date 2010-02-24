all: moogle

FILES=order.ml dict.ml set.ml util.ml query.ml moogle.ml

moogle: $(FILES)
	ocamlc -g -o moogle unix.cma str.cma $(FILES)

check: $(FILES)
	chmod u+x ./check_width
	./check_width $(FILES)

submit1: $(FILES)
	chmod u+x ./submit.sh
	./submit.sh 4

submit2: $(FILES)
	chmod u+x ./submit.sh
	./submit.sh 5


clean: 
	rm -f moogle *.cmi *.cmo
