FILE = miniOO

.PHONY : all
all :
	ls
	@echo '# Lexer specification:'
	cat ${FILE}LEX.mll
	@echo '#Lexer creation:'
	ocamllex ${FILE}LEX.mll
	ls
	@echo '# Parser specification:'
	cat ${FILE}YACC.mly
	@echo '# Parser creation:'
	ocamlyacc ${FILE}YACC.mly
	ls
	@echo '# Types of lexem returned values:'
	cat ${FILE}YACC.mli
	@echo '# Compilation of the lexer and parser:'
	ocamlc -c ${FILE}YACC.mli
	ocamlc -c ${FILE}LEX.ml
	ocamlc -c ${FILE}YACC.ml
	@echo '# Specification of the miniOO'
	cat ${FILE}.ml
	ocamlc -c ${FILE}.ml
	@echo '# Linking and code generation for the lexer, '
	@echo '# parser and miniOO:'
	ocamlc -o ${FILE} ${FILE}LEX.cmo ${FILE}YACC.cmo ${FILE}.cmo
	ls
	@echo '# of Using the miniOO:'
	echo "var P; {P = proc Y:if Y < 1 then P = 1 else P(Y-1); P(1)}" | ./${FILE}

.PHONY : clean
clean :
	@-/bin/rm -f *.cmo *.cmi ${FILE}LEX.ml ${FILE}YACC.mli ${FILE}YACC.ml *~

.PHONY : distrib
distrib : clean
	@-bin/rm -f ${FILE}

.PHONY : delete
	delete : distrib
	@-/bin/rm -f typescript
