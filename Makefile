
F_SOURCES = ./functional
J_SOURCES = ./oop
BINARIES  = ./bin
LIBRARIES = ./lib
FRACTALS  = ./fractals

GHC  = ghc -rtsopts -with-rtsopts="-K500M"
FRC7 = java -Xss1m -jar lib/fregec7.jar -target 1.7
FRC8 = java -Xss1m -jar lib/fregec8.jar

all: all8 all7 app5-hs app8-hs native

all7: dirs app0-7 app1-7 app2-7 app3-7 app4-7 app6-7
all8: dirs app0 app1 app2 app3 app4 app6



dirs:
	/bin/mkdir -p $(LIBRARIES) $(BINARIES) $(FRACTALS)
	/bin/ln -nfs ../lib $(BINARIES)/lib

	for idx in $$(seq 0 7) ; do \
		/bin/mkdir -p $(F_SOURCES)/$${idx} ; \
		/bin/mkdir -p $(J_SOURCES)/j7/$${idx} ; \
		/bin/mkdir -p $(J_SOURCES)/j8/$${idx} ; \
	done


fregec:
	wget -c https://github.com/Frege/frege/releases/download/3.24alpha/frege3.24.100.jar -O $(LIBRARIES)/fregec8.jar
	wget -c https://github.com/Frege/frege/releases/download/3.24alpha/frege3.24-7.100.jar -O $(LIBRARIES)/fregec7.jar


app%-hs:
	@echo -e "\n--------------------  Haskell  --------------------"
	$(GHC) $(F_SOURCES)/$*/*.hs -o $(BINARIES)/app$*


app%-frege:
	@echo -e "\n--------------------   Frege   --------------------"
	$(FRC8) -d $(J_SOURCES)/j8/$*/ $(F_SOURCES)/$*/Module*.fr || true
	$(FRC8) -d $(J_SOURCES)/j8/$*/ $(F_SOURCES)/$*/main.fr


app%-frege7:
	@echo -e "\n--------------------   Frege   --------------------"
	$(FRC7) -d $(J_SOURCES)/j7/$*/ $(F_SOURCES)/$*/Module*.fr || true
	$(FRC7) -d $(J_SOURCES)/j7/$*/ $(F_SOURCES)/$*/main.fr


app%-jar:
	@echo -e "\n--------------------    Ant    --------------------"
	ant -f build.xml app$*


app%-jar7:
	@echo -e "\n--------------------    Ant    --------------------"
	ant -f build-j7.xml app$*


app%:
	@echo -e "\n==================== Example $* ====================\n"
	make app$*-hs
	make app$*-frege
	make app$*-jar


app%-7:
	@echo -e "\n==================== Example $* ====================\n"
	make app$*-hs
	make app$*-frege7
	make app$*-jar7

native:
	@echo -e "\n=================== Native Java ===================\n"
	ant native

clean:
	find $(F_SOURCES)/ $(J_SOURCES)/ \
		-name "*.o" -o -name "*.hi" -o -name "*.class" | xargs rm -vf


clean-all: clean
	find $(BINARIES) $(J_SOURCES)/j7/ $(J_SOURCES)/j8/ -type f | xargs rm -vf

