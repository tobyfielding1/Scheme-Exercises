FLAGS=-O6

VERIF=bin/tester

COMP2209_DIST=comp2209-dist

comp:
	mkdir -p bin
	gsc -o $(VERIF) -exe src/eps.scm src/core-cps.scm src/macros.scm src/primitives.scm src/eval-cps.scm src/tester.scm src/toplevel.scm src/main.scm

comp.js:
	mkdir -p target/js
	gambit-master/gsc/gsc -o target/js -target js -c src/eps.scm src/core-cps.scm src/macros.scm src/primitives.scm src/eval-cps.scm src/tester.scm src/toplevel.scm src/main.scm

comp.java:
	mkdir -p target/java
	gambit-master/gsc/gsc -o target/java -target java -c src/eps.scm src/core-cps.scm src/macros.scm src/primitives.scm src/eval-cps.scm src/tester.scm src/toplevel.scm src/main.scm

target:
	mkdir target

check: target
	$(VERIF) -summary target/sum.txt -bound 1000 -r5rs -tests test-dir/get-path.tst

clean:
	rm -r -f target

dist:
	rm -r -f $(COMP2209_DIST)
	mkdir $(COMP2209_DIST) $(COMP2209_DIST)/src 
	cp Makefile $(COMP2209_DIST)
	cp README.txt $(COMP2209_DIST)
	tar cf - src bin | (cd  $(COMP2209_DIST); tar xf -)
	tar zcvf $(COMP2209_DIST).tar.gz $(COMP2209_DIST)

pub.dist:
	mv $(COMP2209_DIST).tar.gz  ~/luc-git/lecture-notes/comp2209/www/15-16/


js:
	d8  target/js/eps.js target/js/core-cps.js target/js/macros.js target/js/primitives.js target/js/eval-cps.js target/js/tester.js target/js/toplevel.js target/js/main.js



export:
	cd gambit-mh-switch-3c4f29d187ce68f6e0b21a59eb8399a7c6e6eb16/emscripten-gsi/; cp gsi.js gsi.new.js; tar cvf - gsi.new.js | ssh login.ecs.soton.ac.uk "cd /home/notes/comp2209/15-16/gambit-in-the-browser; tar xf -"
