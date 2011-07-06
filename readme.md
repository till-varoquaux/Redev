This is a toy library to match regular expression in ocaml. Real regular expression library such as [pcre](http://pcre.org), [re2](http://code.google.com/p/re2/) or [tre](http://laurikari.net/tre/) are big and complex this is not meant to be used as a serious library. The plan is to use memoization on top of Bzrowosky derivatives to reach an asymptotic O(1) behaviour (derivatives can be used to constructed DFAs). Not quite sure yet how to tie tagged transitions in all this.

Interesting articles:

+ [Regular expressions can be simple and fast](http://swtch.com/~rsc/regexp/regexp1.html): Russ cox has a great series of articles on regular expression matching. This article explains the problems with recursive backtrace based engines and gives a great overview of NFA/DFA based methods.

+ [Regular expression derivatives re-examined](http://www.cl.cam.ac.uk/~so294/documents/jfp09.pdf): This is not the first article on Brzowosky derivatives (this would be his Brzowsky 1964 [Derivatives of Regular expressions](http://doi.acm.org/10.1145/321239.321249)) but it is not behind a paywall and does a good job summarizing the previous research. The author also show that with some small reductions on the derived regular expressions there's a finite number of different regular expressions that can be reached while parsing any regular expression thus making the algorithm sound to generate DFA's

+ [NFAs with Tagged Transitions, their Conversion to Deterministic Automata and Application to Regular Expressions](http://laurikari.net/ville/spire2000-tnfa.ps): Ville Laurikari's article explaining how to do subgroup matching in NFA and DFA without using backtracking.
This technique is used in in Ville's [TRE](http://laurikari.net/tre/).

+ [Hash consing](http://en.wikipedia.org/wiki/Hash_consing): Hash consig can be used to reduce structural equality test to pointer equality. This, combined with hashing (embedded in the node) can be used to make O(1) smart constructors (they require equality tests) and hashtables.

+ [LTU's discussion on Regular expression matching](http://lambda-the-ultimate.org/node/2064): Plenty of interesting insights.


OCaml specific:

+ [Christian Lindig and Norman Ramsey's RX](http://www.cminusminus.org/rsync/qc--/cllib/rx.nw): Derivative based regular expression engine. Small and clean.

+ [Jerome Vouillon's RE](http://sourceforge.net/projects/libre/): Uses a Lazy DFA construction. Probably the most full-featured pure ocaml regular expression library.

+ [Claude Marche's Regexp](http://www.lri.fr/~marche/regexp/): Standard NFA->DFA construction; uses fancy datastructures.

+ [Jean Cristophe Filliatre's hash consing](www.lri.fr/~filliatr/ftp/publis/hash-consing2.pdf): paper about hash consing.
