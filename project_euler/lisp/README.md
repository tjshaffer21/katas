The **project-euler** package defines each of the problems as *problem-*[number word] (e.g. problem-one). None of which contain driver code as unit tests are used instead.

Each test is defined as *test-problem-*[number word], and can be run similarly: ```(lisp-unit:run-tests '(project-euler.tests::test-problem-one) 'project-euler.tests)```. All of the actual problems can be run using ```(project-euler.tests:run-euler)```, auxilary functions can be tested with ```(project-euler.tests:run-aux-tests)```, and both sets can be run with ```(project-euler.tests:run-tests)```.  Errors can be printed with ```(lisp-unit:print-errors *)``` and failures with ```(lisp-unit:print-failures *)```.

Benchmarking can be accessed using the **project-euler.benchmarks** package. In general, a benchmark can be performed with ```(project-euler.benchmark:do-benchmark-report n fn)``` where *n* is the sample size and *fn* is the function. Several functions exist for convience.

#### Dependencies
* Clozure Lisp
* cl-utilities
* iterate
* lisp-unit
* trivial-benchmark