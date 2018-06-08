The **project-euler** package defines each of the problems as *problem-*[number] (e.g. problem-one). None of which contain driver code as unit tests are used instead.

Each test is defined as *test-problem-*[number], and can be run similarly: ```(lisp-unit:run-tests '(project-euler.tests::test-problem-one) 'project-euler.tests)```. Alternatively, all tests can be run using ```project-euler.tests::run-euler```. Failures and errors can be printed using the associated lisp-unit function: ```(lisp-unit:print-failures *)``` or ```(lisp-unit:print-errors *)```.
Unit tests for all functions can be run using ```(project-euler.tests:run-tests)```.

Benchmarking can be accessed using the **project-euler.benchmarks** package. *benchmarking.lisp* can be looked at for available functions. Each run constitutes a single sample. To print the timing information ```(benchmark:report *timer*)``` is used. In order to reset between calls use ```(benchmark:reset *timer*)```.

#### Dependencies
* Clozure Lisp 
* cl-utilities
* iterate
* lisp-unit 
* trivial-benchmark 