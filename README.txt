Didn't cheat. I don't remember the full statement

Part 1: Compiler Basics
Types: 
boolean - represented #t for true and #f for false
strings - immutable and global character arrays. Denoted by "string" 
integers - integers stored as signed 32-bit ints
symbols -  immutable and global character arrays similar to strings. Denoted by 
	'symbol
cons - pairs with a head and tail. If the tail is a list, then the cons-cell is 
	also a list. Denoted (head . tail).
vector - untyped array. Always initialized with a size and that size is fixed. 
	Denoted #(e, e, e ...)
void - #<void> 
null - '() represents an empty list  
char - 32 bit character

Primitive Operations: All prims can be used with an apply form as well
(null? v) - returns #t if v is the empty list '() or #f otherwise
(cons? c) - returns #t if c is a cons cell or #f if c is not a cons cell. 
(car [cons? c]) - returns the head of a cons-cell c. If c is not a cons cell the 
	program will print [[expected a cons cell or smth]] at runtime
(cdr [cons? c]) - returns the tail of a cons-cell c. If c is not a cons cell the
	program will print [[expected a cons cell or smth]] at runtime
(cons h t) - returns a cons cell with h as the head and t as the tail. If t 
	satisfies (list? t), then (cons h t) will return a list. Otherwise, 
	(cons h t) will return a pair. 
(+ [number? e] ...) - takes a variable number of integers e and returns the sum 
	of all e. If called on no variables, (+) will return 0. If given arguments 
	that are not integers the program will print error statement at runtime. 
(- [number? n] [number? d] ...) - takes an integer n and a variable number of 
	integers d. Returns the difference of n and the sum of all d. If called 
	with no d, (- n) will return 0-n. 
(* [number? e] ...) - takes a variable number of integers e and returns the product 
	of all e. If called on no variables, (*) will return 1. If given arguments 
	that are not integers the program will print error statement at runtime. 
(/ [number? n] [number? d] ...) - takes an integer n and a variable number of integers 
	d. Returns the quotient of n and the product of all d. If called with no d, 
	(/ n) will return 1/n, however in the version with the gc, this may cause the 
	program to segfault. If the product of n is equal to 0, the program will raise 
	a "Error:_Division_by_0" error, which can be caught with a guard. 

(> [number? a] [number? b]) - will #t return if a is greater than b or #f otherwise
(< [number? a] [number? b]) - will #t return if a is less than b or #f otherwise
(>= [number? a] [number? b]) - will #t return if a is greater than or equal to b or 
	#f otherwise
(<= [number? a] [number? b]) - will #t return if a is less than or equal to b or #f
	otherwise
(= [number? a] [number? b]) - will #t return if a is equal to b or #f otherwise
(promise? p) - returns true if p is a promise. Promises are not natural datums; p 
	must be be a previously delayed value (delay x). 
(void? v) - returns #t if v is void or #f otherwise
(void) - returns #<void>
(eq? a b) - returns true if a and b are equal
(halt r) - halts the program immediately when called and prints the value of r
(void) - returns a void value

(number? n), (integer? n) - as this compiler does not take non-integer number values, 
	both these functions will return #t if n is an integer and #f otherwise
(vector-ref [vector? v] [number? i]) - returns the value at index i in vector v
(make-vector [number? n] v) - creates a vector of size n where each element is set 
	to v.
(vector v ...) - creates a vector where the all the elements are the v in the order 
	they are given. The size is set to be the number of v given. 
(vector-set! [vector? v] [number? i] a) - takes vector v and updates its index i to 
	be the value a
(not [boolean? b]) - will turn #f to #t and vice versa
(print v) - prints any value. The way various datatypes are printed is listed above.
(procedure? p) - returns #t if p is a procedure, #f otherwise.




PART 2: Runtime Errors

Handled Errors
Function is provided too many arguments: When a function is applied on more arguments
	 than it takes, the program will halt at runtime and print a 
	"Too many arguments" error. 
This is implemented in remove-varargs, where every lambda with fixed number of 
	arguments includes a series of cars and cdrs to separate the passed in list. 
	If the final cdr operation does not result in an empty list, the function was 
	applied too many arguments. 

Function is provided too few arguments: When a function is applied on fewer arguments 
	than it takes, the program will halt at runtime and print a "Too few arguments" 
	error. 
This is implemented in remove-varargs, where every lambda with fixed number of arguments 
	includes a series of cars and cdrs to separate the passed in list. If any car 
	operation is attempted on a non-cons cell, then the function was provided too
	few arguments.

Division by zero: When the division would result in dividing by zero, the program will 
	raise a "Division by zero" error at the site of the function call. This error 
	can be caught by a guard statement. 
This is implemented in top-level by wrapping all calls to the division operator with a 
	lambda that checks whether the divisor is zero.

Non-function value is applied: When a non-function value is applied, the program will 
	halt at runtime and print a "Non-function application" error.
This is implemented in remove-varargs where function application has been simplified. 
	All values that are in function position are simply wrapped with a call to 
	procedure?

Use of not-yet-initialized letrec or letrec* variable: When using an a variable that 
	hasn't been bound using either lambda, let, let*, letrec, letrec*, define, or 
	guard, the program will halt at runtime and print a "Uninitialized variable" error. 
This is implemented in its own pass, catch-unbound, called immediately after top-level. 
	catch-unbound passes along a set of variables bound through calls to lambda, 
	let, let*, letrec, letrec*, define, or guard. Any symbols found not in that set 
	will changed to a call to halt. The error is called using halt instead of raise 
	because for symbols in argument position, a raise statement would be of the 
	wrong datatype, causing a different error. 

Unhandled Errors
A memory-use cap (e.g., 256MB) is exceeded. This error is not handled. This compiler 
	does not have a memory use cap. 

Vector-ref out of bounds - There is no check for using vector-ref with out of bounds 
	arguments. This would likely cause errors with memory, as the vector-ref 
	function will simply retrieve data from outside its bounds.

PART 3: New Feature (chars & string functions) (NOT IMPLEMENTED)
\#[char] - chars have been added to compilers but have limited functionality
mutable strings - among the PTR tags is STR_PTR, which would have been used as a 
	mutable character array to implement string operations. 


PART 4: Boehm Garbage collection

There are two headers provided, header.cpp, the reference header and header-gc.cpp, 
	which has a different tagging system and uses the Boehm gc. 

When using header-gc with TOGGLE_GC set to true, the compiler will use the Boehm 
	Garbage collector. 
In header-gc, all non-pointer values (and strings and symbols) are tagged as usual. All 
	pointer values are stored under a PTR_TAG and specific tags, similar to vectors 
	in the original header. All tags still take up the last 3 bits. PTR_TAG is 
	equal to 0, so all pointers allocated by GC_MALLOC will already have the 
	pointer tag and encoding pointers will not change the last 3 bits of a pointer.
 
Other than closures, all pointer types (closures/procedures, vectors, mutable strings, 
	cons) are wrapped in an array (who's pointer is from GC_MALLOC) where the first 
	element holds the specific pointer tag for that type. For example, a cons-cell 
	is now stored as a 3 part array, where the first element is equal to the CONS_PTR 
	tag and can be used to identify it was a cons. Vectors already had a similar 
	encoding, and as they are also pointer object, the OTHER_TAG has now been removed 
	and vectors are tagged as pointers. The exception to this tagging scheme are closures. 
	Closures are created outside of the header, so it is inconvenient to manipulate them, 
	so the tag for closures is simply 0 so they don't have to be encoded or decoded.

The emitted llvm has also been modified to store all local variables to the stack so the garbage 
	collector doesn't accidentally collect them.


The compiler includes a file called "test.rkt" which will run all public tests when run, 
	as well as a single error handling test. 
