# CyBASIC
## Official documentation for the programming language CyBASIC
### Version 0.0.3 (10/7/2024)

## Keywords
### `var`
#### Purpose
Used to declare variables.
#### Syntax
```var IDENTIFIER = VALUE```\
`IDENTIFIER` is the variable name.\
`VALUE` is the value assigned to it.
#### Returns
Always returns the new value of the variable.
### `and`
#### Purpose
Used for a logical AND operation.\
Can be considered coalescing.
#### Syntax
```LEFT and RIGHT```
#### Returns
Returns RIGHT if LEFT is false, else LEFT
### `or`
#### Purpose
Used for a logical OR operation.\
Can be considered coalescing.
#### Syntax
```LEFT or RIGHT```
#### Returns
Returns LEFT if RIGHT is false, else RIGHT
### `not`
#### Purpose
Used for a logical NOT operation.
#### Syntax
```not OPERAND```
#### Returns
Returns 1 if OPERAND is false, else 0
### `if`
#### Added in v0.0.2
#### Purpose
Used for an if statement
#### Syntax
```if COND then RESULT```
#### Returns
Returns RESULT if COND is truthy otherwise returns the next condition.
### `then`
#### Added in v0.0.2
#### Purpose
Used to declare code blocks in certain statements
#### Syntax
```<expression> then BLOCK```
#### Returns
Does not return anything
### `elif`
#### Added in v0.0.2
#### Purpose
Used to add more conditions to an if statement but is only executed\
if the 'if' statement fails.
#### Syntax
```<if_stmt> elif COND then RESULT```
#### Returns
Returns RESULT if COND is truthy otherwise returns the next condition.
### `else`
#### Added in v0.0.2
#### Purpose
Used to add a result if all conditions fail in an if statement.
#### Syntax
```<conditional> else RESULT```
#### Returns
Returns RESULT.
### `while`
#### Added in v0.0.2
#### Purpose
Used for a while loop, which loops *while* the condition is true.
#### Syntax
```while COND then RESULT```
#### Returns
Doesn't return anything.
### `for`
#### Added in v0.0.2
#### Purpose
Used for a for loop.
#### Syntax
1. ```for NAME = START to END then RESULT```
2. ```for NAME = START to END step STEP then RESULT```
#### Returns
Doesn't return anything.
### `to`
#### Added in v0.0.2
#### Purpose
Used in ranges to specify an end value
#### Syntax
1. ```START to ENDT```
2. ```START to END step STEP```
#### Returns
Doesn't return anything.
### `step`
#### Added in v0.0.2
#### Purpose
Used in ranges to specify a step value
#### Syntax
1. ```START to END step STEP```
#### Returns
Doesn't return anything.
### `func`
#### Added in v0.0.2
#### Purpose
Used to define functions
#### Syntax
```func IDENTIFIER(<args>) -> RESULT```
&lt;args&gt; is the arguments
#### Returns
Returns RESULT only if <args> has the correct length.
### `end`
#### Added in v0.0.3
#### Purpose
Ends a code block
#### Syntax
```
<code_block>
end
```
#### Returns
Doesn't return anything
## Operators
### Binary
#### `+`
```L + R```\
Returns the sum of L and R or concatenates them
#### `-`
```L - R```
Returns the difference between L and R
#### `*`
```L * R```\
Returns the product of L and R
#### `/`
```L / R```\
Returns the quotient of L and R
#### `**`
```L ** R```\
Returns L to the Rth power
#### `==`
```L == R```\
Returns 1 if L is equal to R else 0
#### `<`
```L < R```\
Returns 1 if L is less than R else 0
#### `>`
```L > R```\
Returns 1 if L is greater than R else 0
#### `<=`
```L <= R```\
Returns 1 if L is less than or equal to R else 0
#### `>=`
```L >= R```\
Returns 1 if L is greater than or equal to R else 0
#### `()`
```C(A)```
Calls C with arguments A (May not always fill in A, A may also have multiple values)
#### `[]`
```S[I]```
Performs a subscript operation on S. 'I' is the subscript.
#### `{}`
##### New in v0.0.3
```P{A}```
Gets attribute A of P.
#### `:`
##### New in v0.0.3
```L:R```
Gets attribute L of R
#### `|`
##### New in v0.0.3
```L|R```\
Performs bitwise or on L and R.
#### `^`
##### New in v0.0.3
```L^R```\
Performs bitwise xor on L and R.
#### `&`
##### New in v0.0.3
```L&R```\
Performs bitwise and on L and R.
### Unary
#### `+`
```+O```\
Returns O
#### `-`
```-O```\
Returns O * -1
#### `-`
```-O```\
Returns O * -1
#### `~`
##### New in v0.0.3
```^O```\
Performs bitwise not on O
## Datatypes
### number
Used for numbers.
### \<function>
Used for functions
### str
Used for strings
#### Escape sequences
| Sequence |   Replacement   |
|:--------:|:---------------:|
|   `\n`   |     newline     |
|   `\t`   |       tab       |
|   `\r`   | carriage return |
### list
Used for lists
### boolean
#### New in v0.0.3
Used for boolean values
### null
#### New in v0.0.3
Used for the built-in null value
### built_in_func
#### New in v0.0.3
Used for built-in functions
## Built-ins
### NULL
A special value representing null
### false
A special value representing false
### true
A special value representing true
### print
Function that prints its argument
### print_ret
Returns its argument
### input
Used for user input
### input_int
Same as input, but converts to int
### clear
Clears the screen
### dir
Returns the global symbol table
### open
Opens a file
### exit
Exits 
