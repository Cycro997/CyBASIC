# PYLANG
## Official documentation for the programming language PyLang
### Version 0.0.2 (24/4/2024)

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
if the if statement fails.
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
&lt;args> is the arguments
#### Returns
Returns RESULT only if <args> has the correct length.
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
#### `^`
```L ^ R```\
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
### Unary
#### `+`
```+O```\
Returns O
#### `-`
```-O```\
Returns O * -1
## Datatypes
### number
Used for numbers.
### \<function>
Used for functions
### str
Used for strings
#### Escape sequences
| Sequence | Replacement |
|:--------:|:-----------:|
|   `\n`   |   newline   |
|   `\t`   |     tab     |
### list
Used for lists

## Built-in values
### NULL
A special value with the value of "0"
### FALSE
A special value with the value of "0"
### TRUE
A special value with the value of "0"
