# PYLANG
## Official documentation for the programming language PyLang
### Version 0.0.0 (4/16/2024)

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
## Operators
### Binary
#### `+`
```L + R```\
Returns the sum of L and R
#### `-`
```L - R```\
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
### Precedence
1. Parenthesis
2. Exponentiation 
3. Unary `+` and Unary '-'
4. Multiplication and Division
5. Addition and Subtraction
6. Logical operators and Comparison
7. Variable assignment