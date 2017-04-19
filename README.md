bind-in
=======

elisp library for typechecking, and some other stuff I guess? I was trying to make macros for checks / operations I do a lot in elisp and realized I could turn it into *The DSL From Macro Hell*.

# Metacharacters

Currently:

1. `>`
2. `<`
3. `|`
4. `~`
5. `/`
6. `:`
7. `$`
8. `=`
9. `^`
10. `@`
11. `!` (can add to invocation of any var to have it throw if nil instead of just returning `nil`)

# TODO

Have a pretty good idea of what I need to do, but don't have enough time right now.

1. metachars transform expressions directly like macros (in `bind-in--deconstruct-symbol`)
2. entries in `bind-in-basic-type-alist` are interpreted (after metachar transformation) with `bind-in--deconstruct-expr` (implicit `and`)
3. allow configuring of what symbols are replaced / allow user to use prefixes if there are clashes with any other macros or real variables they use (lol)

# License

[GPL](GPL.md)
