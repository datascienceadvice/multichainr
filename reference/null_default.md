# Null-default operator

This infix operator provides a convenient way to handle `NULL` values by
providing a default value.

## Usage

``` r
a %||% b
```

## Arguments

- a:

  An object to check for `NULL`.

- b:

  The default value to return if `a` is `NULL`.

## Value

`a` if it is not `NULL`, otherwise `b`.

## Examples

``` r
"value" %||% "default"
#> Error in "value" %||% "default": could not find function "%||%"
NULL %||% "default"
#> Error in NULL %||% "default": could not find function "%||%"
```
