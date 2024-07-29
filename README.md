# Package dtutils

## Introduction

Efficient group-by processing functions for data.table written with Rcpp.

See:

* https://chataignon.com/dtutils.html
* https://chataignon.com/na_fill.html

## `na_fill_by`

### Description

Replace NA by previous or next value, optionally by group

### Usage

```r
na_fill_by(dt, var = NULL, by = NULL, type = 1L, inplace = FALSE)
```

### Arguments

Argument      |Description
------------- |----------------
```dt```     |     a data.table
```var```     |     name(s) of variable(s) with atomic values ; if 'var' is omitted, all variables not in 'by' are selected
```by```     |     name(s) of variable(s) which determines groups (optional) ; if 'by' is ommitted, dt is considered as one group
```type```     |     specifies type of filling : 1 LOCF, 2 NOCB, 3 LOCF then NOCB.
```inplace```     |     when inplace = TRUE, na_fill is compute inplace

### Value


 a list with item for each var


### Note

 When `type = 1` , LOCF = Last Observation Carried Forward. Ex : `c(NA, 1, NA, NA, 2, NA, 3, NA)` gives `c(NA, 1, 1, 1, 2, 2, 3, 3)`
 When `type = 2` , NOCB = Next Observation Carried Backward. Ex : `c(NA, 1, NA, NA, 2, NA, 3, NA)` gives `c(1, 1, 2, 2, 2, 3, 3, NA)`
 When `type = 3` , LOCF then NOCB. Ex : `c(NA, 1, NA, NA, 2, NA, 3, NA)` gives `c(1, 1, 1, 1, 2, 2, 3, 3)` .

## `na_replace`

### Description

Replace NA by fill value

### Usage

```r
na_replace(dt, var = NULL, fill, inplace = FALSE)
```
## `first_by`

### Description

Returns a logical TRUE for first row of each group

## `last_by`

### Description

Returns a logical TRUE for last row of each group

## `row_number_by`

 Create a sequence 1...N by group

### `case_when_mult`

### Description

Case_when like function with multiple constant values

### Usage

```r
case_when_mult(...)
```
### Examples

```r
 library(data.table)
 dt <- data.table(a = c(1:5, NA, 7:10), b=rnorm(10), c=letters[1:10])
 dt[, c("w", "x") :=
 case_when_mult(
 a %% 4 == 0 ~ list("p", 1.5),
 a %% 2 == 0 ~ list("m", 4) ,
 T ~ list("i", 6)
 )
 ]
 print(dt)
 ```

## `coalesce_by`

### Description

Returns first non NA value for var by group

## `shift_by`

### Description

Returns lag or lead values for var by group

## `wsum_by`

### Description

Returns weighted sum for var by group

## `all_by`

### Description

Returns TRUE if all boolean are TRUE by group

## `any_by`

### Description

Returns TRUE if any boolean is TRUE by group
