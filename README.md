# Package dtutils

## Introduction

Efficient group-by processing functions for data.table written with Rcpp.

See:

* https://chataignon.com/dtutils.html
* https://chataignon.com/na_fill.html

## `na_fill_by`

### Description


Replace NA by constant, previous or next value, optionally by group


### Usage

```r
na_fill_by(dt, var = NULL, by = NULL, type = 1L, inplace = FALSE, fill = NA)
```


### Arguments

Argument      |Description
------------- |----------------
```dt```     |     a data.table
```var```     |     name(s) of variable(s) with atomic values ; if 'var' is omitted, all variables not in 'by' are selected
```by```     |     name(s) of variable(s) which determines groups (optional) ; if 'by' is ommitted, dt is considered as one group
```type```     |     specifies type of filling : 0 constant 1 LOCF, 2 NOCB, 3 LOCF then NOCB. See note below.
```inplace```     |     when inplace = TRUE, na_fill is compute inplace
```fill```     |     when type=0, NA is replaced by this 'fill' value. For type = 1, 2 or 3, 'fill' is unused.

### Value


 a list with item for each var


### Note


 When `type = 0` , all NA values are replaced by the `fill` value. Ex with 'fill = 0' : `c(NA, 1, NA, NA, 2, NA, 3, NA)` 
 gives `c(0, 1, 0, 0, 2, 0, 3, 0)` 
 
 When `type = 1` , LOCF = Last Observation Carried Forward. Ex : `c(NA, 1, NA, NA, 2, NA, 3, NA)` gives `c(NA, 1, 1, 1, 2, 2, 3, 3)` 
 
 When `type = 2` , NOCB = Next Observation Carried Backward. Ex : `c(NA, 1, NA, NA, 2, NA, 3, NA)` gives `c(1, 1, 2, 2, 2, 3, 3, NA)` 
 
 When `type = 3` , LOCF then NOCB. Ex : `c(NA, 1, NA, NA, 2, NA, 3, NA)` gives `c(1, 1, 1, 1, 2, 2, 3, 3)` .


### Examples

```r 
 library(data.table)
 ngrp <- 18000
 nbygrp <- 50
 dt <- data.table(
 id=rep(1:ngrp, each=nbygrp),
 a=sample(1:100, nbygrp * ngrp, replace=TRUE),
 b=sample(LETTERS, nbygrp * ngrp, replace=TRUE),
 c=sample((1:100 + 0.5), nbygrp * ngrp, replace=TRUE),
 d=sample(1:100, nbygrp * ngrp, replace=TRUE),
 e=sample(1:100, nbygrp * ngrp, replace=TRUE)
 )
 dt[(1:.N %% 2 == 0), a:=NA]
 dt[(1:.N %% 3 == 0), b:=NA]
 dt[(1:.N %% 3 == 0), c:=NA]
 dt[(1:.N %% 4 == 0), d:=NA]
 dt[(1:.N %% 5 == 0), e:=NA]
 
 vars = c("a", "c", "d", "e")
 
 dt1 <- copy(dt)
 na_fill_by(dt1, var=vars, by="id", inplace=TRUE)
 dt2 <- copy(dt)
 dt2[, (vars) := na_fill_by(.SD, vars, "id")]
 dt3 <- copy(dt)
 dt3[, na_fill_by(.SD, vars, "id", inplace=TRUE)]
 ``` 

## `first_by`

### Description

Returns a logical TRUE for first row of each group

### Usage

```r
first_by(dt, by)
```

### Arguments

Argument      |Description
--------------|----------------
```dt```      |     a data.table
```by```      |     name(s) of variable(s) which determines groups

### Value

 an vector of integer

### Examples

```r 
 dt <- data.table(id = rep(1:10, each=5))
 dt[first_by(dt, "id"), pos := "F"]
 dt[last_by(dt, "id"), pos := "L"]
 ``` 
## `last_by`

### Description

Returns a logical TRUE for last row of each group

### Usage

```r
last_by(dt, by)
```
### Arguments

Argument      |Description
------------- |----------------
```dt```     |     a data.table
```by```     |     name(s) of variable(s) which determines groups

### Value


an vector of integer


### Examples

```r 
 dt <- data.table(id = rep(1:10, each=5))
 dt[first_by(dt, "id"), pos := "F"]
 dt[last_by(dt, "id"), pos := "L"]
 ``` 

## `row_number_by`

### Description


 Create a sequence 1...N by group


### Usage

```r
row_number_by(dt, by)
```


### Arguments

Argument      |Description
------------- |----------------
```dt```     |     a data.table
```by```     |     name(s) of variable(s) which determines groups

### Value


 an vector of integer


### Examples

```r 
 dt1 <- data.table(id = rep(1:10, each=5))
 dt1[, row_num := row_number_by(dt, "id")]
 dt2 <- data.table(id1 = rep(1:100, each=50), id2 = rep(1:1000, each=5))
 dt2[, row_num := row_number_by(dt, c("id1", "id2"))]
 ``` 

### `case_when_mult`

### Description


 Case_when like function with multiple constant values


### Usage

```r
case_when_mult(...)
```


### Arguments

Argument      |Description
------------- |----------------
```...```     |     A sequence of two-sided formulas. The left hand side (LHS) determines which values match this case. The right hand side (RHS) provides the replacement value.  The LHS must evaluate to a logical vector. Each logical vector can either have length 1 or a common length. All RHSs must have the same length, generally list or vector of constant values

### Value


 A vector when only one-length value ; otherwise a data.table with
 one column per value


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


### Usage

```r
coalesce_by(dt, by = NULL, var = NULL)
```


### Arguments

Argument      |Description
------------- |----------------
```dt```     |     a data.table
```by```     |     name(s) of variable(s) which determines groups
```var```     |     name(s) of variable(s) with atomic values ; if 'var' is missing, all variables not in 'by' are selected

### Value


 a named list with item for each var


### Examples

```r 
 library(data.table)
 ngrp <- 18000
 nbygrp <- 50
 dt <- data.table(
 id=rep(1:ngrp, each=nbygrp) * 2,
 a=sample(1:100, nbygrp * ngrp, replace=TRUE),
 b=sample(LETTERS, nbygrp * ngrp, replace=TRUE),
 c=sample((1:100 + 0.5), nbygrp * ngrp, replace=TRUE),
 d=sample(1:100, nbygrp * ngrp, replace=TRUE),
 e=sample(1:100, nbygrp * ngrp, replace=TRUE)
 )
 dt[(1:.N %% 2 == 1), a:=NA]
 dt[(1:.N %% 3 < 2), b:=NA]
 dt[(1:.N %% 4 < 3), c:=NA]
 dt[(1:.N %% 5 < 4), d:=NA]
 dt[(1:.N %% 6 < 5), e:=NA]
 
 coalesce_by(dt, by="id")
 vars = c("b", "c", "e")
 coalesce_by(dt, var=vars, by=c("id", "a"))
 coalesce_by(dt, var=vars, by=c("a"))
 ``` 
