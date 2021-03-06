---
title: "hw03-daniel-vo.Rmd"
author: "Daniel Vo"
date: "3/21/2018"
output: github_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE, error = TRUE, fig.path = 'images/')
```


```{bash}
curl -O https://raw.githubusercontent.com/ucb-stat133/stat133-spring-2018/master/data/nba2017-roster.csv
```

```{bash}
cut -d "," -f 2 nba2017-roster.csv | tail +2 > a.txt

sort -u a.txt > team-names.txt

head -n 5 team-names.txt
```


```{bash}
cut -d "," -f 3 nba2017-roster.csv | tail +2 > b.txt

sort -u b.txt > position-names.txt

head -n 5 position-names.txt

```


```{bash}
cut -d "," -f 7 nba2017-roster.csv | tail +2| sort | uniq -c | sort -r | head -n 5 > experience-counts.txt
```


```{bash}
grep "team\|LAC" nba2017-roster.csv > LAC.csv
cat LAC.csv
```

```{bash}
grep  "LAL" nba2017-roster.csv > LAL.csv
```

```{bash}
cut -d "," -f 6 LAL.csv| sort | uniq -c
```
```{bash}
grep "CLE" nba2017-roster.csv | wc -l
```
```{bash}
grep "team\|GSW" nba2017-roster.csv | cut -d "," -f 1,4,5 > gsw-height-weight.csv
cat gsw-height-weight.csv
```
```{bash}
grep "team" nba2017-roster.csv | cut -d "," -f 1,8 > top10-salaries.csv 

cut -d "," -f 1,8 nba2017-roster.csv | sort -n -r -t "," -k 2 | head -n 10 >> top10-salaries.csv

cat top10-salaries.csv
```

```{R}
is_integer <- function (x){
  if (x %% 1 == 0){
    return(TRUE) } else {
    return(FALSE)
  }
}


is_positive <- function(x){
  if (x > 0) {
    return(TRUE)} else{
    return(FALSE)
  }
}

is_nonnegative <- function(x) {
  if (x>=0) {
    return(TRUE)} else {
      return(FALSE)
  }
}

is_positive_integer <- function(x){
  if (x > 0 & x %% 1 == 0){
    return(TRUE)} else{
    return(FALSE)
  }
}

is_nonneg_integer <- function(x){
  if(x>=0 & x %% 1 == 0) {
    return(TRUE)} else {
    return(FALSE)
  }
}

is_probability <- function(x) {
  if(x>=0 & x<=1) {
    return(TRUE)} else {
    return(FALSE)
  }
}

bin_factorial <- function(x){
  for(i in c(1:x)) {
    y <- 1
    y <- y*(c(1:x)[i])
    print(y)
  }
}

bin_factorial <- function(x){
  factorial <- 1
  for(i in 1:x){
  factorial <- factorial*((1:x)[i])
  }
  return(factorial)
}
bin_factorial(4)


bin_combination <- function(n, k) {
  bin_combination <- bin_factorial(n) / (bin_factorial(k) *bin_factorial(n-k))
  return(bin_combination)
}
bin_combination(5,3)


bin_probability <- function(n, k, p){
  if (is_nonneg_integer(n)==FALSE) {
    stop ("number of trials have to be a positive integer")} else {
      if(is_nonneg_integer(k)==FALSE) {
        stop ("number of successes have to be a positive integer")} else{
          if (is_probability(p)==FALSE){
            stop ("probability has to be between 0 and 1")} else {
  bin_probability <- bin_combination(n, k) * (p^k) * ((1-p)^(n-k))
  return(bin_probability)
            }
        }
    }
}

bin_probability(5, 3, 0.5)
```
