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



bin_combination <- function(n, k) {
  bin_combination <- bin_factorial(n) / (bin_factorial(k) *bin_factorial(n-k))
  return(bin_combination)
}



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

bin_probability(10, 3, 1/6)

for (i in 3:5) {
  print(bin_probability(10, i, 1/6))
}
