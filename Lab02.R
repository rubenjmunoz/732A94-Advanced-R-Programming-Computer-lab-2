name = "Ruben Munoz"
liuid = "123"

sheldon_game <- function(player1 , player2) {
  alt <- c("rock", "lizard", "spock", "scissors", "paper")
  stopifnot(player1 %in% alt, player2 %in% alt)
  alt1 <- which(alt %in% player1)
  alt2 <- which(alt %in% player2)
  if (alt1 == alt2) {
    return("Draw!")
  }
  else{
    if (any((alt1 + c(1, 3)) %% 5 == alt2)) {
      return("Player 1 wins!")
    }
    else {
      return("Player 2 wins!")
    }
  }
}

my_moving_median <- function(x , n, ...){
  return(unlist(lapply(1:(length(x) - n), function(y) median(x[y:(y + n)], na.rm = ...))))
}

for_mult_table <- function(from, to){
  if(!is.numeric(c(from,to))){
    stop()
  }
  return(c(from:to) %o% c(from:to))
}

cor_matrix <- function(df){
  return(cor(df))
}

find_cumsum <- function(x, find_sum) {
  if(!is.numeric(c(x, find_sum))){
    stop()
  }
  s <- 0
  i <- 0
  while (s <= find_sum) {
    i = i + 1
    if (!is.na(x[i])) {
      s <- s + x[i]
    }
    else{
      s <- s + 0
    }
    if(i == length(x)){
      break()
    }
  }
  return(s)
}


while_mult_table <- function(from, to){
  return(c(from:to) %o% c(from:to))
}

trial_division_factorization <- function(x) {
  a <- c()             
  f <- 2               
  while (x > 1) {
    if (x %% f == 0) {
      a <- c(a, f)        
      x <- x / f      
    }
    else{
      f = f + 1       
    }
  }
  return (a)
}

repeat_find_cumsum <- function(x, find_sum) {
  i <- 0
  s <- 0
  repeat {
    i = i + 1
    s = s + x[i]
    if ((i == length(x)) ||(s > find_sum)) {
      break()
    }
  }
  return(s)
}

repeat_my_moving_median <- function(x, n, ...){
  i <- 0
  v <- c()
  repeat{
    i = i + 1
    v = c(v, median(x[i:(n + i)], na.rm = ...))
    if ((length(x) - i) == n){
      break()
    }
  }
  return(v)
}

in_environment <- function(env){
  return(ls(env))
}

cov <- function(X){
  if(!is.data.frame(X)){
    stop()
  }
  return(unlist(lapply(X, function(y) sd(y)/mean(y))))
}

moment <- function(i){
  stopifnot(is.numeric(i), TRUE)
  function(y) {
    a = mean((y - mean(y))^i)
    return(a)
  } 
}
