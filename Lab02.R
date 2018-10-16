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
