populational_stdev <- function(data) {
  mean <- mean(data)
  square_sum <- sum((data - mean) ^ 2)
  result <- sqrt(square_sum/length(data))
  return(result)
}

sample_stdev <- function(data) {
  mean <- mean(data)
  square_sum <- sum((data - mean) ^ 2)
  result <- sqrt(square_sum / (length(data) - 1))
  return(result)
}

populational_var <- function(data) {
  mean <- mean(data)
  square_sum <- sum((data - mean) ^ 2)
  result <- square_sum/length(data)
  return(result)
}

sample_var <- function(data) {
  mean <- mean(data)
  square_sum <- sum((data - mean) ^ 2)
  result <- square_sum / (length(data) - 1)
  return(result)
}
