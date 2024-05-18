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


print_test <- function(test, sample_value, crit_value, crit_value2=NULL) {
  if(test == ">") {
    if(sample_value > crit_value) {
      print(paste("REJECT H0 - ", sample_value, " > ", crit_value))
    }
    
    else {
      print(paste("ACCEPT H0 - ", sample_value, " <= ", crit_value))
    }
  }
  
  else if(test == "<") {
    if(sample_value < crit_value) {
      print(paste("REJECT H0 - ", sample_value, " < ", crit_value))
    }
    
    else {
      print(paste("ACCEPT H0 - ", sample_value, " >= ", crit_value))
    }
  }
  
  else if(test == "!=" && !is.null(crit_value2)) {
    if(sample_value < crit_value || sample_value > crit_value2) {
      print(paste("REJECT H0 - ", sample_value, " is not between ", crit_value, " and ", crit_value2))
    }
    
    else {
      print(paste("ACCEPT H0 - ", crit_value, " <= ", sample_value, " <= ", crit_value2))
    }
  }
}