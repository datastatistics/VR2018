calculate_ssq_scores <- function(ssq.df, var){
  score.df <- melt(read.csv("Experiment I/SSQ/Data/ssq_score.csv", sep = ";"), id.vars = c("N", "O", "D"))
  number.df <- count(ssq.df, vars = var) #users number for each configuration
  
  users_count <- length(ssq.df$user)
  
  max_values.df <- data.frame(matrix(nrow = 1, ncol = 4))
  colnames(max_values.df) <- c("n", "o", "d", "ts")
  max_values.df$n[1] <- 28 * 9.54 #seven symptoms with the 4 score each
  max_values.df$o[1] <- 28 * 7.58
  max_values.df$d[1] <- 28 * 13.92
  max_values.df$ts[1] <- (max_values.df$n[1] + max_values.df$o[1] + max_values.df$d[1]) * 3.74
  
  temp.ar <- array(dim = c(16,6)) #temporary table to get the symptom score for each user
  
  if(var == "output"){
    result.df <- data.frame(ssq.df$user,
                          ssq.df$output,
                          matrix(nrow = users_count, ncol = 8))
  }else if (var == "setup"){
    result.df <- data.frame(ssq.df$user,
                            ssq.df$setup,
                            matrix(nrow = users_count, ncol = 8))
  }
  
  result.df <- result.df[1:(users_count),]
  colnames(result.df) <- c("user", var, "bn", "bo", "bd", "bts", "an", "ao", "ad", "ats")
  
  ssq.df <- melt(ssq.df, id.vars = c("user", var)) #turn the table in a data frame
  ssq.df <- ssq.df[with(ssq.df, order(user)),] #reorder df by user value
  
  i = 1 #line counter
  row = 1 #row counter
  
  while (i <= length(ssq.df$user)){
    x = 1 #symptom counter
    while(x <= 16){
      temp.ar[x, 1] <- ssq.df$value[i] * score.df$N[x]
      temp.ar[x, 2] <- ssq.df$value[i] * score.df$O[x]
      temp.ar[x, 3] <- ssq.df$value[i] * score.df$D[x]
      x <- x + 1
      i <- i + 1
    }
    
    x = 1
    while(x <= 16){
      temp.ar[x, 4] <- ssq.df$value[i] * score.df$N[x]
      temp.ar[x, 5] <- ssq.df$value[i] * score.df$O[x]
      temp.ar[x, 6] <- ssq.df$value[i] * score.df$D[x]
      x <- x + 1
      i <- i + 1
    }
    
    result.df[row, 3] <- sum(temp.ar[,1]) * 9.54
    result.df[row, 4] <- sum(temp.ar[,2]) * 7.58
    result.df[row, 5] <- sum(temp.ar[,3]) * 13.92
    result.df[row, 6] <- (sum(temp.ar[,1]) + sum(temp.ar[,2]) + sum(temp.ar[,3])) * 3.74
    result.df[row, 7] <- sum(temp.ar[,4]) * 9.54
    result.df[row, 8] <- sum(temp.ar[,5]) * 7.58
    result.df[row, 9] <- sum(temp.ar[,6]) * 13.92
    result.df[row, 10] <- (sum(temp.ar[,4]) + sum(temp.ar[,5]) + sum(temp.ar[,6])) * 3.74
    row = row + 1
  }
  
  return(result.df)
}