shapiro_non_significance <- "you can accept the null hypothesis that the samples come from a normal distribution"
fligner_non_significance <- "you can reject the null hypothesis that all populations variances are equal"

significance_level_star <- function(value)
{
  star <- "ns"
  if(value <= 0.001)
    star <- "***"
  else if(value <= 0.01)
    star <- "**"
  else if(value <= 0.05)
    star <- "*"
  
  return(star)
}

alpha <- 0.05

transform_data <- function(group)
{
  for (i in range(1, length(group))){
    group[i] <- log10(group[i])
  }
  return(group)
}

############################################################################
# analysis between subjects  ###############################################
# kruskal -> var1 = value; var2 = variable; list = groups  ###############
# friedman -> var1 = value; var2 = variable1; var3 = variable2  #########
# groups = numer of groups to be compared  #################################
############################################################################

parametric_independent_analysis <- function(var1, var2, var3, groups)
{
  result = NULL
  if(groups > 2){
    res = anova(lm(var1 ~ var2))
    print (res)
    result = res[1,5]
  }else{
    res = t.test(var1 ~ var2, paired = FALSE)
    print(res)
    result = res$p.value
  }
  
  return(result)
}

nonparametric_independent_analysis <- function(var1, var2, var3, groups){
  result = NULL
  if(groups > 2){
    res = fligner.test(var1, var2)
    if(res$p.value > alpha){ #verifica se as variâncias são iguais, se sim, então executa o teste kruskal
      res = kruskal.test(var1 ~ var2)
      print(res)
      result = res$p.value
    }else{
      #one-way analysis of means (not assuming equal variances)
      res = oneway.test(var1 ~ var2, na.action=na.omit, var.equal=FALSE)
      print(res)
      result <- res$p.value
    }
  }else{
    res = wilcox.test(var1 ~ var2, paired = FALSE)
    print(res)
    result = res$p.value
  }
  return(result)
}

parametric_dependent_analysis <- function(var1, var2, var3, groups){
  result = NULL
  if(groups > 2){
    res = anova(lm(var1 ~ var2))
    print (res)
    result = res[1,5]
  }else{
    #Welch Two Sample t-test
    res = t.test(var1 ~ var2)
    print(res)  
    result = res$p.value
  }
  return(result)
}

nonparametric_dependent_analysis <- function(var1, var2, var3, groups){
  result = NULL
  if(groups > 2){
    res = fligner.test(var1, var2)
    if(res$p.value > alpha){ #verifica se as variâncias são iguais, se sim, então executa o teste kruskal
      res = friedman.test(var1, var2, var3)
      print(res)     
      result = res$p.value
    }else{
      #one-way analysis of means (not assuming equal variances)
      res = oneway.test(var1 ~ var2, na.action=na.omit, var.equal=FALSE)
      print(res)
      result <- res$p.value
    }
  }else{
    print("here")
    res = wilcox.test(var1 ~ var2, paired = TRUE)
    print(res)
    result = res$p.value
  }
  return(result)
}

cochranq.test <- function(mat)
{
  k <- ncol(mat)
  
  C <- sum(colSums(mat) ^ 2)
  R <- sum(rowSums(mat) ^ 2)
  T <- sum(rowSums(mat))
  
  num <- (k - 1) * ((k * C) - (T ^ 2))
  den <- (k * T) - R
  
  Q <- num / den
  
  df <- k - 1
  names(df) <- "df"
  names(Q) <- "Cochran's Q"
  
  p.val <- pchisq(Q, df, lower = FALSE)
  
  QVAL <- list(statistic = Q, parameter = df, p.value = p.val,
               method = "Cochran's Q Test for Dependent Samples",
               data.name = deparse(substitute(mat)))
  class(QVAL) <- "htest"
  return(QVAL)
}





