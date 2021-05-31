#Aux functions

#split data:
split_data <- function(data,cut_idx){
  train <- slice(data,(1:cut_idx))
  test <- slice(data,(cut_idx:n()))
  return <- list("train"=train,"test"=test)
  return(return)
}

#convert the price to returns
convert_to_logret <- function(stock_price){
  returns <- 100*diff(log(stock_price))
  return(returns)
}

convert_from_logret <- function(returns, initial){
  stock_price <- exp(cumsum(returns/100))*initial
  return(stock_price)
}

#shifting function
shift <- function(df,column,list_shifts){
  for (i in list_shifts) {
    name <- paste(column,toString(i))
    if (i > 0){
      df[name] <- c(df[-seq(i),column], rep(NA, i))
    }
    else if (i < 0){
      temp <- seq(from=length(df[,column])+i+1,to=length(df[,column]))
      df[name] <- c(rep(NA,-i), df[-temp,column])
    }
  }
  return(df)
}