#Load data
options(digits=10)
setwd('C:/Users/kkarr/Documents/Share/Projects/Realtime_stocks')
dt = read.csv('data_pre_AAPL.csv')
head(dt)
options(warn=-1)
bin_acc=0
library(rjson)
result <- fromJSON(file = "https://www.alphavantage.co/query?function=TIME_SERIES_DAILY&symbol=AAPL&apikey=PFU1A043W2NWFMES")
#View(result$`Time Series (Daily)`)

#Merge API results
dates <- names(result$`Time Series (Daily)`) 
vec <- vector()
for (i in 1:length(dates)){
  if (dates[i] %in% dt$date){break}
  val = as.numeric((result$`Time Series (Daily)`)[[i]]$`4. close`)
  vec <- c(vec,val)
}
num=i-1
if (num>0){
  x <- data.frame("date" = dates[1:num], "close" = vec)
  dt <- merge(dt,x,all=TRUE)
}
tail(dt)

#Cross Validation and Rolling Origin split
write.csv(dt,'data_pre_AAPL.csv',row.names=FALSE)
dt=dt[1000:length(dt),]
s_mean = mean(dt$close)
s_sd = sd(dt$close)
dt$close = (dt$close-s_mean)/s_sd

head(dt)

#Seperate data
addim <- function(mat,len){
  if (len ==1){dim(mat) <- c(length(mat),1)
  } else{dim(mat) <- c(dim(mat)[1],dim(mat)[2],1)}
  
  return(mat)
}

check <- function(p_in,p_out,data,bin_acc){
  p <- p_in+p_out
  num <- length(data$close)-(p)+1
  mat <- matrix(1,num,p)
  if (bin_acc==1){for(i in 1:(num)){mat[i,] <- c(data$close[i:(i+(p-2))],dt$action[(i+(p-2))])}
  } else {for(i in 1:(num)){mat[i,] <- data$close[i:(i+(p-1))]}}
  tr_size = floor(0.8*nrow(mat))
  val_size = floor(0.1*tr_size)
  tr_dt = mat[1:tr_size,]
  val_dt = tr_dt[(tr_size-val_size+1):tr_size,]
  #tr_dt = tr_dt[1:(tr_size-val_size),]
  ts_dt = mat[(tr_size):(dim(mat)[1]-1),]
  
  return(list("x_train" = tr_dt[,1:p_in], "y_train" = tr_dt[,(p_in+1):(p)],
              "x_val" = val_dt[,1:p_in], "y_val" = val_dt[,(p_in+1):(p)],
              "x_test" = ts_dt[,1:p_in], "y_test" = ts_dt[,(p_in+1):(p)]))
}

x_dim <- 12
y_dim <- 1

mat <- check(x_dim,y_dim,dt,bin_acc)
for (n in 1:length(mat)){
  if (is.null(dim(mat[[n]]))){a = 1
  } else {a = 0 }
  print(a)
  #mat[[n]] <- addim(mat[[n]],a)
}
ss = c(s_mean,s_sd)
saveRDS(mat, "mat.RDS")
saveRDS(ss, "ss.RDS")


