#Data import
setwd('C:/Users/kkarr/Documents/Share/Projects/Realtime_stocks')
dt = read.csv("data_AAPL.csv") 
print(head(dt))
t_ceo='2011-08-24'
t_index = match(t_ceo,dt$date)
dt$date <- as.Date(dt$date,format = "%Y-%m-%d") 
print(t_index)
set.seed(1)

#Plot function
plot_fun <- function(dt1,dt2,type,title,xlab,ylab,col,jpg,save) {
  if (save == 1) {jpeg(jpg+".jpg")}
  plot(dt1,dt2,type,main=title,xlab=xlab,ylab=ylab,col=col)
  if (save == 1) {dev.off()}}

save=0 #Save jpg image

#Main Plot
p1 <- plot_fun(dt[,1],dt[,5],"l","AAPL Closing Price vs Time"
         ,"Year","Closing Price in USD","black",save,"main_plot")
abline(v=dt[t_index,1],col="red",lty=2)

#Tim Cook Data Plot
dt=dt[-(0:t_index),]

plot_fun(dt[,1],dt[,6],"l","Volume Traded since Tim Cook became CEO vs Time"
         ,"Year","Volume in shares","red",save,"volume_tplot")

p2 <- plot_fun(dt[,1],dt[,5],"l","Closing Price since Tim Cook became CEO vs Time"
         ,"Year","Closing Price in USD","steelblue",save,"close_tplot")

# Data Preprocessing
#empty <- array(1:nrow(dt))  
#change index
rownames(dt) <- 1:nrow(dt)
dt=subset(dt, select = -c(open,high,low,volume))

binary <- function(dt){
  empty <-integer(nrow(dt))
  dt$action <- empty
  for(i in 1:(nrow(dt)-1)){
    if (dt[i,"close"]<dt[i+1,"close"]){
      dt[i,"action"]=1
    }
    
  }
  return(dt)
}
#dt <-binary(dt)
head(dt)


check <- function(p_in,p_out,data){
  p <- p_in+p_out
  num <- length(data)-(p)+1
  mat <- matrix(1,num,p)
  for(i in 1:(num)){mat[i,] <- data[i:(i+(p-1))]}
  #return(mat)
  return(list("x" = mat[,1:p_in], "y" = mat[,(p_in+1):(p)]))
}

mat <- check(12,12,dt$close)
x_train <- reshape_X_3d(mat$x)
y_train <- reshape_X_3d(mat$y)
nrow(x_train)
dim(y_train)
#NN
library(tensorflow)
library(keras)
model <- keras_model_sequential() 

model %>%
  layer_lstm(
    units = 128,
    batch_input_shape = c(1, 12, 1)
  )
summary(model)

model %>% compile(
  loss = "logcosh",
  optimizer = optimizer_adam( lr= 0.02, decay = 1e-6 ),  
  metrics = c('accuracy')
)

Epochs = 50   
for(i in 1:Epochs ){
  model %>% fit(x_train, y_train, epochs=1, batch_size=1, verbose=1, shuffle=FALSE)
  model %>% reset_states()
}




