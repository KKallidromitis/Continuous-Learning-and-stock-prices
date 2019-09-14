library(devtools)
library(tensorflow)
library(keras)
#Data import
setwd('C:/Users/kkarr/Documents/Share/Projects/Realtime_stocks')
dt = read.csv("data_AAPL.csv") 
print(head(dt))
t_ceo='2011-08-24'
t_index = match(t_ceo,dt$date)
dt$date <- as.Date(dt$date,format = "%Y-%m-%d") 
print(t_index)
set.seed(1)

bin_acc = 0
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
if (bin_acc ==1) {dt <-binary(dt)}
head(dt)

library(recipes)
rec_obj <- recipe(close ~ ., dt) %>%
  step_sqrt(close) %>%
  step_center(close) %>%
  step_scale(close) %>%
  prep()

dt <- bake(rec_obj, dt)
head(dt)

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
  sample_size = floor(0.8*nrow(mat))
  View(mat)
  return(train_ind)
  
  return(list("x" = mat[,1:p_in], "y" = mat[,(p_in+1):(p)]))
}
x_dim <- 12
y_dim <- 1
if (bin_acc ==1) {mat <- check(x_dim,1,dt,1)
} else {mat <- check(x_dim,y_dim,dt,0)}
STOP
x_train <- addim(mat$x,0)

if (bin_acc ==1 | y_dim ==1){y_train <- addim(mat$y,1)
} else {y_train <- addim(mat$y,0)}
t_split=0.20



#NN
batch = 100
epochs = 10
hidden = 50

model <- keras_model_sequential() 
model %>%
  layer_lstm(units = hidden,batch_input_shape = c(batch,12,1),return_sequences=TRUE,dropout = 0)%>% 
  layer_lstm(units = hidden,return_sequences=TRUE,dropout = 0)%>% 
  layer_lstm(units = hidden,return_sequences=TRUE,dropout = 0)%>% 
  layer_lstm(units = hidden,return_sequences=FALSE,dropout = 0)%>% 
  layer_dense(units = 64, activation='relu')%>%
  layer_dense(units = 1, activation = 'linear')
summary(model)

model %>% compile(
  loss = "logcosh",
  optimizer = 'sgd',  
  metrics = c("mean_squared_error")
)
dim(x_train)
history <-model %>% fit(x=x_train, y=y_train, 
                        epochs=epochs, batch_size=batch, 
                        verbose=1, shuffle=FALSE,
                        validation_split = 0.1)




