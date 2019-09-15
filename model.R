library(tensorflow)
library(keras)

#Load object
setwd('C:/Users/kkarr/Documents/Share/Projects/Realtime_stocks')
dt = readRDS("mat.RDS")
#dt$x_test dt$y_test dt$x_val dt$y_val

#NN
batch = 100
epochs = 50
hidden = 128

model <- keras_model_sequential() 
model %>%
  layer_lstm(units = hidden,batch_input_shape = c(batch,12,1),return_sequences=TRUE,dropout = 0)%>% 
  layer_lstm(units = hidden,return_sequences=TRUE,dropout = 0)%>% 
  layer_lstm(units = hidden,return_sequences=TRUE,dropout = 0)%>% 
  layer_lstm(units = hidden,return_sequences=FALSE,dropout = 0)%>% 
  layer_dense(units = 128, activation='relu')%>%
  layer_dense(units = 64, activation='relu')%>%
  layer_dense(units = 32, activation='relu')%>%
  layer_dense(units = 16, activation='relu')%>%
  layer_dense(units = 1, activation = 'linear')
summary(model)

model %>% compile(
  loss = "logcosh",
  optimizer = 'sgd',  
  metrics = c("mean_squared_error")
)
history <-model %>% fit(x=dt$x_train, y=dt$y_train, 
                        epochs=epochs, batch_size=batch, 
                        verbose=1, shuffle=FALSE)


predictions <- model %>% predict(dt$x_test,batch_size=batch)

View(predictions)
View(dt$y_test)
View(dt$x_test)

print(dt$xtest)

compare_bin <-function(a,b){
  print(a[,ncol(a)],1)
  
}

compare_bin(dt$x_test,dt$y_test)
