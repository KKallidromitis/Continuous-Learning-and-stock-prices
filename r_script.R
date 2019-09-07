# Core Tidyverse
library(tidyverse)
library(glue)
library(forcats)

# Time Series
library(timetk)
library(tidyquant)
library(tibbletime)

# Visualization
library(cowplot)

# Preprocessing
library(recipes)

# Sampling / Accuracy
library(rsample)
library(yardstick) 

# Modeling
library(keras)
library(tfruns)

#Data import
setwd('C:/Users/kkarr/Documents/Share/Projects/Realtime_stocks')
dt = read.csv("data_AAPL.csv") 
print(head(dt))
t_ceo='2011-08-24'
t_index = match(t_ceo,dt$date)
dt$date <- as.Date(dt$date,format = "%Y-%m-%d") 
print(t_index)

#Plot function
plot_fun <- function(dt1,dt2,type,title,xlab,ylab,col,jpg,save) {
  if (save == 1) {jpeg(jpg+".jpg")}
  plot(dt1,dt2,type,main=title,xlab=xlab,ylab=ylab,col=col)
  if (save == 1) {dev.off()}}

save=0 #Save jpg image

#Main Plot
plot_fun(dt[,1],dt[,5],"l","AAPL Closing Price vs Time"
         ,"Year","Closing Price in USD","black",save,"main_plot")
abline(v=dt[t_index,1],col="red",lty=2)

#Tim Cook Data Plot
dt=dt[-(0:t_index),]

plot_fun(dt[,1],dt[,5],"l","Closing Price since Tim Cook became CEO vs Time"
         ,"Year","Closing Price in USD","steelblue",save,"close_tplot")

plot_fun(dt[,1],dt[,6],"l","Volume Traded since Tim Cook became CEO vs Time"
         ,"Year","Volume in shares","red",save,"volume_tplot")


# Data Preprocessing
#change index
rownames(dt) <- 1:nrow(dt)







