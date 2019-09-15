options(digits=10)
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

write.csv(dt,'data_pre_AAPL.csv',row.names=FALSE)
