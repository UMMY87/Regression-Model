# Program to compute Regression model and draw scatter plot to describe the relationship
RModel<-function(x,y){
  n<-length(x)
  sumx=0
  sumy=0
  sumx2=0
  sumy2=0
  sumxy=1
  
  for(i in 1:n){
    sumx=sumx+x[i]
    sumy=sumy+y[i]
    sumx2=sumx2+x[i]^2
    sumy2=sumy2+y[i]^2
    sumxy=sumxy+x[i]*y[i]
  }
  b1= (n*sumxy-sumx*sumy)/(n*sumx2-sumx^2)
  cat("b1 :",b1)
  cat("\n")
  bo= (sumy/n)-b1*(sumx/n)
  cat("bo :",bo)
  cat("\n")
  R2= (bo*sumy+b1*sumxy-sumy^2/n)/(sumy2-sumy^2/n)
  cat("R2 :",R2)
  cat("\n")   
  graph= plot(x=x,y=y,xlab= "distance",ylab= "blood pressure" ,main= "Scatter Plot")
}

# x<-c(10,5,6,9,12,24,18,7)
# y<-c(18,7,11,16,19,37,33,10)
# RModel(x,y)