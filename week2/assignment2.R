
pollutantmean<- function (directory, pollutant, id) {
  
  #setwd(directory)
  
  sums=0
  len=0
  
  for (i in id) {
    
    a <- i
    file_name=paste(formatC(a, width=3, flag="0"), ".csv", sep="")  
    df<- read.csv(file=file_name, header = TRUE)
    
    if (pollutant=="nitrate") 
      { df_col = na.omit(df$nitrate)
      } else 
      { df_col = na.omit(df$sulfate) }
    
    sums = sums + sum(df_col)
    len = len + length(df_col)
  }
  
  sums/len
  
}




complete <- function (directory, id) {
  #setwd(directory)
  count = 1
 # id <- c(2, 4, 8, 10, 12)
  nobs <- vector(mode="numeric", length=length(id))
  for (i in id){
    a <- i
    file_name=paste(formatC(a, width=3, flag="0"), ".csv", sep="")  
    df<- read.csv(file=file_name, header = TRUE)
    nobs[count] = nrow(na.omit(df))
    count = count+1
  }
  df2 <- data.frame(id , nobs) 
  df2
}



corr<- function (directory, threshold){
df_nobs<- complete("specdata", 1:332)
#nrow(df[df$nobs>150,])
#threshold = 400
val = df_nobs[df_nobs$nobs>threshold,1] # array of id's which satisfy threshold
count = 1
res_vec = vector(mode = "numeric", length = length(val))
for (i in val){
  
  a <- i
  file_name=paste(formatC(a, width=3, flag="0"), ".csv", sep="")  
  df_temp<- read.csv(file=file_name, header = TRUE)
  df_work<- na.omit(df_temp)
  
  res_vec[count] = cor(df_work$sulfate, df_work$nitrate)
  count = count + 1
}

res_vec
}



