setwd("R")

# Write a function named 'pollutantmean' that calculates the mean of a pollutant (sulfate or nitrate) across a specified list of monitors. 
# The function 'pollutantmean' takes three arguments: 'directory', 'pollutant', and 'id'. Given a vector monitor ID numbers, 'pollutantmean' reads that monitors' particulate matter data from the directory specified in the 'directory' argument and returns the mean of the pollutant across all of the monitors, ignoring any missing values coded as NA. A prototype of the function is as follows
library(stringr)


pollutantmean<-function(directory, pollutant,id=1:332){
  m<-NULL
  for(i in id){
    id_<-str_pad(i,3,pad="0")
    data<-read.csv(paste0(directory,"//",id_,".csv"))
    m<-c(m,na.omit(data[,pollutant]))
    #print(m[i])
  }
  return(mean(na.omit(m)))
}

pollutantmean("specdata","sulfate",id=1:332)


# Write a function that reads a directory full of files and reports the number of completely observed cases in each data file. The function should return a data frame where the first column is the name of the file and the second column is the number of complete cases. A prototype of this function follows

complete<-function(directory,id=1:332){
  df<-data.frame()
  for(i in id){
    id_<-str_pad(i,3,pad="0")
    data<-read.csv(paste0(directory,"//",id_,".csv"))
    nobs<-nrow(data[!is.na(data$nitrate)&!is.na(data$sulfate),])
    df<-rbind(df,c(i,nobs))
  }
  colnames(df)<-c("id","nobs")
  return(df)
}

complete("specdata",id=1:10)
set.seed(42)
cc <- complete("specdata", 332:1)
use <- sample(332, 10)
print(cc[use, "nobs"])

# Write a function that takes a directory of data files and a threshold for complete cases and calculates the correlation between sulfate and nitrate for monitor locations where the number of completely observed cases (on all variables) is greater than the threshold. The function should return a vector of correlations for the monitors that meet the threshold requirement. If no monitors meet the threshold requirement, then the function should return a numeric vector of length 0. A prototype of this function follows

corr<-function(directory,threshold=0){
  df<-complete("specdata")
  id<-df$id[df$nobs>threshold]
  correlations<-c()
  for(i in id){
    id_<-str_pad(i,3,pad="0")
    data<-read.csv(paste0(directory,"//",id_,".csv"))
    data<-data[!is.na(data$nitrate)&!is.na(data$sulfate),]
    c_<-cor(data$nitrate,data$sulfate)
    correlations<-c(correlations,c_)
  }
  return(correlations)
}
corr("specdata",10)

cr <- corr("specdata")                
cr <- sort(cr)                
set.seed(868)                
out <- round(cr[sample(length(cr), 5)], 4)
print(out)

cr <- corr("specdata", 2000)                
n <- length(cr)                
cr <- corr("specdata", 1000)                
cr <- sort(cr)
print(c(n, round(cr, 4)))
  
  
  
  