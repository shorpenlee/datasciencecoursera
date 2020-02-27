test1[which(test1$Month==6),]


colummean <- function(y,removeNA=TRUE){
  nc <- ncal(y)
  means <- numeric(nc)
  for(i in 1:nc){
      means[i] <-mean(y[,i],na.rm=removeNA)
  }
  means
}
#find items in a list
a<-c("name1","name2","name3")
sapply(c("name3","name1"),grep,a)


for (i in 1:2){
  inp <- read.csv(file=paste(getwd(),"/","specdata","/",files[i],sep=""),header = TRUE)
  cname <- name <- sub(".csv", "",files[i])
  cat("Read:", files[i], "\trows: ", nrow(inp), " cols: ", ncol(inp),"\n")
  assign( name, inp)
}
#setwd("/Users/shorpen/Downloads/")
#setwd("/Users/shorpen/Coursera")
# getwd, setwd. R is always pointed at a directory on your computer
readdata<-function(directory,id=1:332){
  #setwd(directory)
  files <- list.files(directory,pattern = "*.csv")
  for (i in id){
    inp <- read.csv(file=paste(getwd(),"/",directory,"/",files[i],sep=""),header = TRUE)
    cname <- name <- sub(".csv", "",files[i])
    cat("Read:", files[i], "\trows: ", nrow(inp), " cols: ", ncol(inp),"\n")
    assign( name, inp)
  }
}
pollutantmean<-function(directory,pollutant,id=1:332){
  #setwd(directory)
  files <- list.files(directory,pattern = "*.csv")
  c2<-vector()
  for (i in id){
    inp <- read.csv(file=paste(getwd(),"/",directory,"/",files[i],sep=""),header = TRUE)
    cname <- name <- sub(".csv", "",files[i])
    #cat("Read:", files[i], "\trows: ", nrow(inp), " cols: ", ncol(inp),"\n")
    c2<-c(c2,inp[,which(colnames(inp)==pollutant)])
    assign(name, inp)
  }
  #cat(c2)
  mean(c2,na.rm = TRUE)
}
pollutantmean("specdata","sulfate",1:10)
pollutantmean("specdata", "nitrate", 70:72)




complete <- function(directory, id=1:332){
  files <- list.files(directory,pattern = "*.csv")
  c2<-vector()
  for (i in 1:332){
    inp <- read.csv(file=paste(getwd(),"/",directory,"/",files[i],sep=""),header = TRUE)
    c2<-rbind(c2,inp)
  }
  test<-data.frame()
  c2 <- c2[complete.cases(c2),]
  for (i in id){
    test<-rbind(test,c(i,nrow(c2[which(c2$ID==i),])))
  }
  names(test) <- c("id","nobs")
  test
}
complete("specdata", c(2, 4, 8, 10, 12))
complete("specdata", 30:25)



corr <- function(directory,threshold=0){
  files <- list.files(directory,pattern = "*.csv")
  c2<-vector()
  for (i in 1:332){
    inp <- read.csv(file=paste(getwd(),"/",directory,"/",files[i],sep=""),header = TRUE)
    c2<-rbind(c2,inp)
  }
  test<-data.frame()
  c2 <- c2[complete.cases(c2),]
  for (i in 1:332){
    test<-rbind(test,c(i,nrow(c2[which(c2$ID==i),]),cor(c2[which(c2$ID==i),2],c2[which(c2$ID==i),3])))
  } 
  names(test) <- c("id","nobs","cor")
  show(test)
  test[which(test[,2]>threshold),3]
}


cr <- corr("specdata", 150)
head(cr)
summary(cr)
