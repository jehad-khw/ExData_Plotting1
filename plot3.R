##x<- readLines("household_power_consumption.txt",n=5); x ## check what data look like
########Scan file for the needed records###############
filename<- paste0("C:/R/PRA -ExploreData -CourseProj1/","household_power_consumption.txt")
conn<-file(filename,"r",blocking = FALSE)
datarownames<-scan(conn,what="a",nlines=1,sep=";") ##read header
chunksize<-10000
coll_data<-data.frame()

for(i in seq(1,2075259,chunksize)){
  
  d<-scan(conn,what="a",nlines=chunksize,sep=";",quiet=TRUE,na.strings = "?")
  d<-t(matrix(d,nrow=9))
  d<-data.frame(d,stringsAsFactors = FALSE)
  d[,1]<-as.Date(strptime(d[,1],"%d/%m/%Y"))
  ##d[,2]<-strptime(d[,2],"%H:%M:%S")
  d<-d[d$X1==as.Date(strptime("01/02/2007","%d/%m/%Y")) | d$X1==as.Date(strptime("02/02/2007","%d/%m/%Y")) ,]
  
  if (nrow(d) > 0) {
    if (nrow(coll_data)==0) coll_data<-d
    if (nrow(coll_data)>0) coll_data<-rbind(coll_data,d)
    print(Sys.time())
  }
}
close.connection(conn)
names(coll_data)<-datarownames
rm(d);rm(filename);rm(i);rm(datarownames);rm(conn);rm(chunksize)###remove unneeded objects
coll_data<-coll_data[order(coll_data$Date,coll_data$Time),] ## sort data to avoid issues in Plot "line"
coll_data[,3:9]<- sapply(coll_data[,3:9],as.numeric ) ## convert all values to numeric to be plotted


table(coll_data$Date) ##Print summary for dates selected
#######################################################
par(mfcol = c(1, 1))

###Plot the third graph
plot.new()
png(filename = "plot3.png", width = 480, height = 480)
plot(as.POSIXct(paste(coll_data$Date,coll_data$Time)),coll_data$Sub_metering_1,type = "l",xlab = "",ylab = "Energy sub metering")
lines(as.POSIXct(paste(coll_data$Date,coll_data$Time)),coll_data$Sub_metering_2,col="red")
lines(as.POSIXct(paste(coll_data$Date,coll_data$Time)),coll_data$Sub_metering_3,col="blue")
legend("topright", lty=c(1,1,1), lwd=c(2.5,2.5,2.5),col=c("black","red","blue"),legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"))
dev.off(4)
rm(coll_data)