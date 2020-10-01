#Function takes in file name of igotu file w/ satelite data

igotu_read <- function(file) {
  require(lubridate)
  require(tidyr)

  # Read in the data
  data<-read.csv(file,fill = T,stringsAsFactors = F,header = T,skipNul = T, sep=",")

  a<-paste(data$Date,data$Time)
  DateTime=ymd_hms(paste(data$Date,data$Time), tz="GMT")

  ###takes the Satelite column and splits it into 1s and 0s
  nrow(data)
  S<-NULL
  for (i in 1:nrow(data)){
    x<-data$Satelite[i]
    print(i)
    g<-substring(x, seq(1,nchar(x),1), seq(1,nchar(x),1))
    g<-t(data.frame(substring(x, 1:37, 1:37)))
    g[g=="-"]<-0
    g[g=="X"]<-1
    g<-as.numeric(g)
    gg=c(g[3:10],g[12:19],g[21:28],g[30:37])
    S=rbind(S,gg)
  }

  #calculates the number of satilites for each location
  SatNum=(rowSums (S, na.rm = FALSE, dims = 1))

  Locs<-data.frame(Index=as.numeric(data$Index),
                   DateTime=DateTime,
                   lat=as.numeric(data$Latitude),
                   lon=as.numeric(data$Longitude),
                   Altitude=as.numeric(data$Altitude),
                   Speed=as.numeric(data$Speed),
                   Course=as.numeric(data$Course),
                   Distance=as.numeric(data$Distance),
                   SatNum)
  return(Locs)
}

