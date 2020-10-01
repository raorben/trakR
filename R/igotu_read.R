#' igotu_read function is specifically for reading in the long format log files
#'
#' Index Date	       Time	    Latitude	 Longitude	 Altitude	 Speed	  Course	 Distance	 Type	        Timeout	 MSVs_QCN	 Weight Criteria	 SleepTime	 EHPE	     Satelite ID	  Satelite
#'   1	 2000/00/00	 04:00:00	-213.05751 -181.193954 -15137926	1526868	231	     0	       00(00000000)	14	     B0	       A1	               94	        0	         -1599995880	  X-X----- X-X---X- -------- ---XX---
#'   2	 2015/04/30	 22:32:16	0	         0	         21.859999	0	      0	       0	       -2(000000D2)	14	     B0	       A1	               111	      0	         0	            -------- -------- -------- --------
#'   3	 2015/04/30	 22:32:59	44.622578	-124.045845	 11.16	    0	      291	     0	       00(00000000)	42	     C8	       3E	               0	        3008	     671354896	    --X-X--- -----X-- ---X---- ---X----
#'   4	 2015/04/30	 22:33:00	44.622578	-124.045845	 11.16	    0	      291	     0	       00(00000000)	1	       B8	       45	               0	        2832	     671354896	    --X-X--- -----X-- ---X---- ---X----
#'
#' The objective of this function to parse the igotu files (file sturcture
#' shown above) and specifically count the number of satelites that were
#' registered in the last column. NOTE: it takes a long time to run & I am sure
#' there are ways to make it go faster!
#'
#' @author Rachael A. Orben \email{rachael.orben@@oregonstate.edu}
#' @param file file with the structure from above
#' @return A new data frame with Index, DateTime, la, lon, Altitude, Speed,
#' Course, Distance, EHPE, and SatNum
#'
#' all original data plus two new columns: TripNum
#'   (consecutive trip number 0 = at colony) and ColonyMovement (in/out colony
#'   movements)
#' @importFrom dplyr lead bind_rows
#' @export


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
                   EHPE=data$EHPE,
                   SatNum)
  return(Locs)
}

