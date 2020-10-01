#' InterpointTime Calculate the time between points on a track for each bird
#'
#' Vector of times between points
#'
#' @author Abram B. Fleishman \email{abram@@conservationmetrics.com}
#' @param tracks data.frame of data to be queried for lat and long values
#' @param ID quoted name of column in data that is and ID field
#' @param DateTime quoted name of column in data that has DateTime values in the format YYYY-mm-dd HH:MM:SS
#' @importFrom dplyr lead
#' @return A vector of times differnces in seconds between adjacent points in an animal track
#' PointDurLag_sec is the time difference between the point and the proceeding point
#' PointDurLead_sec is the time difference between the point and the following point
#' @export
#############################################################################################
# Calculate time between points in a timeseries
#############################################################################################

InterpointTime<-function(tracks, ID="File", DateTime="DateTime"){

    # Initialize a vector wehere the data will be dumped, for time differences.
  dataOut <- NULL
  Birds <- unique(tracks[[ID]])

  # Run a for loop, where for each unique key, it subsets the data by that key and calculates the difference in time.
  for(i in 1:length(Birds)) {

    Data<-tracks[tracks[[ID]] == Birds[i],]

    Data$PointDur <- NA
    Data$PointDur <- lead(Data[[DateTime]]) - Data[[DateTime]]

    Data$PointDurLag_sec <- NA
    Data$PointDurLag_sec <- difftime(Data[[DateTime]], dplyr::lag(Data[[DateTime]], 1),units = "secs")

    Data$PointDurLead_sec <- NA
    Data$PointDurLead_sec <- difftime( dplyr::lead(Data[[DateTime]]), Data[[DateTime]],units = "secs")


    dataOut<-c(dataOut,Data$PointDur,Data$PointDur_sec)
  }

  return(dataOut)
}