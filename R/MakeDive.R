#' MakeDive Function to make a DiveNum variable
#'
#' The objective of this function to to partition diving data into "dives"
#' based on begining and returning to a fixed location (the surface).
#' It was based on the MakeTrip codes.  This function relies on a column with a
#' distance in m from each point along the track to the colony and a distance
#' cutoff.  It numbers each dive starting with the first point beyond the
#' distance cutoff and ending with the last point beyond the cutoff with a
#' sequential number starting with 1.  There is also a time gap column and criteria to
#' prevent dives from spanning over unsampled intervals. All points inside
#' the cutoff (surface) are labled 0.  In the future it could be nice to have
#' these labeled with sequetial numbers as well. The input variables are identified by
#' column index numbers in the data.frame (tracks).
#'
#' @author Rachael A. Orben & Abram B. Fleishman \email{abram@@conservationmetrics.com}
#' @param tracks data.frame of data that you want to parse into trips
#' @param ID quoted name of column in data that is a unique key to individual
#'   bird_tag_deployment combos. This is the File from the GypsyLocRead output
#' @param DistCutOff Distance in km to use as a cut off radius around the colony
#'   to use to split the trips
#' @param Dist2Colony quoted name of column in data that has the distance in km
#'   from each point to the colony
#' @param NumLocCut Minimum number of points to consider in a trip
#' @return A new data frame with all original data plus two new columns: TripNum
#'   (consecutive trip number 0 = at colony) and ColonyMovement (in/out colony
#'   movements)
#' @importFrom dplyr lead bind_rows
#' @export
