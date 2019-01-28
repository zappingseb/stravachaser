#' Strava Segments all over Europe
#'
#' Dataset containing information on Strava segments for cycling
#' 
#' @format A data frame with a lot of STRAVA data
#' \describe{
#'  \item{id}{Strava Segment ID}
#'  \item{resource_state}{xx}
#'  \item{name}{Name of the segment}
#'  \item{climb_category}{cycling climbing category}
#'  \item{climb_category_desc}{ description of category}
#'  \item{avg_grade}{ grade compared to other segments}
#'  \item{total_elevation_gain}{ elevation gain in m}
#'  \item{distance}{ length of segment}
#'  \item{polyline}{ Google Encoded Polyline}
#'  \item{starred}{ whether starred by package developer}
#'  \item{lat}{ latitude of starting point}
#'  \item{lng}{ longitude of starting point}
#'  \item{end_lat}{ latitude of ending point (sparse)}
#'  \item{end_lng}{ latitude of ending point (sparse)}
#'  \item{average}{ average speed of top 60 chasers 30m + 30f}
#'  \item{median}{ median speed of top 60 chasers 30m + 30}
#'  \item{average_M}{ average speed of top 30 men on segment}
#'  \item{median_M}{ median speed of top 30 men on segment}
#'  \item{average_F}{ average speed of top30 women on segment}
#'  \item{median_F}{ median speed of top30 women on segment}
#'  \item{chaser_F}{ # of female participants on segment}
#'  \item{chaser_M}{ # of male participants on segment}
#'  \item{chaser}{ # of participants (top30 male + top30 female)}
#' }
#' @source \url{http://www.strava.com}
"all_data_table_strava"