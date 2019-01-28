#' Module server function
#' 
#' @param filters list of filters
#' @param city_data Data frame containing segments for two cities
#' 
#' @return data filtered by statistic
#' 
#' @importFrom prettymapr geocode
#' @importFrom dplyr mutate filter select
#' @importFrom rlang .data
city_statistic <- function(filters=NULL, city_data=NULL) {
  
      # Filter the data by length and # of athletes
      data_inter <- city_data %>% dplyr::filter(
        as.numeric(unlist(.data$distance)) > filters$distance[1]*1000 & as.numeric(unlist(.data$distance)) < filters$distance[2]*1000 
      ) %>% dplyr::filter(as.numeric(.data$chaser) > filters$chaser)
      
      return(
        switch(EXPR = filters$gender,
                           "a" = data_inter %>% dplyr::select("average","median","chaser","distance","id","lng","lat","total_elevation_gain","city_name","radius"),
                           "f" = data_inter %>% dplyr::select(average="average_F",median="median_F",chaser="chaser_F","distance","id","lng","lat","total_elevation_gain","city_name","radius"),
                           "m" = data_inter %>% dplyr::select(average="average_M",median="median_M",chaser="chaser_M","distance","id","lng","lat","total_elevation_gain","city_name","radius")
        )
      )
  
}