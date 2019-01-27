#' Module server function
#' @importFrom prettymapr geocode
#' @importFrom dplyr mutate filter select
city_statistic <- function(filters=NULL, city_data=NULL) {
  
      data_inter <- city_data %>% dplyr::filter(
        as.numeric(unlist(distance)) > filters$distance[1]*1000 & as.numeric(unlist(distance)) < filters$distance[2]*1000 
      )
      
      return(
        switch(EXPR = filters$gender,
                           "a" = data_inter %>% dplyr::select("average","median","chaser","distance","id","lng","lat","total_elevation_gain","city_name"),
                           "f" = data_inter %>% dplyr::select(average="average_F",median="median_F",chaser="chaser_F","distance","id","lng","lat","total_elevation_gain","city_name"),
                           "m" = data_inter %>% dplyr::select(average="average_M",median="median_M",chaser="chaser_M","distance","id","lng","lat","total_elevation_gain","city_name")
        )
      )
  
}