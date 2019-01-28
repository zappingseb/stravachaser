#' Plot two datasets in left and right direction
#' 
#' @param label \code{character} Character vector of the labels for each row of
#'    the histogram
#' @param left  \code{numeric}  Vector of the left side plot values
#' @param right  \code{numeric}  Vector of the right side plot values
#' @param unit  \code{character}  A unit that can be added to all numerical values and
#'    will be displayed in the plot
#' 
#' @import grid
#' @import gridExtra
#' @import ggplot2
#' @importFrom rlang .data
#' @author Sebastian Wolf \email{sebastian@@mail-wolf.de}
#' 
#' @details 
#' from: https://stackoverflow.com/questions/18265941/two-horizontal-bar-charts-with-shared-axis-in-ggplot2-similar-to-population-pyr
left_right_plot <- function(label="# of segments",
                            left=500,
                            right=200,
                            unit = ""
){
  if(length(left)==0)left<-0
  if(length(right)==0)right<-0
  
  DATA<-data.frame(state=c(label),sales=c(right),sales_staff=c(left))
  DATA$state <- factor(DATA$state, levels = label)
  
  # Center labels
  g.mid<-ggplot(DATA,aes(x=1,y=.data$state))+geom_text(aes(label=.data$state),angle = 90)+
    geom_segment(aes(x=0.94,xend=0.96,yend=.data$state))+
    geom_segment(aes(x=1.04,xend=1.065,yend=.data$state))+
    ggtitle("")+
    ylab(NULL)+
    scale_x_continuous(expand=c(0,0),limits=c(0.94,1.065))+
    theme(axis.title=element_blank(),
          panel.grid=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          panel.background=element_blank(),
          axis.text.x=element_text(color=NA),
          axis.ticks.x=element_line(color=NA),
          plot.margin = unit(c(1,-1,1,-1), "mm"))
  
  # Left side plot
  g1 <- ggplot(data = DATA, aes(x = .data$state, y = .data$sales_staff)) +
    geom_bar(stat = "identity",fill="#fc4c02") +
    theme(axis.title.x = element_blank(), 
          axis.title.y = element_blank(), 
          axis.text.y = element_blank(), 
          axis.ticks.y = element_blank(), 
          plot.margin = unit(c(1,-1,1,0), "mm"),
          panel.background = element_blank())+ ylim(0,max(left,right,na.rm=T)) + 
    geom_text(aes(label = paste(.data$sales_staff,unit)),
              position = position_dodge(width = .5), hjust = -2, size = 6,
              angle=00,color="white") + 
    scale_y_reverse() + coord_flip()
  
  # Right side plot
  g2 <- ggplot(data = DATA, aes(x = .data$state, y = .data$sales)) +xlab(NULL)+
    geom_bar(stat = "identity",fill="#fc4c02") +
    theme(axis.title.x = element_blank(), axis.title.y = element_blank(), 
          axis.text.y = element_blank(), axis.ticks.y = element_blank(),
          plot.margin = unit(c(1,0,1,-1), "mm"),panel.background = element_blank()
    ) + ylim(0,max(left,right,na.rm=T)) +
    geom_text(aes(label = paste0(.data$sales," ",unit)),
              position = position_dodge(width = .5), hjust = 2, size = 6,
              angle=00,color="white") + 
    coord_flip()
  
  
  gg1 <- ggplot_gtable(ggplot_build(g1))
  gg2 <- ggplot_gtable(ggplot_build(g2))
  gg.mid <- ggplot_gtable(ggplot_build(g.mid))
  
  grid.arrange(gg1,gg.mid,gg2,ncol=3,widths=c(4/9,1/9,4/9))
}

#' Create a Leaflet map with radius + circle markers
#' 
#' Create a map of a certain location. It is surrounded by a ring
#' in a certain radius. Additionally for each segment of \code{segment_data}
#' a point will be created with the link to the STRAVA segment
#' 
#' @param segment_data \code{data.frame} DF with cols lng, lat, name, id
#'   for each segment
#' @param radius \code{numeric} in meter
#' @param marker_list \code{numeric} center of the map containing lon and lat variable
#' @param zoom \code{numeric} leaflet zoom
#' 
#' @import leaflet
#' @importFrom glue glue
#' @importFrom rlang .data
#' @author Sebastian Wolf
segment_map <- function(
  segment_data=data.frame('id'=character(0),'lat'=0,'long'=0,'name'=character(0)),
  radius=2000,
  marker_list = data.frame("lon"=0,"lat"=45),
  zoom=4){
  icons <- awesomeIcons(
    icon = 'bicycle',
    iconColor = 'black',
    library = 'fa',
    markerColor = "#fc4c02"
  )
  
  leaflet() %>%
    addProviderTiles(leaflet::providers$Stamen.TonerLite,
                     options = providerTileOptions(noWrap = TRUE)
    ) %>%
    setView(lat = marker_list$'lat',lng = marker_list$'lon', zoom = zoom) %>%
    addAwesomeMarkers(data = marker_list, icon=icons) %>%
    addCircles(lat = marker_list$'lat',lng=marker_list$'lon',radius = radius,color="#fc4c02",fillColor="#e6e6eb") %>%
    addCircleMarkers(data=segment_data[,c("lng","lat")],
                     popup = glue::glue(
                       "<a target=\\'_new\\' href=\\'https://www.strava.com/segments/{segment_data$id}\\'>{segment_data$name}</a>"),
                     radius = 0.5,color = "#fc4c02")
}