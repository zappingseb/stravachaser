#' About description module
#' 
#' @param id \code{character} ID of this module
#' @export
#' @author Sebastian Wolf \email{sebastian@@mail-wolf.de}
about <- function(id){
  ns <- NS(id)
  div(class="about",
      div(HTML("&nbsp;")),
      div(HTML("&nbsp;")),
      fluidRow(
        column(12,
               tags$h2("How can cities race against each other?"),
               column(12,
                      tags$img(id="race1",onclick="enlarge('race1')",src="images/race1.jpg", width="100%", class="about image left")),
               column(12,
                      tags$h3("Background"),
                      HTML("<p>This app uses data from <b><a href='https://strava.com'>strava</a></b> to compare the cyclists in two cities.
                      How is this possible? Strava is a sports tracking app. Basically whenever people cycle they can track themselves using
                      GPS. Afterwards they will know how fast they went on their track. This is not comparable. Though, <a href='https://strava.com'>strava</a>
                      build in a feature called <b><a href='https://support.strava.com/hc/en-us/articles/216918167-Strava-Segments'>segments</a></b>. These
                      segments are tracks of any length all around the world. On each of these segments <a href='https://strava.com'>strava</a> the users
                      can have a virtual race. The time they needed for the segment is stored on a leaderboard. 
                      The fastest cyclist is the King of the Mountain on this segment. The data is available for everybody having a <a href='strava.com'>strava</a>
                      account via the <a href='https://developers.strava.com/'>strava API</a>.
                    </p>
                    ")
               ),
               column(12,
                      tags$img(id="leaderboard",onclick=paste0("enlarge('",ns('leaderboard'),"')"),
                               src="images/leaderboard.png", width="100%", class="about image right")
               ),
               
               column(12,
                      tags$h3("Calculations performed"),
                      HTML("<p>The leaderboards are not just kept for each segment, but also seperately per year and gender.
                To find out which of the four
                cities inside this app is the fastest, I went through some steps. First I went over the 
                <a href='https://developers.strava.com/docs/reference/#api-Segments-exploreSegments'>explore segments</a> API to find segments within
                  the area of the city. I set a maximum radius of ~30 km. Afterwards I crawled the leaderboards of all those segments. By crawling
                  the leaderboards I got the <b>top 30</b> male and top 30 female athletes. A segment can have a maximum number of 60 participants
                  for the statistics I am using. After receiving the data I calculated the <a href='https://en.wikipedia.org/wiki/Median'>median</a> and average speed for males, females and both. 
                  </p>"),
                      tags$h3("The data"),
                      HTML("<p>The data used for this app is not all the data you can have for the cities. The approach
                           used tried to clean the data as good as possible and find good measures. All general numbers
                           around the data are given below."),
                      HTML(paste("<table>
                            <tr>
                              <td># of segments</td><td>421,685</td>
                           </tr>
                            <tr>
                              <td># of segments with average speed (leaderboard crawled)</td><td> 22,589</td>
                           </tr>
                            
                            <tr>
                              <td>Average speed per segment</td><td> ",
                all_data_table_strava %>% select(average) %>% mutate(average = as.numeric(average)) %>%
                  filter(!is.na(average)) %>% filter(average > 0) %>% filter(average < 100) %>%
                  summarise(average = mean(average)) %>% round(2) %>% as.character(),
                          " km/h</td>
                           </tr>
                            <tr>
                              <td>Average <b>speed</b> per segment (men)</td><td> ",
                all_data_table_strava %>% select(average_M) %>% mutate(average = as.numeric(average_M)) %>%
                  filter(!is.na(average)) %>% filter(average > 0) %>% filter(average < 100) %>% 
                  summarise(average = mean(average)) %>% round(2) %>% as.character(),
                          " km/h</td>
                           </tr>
                            <tr>
                              <td>Average speed per segment (women)</td><td> ",
                all_data_table_strava %>% select(average_F) %>% mutate(average = as.numeric(average_F)) %>%
                  filter(!is.na(average)) %>% filter(average > 0) %>% filter(average < 100) %>% 
                  summarise(average = mean(average)) %>% round(2) %>% as.character(),
                          " km/h</td>
                           </tr>
                            <tr>
                              <td>Average <b>length</b> of a segment</td><td> ",
                all_data_table_strava %>% select(distance) %>% mutate(distance = as.numeric(distance)) %>%
                  filter(!is.na(distance)) %>% filter(distance > 0) %>%
                  summarise(distance = mean(distance)) %>% round(2) %>% as.character(),
                          " m</td>
                           </tr>
                            <tr>
                              <td>Variables measured per segment</td><td> ",dim(all_data_table_strava)[2],"</td>
                           </tr>
                  </table>
  
                           ")),
                      tags$h3("Filtering the data"),
                      HTML("<p>This data can now be filtered by clicking <b>Race settings</b>.
                    The user can also define the size of the city. This might help you if you do not want
                    to include segments that are in the outskirts of a city.
                Second the segments can be compared only by male or female. The evaluation can be performed selectively
                  by <a href='https://en.wikipedia.org/wiki/Median'>medians</a> or average. The segment length can be chosen to lay within
                  a certain area. In some cities people might be faster on short segments, while in others they are faster on long segments.
                  An elevation factor is added. The elavation score is added to the speed per segment as 'speed = elevation_factor * 0.1 * climb (in meters)'.
                  Finally segments can be chosen that contain a certain number of athletes.</p>")
               ),
               column(12,
                      tags$img(id=ns("math"),onclick=paste0("enlarge('",ns('math'),"')"),src="images/math.png", width="100%", class="about image left")
                      
               ),
               column(12,
                      tags$h3("App usage"),
                      
                      HTML("<p>The app now visualizes some basics. On the front page the cities are shown with name and speed. A radius selector can
                    be used to change the size of each city. You can compare e.g. the center of London vs. the whole city of Paris. The
                    segments are all shown inside a map. Each dot is a segment there. Clicking on those leads directly to the segments. Under
                    each map the app shows how many segments are used for the race calculations. Moreover one can visualize the distribution
                    of the length of segments by clicking on the bar. Some more features are shown below:</p>")
               ),
               column(12,
                        tags$img(id=ns("app"),onclick=paste0("enlarge('",ns('app'),"')"),src="images/app.png", width="100%", class="about image left")
                        
               ),
               tags$p("Map feature"),
               column(12,
                      tags$img(id=ns("app_split"),onclick=paste0("enlarge('",ns('app_split'),"')"),src="images/app_split.png", width="100%", class="about image left")
                      
               ),
               tags$p("Segment split feature"),
               column(12,
                      tags$img(id=ns("app_segments_stats"),onclick=paste0("enlarge('",ns('app_segments_stats'),"')"),src="images/app_segments_stats.png", width="100%", class="about image left")
                      
               ),
               tags$p("Segment statistics feature"),
               column(12,
                      tags$img(id=ns("filter"),onclick=paste0("enlarge('",ns('filter'),"')"),src="images/filter.png", width="100%", class="about image left")
                      
               ),
               tags$p("Data filtering feature."),
               column(12,
                      tags$img(id=ns("segment_access"),onclick=paste0("enlarge('",ns('segment_access'),"')"),src="images/segment_access.png", width="100%", class="about image left")
                      
               ),
               tags$p("Accessing a segment on STRAVA.")
               
               
        )
        
        
      )
      
  )
}