#' About description module
#' 
#' @param id \code{character} ID of this module
#' @export
#' @author Sebastian Wolf \email{sebastian@@mail-wolf.de}
about <- function(id){
  ns <- NS(id)
  div(class="about",
      fluidRow(
        column(12,
               tags$h2("How can cities race against each other?"),
               tags$img(id="race1",onclick="enlarge('race1')",src="images/race1.jpg", width="100%", class="about image left"),
               tags$h3("Background"),
               HTML("<p>This app can be basically used to compare the cyclists in two cities using data from <a href='strava.com'>strava</a>.
                    How is this possible? Strava is a sports tracking app. Basically whenever people cycle they can track themselves using
                    GPS. Afterwards they will know how fast they went on their track. This is not comparable. Though, <a href='strava.com'>strava</a>
                    build in a feature called <b><a href='https://support.strava.com/hc/en-us/articles/216918167-Strava-Segments'>segments</a></b>. These
                    segments are tracks of any length all around the world. On each of these tracks <a href='strava.com'>strava</a> let's the users
                    run on a leaderboard. The fastest cyclist is the King of the Mountain on this segment.
                  </p>
                  "),
               tags$h3("Calculations performed"),
               tags$img(id="leaderboard",onclick="enlarge('leaderboard')",src="images/leaderboard.png", width="100%", class="about image right"),
               HTML("<p>These leaderboards are not just kept for each segment, but also seperately per year and gender. To find out which of the four
                cities inside this app is the fastest, I went through some steps. First I went over the 
                <a href='https://developers.strava.com/docs/reference/#api-Segments-exploreSegments'>explore segments</a> API to find segments within
                  the area of the city. I set a maximum radius of ~30 km. Afterwards I craweled the leaderboards for all those segments. By crawling
                  the leaderboards I got the <b>top 30</b> male and top 30 female athletes. A segment can have a maximum number of 60 participants
                  for the statistics I am using. After receiving the data I calculated the <a href='https://en.wikipedia.org/wiki/Median'>median</a> and average speed for males, females and both. 
                  </p>"),
               tags$img(id="math",onclick="enlarge('math')",src="images/math.png", width="100%", class="about image left"),
               tags$h3("Filtering the data"),
               HTML("<p>This data can now be filtered. The user can define the size of the city. This might help you if you do not want
                    to include segments that are in the outskirts of a city.
                Second the segments can be compared only by male or female. The evaluation can be performed selectively
                  by <a href='https://en.wikipedia.org/wiki/Median'>medians</a> or average. The segment length can be chosen to lay within
                  a certain area. In some cities people might be faster on short segments, while in others they are faster on long segments.
                  An elevation factor is added. The elavation score is added to the speed per segment as 'speed = elevation_factor * 0.1 * climb (in meters)'.
                  Finally segments can be chosen that contain a certain number of athletes.</p>"),
               tags$h3("App usage"),
               HTML("<p>The app now visualizes some basics. On the front page the cities are shown with name and speed. A radius selector can
                    be used to change the size of each city. You can compare e.g. the center of London vs. the whole city of Paris. The
                    segments are all shown inside a map. Each dot is a segment there. Clicking on those leads directly to the segments. Under
                    each map the app shows how many segments are used for the race calculations. Moreover one can visualize the distribution
                    of the length of segments by clicking on the bar.</p>")
               
               
        )
        
      ))
}