
library(maptools)

# Go from Greenland to Jordan:
lat <- seq(from=28.818819,to=75.791224,by=0.045)
long <- seq(from = -25.028502,to=42.180141,by=0.07)# to=, by= 0.14)
data(wrld_simpl)

# Build a 5 km covering grid
points <- expand.grid(long, lat)  # Note that I reversed OP's ordering of lat/long
names(points) <- c("long","lat")

# Map the grid to the world map
pts <- SpatialPoints(points, proj4string=CRS(proj4string(wrld_simpl)))

## Find which points fall over land
ii <- !is.na(over(pts, wrld_simpl)$FIPS)

# Test Plot
around_munich <- intersect(intersect(intersect(which(points$lat>48),which(points$lat<48.5)), which(points$long<12)),which(points$long>11))

library(rosm)
mucmap <- searchbbox("Munich, Germany")
osm.plot(mucmap)
osm.points(points[around_munich,], 
           pch=15, cex=0.6)


# Filter the grid by being in the sea or on land
my_df <- cbind(points,ii)
my_df <- my_df[which(my_df[,3]),]

# Create URLs from all land points
urls <- paste0(
  "https://www.doogal.co.uk/StravaSegments.ashx?",
  "&swLat=",
  formatC(my_df[,"lat"], format="f", digits=14),
  "&swLng=",
  formatC(my_df[,"long"], format="f", digits=14),
  "&neLat=",
  formatC(my_df[,"lat"]+0.045, format="f", digits=14),
  "&neLng=",
  formatC(my_df[,"long"]+0.7, format="f", digits=14),
  "&type=riding&ridersLt=&min_cat=0"
)

names(urls) <- paste0(my_df$lat+0.0225,",",my_df$long+0.35)

# swLat=47.57518064076296&swLng=10.400523396303555&neLat=48.67513713589727&neLng=12.515391560366055&type=riding&ridersLt=&min_cat=0
# 
library(async)

# Create a function to asynchronously crawl the data
# Catch content of JSON by an own stringr parser
revdep_authors <- function(x) {
  
  # get the URL
  get_author <- function(url) {
    http_get(url)$
      
      # Check if the content can be converted to a CHAR
      then(function(x) tryCatch(
        rawToChar(x$content),
        error=function(e)list(x="")
        ))$
      # Derive the data from the char that seems not to be empty in most cases
      then(function(x){
          list(
            id = stringr::str_match_all(x,"\\\"id\\\"\\:(\\d{1,10})")[[1]][,2],
            avg_grade = stringr::str_match_all(x,"\\\"average_grade\\\"\\:(\\-{0,1}\\d{1,10}\\.{0,1}\\d{1,10})")[[1]][,2],
            name = stringr::str_match_all(x,"\\\"name\\\"\\:\\\"([^\\\"]*)")[[1]][,2],
            distance = stringr::str_match_all(x,"\\\"distance\\\"\\:(\\-{0,1}\\d{1,10}\\.{0,1}\\d{1,10})")[[1]][,2],
            polyline = stringr::str_match_all(x,"\\\"polyline\\\"\\:\\\"([^\\\"]*)")[[1]][,2],
            total_elevation_gain = stringr::str_match_all(x,"\\\"total_elevation_gain\\\"\\:(\\-{0,1}\\d{1,10}\\.{0,1}\\d{1,10})")[[1]][,2],
            kom_time = stringr::str_match_all(x,"\\\"kom_time\\\"\\:(\\-{0,1}\\d{1,10}\\.{0,1}\\d{1,10})")[[1]][,2],
            elevation_high = stringr::str_match_all(x,"\\\"elevation_high\\\"\\:(\\-{0,1}\\d{1,10}\\.{0,1}\\d{1,10})")[[1]][,2],
            athlete_count = stringr::str_match_all(x,"\\\"athlete_count\\\"\\:(\\-{0,1}\\d{1,10}\\.{0,1}\\d{1,10})")[[1]][,2]
          )
        })
      
  }
  
  async_map(x, get_author)
}

url_list <- split(urls,1:4001)

#------------- Crawl via doogal API -------------

#data_list_search <- list()
# Crawl the URLs
start.time <- Sys.time()

# Scraping data from URLs 
# URLs are cut into 1001 pieces
for(i in 1604:4001){
  
  data_downloaded <-  synchronise(revdep_authors(url_list[[i]]))
  
  # names(data_downloaded) <- names(url_list[[i]])
  
  # Filter for empty positions on the map
  data_downloaded_filtered <- Filter(function(x) length(x$id)>0, data_downloaded)
  
  data_list_search <- append(
    data_list_search,
    data_downloaded_filtered
  )
}

#------- work on missed points ----------
missed_points <- list(
  c(51.836319,-1.368502),
  c(51.836319,-1.298502),
  c(51.836319,-1.228502),
  c(51.836319,-1.158502),
  c(51.836319,-1.088502),
  c(51.836319,-1.018502),
  c(51.836319,-0.948501999999996)
)
my_df_missed <- t(as.data.frame.list(missed_points))
colnames(my_df_missed) <- c("lat","long")
urls_missed <- paste0(
  "https://www.doogal.co.uk/StravaSegments.ashx?",
  "&swLat=",
  formatC(my_df_missed[,"lat"]-0.0225, format="f", digits=14),
  "&swLng=",
  formatC(my_df_missed[,"long"]-0.35, format="f", digits=14),
  "&neLat=",
  formatC(my_df_missed[,"lat"]+0.0225, format="f", digits=14),
  "&neLng=",
  formatC(my_df_missed[,"long"]+0.35, format="f", digits=14),
  "&type=riding&ridersLt=&min_cat=0"
)
url_list_missed <- split(urls_missed,1)
data_list_search_missed <- list()
for(i in 1:7){
  result <- httr::GET(urls_missed[i])
  string_download <- rawToChar(result$content)
  
  
  x_split <- strsplit(string_download,"\\}\\,\\{")
  
  data_downloaded <- list()
  for(x in x_split[[1]]){
    data_downloaded <- append(
      data_downloaded,
      list(list(
        id = stringr::str_match_all(x,"\\\"id\\\"\\:(\\d{1,10})")[[1]][,2],
        avg_grade = stringr::str_match_all(x,"\\\"average_grade\\\"\\:(\\-{0,1}\\d{1,10}\\.{0,1}\\d{1,10})")[[1]][,2],
        name = stringr::str_match_all(x,"\\\"name\\\"\\:\\\"([^\\\"]*)")[[1]][,2],
        distance = stringr::str_match_all(x,"\\\"distance\\\"\\:(\\-{0,1}\\d{1,10}\\.{0,1}\\d{1,10})")[[1]][,2],
        polyline = stringr::str_match_all(x,"\\\"polyline\\\"\\:\\\"([^\\\"]*)")[[1]][,2],
        total_elevation_gain = stringr::str_match_all(x,"\\\"total_elevation_gain\\\"\\:(\\-{0,1}\\d{1,10}\\.{0,1}\\d{1,10})")[[1]][,2],
        kom_time = stringr::str_match_all(x,"\\\"kom_time\\\"\\:(\\-{0,1}\\d{1,10}\\.{0,1}\\d{1,10})")[[1]][,2],
        elevation_high = stringr::str_match_all(x,"\\\"elevation_high\\\"\\:(\\-{0,1}\\d{1,10}\\.{0,1}\\d{1,10})")[[1]][,2],
        athlete_count = stringr::str_match_all(x,"\\\"athlete_count\\\"\\:(\\-{0,1}\\d{1,10}\\.{0,1}\\d{1,10})")[[1]][,2]
      ))
    )
  }
  #data_downloaded <-  synchronise(revdep_authors(url_list_missed[[i]][1]))
  
  # names(data_downloaded) <- names(url_list[[i]])
  
  # Filter for empty positions on the map
  data_downloaded_filtered <- Filter(function(x) length(x$id)>0, data_downloaded)
  
  data_list_search_missed <- append(
    data_list_search_missed,
    data_downloaded_filtered
  )
}

end.time <- Sys.time()
time.taken <- end.time - start.time
message(time.taken)

#----------- Crawl Via Strava API -----------------
data_list_strava <- list()

# Scrape by STRAVA API
for( city in c("Berlin")){
  
  points_to <- geocode(city)[1,]
  points_to$lng <- points_to$lon
  
  # Build a grid around the city
  lat <- seq(from=points_to$lat-0.305,to=points_to$lat+0.305,by=0.02)
  long <- seq(from = points_to$lng-0.24,to=points_to$lng+.24,by=0.02)# to=, by= 0.14)
  
  # Build a 5 km covering grid
  points <- expand.grid(long, lat)  # Note that I reversed OP's ordering of lat/long
  names(points) <- c("lng","lat")
  
  for(i in 1:dim(points)[1]){
    
    extract_matches <- function(x)paste(x[,2],collapse=", ")
    
    # Generate bounds from grid
    bounds <- paste(c(
      points$lat[i]-0.01,
      points$lng[i]-0.01,
      points$lat[i]+0.01,
      points$lng[i]+0.01),collapse=", ")
    
    print(glue("{city}({i}/{dim(points)[1]}):{bounds}"))
      # Get Segments
      download_list <- tryCatch(
        get_explore(stoken=stoken, bounds=bounds),
        error=function(e){
          print(paste("error in ",bounds))
          print("..")
          list(segments=list())
        }
      )
      Sys.sleep(1.66)
      
      if(length(download_list$segments)>0){
        data_list_strava <- append(
          data_list_strava,
          download_list
        )
      }# if/download
    
  }  # points of grid
}# cities


save(list="data_list_strava",file="data_list_strava.RData",overwrite=T)

save(list="data_list_search",file="data_list_search2.RData")

#-------- Visualizing the area crawled -------------------
all_names <- names(data_list_search)
coordinates_crawled <- matrix(all_names %>% strsplit(",") %>% unlist(),nrow=length(all_names),byrow=T)

coordinates_crawled <- as.data.frame(coordinates_crawled,stringsAsFactors = F)
names(coordinates_crawled) <- c("long","lat")
coordinates_crawled[,"lat"] <- as.numeric(coordinates_crawled[,"lat"])
coordinates_crawled[,"long"] <- as.numeric(coordinates_crawled[,"long"])

pts <- SpatialPoints(coordinates_crawled[,c(2,1)], proj4string=CRS(proj4string(wrld_simpl)))

## Check that it worked
plot(wrld_simpl,ylim=c(46,49),xlim=c(11,12))
points(pts, col=2, pch=1)

# Name the URLs such that the segments can be mapped back to places where they come from
names(my_data) <- paste0("long:",points$long,",","lat:",points$lat)

#------- Extract the data into a DF -----------------
library(dplyr)
data_list_new <- lapply(data_list,function(x){x$kom_score<-NULL;x})

all_data_table <- bind_rows(lapply(data_list_new,as.data.frame.list))


