table_from_elem <- function(table){
  the_table <- table$getElementAttribute("innerHTML")
  
  the_table <- htmlTreeParse(the_table[[1]],asText=T,encoding = "UTF-8")
  the_table <- the_table$children$html$children$body 			
  
  # Get the table headers
  headers <- the_table$children[["thead"]]
  column_names <- lapply(headers$children[[1]]$children, function(x) xmlValue(x))
  

  # Get the table content
  content <- c()
  # For each row
  for(i in 1:length(the_table[["tbody"]]$children))
  {
    table_row <- the_table[["tbody"]]$children[[i]]
    row_content<-c()
    # for each column
    for(j in 1:length(table_row$children)){
      
      v <- xmlValue(table_row[[j]])
      
      if(is.null(v)) v2 <- as.character("")
      else if(length(v) == 0) v2 <- as.character("") 
      else if(is.na(v)) v2 <- as.character("")
      else v2 <- as.character(v)
      
      row_content <- c(row_content, v2)
      
    }
    # Append ID to row
    row_content <- c(row_content,
                     stringr::str_match(as.character(table_row[[1]])[3],"id=(\\d*)")[,2]
                     )
    
    content <- rbind(content, row_content)
  }
  if(content[1,1]!=""){
    message("TABLE found")
    # Write out the table as a data.frame and delete row.names
    colnames(content) <- c(as.character(column_names),"id")
    rownames(content) <- NULL
  
    return(as.data.frame(content,stringsAsFactors=F,check.names	= F))
  }else{
    return(NULL)
  }
}

# scraping_data

Sys.setenv("JAVA_HOME"="C:/Program Files/Java/jdk1.8.0_171")
Sys.setenv(PATH=paste("C:\\Programme_2\\ROCHE-R\\Selenium\\",
                      Sys.getenv("PATH"),
                      sep=";"))
library(RSelenium)

driver_location <- "C:\\Programme_2\\ROCHE-R\\Selenium\\selenium-server-standalone-3.4.0.jar"
sys_log <- paste("\"",
                 Sys.getenv("JAVA_HOME"),
                 "\\bin\\java.exe\" -jar ",
                 "\"",driver_location,"\"",
                 sep="")

message(paste("starting selenium server from",sys_log))

system(sys_log, wait=FALSE,intern = TRUE)
Sys.sleep(5)
print("started")


extraCapabilities=list(chromeOptions=list(
  args = c('--disable-geolocation',
           '--enable-strict-powerful-feature-restrictions',
           '--disable-gpu',
           '--test-type',
           '--start-maximized')
))
remDr$close()
remDr <- remoteDriver(browserName = 'chrome',extraCapabilities = extraCapabilities)
remDr$open()
remDr$navigate("https://www.doogal.co.uk/strava.php")
Sys.sleep(22)

lapply(buttons, function(x){
  title_elem <- x$getElementAttribute("title")
  if(title_elem[[1]] == "Zoom in"){
    print("IN")
    for(i in 1:12){
      x$clickElement()
    }
  }
})


#input_element <- remDr$findElements("css selector","canvas")[[1]]
Sys.sleep(5)

input_element <- remDr$findElements("id","map")[[1]]
remDr$mouseMoveToLocation(x=855,y=505,input_element)
remDr$mouseMoveToLocation(x=55,y=55,input_element)

Sys.sleep(1)
remDr$buttonup()
Sys.sleep(1)
remDr$buttondown()
Sys.sleep(1)


my_result <- data.frame(
  "Segment" = c(),
  "Distance" = c(),
  "Average grade" = c(),
  "Elevation difference" = c(),
  "# Athletes" = c(),
  "Category" = c(),
  "KOM time" = c(),
  "Distance to" = C(),
  "id" = c(),
  stringsAsFactors = F
  
)
loc_y = loc_y_nav = 505
loc = 805
count_attach <- 0

for(i in 1:100){
  
  loc = loc + i*850
  remDr$mouseMoveToLocation(x=loc,y=loc_y_nav,input_element)

  for(g in 0:100){
    loc_y_nav = loc_y + g * 550
    tryCatch(
      {
        remDr$mouseMoveToLocation(x=loc,y=loc_y_nav,input_element)
      },error=function(e){
        Sys.sleep(2)
        remDr$mouseMoveToLocation(x=loc,y=loc_y_nav,input_element)
      }
      
    )
    
    
    Sys.sleep(10)
    table <- remDr$findElements("css selector","#segments-table table")[[1]]
    
    attach_table <- table_from_elem(table)
    
    
    
    if(!is.null(attach_table)){
      colnames(attach_table) <- colnames(my_result)
      my_result <- rbind(
        my_result,
       attach_table
      )
    }else{
      count_attach <- count_attach + 1
    }
  
    if(count_attach > 20){
      break
    }
  }
  loc_y = loc_y_nav
}


print(dim(my_result[!duplicated(my_result[,c('id')]),]))



Sys.sleep(1)
remDr$buttonup()

lat = 51.0
lon = 0.0

R=6378137

dn = 10000
de = 10000

dLat = dn/R
dLon = de/(R*cos(Pi*lat/180))

latO = lat + dLat * 180/Pi
lonO = lon + dLon * 180/Pi 

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
  "&nwLat=",
  formatC(my_df[,"lat"]+0.045, format="f", digits=14),
  "&nwLng=",
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

url_list <- split(urls,1001)

data_list_search <- list()
# Crawl the URLs
start.time <- Sys.time()

# Scraping data from URLs 
# URLs are cut into 1001 pieces
for(i in 1:1001){
  
  data_downloaded <-  synchronise(revdep_authors(url_list[[i]]))
  
  names(data_downloaded) <- names(url_list[[i]])
  
  # Filter for empty positions on the map
  data_downloaded_filtered <- Filter(function(x) length(x$id)>0, data_downloaded)
  
  data_list_search <- append(
    data_list_search,
    data_downloaded_filtered
  )
}

end.time <- Sys.time()
time.taken <- end.time - start.time
message(time.taken)

# Name the URLs such that the segments can be mapped back to places where they come from
names(my_data) <- paste0("long:",points$long,",","lat:",points$lat)


library(dplyr)
data_list_new <- lapply(data_list,function(x){x$kom_score<-NULL;x})

all_data_table <- bind_rows(lapply(data_list_new,as.data.frame.list))

