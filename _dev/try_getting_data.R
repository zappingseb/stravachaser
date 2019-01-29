#------- rStrava usage

stoken <- httr::config(token = strava_oauth("ChaseSegments", 31906, "17c291d0f8a090239bee96473aba845bfb53b434"))
# stoken: bf7d7ddf6f9727ad0fd84865f7e1ff65b9782749

for(i in 1:length(my_acts)){
  if(i==1){
    strms_data <- get_activity_streams(my_acts, stoken, acts = 1)
  }  else{
    strms_data <- tryCatch({
      
      print(paste0("Getting act:",i))
      rbind(strms_data,
            get_activity_streams(my_acts, stoken, acts = i)   
                        )
    },error=function(e){
      message(paste0("Problem getting data of activity", my_acts[[i]]$id))
    })
  }
}

#Artur Kutscher Platz   - St Emeran
#long         # lat      # long    #lat
bounds <- "48.163504,11.590543,48.173242,11.623381"

segments_at_home <- get_explore(stoken=stoken, bounds = bounds)
  
lapply(segments_at_home[[1]],function(x){print(x$id)})


#------------- Work with craweld data -----------------

# - Check which data could not be crawled
for(i in 1:length(data_list_new)){
  
  if(length(data_list_new[[i]]$id)==1){
    
    if(!length(data_list_new[[i]]$id)==length(data_list_new[[i]]$avg_grade) ||
       !length(data_list_new[[i]]$id)==length(data_list_new[[i]]$name)          ||
       !length(data_list_new[[i]]$id)==length(data_list_new[[i]]$polyline)          ||
       !length(data_list_new[[i]]$id)==length(data_list_new[[i]]$total_elevation_gain)          ||
       !length(data_list_new[[i]]$id)==length(data_list_new[[i]]$distance) 
    ){
      print("....")
      print("i:")
      print(i)
      print(".")
      print(names(data_list_new)[i])
      data_list_new[[i]]$polyline <- " "
      # print(length(data_list_new[[i]]$id))
      # print(length(data_list_new[[i]]$avg_grade))
      # print(length(data_list_new[[i]]$name))
      # # print(length(data_list_new[[i]]$kom_time))
      # print(length(data_list_new[[i]]$distance))
      # print(length(data_list_new[[i]]$polyline))
      # print(length(data_list_new[[i]]$total_elevation_gain))
    }
    # print(print(length(data_list_search[[i]]$elevation_high)))
    # print(print(length(data_list_search[[i]]$athlete_count)))
  }
}

data_list_store <- data_list_new

data_list_search[c(31419,31436,31453,31470,31485,31499,31514)] <- NULL
data_list_search <- append(data_list_search,data_list_search_missed)

data_list_new <- lapply(data_list_search,function(x){x$kom_time<-NULL;x$elevation_high<-NULL;x$athlete_count<-NULL;x})

all_data_table_second_crawl <- bind_rows(lapply(data_list_new,as.data.frame.list))
all_data_table_second_crawl <- all_data_table_second_crawl[!duplicated(all_data_table_second_crawl$id),]

data_list_new <- lapply(data_list,function(x){x$kom_score<-NULL;x})

all_data_table <- bind_rows(lapply(data_list_new,as.data.frame.list))
all_data_table <- all_data_table[!duplicated(all_data_table$id),]

all_data_table <- all_data_table[,-7]

all_data_table <- rbind(all_data_table, all_data_table_second_crawl)

all_data_table <- all_data_table[!duplicated(all_data_table$id),]

#------- Strava to table -------------------
start_end_geocoord <- function(segment){
  
  segment[["start_lat"]] <- segment[["start_latlng"]][[1]]
  segment[["start_lng"]] <- segment[["start_latlng"]][[2]]
  segment[["end_lat"]] <- segment[["end_latlng"]][[1]]
  segment[["end_lng"]] <- segment[["end_latlng"]][[2]]
  segment[["start_latlng"]] <- NULL
  segment[["end_latlng"]] <- NULL
  segment
}

strava_data_all <- bind_rows(lapply(data_list_strava,function(x)bind_rows(lapply(x,function(y)
  
  y %>% start_end_geocoord %>% as.data.frame.list))))

names(strava_data_all)[which(names(strava_data_all)=="elev_difference")] <- "total_elevation_gain"
names(strava_data_all)[which(names(strava_data_all)=="points")] <- "polyline"
names(strava_data_all)[which(names(strava_data_all)=="start_lng")] <- "lng"
names(strava_data_all)[which(names(strava_data_all)=="start_lat")] <- "lat"
strava_data_all$id <- as.character(strava_data_all$id)
strava_data_all$avg_grade <- as.character(strava_data_all$avg_grade)
strava_data_all$distance <- as.character(strava_data_all$distance)
strava_data_all$total_elevation_gain <- as.character(strava_data_all$total_elevation_gain)
strava_data_all$average <- NA
strava_data_all$median <- NA
strava_data_all$average_M <- NA
strava_data_all$median_M <- NA
strava_data_all$average_F <- NA
strava_data_all$median_F <- NA
strava_data_all$chaser_F <- NA
strava_data_all$chaser_M <- NA
strava_data_all$chaser <- NA

all_data_table_strava <- bind_rows(strava_data_all,all_data_table)

print(length(which(!is.na(all_data_table_strava$average))))
temp <- bind_rows(all_data_table_strava,strava_data_all)
temp <- temp[!duplicated(temp$id),]
print(length(which(!is.na(temp$average))))

all_data_table_strava <- all_data_table_strava[!duplicated(all_data_table_strava$id),]

#------------- Leaderboard generation -------------------

get_speed <- function(stoken=get("stoken",.GlobalEnv), id=NULL,distance="1000"){
  
  stopifnot(!is.null(id))
  
  select_entries <- function(x){
    x$entries
  }
  print(id)

  lb_F <- tryCatch({
    
    get_pages(url_=glue("https://www.strava.com/api/v3/segments/{id}/leaderboard?gender=F&per_page=300&page=1"),stoken=stoken) %>% select_entries() %>%
    lapply(FUN=as.data.frame.list) %>% bind_rows() %>%
    mutate("speed"=if(exists("elapsed_time")){as.numeric(!!distance)/elapsed_time*3.6}else{NA})
  },error=function(e){
    if(grepl("Too\\sMany",e)){
      stop(e)
    }else{
      
      data.frame(speed=c(0,0))
    }
  }
  
  )
  
  lb_M <- tryCatch({
    get_pages(url_=glue("https://www.strava.com/api/v3/segments/{id}/leaderboard?gender=M&per_page=300&page=1"),stoken=stoken) %>% select_entries() %>% 
    lapply(FUN=as.data.frame.list) %>% bind_rows() %>%
    mutate("speed"=if(exists("elapsed_time")){as.numeric(!!distance)/elapsed_time*3.6}else{NA})},error=function(e){
      data.frame(speed=c(0,0))
    }
  
  )
  
  lb_A <- bind_rows(lb_F,lb_M)
  
  segment_info <- c()
  segment_info["average"] <- mean(lb_A$speed)
  segment_info["median"] <- median(lb_A$speed)
  segment_info["average_M"] <- mean(lb_M$speed)
  segment_info["median_M"] <- median(lb_M$speed)
  segment_info["average_F"] <- mean(lb_F$speed)
  segment_info["median_F"] <- median(lb_F$speed)
  segment_info["chaser_F"] <- length(lb_F$speed)
  segment_info["chaser_M"] <- length(lb_M$speed)
  segment_info["chaser"] <- length(lb_A$speed)
  return(segment_info)
}

#----------- Starting points  -----------------

# Generate the starting point coordinates from a polyline
library(gepaf)

polyLine_dat = function(polyline_string){
  decoder <- decodePolyline(polyline_string)
  paste(decoder[1,"lat"], decoder[1,"lon"])
}

# Get old polyline data
for(row_num in 1:dim(all_data_table_strava)[1]){
  
  if(is.na(all_data_table_strava[row_num,"lng"]) || is.na(all_data_table_strava[row_num,"lat"])){
    
    index_old <- which(all_data_table_strava_236779$id
                       == as.character(all_data_table_strava[row_num,"id"])
                       )
    if(length(index_old)>0){
      
      all_data_table_strava[row_num,"lng"] <- as.numeric(all_data_table_strava_236779[index_old,"lng"])
      all_data_table_strava[row_num,"lat"] <- as.numeric(all_data_table_strava_236779[index_old,"lat"])
    }
  }
}

# Add geo coordinates of the start of the segment to each segment
all_data_table_strava[which(is.na(all_data_table_strava$lng)),] <- 
  all_data_table_strava[which(is.na(all_data_table_strava$lng)),] %>% rowwise() %>%
  mutate(coor_polyline = polyline %>% polyLine_dat()) %>%
  mutate(coor_lat = strsplit(coor_polyline, split=" ")[[1]][1]) %>%
  mutate(coor_lng = strsplit(coor_polyline, split=" ")[[1]][2]) %>% 
  mutate(lng = as.numeric(coor_lng),lat=as.numeric(coor_lat))


#-------- Find points in Radius --------

calc_distance <- function(
  points_from = data.frame(lng=c(),lat=c()),
  points_to = data.frame(lng=c(),lat=c())
){
  library(sp)
  library(rgdal)
  library(RANN)
  library(tibble)
  
  if(!"lng" %in% names(points_to)){
    stop("longitude needed in 'points_to' as lng")
  }
  if(!"lat" %in% names(points_to)){
    stop("latitude needed in 'points_to' as lat")
  }
  
  ## First define a function from Josh OBrien's answer to convert
  ## a longitude to its UTM zone
  long2UTM <- function(long) {
    (floor((long + 180)/6) %% 60) + 1
  }
  
  ## Assuming that all points are within a zone (within 6 degrees in longitude),
  ## we use the first shop's longitude to get the zone.
  z <- long2UTM(points_from[ceiling(dim(points_from)[1]/2),"lng"])
  
  ## convert the bus lat/long coordinates to UTM for the computed zone
  ## using the other Josh O'Brien linked answer
  bus2 <- points_from[,c("lng","lat")]
  names(bus2) <- c("lng","lat")
  
  coordinates(bus2) <- c("lng", "lat")
  proj4string(bus2) <- CRS("+proj=longlat +datum=WGS84")
  bus.xy <- spTransform(bus2, CRS(paste0("+proj=utm +zone=",z," ellps=WGS84")))
  
  # Create a Berlin Pointsmap
  points_to <- points_to[,c("lng","lat")]
  coordinates(points_to) <- c("lng", "lat")
  proj4string(points_to) <- CRS("+proj=longlat +datum=WGS84")
  berlin.xy <- spTransform(points_to, CRS(paste0("+proj=utm +zone=",z," ellps=WGS84")))
  
  ## Compile the distance between Berlin and all Segments
  res <- nn2(berlin.xy@coords, bus.xy@coords,1)
  return(res)
}

#-------- Crawling leaderboards ----------------------

# all_data_table_strava$average <- NA
# all_data_table_strava$median <- NA
# all_data_table_strava$average_M <- NA
# all_data_table_strava$median_M <- NA
# all_data_table_strava$average_F <- NA
# all_data_table_strava$median_F <- NA
# all_data_table_strava$chaser_F <- NA
# all_data_table_strava$chaser_M <- NA
# all_data_table_strava$chaser <- NA

all_data_table_strava$lng <- as.numeric(all_data_table_strava$lng)
all_data_table_strava$lat <- as.numeric(all_data_table_strava$lat)
#Sys.sleep(5100)
for(city in c("Munich","Barcelona","Warsaw","Manchester","Leeds")){
  
  
  points_to <- geocode(city)[1,]
  points_to$lng <- points_to$lon
  
  distance <- calc_distance(
    points_from = all_data_table_strava,
    points_to = points_to
  )
  
  
  segment_indeces <- which(distance$nn.dists <= 30000)
  for(segment_index in 1:length(segment_indeces)){
    segment <- segment_indeces[segment_index]
    
    # Check if speed was already calculated
    if(is.na(all_data_table_strava[segment,"average"]) || all_data_table_strava[segment,"average"]==0){
      
      print(segment %in% no_data_list[[city]])
      
      all_data_table_strava[segment,] <- all_data_table_strava[segment,] %>%
        rowwise() %>%
        mutate(mykey=paste(get_speed(stoken=!!stoken,id=id,distance=distance),collapse = ",")) %>% 
        separate(
          col=mykey,
          sep=",",
          into=c("average","median","average_M","median_M","average_F","median_F","chaser_F","chaser_M","chaser"))
      
      print(all_data_table_strava[segment,"chaser"])
      Sys.sleep(4.5)
    }
  }
}
#city <- "Paris"

#---------- Visualizing segments ----------

# Plot all segments wihtin Munich
library(rosm)
library(prettymapr)
citymap <- searchbbox(city)
osm.plot(citymap)
osm.points(all_data_table_strava[which(distance$nn.dists <= 30000),c("lng","lat")], 
           pch=15, cex=0.6)

print(paste0("In ",city," I found: ",length(which(distance$nn.dists <= 30000))," segments."))


#-------- Validate crawled segments -------

no_data_list <- list()
for(city in c("Paris","London","Berlin")){
  
  
  points_to <- geocode(city)[1,]
  points_to$lng <- points_to$lon
  
  distance <- calc_distance(
    points_from = all_data_table_strava,
    points_to = points_to
  )
  
  
  segment_indeces <- which(distance$nn.dists <= 30000)
  
  no_data <- c( which(is.na(all_data_table_strava$average)),
             which(all_data_table_strava$average==0))
  no_data <- no_data[which(!duplicated(no_data))]
  print(
    paste0(
      length(intersect(segment_indeces,no_data)),
      "/",
      length(segment_indeces)
    )
    
  )
  no_data_list[[city]] <- intersect(segment_indeces,no_data)
}
  