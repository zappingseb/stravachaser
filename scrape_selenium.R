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
    ids <- stringr::str_match(as.character(unlist(the_table[["tbody"]]$children[[i]])),"segments\\/(\\d*)")[,2]
    id <- ids[which(!is.na(ids))]
    row_content <- c(row_content,
                     id
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

#Sys.setenv("JAVA_HOME"="C:/Program Files/Java/jdk1.8.0_171")
Sys.setenv("JAVA_HOME"="/Library/Java/JavaVirtualMachines/jdk1.8.0_201.jdk/Contents/Home/")


Sys.setenv(PATH=paste("/Volumes/Macintosh HD/Users/liv/Downloads",
                      Sys.getenv("PATH"),
                      sep=";"))
library(RSelenium)

driver_location <- "/Volumes/Macintosh HD/Users/liv/Downloads/selenium-server-standalone-3.141.59.jar"
sys_log <- paste0(Sys.getenv("JAVA_HOME"),"/bin/java -jar ",
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
remDr <- remoteDriver(extraCapabilities = extraCapabilities,port=4545)
remDr$open()
p <- c(1:13749)

remDr$navigate("https://www.strava.com/segments/search?utf8=%E2%9C%93&keywords=Germany&filter_type=cycling&min-cat=0&max-cat=5&terrain=all")
#start <- remDr$findElements("class","start-icon")[[1]]
#start$executeScript("arguments[0].click();",args=list(start));
input_mail <- remDr$findElements("id","email")[[1]]
input_passw <- remDr$findElements("id","password")[[1]]
input_login <- remDr$findElements("id","login-button")[[1]]
input_login$executeScript("arguments[0].click();",args=list(input_login));

p <- c(4:13749)
for(i in 334:13749){
  remDr$navigate(
    paste0(
      "https://www.strava.com/segments/search?utf8=%E2%9C%93&keywords=Germany&filter_type=cycling&min-cat=0&max-cat=5&terrain=all",
      "&page=",i)
  )
  table <- remDr$findElements("class","search-results")[[1]]
  table_data <- rbind(
    table_data,
    table_from_elem(table)
  )
  
}
length(which(grepl(pattern=", Germany",x=table_data$Location)))


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



