

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
