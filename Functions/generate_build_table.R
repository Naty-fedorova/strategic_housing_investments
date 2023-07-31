ub_data <- read_csv("Data/joined_df.csv")
time_ub_merge <- read_csv("Data/time_ub_joined_df.csv")


# table for "build" (i.e. transition from ger to house)

# for here, let's only look at complete cases - so info that has dwelling type, and year in time_ub_merge
time_ub_merge_c <- time_ub_merge[complete.cases(time_ub_merge),]

min_year <- min(ub_data$ub_since_when[!is.na(ub_data$ub_since_when)])
max_year <- max(ub_data$ub_since_when[!is.na(ub_data$ub_since_when)])

build_table <- matrix(data = 0, ncol = (max_year - min_year) + 2, nrow = nrow(ub_data))

colnames(build_table) <- c("id", min_year:max_year)

build_table[, "id"] <- ub_data$id


for(i in 1:nrow(ub_data)){
  hh_id <- ub_data$id[i]
  
  if(hh_id %in% time_ub_merge_c$id){
    
    id_set <- time_ub_merge_c[which(time_ub_merge_c$id == hh_id),]
    
    for(j in 1:nrow(id_set)){
      if(!is.na(id_set$dwelling_type[j])){
        
        years_listed <- id_set$since_when
        
        # live in bashin
        if(id_set$dwelling_type[j] == "bashin"){
          
          y_b1 <- as.character(id_set$since_when[j])
          
          # for filling in years
          if(j < nrow(id_set)){
            # to next year listed
            to_year <- years_listed[j+1]
          }else{
            # until survey for last year listed
            to_year <- "2020"
          }
          
          build_table[i, as.character(y_b1:to_year)] <- 2
          
          
        }else{
          # for gers, the value is 0
          y_g1 <- as.character(id_set$since_when[j])
          
          # for filling in years
          if(j < nrow(id_set)){
            # to next year listed
            to_year <- years_listed[j+1]
          }else{
            # until survey for last year listed
            to_year <- "2020"
          }
          
          build_table[i, as.character(y_g1:to_year)] <- 1
        }
      }
    }
  } else{
    # for the ones where the hh id doesn't match between d and time_ub
    # don't have residential history in detail so take hasha move in year instead 
    if(!is.na(ub_data$h_state[i]) & !is.na(ub_data$hasha_move_in_year[i]) ){
      if(ub_data$h_state[i] == 2){
        
        y_b <- as.character(ub_data$hasha_move_in_year[i])
        build_table[i, as.character(y_b:"2020")] <- 2
        
      }else{
        # for gers, the value is 0
        y_g <- as.character(ub_data$hasha_move_in_year[i])
        build_table[i, as.character(y_g:"2020")] <- 1
      }
    }
    
  }
}
