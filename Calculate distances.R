# packages to import:
library(Rfast)


## below is a set of function for adding a column to you data that include the average distance between spots 
## inside a nuclues, the idx-spots1-2 is the string name for the spots in the dataframe for the spots, and '
## depends on what you called it in the scanR analysis software.
## The decfriend if set to true will calculate whether the spot1 and spot2 is matched pairwise or not
## it can take 3 values: BFF - paired, drama - not paired, unsymetrical - not the same amount of spot1 as spot2
## it depends on the two function below: distance_matrix and point_distance

calculate_distance = function(main_df, idx_spots1, idx_spots2, 
                              df_spot1, df_spot2, decfriend = FALSE){
  for (idx in c(1:nrow(main_df))){
    # check whether both spots are present for the nucleus and draw the data from the two spots_df
    if (main_df[idx, idx_spots1] > 0 & main_df[idx, idx_spots2] > 0){
      sub_spot1 = df_spot1[df_spot1$Parent.Object.ID..MO. == main_df[idx, 'Object.ID'],]
      sub_spot2 = df_spot2[df_spot2$Parent.Object.ID..MO. == main_df[idx, 'Object.ID'],]
      
      # calculate the distance between all spots
      dist_matrix = distance_matrix(sub_spot1,sub_spot2)
      
      #locate nearest spots for sport 1 and spot 2 respectivly, from the distance matrix
      spots1_dist = rowMins(dist_matrix, value = TRUE)
      spots2_dist = colMins(dist_matrix, value = TRUE)
      
      # add the value to the dataframe 
      main_df[idx,'SpotDistance'] = mean(c(spots1_dist, spots2_dist))
      
      # calculte the whether they are paired or not, if instructed in the begining
      if (decfriend == TRUE){
        # 3 cases BFF means spots are paired, unsymetrical means not equaly amount of two spots
        # drama means spots are not paired
        if (sum(spots1_dist) == sum(spots2_dist)){
          # if the distanse is identical they are all paired
          main_df[idx,'SpotsFriend'] = 'BFF'
        }
        else if (nrow(sub_spot1) == nrow(sub_spot2)){
          # if there is the same number but the distance is different they are not paired
          main_df[idx,'SpotsFriend'] = 'Drama'
        }
        else {
          #the last case is for different numbers of spot1 and spot2
          main_df[idx,'SpotsFriend'] = 'Unsymetrical'
        }
      }
    }
    else {
      # add standard values if there is not spot in either spot1 or spot2
      # I have used NA to make it easier to do further calculation.
      main_df[idx,'SpotDistance'] = NA
      if (decfriend == TRUE){
        main_df[idx,'SpotsFriend'] = NA
      }
    }
  }
  
  return(main_df)
}


# calculate all the distance between two sets of point - depends on the point_distance function below
distance_matrix = function(pointset1, pointset2){
  result_matrix = matrix(0,nrow(pointset1),nrow(pointset2))
  for (rowidx in c(1:nrow(pointset2))){
    dist_vector = apply(pointset1, 1, point_distance, point2 = pointset2[rowidx,])
    result_matrix[,rowidx] = unlist(dist_vector)
  }
  return(result_matrix)
}


# calculate the distance between two points
point_distance = function(point1, point2){
  distance = ((point1['Center.X']-point2['Center.X'])^2+(point1['Center.Y']-point2['Center.Y'])^2)^0.5
  return(distance)
}



# for testing create a limited df with shorter name

# df_main_filtered = data.frame(
#   'Object.ID' = df_main$Object.ID,
#   'Spots_bp1' = df_main$X53bp1_spots..Counts,
#   'Spots_telo' = df_main$telo550_spots..Counts
# )

