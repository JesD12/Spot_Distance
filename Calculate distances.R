# packages to import:
library(Rfast)

# file names
main = './data/ninth_Main.txt'
spot_telo = './data/ninth_telo550_spots.txt'
spot_bp1 = './data/ninth_53bp1_spots.txt'


# import the datatabels 

df_main = read.table(main, header = TRUE, sep = "\t")
df_telo = read.table(spot_telo, header = TRUE, sep = "\t")
df_bp1 = read.table(spot_bp1, header = TRUE, sep = "\t")


calculate_distance = function(main_df, idx_spots1, idx_spots2, 
                              df_spot1, df_spot2, decfriend = FALSE){
  for (idx in c(1:nrow(main_df))){
    if (main_df[idx, idx_spots1] > 0 & main_df[idx, idx_spots2] > 0){
      sub_spot1 = df_spot1[df_spot1$Parent.Object.ID..MO. == main_df[idx, 'Object.ID'],]
      sub_spot2 = df_spot2[df_spot2$Parent.Object.ID..MO. == main_df[idx, 'Object.ID'],]
      dist_matrix = distance_matrix(sub_spot1,sub_spot2)
      spots1_dist = rowMins(dist_matrix, value = TRUE)
      spots2_dist = colMins(dist_matrix, value = TRUE)
      main_df[idx,'SpotDistance'] = mean(c(spots1_dist, spots2_dist))
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
      main_df[idx,'SpotDistance'] = NA
      if (decfriend == TRUE){
        main_df[idx,'SpotsFriend'] = NA
      }
    }
  }
  
  return(main_df)
}

point_distance = function(point1, point2){
  distance = ((point1['Center.X']-point2['Center.X'])^2+(point1['Center.Y']-point2['Center.Y'])^2)^0.5
  return(distance)
}

distance_matrix = function(pointset1, pointset2){
  result_matrix = matrix(0,nrow(pointset1),nrow(pointset2))
  for (rowidx in c(1:nrow(pointset2))){
    dist_vector = apply(pointset1, 1, point_distance, point2 = pointset2[rowidx,])
    result_matrix[,rowidx] = unlist(dist_vector)
  }
  return(result_matrix)
}

# for testing create a limited df with shorter name

df_main_filtered = data.frame(
  'Object.ID' = df_main$Object.ID,
  'Spots_bp1' = df_main$X53bp1_spots..Counts,
  'Spots_telo' = df_main$telo550_spots..Counts
)

