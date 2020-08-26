# packages to import:


# file names
main = './data/ninth_Main.txt'
spot_telo = './data/ninth_telo550_spots.txt'
spot_bp1 = './data/ninth_53bp1_spots.txt'


# import the datatabels 

df_main = read.table(main, header = TRUE, sep = "\t")
df_telo = read.table(spot_telo, header = TRUE, sep = "\t")
df_bp1 = read.table(spot_bp1, header = TRUE, sep = "\t")


calculate_distance = function(main_df, idx_spots1, idx_spots2, 
                              df_spot1, df_spot2,decfriend = FALSE){
  if (main_df[idx_spots1] > 0 & main_df[idx_spots2] > 0){
    sub_spot1 = df_spot1[df_spot1$Parent.Object.ID..MO. == main_df$Object.ID,]
    sub_spot2 = df_spot2[df_spot2$Parent.Object.ID..MO. == main_df$Object.ID,]
  }
  return('NO')
}


# for testing create a limited df with shorter name

df_main_filtered = data.frame(
  'Object.ID' = df_main$Object.ID,
  'Spots_bp1' = df_main$X53bp1_spots..Counts,
  'Spots_telo' = df_main$telo550_spots..Counts
)
