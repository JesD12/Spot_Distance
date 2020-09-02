

# file names

main = './data/ninth_Main.txt'
spot_telo = './data/ninth_telo550_spots.txt'
spot_bp1 = './data/ninth_53bp1_spots.txt'


# import the datatabels 

df_main = read.table(main, header = TRUE, sep = "\t")
df_telo = read.table(spot_telo, header = TRUE, sep = "\t")
df_bp1 = read.table(spot_bp1, header = TRUE, sep = "\t")

# calculate the distances

df_main_with_distance = calculate_distance(df_main,'telo550_spots..Counts', 'X53bp1_spots..Counts', df_telo, df_bp1, decfriend = T)
