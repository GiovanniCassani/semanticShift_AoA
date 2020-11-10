library(dplyr)
library(readxl)
library(MASS)

# define possible window sizes and embedding dimensionality
window_sizes = c(3, 5, 20)
dims = c(40, 100)

# create directories to store processed data
dir.create("data/processed")
dir.create("data/processed/LNC.fixed.minCount10")

# create the paths to access the raw data and to store the processed data
source_paths = NULL
destination_paths = NULL
i = 1
for (d in dims) {
  for (w in window_sizes) {
    source_paths[i] = sprintf("data/raw/LNC.fixed.minCount10/%s_%s_10min.csv", d, w)
    destination_paths[i] = sprintf("data/processed/LNC.fixed.minCount10/semanticShift_d%s_w%s.csv", 
                                   d, w)
    i = i+1
  }
}
rm(i, window_sizes, dims, w, d)

# read all the resources containing covariates
aoa = read_excel("Resources/AoA/AoA_ratings_from_all_sources.xlsx", sheet = 'Kuperman et al.')
concreteness = read.csv("Resources/Brysbaert_concreteness.txt", header = T, sep = '\t')
subtlex = read.csv("Resources/SUBTLEX/SUBTLEX-US.txt", header = T, sep = '\t')
old_len = read.csv("Projects/semanticShift/data/word_OLD_len.csv", header = T, sep = '\t')
coha_freqs = read.csv("Projects/semanticShift/data/word_freq.csv", header = T, sep = ',')
coha_freqs = coha_freqs %>%
  dplyr::rename(
    Word = word,
    FpM.SUM = sum
  )

# loop through all raw datasets and preprocess them, combining target measures and covariates
for (i in 1:length(source_paths)) {
  
  print(source_paths[i])
  
  measures = read.csv(source_paths[i], header = T, sep = ',')
  measures = measures %>% 
    dplyr::rename(
      Word = word,
      VC.SUM = C.SUM, 
      rJ.SUM = RJ.SUM, 
      rVC.SUM = RC.SUM, 
      LNC.SUM = LNC2.SUM,
      rLNC.SUM = RLNC2.SUM
    )  
  
  # merge datasets aligning on words and selecting only the relevant columns
  tmp1 = merge(aoa, concreteness, by = 'Word')
  tmp2 = merge(tmp1, subtlex, by = 'Word')
  tmp3 = merge(dplyr::select(tmp2, Word, Rating.Mean, Conc.M, FREQcount, CDcount, Dom_Pos), 
               old_len, by = 'Word')
  tmp4 = merge(tmp3, dplyr::select(coha_freqs, Word, FpM.SUM), by = "Word")
  df = merge(tmp4, dplyr::select(measures, Word, LNC.SUM, rLNC.SUM, LNC1.SUM, J.SUM, 
                                 rJ.SUM, VC.SUM, rVC.SUM), by = 'Word')
  
  df = df %>% 
    dplyr::rename(
      AoA = Rating.Mean,
      Concr = Conc.M,
      Freq = FREQcount,
      CD = CDcount
    )
  rm(tmp1, tmp2, tmp3, measures)
  
  write.csv(dplyr::select(df, 
                          Word, AoA, Concr, Freq, CD, Dom_Pos, OLD20, len, FpM.SUM, 
                          LNC.SUM, rLNC.SUM, LNC1.SUM, VC.SUM, J.SUM, rVC.SUM, rJ.SUM), 
            destination_paths[i], row.names = FALSE)
}

rm(list = ls())
