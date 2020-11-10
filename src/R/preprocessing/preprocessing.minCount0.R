# pre-process datasets for semantic shift analysis: AoA

library(dplyr)
library(readxl)
library(MASS)

# define possible window sizes and embedding dimensionality
window_sizes = c(3, 5, 20)
dims = c(40, 100)

# create directories to store processed data
dir.create("data/processed")
dir.create("data/processed/LNC.fixed.minCount0")

# create the paths to access the raw data and to store the processed data
source_paths_LNC = NULL
source_paths_J.VC = NULL
destination_paths = NULL
i = 1
for (d in dims) {
  for (w in window_sizes) {
    source_paths_LNC[i] = sprintf("Projects/semanticShift/data/raw/LNC.fixed.minCount0/%s_%s_results_fixed_lnc2.csv", d, w)
    source_paths_J.VC[i] = sprintf("Projects/semanticShift/data/raw/LNC.fixed.minCount0/%s_%s_results.csv", d, w)
    destination_paths[i] = sprintf("Projects/semanticShift/data/processed/LNC.fixed.minCount0/semanticShift_d%s_w%s.csv", d, w)
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

df.LNC = read.csv(source_paths_LNC[1], header = T, sep = ',')
df.J_VC = read.csv(source_paths_J.VC[1], header = T, sep = ',')

coha_freqs = coha_freqs %>%
  dplyr::rename(
    Word = word,
    FpM.SUM = sum
  )


# loop through all raw datasets and preprocess them, combining target measures and covariates
for (i in 1:length(source_paths_LNC)) {
  
  print(source_paths_LNC[i])
  print(source_paths_J.VC[i])
  
  # read in the datasets which contain VC and J, and LNC, then adjust variable names
  LNC = read.csv(source_paths_LNC[i], header = T, sep = ',')
  J_VC = read.csv(source_paths_J.VC[i], header = T, sep = ',')

  LNC = LNC %>% 
    dplyr::rename(
      Word = word,
      LNC.SUM = LNC2F.SUM,
      rLNC.SUM = RLNC2F.SUM,
      LNC1.SUM = LNC1F.SUM
      ) 
  
  J_VC = J_VC %>% 
    dplyr::rename(
      Word = word,
      VC.SUM = C.SUM, 
      J.SUM = JAC.SUM, 
      rVC.SUM = RandC.SUM,
      rJ.SUM = RandJAC.SUM
      ) 
  
  # merge datasets aligning at the word and select only the relevant variables
  tmp1 = merge(aoa, concreteness, by = 'Word')
  tmp2 = merge(tmp1, subtlex, by = 'Word')
  tmp3 = merge(dplyr::select(tmp2, Word, Rating.Mean, Conc.M, FREQcount, CDcount, Dom_Pos), old_len, by = 'Word')
  tmp4 = merge(tmp3, dplyr::select(coha_freqs, Word, FpM.SUM), by = "Word")
  tmp5 = merge(tmp4, dplyr::select(LNC, Word, LNC.SUM, rLNC.SUM, LNC1.SUM), by = "Word")
  df = merge(tmp5, dplyr::select(J_VC, Word, J.SUM, rJ.SUM, VC.SUM, rVC.SUM), by = 'Word')
  
  df = df %>% 
    dplyr::rename(
      AoA = Rating.Mean,
      Concr = Conc.M,
      Freq = FREQcount,
      CD = CDcount
    )

  write.csv(dplyr::select(df, 
                          Word, AoA, Concr, Freq, CD, Dom_Pos, OLD20, len, FpM.SUM, LNC.SUM, rLNC.SUM, 
                          LNC1.SUM, VC.SUM, J.SUM, rVC.SUM, rJ.SUM), destination_paths[i], row.names = FALSE)
}

rm(list = ls())
