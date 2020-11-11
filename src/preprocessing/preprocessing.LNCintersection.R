library(dplyr)
library(readxl)
library(MASS)

setwd(sub("src.*", "", getwd()))

# Fetch all relevant measures of language change
measures = read.csv("data/raw/LNC.intersection/40_3_intersection.csv", header = T, sep = ',') 
measures = measures %>%
  dplyr::rename(
    Word = word
  )

# make sure to run the code in the script preprocessing.minCount10.R before running this, or the
# necessary file won't exist yet
covar = read.csv("data/processed/LNC.fixed.minCount10/semanticShift_d40_w3.csv", header = T, 
                 sep = ',')
df = merge(dplyr::select(measures, Word, J.SUM, RJ.SUM, C.SUM, RC.SUM, LNC2.SUM, RLNC2.SUM),
           dplyr::select(covar, Word, AoA, Concr, CD, Freq, Dom_Pos, OLD20, len, FpM.SUM))

df = df %>% 
  dplyr::rename(
    FpM = FpM.SUM,
    VC = C.SUM,
    rVC = RC.SUM,
    J = J.SUM,
    rJ = RJ.SUM,
    LNC = LNC2.SUM,
    rLNC = RLNC2.SUM
  ) 
rm(measures, covar)
  
write.csv(dplyr::select(df, 
                        Word, AoA, Concr, Freq, CD, Dom_Pos, OLD20, len, FpM, 
                        LNC, rLNC, VC, J, rVC, rJ), 
          "data/processed/semanticShift_d40_w3.LNCintersection.csv", row.names = FALSE)
rm(list = ls())
