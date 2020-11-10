library(dplyr)
library(cocor)
library(reshape2)
library(plyr)
library(ggplot2)
library(Rmisc)
library(RColorBrewer)

# get the upper half of a correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

# box-cox transform a variable by first finding the lambda which maximises negative log-likelihood
box_cox_transf <- function(x, min, max) {
  bc = boxcox(x ~ 1, lambda = seq(min, max, 0.1))
  df = data.frame(bc$x, bc$y)
  df2 = df[with(df, order(-df$bc.y)),]
  lambda = df2[1, "bc.x"]
  print(lambda)
  if (lambda != 0) {
    x.transf = (x ^ lambda - 1)/lambda
  } else {
    x.transf = log(x)
  }
  return(x.transf)
}

pd = position_dodge(0.1)

# define frequency thresholds to consider
thresholds = c(25, 50, 75, 100)

# read word frequencies
freqs = read.csv("data/target_freq.csv", header = T, sep = ',')
freqs = freqs %>% 
  dplyr::rename(
    Word = word
  )

# define target variables
targets = c('J', 'VC', 'LNC', 'rJ', 'rVC', 'rLNC')

measures = read.csv("data/processed/LNC.fixed.minCount10/semanticShift_d40_w3.csv", 
                    header = T, sep = ',') 
measures = measures %>% 
  dplyr::rename(
    FpM = FpM.SUM,
    VC = VC.SUM,
    rVC = rVC.SUM,
    J = J.SUM,
    rJ = rJ.SUM,
    LNC = LNC.SUM,
    rLNC = rLNC.SUM
  )

stats = NULL
corrs = NULL

# loop through all frequency thresholds and fit linear models
for (t in thresholds) {
  
  df = merge(freqs[apply(freqs[, c(-1, -2)], MARGIN = 1, function(x) all(x >= t)), ],
             measures, by = 'Word')
  
  df$Dom_Pos = relevel(factor(df$Dom_Pos), 'Noun')
  
  df$F.z = scale(box_cox_transf(df$Freq, -10, 10), center = T, scale = T)
  df$C.z = scale(box_cox_transf(df$Concr, -10, 10), center = T, scale = T)
  df$CD.z = scale(box_cox_transf(df$CD, -10, 10), center = T, scale = T)
  df$OLD.z = scale(box_cox_transf(df$OLD20, -10, 10), center = T, scale = T)
  df$L.z = scale(box_cox_transf(df$len, -10, 10), center = T, scale = T)
  df$FpM.z = scale(box_cox_transf(df$FpM, -10, 10), center = T, scale = T)
  
  df$J.z = scale(box_cox_transf(df$J + 0.0001, -10, 10), center = T, scale = T)
  df$VC.z = scale(box_cox_transf(df$VC, -10, 10), center = T, scale = T)
  df$LNC.z = scale(box_cox_transf(df$LNC, -10, 10), center = T, scale = T)
  
  df$rJ.z = scale(box_cox_transf(df$rJ + 0.0001, -10, 10), center = T, scale = T)
  df$rVC.z = scale(box_cox_transf(df$rVC, -10, 10), center = T, scale = T)
  df$rLNC.z = scale(box_cox_transf(df$rLNC, -10, 10), center = T, scale = T)
  
  cormat <- round(cor(df[c("AoA", "C.z", "CD.z", "F.z", "L.z", "FpM.z", "OLD.z", "LNC.z", 
                           "VC.z", "J.z", "rLNC.z", "rVC.z", "rJ.z")]),3)
  upper_tri <- get_upper_tri(cormat)
  melted_cormat <- melt(upper_tri, na.rm = TRUE)
  levels(melted_cormat$Var1)
  melted_cormat$Var1 = plyr::mapvalues(melted_cormat$Var1, 
                                       from = c("AoA", "C.z", "CD.z", "F.z", "L.z", "FpM.z", 
                                                "OLD.z", "LNC.z", "VC.z", "J.z", "rLNC.z", 
                                                "rVC.z", "rJ.z"), 
                                       to = c("AoA", "Concr", "CD", "Freq", "Len", "FpM", 
                                              "OLD20", "LNC", "VC", "J", "rLNC", "rVC", "rJ"))
  melted_cormat$Var2 = plyr::mapvalues(melted_cormat$Var2, 
                                       from = c("AoA", "C.z", "CD.z", "F.z", "L.z", "FpM.z", 
                                                "OLD.z", "LNC.z", "VC.z", "J.z", "rLNC.z", 
                                                "rVC.z", "rJ.z"), 
                                       to = c("AoA", "Concr", "CD", "Freq", "Len", "FpM", 
                                              "OLD20", "LNC", "VC", "J", "rLNC", "rVC", "rJ"))
  
  ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
    geom_tile(color = "white")+
    scale_fill_gradient2(high = "darkred", low = "steelblue", mid = "lightgray", 
                         midpoint = 0, limit = c(-1,1), space = "Lab", 
                         name="Pearson\nCorrelation") +
    theme_minimal()+ 
    theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 11, hjust = 1),
          axis.text.y = element_text(vjust = 1, size = 11, hjust = 1)) +
    coord_fixed() + 
    geom_text(aes(Var2, Var1, label = value), color = "white", size = 4) +
    ggtitle(sprintf("Pairwise correlations (d=40, w=3, f >= %s", t)) +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.grid.major = element_blank(),
      panel.border = element_blank(),
      panel.background = element_blank(),
      axis.ticks = element_blank(),
      legend.justification = c(1, 0),
      legend.position = c(0.5, 0.75),
      legend.direction = "horizontal")+
    guides(fill = guide_colorbar(barwidth = 10, barheight = 2,
                                 title.position = "top", title.hjust = 0.5))
  ggsave(sprintf("corrMatrix_f%s.pdf", t), plot = last_plot(), device = "pdf", path = 'plots', 
         width = 18, height = 18, units = "cm") 
  
  
  # store the r_squared of simple correlations between language change measures and AoA
  for (target in targets) {
    r_squared = cor(df$AoA, df[[target]])**2
    corrs = rbind(corrs,
                  data.frame('AoA', target, t, r_squared))
  }
  
  lm.baseline = lm(AoA ~ F.z + C.z + CD.z + OLD.z + L.z + Dom_Pos + FpM.z, data = df)
  AIC.base = AIC(lm.baseline)
  var = 'base'
  AIC = AIC.base
  rsq =  summary(lm.baseline)$r.sq
  b = NA
  deltaAIC = 0
  stats = rbind(stats,
                data.frame(t, var, AIC, rsq, b, deltaAIC))
  
  lm.VC = lm(AoA ~ F.z + C.z + CD.z + OLD.z + L.z + Dom_Pos + FpM.z + VC.z, data = df)
  var = 'VC'
  AIC = AIC(lm.VC)
  rsq =  summary(lm.VC)$r.sq
  b = tail(lm.VC$coefficients, n=1)
  deltaAIC = AIC.base - AIC
  stats = rbind(stats,
                data.frame(t, var, AIC, rsq, b, deltaAIC))
  
  lm.rVC = lm(AoA ~ F.z + C.z + CD.z + OLD.z + L.z + Dom_Pos + FpM.z + rVC.z, data = df)
  var = 'rVC'
  AIC = AIC(lm.rVC)
  rsq =  summary(lm.rVC)$r.sq
  b = tail(lm.rVC$coefficients, n=1)
  deltaAIC = AIC.base - AIC
  stats = rbind(stats,
                data.frame(t, var, AIC, rsq, b, deltaAIC))
  
  lm.LNC = lm(AoA ~ F.z + C.z + CD.z + OLD.z + L.z + Dom_Pos + FpM.z + LNC.z, data = df)
  var = 'LNC'
  AIC = AIC(lm.LNC)
  rsq =  summary(lm.LNC)$r.sq
  b = tail(lm.LNC$coefficients, n=1)
  deltaAIC = AIC.base - AIC
  stats = rbind(stats,
                data.frame(t, var, AIC, rsq, b, deltaAIC))
  
  lm.rLNC = lm(AoA ~ F.z + C.z + CD.z + OLD.z + L.z + Dom_Pos + FpM.z + rLNC.z, data = df)
  var = 'rLNC'
  AIC = AIC(lm.rLNC)
  rsq =  summary(lm.rLNC)$r.sq
  b = tail(lm.rLNC$coefficients, n=1)
  deltaAIC = AIC.base - AIC
  stats = rbind(stats,
                data.frame(t, var, AIC, rsq, b, deltaAIC))
  
  lm.J = lm(AoA ~ F.z + C.z + CD.z + OLD.z + L.z + Dom_Pos + FpM.z + J.z, data = df)
  var = 'J'
  AIC = AIC(lm.J)
  rsq =  summary(lm.J)$r.sq
  b = tail(lm.J$coefficients, n=1)
  deltaAIC = AIC.base - AIC
  stats = rbind(stats,
                data.frame(t, var, AIC, rsq, b, deltaAIC))
  
  lm.rJ = lm(AoA ~ F.z + C.z + CD.z + OLD.z + L.z + Dom_Pos + FpM.z + rJ.z, data = df)
  var = 'rJ'
  AIC = AIC(lm.rJ)
  rsq =  summary(lm.rJ)$r.sq
  b = tail(lm.rJ$coefficients, n=1)
  deltaAIC = AIC.base - AIC
  stats = rbind(stats,
                data.frame(t, var, AIC, rsq, b, deltaAIC))
  
}

colnames(stats) = c("Threshold", "var", "AIC", "r_squared", 'coeff', 'delta')
colnames(corrs) = c("Target", "var", "Threshold", "r_squared")

rm(t, list = ls(pattern = "lm"), cormat, df, melted_cormat, upper_tri, AIC, 
   AIC.base, b, deltaAIC, r_squared, rsq, target, var)

corrs$Threshold = as.factor(as.character(corrs$Threshold))
corrs$Threshold = factor(corrs$Threshold, levels = c("25", "50", "75", "100"))
corrs$var = factor(corrs$var, levels = c("VC", "rVC", "LNC", "rLNC", "J", "rJ"))

ggplot(data = corrs, aes(x = var, y = r_squared)) +
  geom_point(aes(color = Threshold, fill = Threshold), position=pd, size=2) +
  theme(axis.title.x = element_blank()) +
  scale_color_brewer(palette = "Set2") +
  scale_fill_brewer(palette = "Set2") +
  ggtitle("R squared (AoA ~ var)", subtitle = "min count = 10") +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 11, hjust = 1))
  


stats$Threshold = as.factor(as.character(stats$Threshold))
stats$Threshold = factor(stats$Threshold, levels = c("25", "50", "75", "100"))
stats$var = factor(stats$var, levels = c("VC", "rVC", "LNC", "rLNC", "J", "rJ"))
stats$delta = -stats$delta

ggplot(data = stats[stats$var %in% targets, ], aes(x = var, y = delta)) +
  geom_point(aes(color = Threshold, fill = Threshold), position=pd, size=2) +
  theme(axis.title.x = element_blank()) +
  scale_color_brewer(palette = "Set2") +
  scale_fill_brewer(palette = "Set2") +
  ggtitle("delta AIC ((base+var) - base)", subtitle = "min count = 10") +
  ylab("delta AIC") +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 11, hjust = 1))

