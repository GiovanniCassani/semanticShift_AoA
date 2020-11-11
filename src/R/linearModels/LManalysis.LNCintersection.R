library(dplyr)
library(cocor)
library(reshape2)
library(plyr)
library(ggplot2)
library(Rmisc)

# the code and logic are the same as detailed in the script LManalysis.R. Check the comments there
# for details about what happens here when not specified here.

get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

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

setwd(sub("src.*", "", getwd()))

df = read.csv("data/processed/semanticShift_d40_w3.LNCintersection.csv", 
             header = T, sep = ',')  

##### Transformations ####
df$Dom_Pos = relevel(factor(df$Dom_Pos), 'Noun')

df$F.z = scale(box_cox_transf(df$Freq, -10, 10), center = T, scale = T)
df$C.z = scale(box_cox_transf(df$Concr, -10, 10), center = T, scale = T)
df$CD.z = scale(box_cox_transf(df$CD, -10, 10), center = T, scale = T)
df$OLD.z = scale(box_cox_transf(df$OLD20, -10, 10), center = T, scale = T)
df$L.z = scale(box_cox_transf(df$len, -10, 10), center = T, scale = T)
df$FpM.z = scale(box_cox_transf(df$FpM, -10, 10), center = T, scale = T)

df$J.z = scale(box_cox_transf(df$J + 0.0001, -10, 10), center = T, scale = T)
df$VC.z = scale(box_cox_transf(df$VC, -10, 10), center = T, scale = T)
df$LNC.z = scale(box_cox_transf(df$LNC, -10, 20), center = T, scale = T)

df$rJ.z = scale(box_cox_transf(df$rJ + 0.0001, -10, 10), center = T, scale = T)
df$rVC.z = scale(box_cox_transf(df$rVC, -10, 10), center = T, scale = T)
df$rLNC.z = scale(box_cox_transf(df$rLNC, -10, 10), center = T, scale = T)


##### Correlation Analyses #####
cormat <- round(cor(df[c("AoA", "C.z", "CD.z", "F.z", "L.z", "FpM.z", "OLD.z", "LNC.z", "VC.z", 
                         "J.z", "rLNC.z", "rVC.z", "rJ.z")]),3)
upper_tri <- get_upper_tri(cormat)
melted_cormat <- melt(upper_tri, na.rm = TRUE)
levels(melted_cormat$Var1)
melted_cormat$Var1 = plyr::mapvalues(melted_cormat$Var1, 
                                     from = c("AoA", "C.z", "CD.z", "F.z", "L.z", "FpM.z", 
                                              "OLD.z", "LNC.z", "VC.z", "J.z", "rLNC.z", "rVC.z", 
                                              "rJ.z"), 
                                     to = c("AoA", "Concr", "CD", "Freq", "Len", "FpM", "OLD20", 
                                            "LNC", "VC", "J", "rLNC", "rVC", "rJ"))
melted_cormat$Var2 = plyr::mapvalues(melted_cormat$Var2, 
                                     from = c("AoA", "C.z", "CD.z", "F.z", "L.z", "FpM.z", 
                                              "OLD.z", "LNC.z", "VC.z", "J.z", "rLNC.z", "rVC.z", 
                                              "rJ.z"), 
                                     to = c("AoA", "Concr", "CD", "Freq", "Len", "FpM", "OLD20", 
                                            "LNC", "VC", "J", "rLNC", "rVC", "rJ"))

ggplot(data = melted_cormat, aes(Var2, Var1, fill = value)) +
  geom_tile(color = "white")+
  scale_fill_gradient2(high = "darkred", low = "steelblue", mid = "lightgray", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 11, hjust = 1),
        axis.text.y = element_text(vjust = 1, size = 11, hjust = 1)) +
  coord_fixed() + 
  geom_text(aes(Var2, Var1, label = value), color = "white", size = 4) +
  ggtitle("Pairwise correlations (d=40, w=3, minCount=10)") +
  
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



##### Base model #####
lm.baseline = lm(AoA ~ F.z + C.z + CD.z + OLD.z + L.z + Dom_Pos + FpM.z, data = df)
AIC(lm.baseline)
summary(lm.baseline)$r.sq
summary(lm.baseline)
base.aic = AIC(lm.baseline)



##### LNC #####
lm.LNC = lm(AoA ~ F.z + C.z + CD.z + OLD.z + L.z + FpM.z + Dom_Pos + LNC.z, data = df)
AIC(lm.LNC)
summary(lm.LNC)$r.sq
summary(lm.LNC)
base.aic - AIC(lm.LNC)



##### LNC random #####
lm.rLNC = lm(AoA ~ F.z + C.z + CD.z + OLD.z + L.z + FpM.z + Dom_Pos + rLNC.z, data = df)
AIC(lm.rLNC)
summary(lm.rLNC)$r.sq
summary(lm.rLNC)
base.aic - AIC(lm.rLNC)



##### JAC ##### 
lm.J = lm(AoA ~ F.z + C.z + CD.z + OLD.z + L.z + FpM.z + Dom_Pos + J.z, data = df)
AIC(lm.J)
summary(lm.J)$r.sq
summary(lm.J)
base.aic - AIC(lm.J)



##### JAC random #####
lm.rJ = lm(AoA ~ F.z + C.z + CD.z + OLD.z + L.z + FpM.z + Dom_Pos + rJ.z, data = df)
AIC(lm.rJ)
summary(lm.rJ)$r.sq
summary(lm.rJ)
base.aic - AIC(lm.rJ)



##### C #####
lm.VC = lm(AoA ~ F.z + C.z + CD.z + OLD.z + L.z + FpM.z + Dom_Pos + VC.z, data = df)
AIC(lm.VC)
summary(lm.VC)$r.sq
summary(lm.VC)
base.aic - AIC(lm.VC)



##### C random #####
lm.rVC = lm(AoA ~ F.z + C.z + CD.z + OLD.z + L.z + FpM.z + Dom_Pos + rVC.z, data = df)
AIC(lm.rVC)
summary(lm.rVC)$r.sq
summary(lm.rVC)
base.aic - AIC(lm.rVC)


