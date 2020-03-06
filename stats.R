library(readxl)
library(lme4)
library(lmerTest)
library(mgcv)
library(itsadug)
library(dplyr)
library(cocor)
library(glue)
library(reshape2)
library(plyr)
library(ggplot2)
library(Rmisc)

get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

setwd("/home/gioca90/Desktop/Research/")

aoa = read_excel("Resources/AoA/AoA_ratings_from_all_sources.xlsx", sheet = 'Kuperman et al.')
concreteness = read.csv("Resources/Brysbaert_concreteness.txt", header = T, sep = '\t')
subtlex = read.csv("Resources/SUBTLEX/SUBTLEX-US.txt", header = T, sep = '\t')
old_len = read.csv("Projects/semanticShift/data/word_OLD_len.csv", header = T, sep = '\t')
measures = read.csv("Projects/semanticShift/data/measures.csv", header = T, sep = ',')

measures = measures %>%
  dplyr::rename(
    Word = word
  )

measures.20nn = measures[c(1,2,5:10,23:28)]
measures.30nn = measures[c(1,3,11:16,23:28)]
measures.50nn = measures[c(1,4,17:22,23:28)]

tmp1 = merge(aoa, concreteness, by = 'Word')
tmp2 = merge(tmp1, subtlex, by = 'Word')
tmp2 = dplyr::select(tmp2, Word, Rating.Mean, Conc.M, Lg10WF)
tmp3 = merge(tmp2, old_len, by = 'Word')
df = merge(tmp3, measures.50nn, by = 'Word')

rm(tmp1, tmp2, measures, aoa, concreteness, subtlex, old_len)

hist(df$Lg10WF)
hist(df$Conc.M)
hist(log10(df$OLD20))
hist(df$len)

df$OLD20 = log10(df$OLD20)
df$C.RANGE = df$C.MAX - df$C.MIN 

df = dplyr::select(df, Word, Rating.Mean, Conc.M, Lg10WF, OLD20, len,
                   J.50, C.SUM, C.RANGE)

df = df %>%
  dplyr::rename(
    J = J.50
  )

hist(log10(df$C.RANGE))

df$C.SUM = df$C.SUM**2
df$C.RANGE = log10(df$C.RANGE)

boxplot(df[2:9])

gam.baseline = bam(Rating.Mean ~ s(Lg10WF) + s(Conc.M) + s(OLD20) + s(len), data = df)
AIC(gam.baseline)
# 57649.7
summary(gam.baseline)
# deviance explained: 54%
plot(gam.baseline)


gam.C.SUM = bam(Rating.Mean ~ s(Lg10WF) + s(Conc.M) + s(OLD20) + s(len) + s(C.SUM), data = df)
plot(gam.C.SUM)
AIC(gam.C.SUM)
# 57562.86
summary(gam.C.SUM)
# deviance explained: 54.3%


gam.C.RANGE = bam(Rating.Mean ~ s(Lg10WF) + s(Conc.M) + s(OLD20) + s(len) + s(C.RANGE), data = df)
plot(gam.C.RANGE)
AIC(gam.C.RANGE)
# 57601.71
summary(gam.C.RANGE)
# deviance explained: 54.1%


gam.J = bam(Rating.Mean ~ s(Lg10WF) + s(Conc.M) + s(OLD20) + s(len) + s(J), data = df)
plot(gam.J)
AIC(gam.J)
# k=20: 57562.45
# k=30: 57555.23
# k=50: 57534.23
summary(gam.J)
# k=20: deviance explained: 54.3%
# k=30: deviance explained: 54.2%
# k=50: deviance explained: 54.3%

plot(df$J, df$Rating.Mean)


str(df)
ggplot(data = df, aes(x = C.SUM, y = C.RANGE, fill = Rating.Mean, color = Rating.Mean)) +
  geom_point(size = 0.75) +
  scale_fill_gradient2(low = "steelblue", high = "darkred", mid = 'lightgrey', midpoint = max(df$Rating.Mean)/2,
                       limit = c(min(df$Rating.Mean), max(df$Rating.Mean)), space = "Lab", name="AoA") +
  scale_color_gradient2(low = "steelblue", high = "darkred", mid = 'lightgrey', midpoint = max(df$Rating.Mean)/2,
                        limit = c(min(df$Rating.Mean), max(df$Rating.Mean)), space = "Lab", name="AoA")


cormat <- round(cor(df[c(2:9)]),3)
upper_tri <- get_upper_tri(cormat)
melted_cormat <- melt(upper_tri, na.rm = TRUE)

ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(high = "darkred", low = "steelblue", mid = "lightgray", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed() + 
  geom_text(aes(Var2, Var1, label = value), color = "white", size = 2) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.7),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))







measures.pca = prcomp(df[7:9], center = T, scale. = T)
summary(measures.pca)

loadings = measures.pca$rotation
melted_loadings = melt(loadings)
melted_loadings$value = round(melted_loadings$value, 3)
ggplot(data = melted_loadings, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(high = "darkred", low = "steelblue", mid = "lightgray", 
                       midpoint = 0, limit = c(min(melted_loadings$value), max(melted_loadings$value)), 
                       space = "Lab", name="loadings") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))+
  coord_fixed() + 
  geom_text(aes(Var2, Var1, label = value), color = "white", size = 2) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = 'right',
    legend.direction = "vertical")+
  guides(fill = guide_colorbar(barwidth = 1, barheight = 7,
                               title.position = "top", title.hjust = 0.5))


princ_comps = measures.pca$x
df.pca = cbind(df[1:6], princ_comps)

cormat <- round(cor(df.pca[2:9]),3)
upper_tri <- get_upper_tri(cormat)
melted_cormat <- melt(upper_tri, na.rm = TRUE)

ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(high = "darkred", low = "steelblue", mid = "lightgray", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed() + 
  geom_text(aes(Var2, Var1, label = value), color = "white", size = 2) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.7),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))


gam.PC1 = bam(Rating.Mean ~ s(Lg10WF) + s(Conc.M) + s(OLD20) + s(len) + s(PC1), data = df.pca)
plot(gam.PC1)
AIC(gam.PC1)
# k=20: 57548.64
# k=30: 57547.16
# k=50: 57541.19
summary(gam.PC1)
# k=20: deviance explained: 54.3%
# k=50: deviance explained: 54.4%


gam.PC2 = bam(Rating.Mean ~ s(Lg10WF) + s(Conc.M) + s(OLD20) + s(len) + s(PC2), data = df.pca)
plot(gam.PC2)
AIC(gam.PC2)
# k=20: 57612.52
# k=30: 57610.12
# k=50: 57598.75
summary(gam.PC2)
# k=20: deviance explained: 54.1%
# k=50: deviance explained: 54.2%


gam.PC3 = bam(Rating.Mean ~ s(Lg10WF) + s(Conc.M) + s(OLD20) + s(len) + s(PC3), data = df.pca)
plot(gam.PC3)
AIC(gam.PC3)
# k=20: 57625.93
# k=30: 57626.31
# k=50: 57626.62
summary(gam.PC3)
# k=20: deviance explained: 54.1%


gam.PC1PC2 = bam(Rating.Mean ~ s(Lg10WF) + s(Conc.M) + s(OLD20) + s(len) + te(PC1,PC2), data = df.pca)
AIC(gam.PC1PC2)
# k=20: 57552.39
# k=30: 57547.68
# k=50: 57527.82
summary(gam.PC1PC2)
# k=20: deviance explained: 54.4%

vis.gam(gam.PC1PC2, too.far=0.1, color="topo", plot.type="contour", 
        view=c('PC1', 'PC2'), main = "AoA norms: PC1*PC2 (GAM)")


