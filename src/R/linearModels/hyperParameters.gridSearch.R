library(dplyr)
library(cocor)
library(reshape2)
library(plyr)
library(ggplot2)
library(Rmisc)
library(RColorBrewer)


# get the top half of a correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

# box-cox transform a variable by first looking for the lambda which maximises negative 
# log-likelihood considering 0.1 increments from min to max
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
# define whether to consider the measures derived with a min_count of 10 or a min_count of 0
min.count = 10

# define possible values for window size and embedding dimensionality
window_sizes = c(3, 5, 20)
dims = c(40, 100)
stats = NULL

# consider all combinations of window size and embedding dimensionality, reading the data and
# fitting the target linear models, storing summary statistics in a df
for (d in dims) {
  
  # create folders and sub-folders to store correlation plots
  directory = sprintf("plots/minCount_%s/d%s/", min.count, d)
  if (file.exists(directory)){
    print(sprintf("dir %s already exists.", directory))
  } else {
    dir.create(directory)
    print(sprintf("dir %s has been created.", directory))
  }
  
  for (w in window_sizes) {
    
    subdirectory = sprintf("plots/minCount_%s/d%s/w%s/", min.count, d, w)
    if (file.exists(subdirectory)){
      print(sprintf("dir %s already exists.", subdirectory))
    } else {
      dir.create(subdirectory)
      print(sprintf("dir %s has been created.", subdirectory))
    }
    
    df = read.csv(sprintf("data/processed/LNC.fixed.minCount%s/semanticShift_d%s_w%s.csv", 
                          min.count, d, w), 
                  header = T, sep = ',')  
    
    df$Dom_Pos = relevel(factor(df$Dom_Pos), 'Noun')

    df$F.z = scale(box_cox_transf(df$Freq, -10, 10), center = T, scale = T)
    df$C.z = scale(box_cox_transf(df$Concr, -10, 10), center = T, scale = T)
    df$CD.z = scale(box_cox_transf(df$CD, -10, 10), center = T, scale = T)
    df$OLD.z = scale(box_cox_transf(df$OLD20, -10, 10), center = T, scale = T)
    df$L.z = scale(box_cox_transf(df$len, -10, 10), center = T, scale = T)
    df$FpM.z = scale(box_cox_transf(df$FpM, -10, 10), center = T, scale = T)
    
    # add a small constant to J to remove 0s
    df$J.z = scale(box_cox_transf(df$J.SUM + 0.0001, -10, 10), center = T, scale = T)
    df$VC.z = scale(box_cox_transf(df$VC.SUM, -10, 10), center = T, scale = T)
    df$LNC.z = scale(box_cox_transf(df$LNC.SUM, -10, 10), center = T, scale = T)
    df$LNC1.z = scale(box_cox_transf(df$LNC1.SUM, -10, 10), center = T, scale = T)
    
    df$rJ.z = scale(box_cox_transf(df$rJ.SUM + 0.0001, -10, 10), center = T, scale = T)
    df$rVC.z = scale(box_cox_transf(df$rVC.SUM, -10, 10), center = T, scale = T)
    df$rLNC.z = scale(box_cox_transf(df$rLNC.SUM, -10, 10), center = T, scale = T)
    
    str(df)
  
    ##### LNC #####
    lm.LNC = lm(AoA ~ F.z + C.z + CD.z + OLD.z + L.z + Dom_Pos + FpM.z + LNC.z, data = df)
    var = "LNC"
    aic = AIC(lm.LNC)
    r_squared = summary(lm.LNC)$r.sq
    stats = rbind(stats,
                  data.frame(w, d, var, aic, r_squared))

    ##### LNC random #####
    lm.rLNC = lm(AoA ~ F.z + C.z + CD.z + OLD.z + L.z + Dom_Pos + FpM.z + rLNC.z, data = df)
    var = "rLNC"
    aic = AIC(lm.rLNC)
    r_squared = summary(lm.rLNC)$r.sq
    stats = rbind(stats,
                  data.frame(w, d, var, aic, r_squared))
    
    ##### LNC1 #####
    lm.LNC1 = lm(AoA ~ F.z + C.z + CD.z + OLD.z + L.z + Dom_Pos + FpM.z + LNC1.z, data = df)
    var = "LNC1"
    aic = AIC(lm.LNC1)
    r_squared = summary(lm.LNC1)$r.sq
    stats = rbind(stats,
                  data.frame(w, d, var, aic, r_squared))
  
    ##### JAC #####
    lm.J = lm(AoA ~ F.z + C.z + CD.z + OLD.z + L.z + Dom_Pos + FpM.z + J.z, data = df)
    var = "J"
    aic = AIC(lm.J)
    r_squared = summary(lm.J)$r.sq
    stats = rbind(stats,
                  data.frame(w, d, var, aic, r_squared))
    
    ##### JAC random #####
    lm.rJ = lm(AoA ~ F.z + C.z + CD.z + OLD.z + L.z + Dom_Pos + FpM.z + rJ.z, data = df)
    var = "rJ"
    aic = AIC(lm.rJ)
    r_squared = summary(lm.rJ)$r.sq
    stats = rbind(stats,
                  data.frame(w, d, var, aic, r_squared))

    ##### C #####
    lm.VC = lm(AoA ~ F.z + C.z + CD.z + OLD.z + L.z + Dom_Pos + FpM.z + VC.z, data = df)
    var = "VC"
    aic = AIC(lm.VC)
    r_squared = summary(lm.VC)$r.sq
    stats = rbind(stats,
                  data.frame(w, d, var, aic, r_squared))
    
    ##### C random #####
    lm.rVC = lm(AoA ~ F.z + C.z + CD.z + OLD.z + L.z + Dom_Pos + FpM.z + rVC.z, data = df)
    var = "rVC"
    aic = AIC(lm.rVC)
    r_squared = summary(lm.rVC)$r.sq
    stats = rbind(stats,
                  data.frame(w, d, var, aic, r_squared))
    
    
    
    ##### Correlation Analyses #####
    cormat <- round(cor(df[c("AoA", "C.z", "CD.z", "F.z", "FpM.z", "L.z", "OLD.z", 
                             "LNC.z", "VC.z", "J.z")]), 3)
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
      geom_text(aes(Var2, Var1, label = value), color = "white", size = 4) +
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
    ggsave("corrMatrix.pdf", plot = last_plot(), device = "pdf", 
           path = subdirectory, width = 18, height = 18, units = "cm")
    
    
    
    cormat <- round(cor(df[c("AoA", "C.z", "CD.z", "F.z", "FpM.z", "L.z", "OLD.z", 
                             "rLNC.z", "rVC.z", "rJ.z")]), 3)
    upper_tri <- get_upper_tri(cormat)
    melted_cormat <- melt(upper_tri, na.rm = TRUE)
    ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
      geom_tile(color = "white")+
      scale_fill_gradient2(high = "darkred", low = "steelblue", mid = "lightgray", 
                           midpoint = 0, limit = c(-1,1), space = "Lab", 
                           name="Pearson\nCorrelation") +
      theme_minimal()+ 
      theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                       size = 12, hjust = 1)) +
      coord_fixed() + 
      geom_text(aes(Var2, Var1, label = value), color = "white", size = 4) +
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
    ggsave("corrMatrix_random.pdf", plot = last_plot(), device = "pdf", path = subdirectory, 
           width = 18, height = 18, units = "cm")    

  }
}


##### Base model #####
lm.baseline = lm(AoA ~ F.z + C.z + CD.z + OLD.z + L.z + FpM.z + Dom_Pos, data = df)
var = 'Base'
aic = AIC(lm.baseline)
r_squared = summary(lm.baseline)$r.sq
w = 0
d = 0

stats = rbind(stats, data.frame(w, d, var, aic, r_squared))
colnames(stats) = c("Window_size", "Dimensionality", "Variable", "AIC", "r_squared")

rm(w, var, subdirectory, r_squared, directory, dims, window_sizes, d, aic, 
   list = ls(pattern = "lm"), upper_tri, melted_cormat, df, cormat)



##### PLOTS #####
stats_vars = stats[stats$Variable %in% c("LNC", "rLNC", "LNC1", "VC", "rVC", "J", "rJ"),]
stats_base = stats[stats$Variable == "Base",]

stats_vars$Window_size = as.factor(as.character(stats_vars$Window_size))
stats_vars$Dimensionality = as.factor(as.character(stats_vars$Dimensionality))

stats_vars$Dimensionality = plyr::mapvalues(stats_vars$Dimensionality, 
                                            from = c("40", "100"), 
                                            to = c("dim=40", "dim=100"))
stats_vars$Dimensionality = factor(stats_vars$Dimensionality, levels = c("dim=40", "dim=100"))
stats_vars$Window_size = factor(stats_vars$Window_size, levels = c("3", "5", "20"))

stats_vars$Variable = factor(stats_vars$Variable, 
                             levels = c("VC", "rVC", "LNC", "rLNC", "J", "rJ", "LNC1"))


ggplot(data = stats_vars, aes(x = Variable, y = AIC)) +
  geom_point(aes(color = Window_size, fill = Window_size), position=pd, size=2) +
  geom_hline(yintercept = stats_base$AIC) +
  facet_grid(. ~ Dimensionality) +
  scale_color_brewer(palette = "Set2") +
  scale_fill_brewer(palette = "Set2") +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 11, hjust = 1),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),  
        legend.position = "bottom",
        legend.direction = "horizontal"
        ) +
  guides(colour = guide_legend(nrow = 1)) +
  ggtitle("TWEC hyper-parameters grid search", subtitle = sprintf("min count = %s", min.count))

ggplot(data = stats_vars, aes(x = Variable, y = r_squared)) +
  geom_point(aes(color = Window_size, fill = Window_size), position=pd, size=2) +
  geom_hline(yintercept = stats_base$r_squared) +
  facet_grid(. ~ Dimensionality) +
  theme(axis.title.x = element_blank())
