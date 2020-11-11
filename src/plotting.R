library(ggplot2)
library(RColorBrewer)
library(dplyr)

setwd(sub("src.*", "", getwd()))

VC.high = read.csv("data/visualisations/VC_export_high.csv", header = T, sep=',')
VC.low = read.csv("data/visualisations/VC_export_low.csv", header = T, sep=',')
VC.high$VC= 'high'
VC.low$VC = 'low'

VC = rbind(VC.high, VC.low)

VC$slice = as.factor(VC$slice)

VC.colors = c("#D9B3FF", "#C28FE0", "#AB6BC2", "#9448A3", "#7D2485", "#660066")

ggplot(data = VC[VC$word %in% c('recorder', 'pregnant', 'finger', 'thunder'),], 
       aes(x=x, y=y, fill=slice, color=slice)) +
  geom_point(size=2) +
  facet_grid(. ~ word) +
  scale_color_manual(values = VC.colors) +
  ylab("PC2") +
  xlab("PC1") +
  ggtitle("Vector coherence") +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal"
    ) +
  guides(colour = guide_legend(nrow = 1))



LNC.high = read.csv("data/visualisations/LNC_export_high.csv", header = T, sep=',')
LNC.high = dplyr::distinct(LNC.high)
LNC.low = read.csv("data/visualisations/LNC_export_low.csv", header = T, sep=',')
LNC.low = dplyr::distinct(LNC.low)

LNC.high$year = as.factor(LNC.high$year)
LNC.low$year = as.factor(LNC.low$year)

LNC.evening = LNC.high[LNC.high$word == 'evening',]
LNC.aids = LNC.low[LNC.low$word == 'aids',]

LNC.evening$neighbor = factor(LNC.evening$neighbor, 
                              levels = c("sunrise", "morning", "midnight", "weekday", 
                                         "yesterday", "saturday", "sunday", "overcast"))

LNC.aids = LNC.aids[LNC.aids$neighbor %in% 
                      c("operations", "culinary", "personnel", "engineers", 
                        "asthma", "epidemic", "cholera", "disease"),]
LNC.aids$neighbor = factor(LNC.aids$neighbor, 
                           levels = c("culinary", "operations", "personnel", "engineers", 
                                      "asthma", "epidemic", "cholera", "disease"))


ggplot(data = LNC.evening, aes(x=neighbor, y=cosine, fill = neighbor, color=neighbor)) +
  geom_col() +
  facet_grid(. ~ year) +
  scale_color_brewer(palette = "Set2") +
  scale_fill_brewer(palette = "Set2") +
  ylab("cosine") +
  xlab("neighbors") +
  ggtitle(sprintf("Local Neighborhood Coherence: %s", LNC.evening$word)) +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position = "bottom",
    legend.direction = "horizontal"
  ) +
  guides(colour = guide_legend(nrow = 2))

ggplot(data = LNC.aids, aes(x=neighbor, y=cosine, fill = neighbor, color=neighbor)) +
  geom_col() +
  facet_grid(. ~ year) +
  scale_color_brewer(palette = "Set2") +
  scale_fill_brewer(palette = "Set2") +
  ylab("cosine") +
  xlab("neighbors") +
  ggtitle(sprintf("Local Neighborhood Coherence: %s", LNC.aids$word)) +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position = "bottom",
    legend.direction = "horizontal"
  ) +
  guides(colour = guide_legend(nrow = 2))
