## load packages
library(dplyr)
library(stringr)
library(ggplot2)

## set working directory to the location of the master "word_embeddings" folder
setwd("")

## load data
across_corr_politics <- readRDS('./data/cr_groups/glove/correlations/across_corr_pearson_politics.rds')
across_corr_random <- readRDS('./data/cr_groups/glove/correlations/across_corr_pearson_random.rds')

# confidence intervals
interval1 <- -qnorm((1-0.9)/2)  # 90% multiplier
interval2 <- -qnorm((1-0.95)/2)  # 95% multiplier

## fg4a_appendix: random cues
plot_dt <- cbind(group = as.factor(c("D", "R")), across_corr_random[c(4,10), 3:4])
ggplot(plot_dt, aes(x = group, y = mean), leg) +
  geom_bar(position = position_dodge(), stat = "identity") + 
  geom_errorbar(aes(ymin = mean - interval2*se, ymax = mean + interval2*se), width = 0.2, position = position_dodge(0.9)) +
  xlab("") + ylab("Mean Pearson Correlation \n with Pretrained Embeddings") +
  ylim(0, 1) +
  theme(plot.title = element_text(size=20, margin = margin(t = 20, r = 0, b = 0, l = 0)), panel.background = element_blank(),
        axis.text.x = element_text(size=15, angle = 90, margin = margin(t = -10, r = 0, b = 0, l = 0)),
        axis.text.y = element_text(size=15),
        axis.ticks.x=element_blank(),
        axis.title.y = element_text(size=18, margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(size=18, margin = margin(t = 20, r = 0, b = 0, l = 0)),
        legend.position="none",
        legend.text = element_text(size=15),
        legend.title = element_blank())
ggsave(paste0('./figures/fg9a_app.eps'), dpi = 800)

## fg4a_appendix: politics cues
plot_dt <- cbind(group = as.factor(c("D", "R")), across_corr_politics[c(4,10), 3:4])
ggplot(plot_dt, aes(x = group, y = mean), leg) +
  geom_bar(position = position_dodge(), stat = "identity") + 
  geom_errorbar(aes(ymin = mean - interval2*se, ymax = mean + interval2*se), width = 0.2, position = position_dodge(0.9)) +
  xlab("") + ylab("Mean Pearson Correlation \n with Pretrained Embeddings") +
  ylim(0, 1) +
  theme(plot.title = element_text(size=20, margin = margin(t = 20, r = 0, b = 0, l = 0)), panel.background = element_blank(),
        axis.text.x = element_text(size=15, angle = 90, margin = margin(t = -10, r = 0, b = 0, l = 0)),
        axis.text.y = element_text(size=15),
        axis.ticks.x=element_blank(),
        axis.title.y = element_text(size=18, margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(size=18, margin = margin(t = 20, r = 0, b = 0, l = 0)),
        legend.position="none",
        legend.text = element_text(size=15),
        legend.title = element_blank())
ggsave(paste0('./figures/fg9b_app.eps'), dpi = 800)
