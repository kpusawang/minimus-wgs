library(tidyverse)
library(svglite)
#import data

hist_dis <- read.csv("hap_distance_min_max_revised.csv")

#create column d to group the data
dat1 <- hist_dis %>%
  group_by(GPX, GPY) %>%
  mutate(d = ifelse(GPX == "Gp 1}" & GPY == "Gp 1}", 1,
                    ifelse(GPX == "Gp 1}" & GPY == "Gp 2}", 2,
                           ifelse(GPX == "Gp 2}" & GPY == "Gp 2}", 3,
                                  ifelse(GPX == "Gp 1}" & GPY == "Gp 3}", 4,
                                         ifelse(GPX == "Gp 2}" & GPY == "Gp 3}", 5,
                                                ifelse(GPX == "Gp 3}" & GPY == "Gp 3}", 6,
                                         NA)))))))

dat1$d <- factor(dat1$d, levels=c("1", "3", "2", "4", "5", "6"))


#create overlaid hist
dat2 <- dat1 %>%
  filter(d != "NA" & d != "4" & d != "5" & d != "6") %>%
  ggplot(aes(x = Dist, fill = d)) +    # Draw overlaying histogram
  geom_histogram(color = "black", position = "identity", alpha = 0.7, bins = 20)+  
  theme_classic()+ 
  scale_x_continuous(name = "Genetic Distance", expand = c(0, 0)) +
  scale_y_continuous(name = "Count", expand = c(0, 0))+ 
  theme(legend.background=element_rect(fill=NA),
        axis.line = element_line(color = "black"),
        axis.text.x = element_text(size = 14, face="bold",color = "black"),
        axis.text.y = element_text(size = 14, face="bold",color = "black"),
        axis.title.y = element_text(size = 16, face="bold",color = "black"),
        axis.title.x = element_text(size = 16, face="bold",color = "black"),
        legend.title=element_text(size= 16, face = "bold", color = "black"),
        legend.text=element_text(size= 14, face = "bold", color = "black"))+ 
  scale_fill_brewer(name = " ", labels = c("Intraspecific A",
                                           "Intraspecific B", 
                                           "Interspecific A & B"),
                    palette = "Set2")+
  guides(color = guide_legend(order = 1),
         fill = guide_legend(override.aes = 
                               list(colour = "black"), order = 1))


print(dat2)

### export svg
svglite("dis_his_min.svg", width = 10, height = 5)
dat2
dev.off()

