library(tidyverse)
library(svglite)

#merge data
snp <- read.csv("snp_rate4_10.csv")
ges <- read.csv("gene_size2.csv")
jointed_table_snp <- merge(x=snp,y=ges,by="geneid")

#calculate snp rates
snp1 <- jointed_table_snp %>%
  rowwise() %>%
  mutate(
    snp_rate1 = count1/coverage1*100,
    snp_rate2 = count2/coverage2*100,
    snp_rate3 = count3/coverage3*100,
    snp_rate4 = count4/coverage4*100,
    snp_rate5 = count5/coverage5*100,
    snp_rate6 = count6/coverage6*100,
    snp_rate7 = count7/coverage7*100,
    snp_rate8 = count8/coverage8*100,
    snp_rate9 = count9/coverage9*100)

#filter(f>)

#pivot longer
snp2 <- snp1 %>% 
  pivot_longer(cols=c("snp_rate1", "snp_rate2", "snp_rate3", "snp_rate4",
                      "snp_rate5", "snp_rate6", "snp_rate7", "snp_rate8",
                      "snp_rate9"),
                    names_to='type',
                    values_to='value')
#calculate_snp_rate
snp3 <- snp2 %>%
  filter(value >= 10) %>%
  group_by(geneid, type, size) %>%
  summarise(
    count = n()
  ) %>%
  mutate(snp_rate = count/size)

#grouping
snp4 <- snp3 %>%
  group_by(type) %>%
  summarise(
    count = n(),
    mean = mean(snp_rate, na.rm = TRUE),
    sd.no = sd(snp_rate, na.rm = TRUE)
  ) %>%
  mutate(se.no = sd.no / sqrt(count),
         lower.ci.no = mean - qt(1 - (0.05 / 2), count - 1) * se.no,
         upper.ci.no = mean + qt(1 - (0.05 / 2), count - 1) * se.no)

#export data as csv
write.csv(snp2, "snprate_raw.csv")
write.csv(snp3, "snprate_ungrouped.csv")
write.csv(snp4, "snprate_grouped.csv")

#mean_snp_count
snp_count1 <- jointed_table_snp %>%
  pivot_longer(cols=c("count1", "count2", "count3", "count4",
                      "count5", "count6", "count7", "count8",
                      "count9"),
               names_to='type',
               values_to='value')
  
snp_count2 <- snp_count1 %>%
  group_by(type) %>%
  summarise(
    count = n(),
    sum = sum(value),
    mean = mean(value, na.rm = TRUE),
    sd.no = sd(value, na.rm = TRUE)
  ) %>%
  mutate(se.no = sd.no / sqrt(count),
         lower.ci.no = mean - qt(1 - (0.05 / 2), count - 1) * se.no,
         upper.ci.no = mean + qt(1 - (0.05 / 2), count - 1) * se.no)
#export data as csv
write.csv(snp_count2, "snpcount_grouped.csv")

#mean_snp_coverage
snp_coverage1 <- jointed_table_snp %>%
  pivot_longer(cols=c("coverage1", "coverage2", "coverage3", "coverage4",
                      "coverage5", "coverage6", "coverage7", "coverage8",
                      "coverage9"),
               names_to='type',
               values_to='value')

snp_coverage2 <- snp_coverage1 %>%
  group_by(type) %>%
  summarise(
    count = n(),
    sum = sum(value),
    mean = mean(value, na.rm = TRUE),
    sd.no = sd(value, na.rm = TRUE)
  ) %>%
  mutate(se.no = sd.no / sqrt(count),
         lower.ci.no = mean - qt(1 - (0.05 / 2), count - 1) * se.no,
         upper.ci.no = mean + qt(1 - (0.05 / 2), count - 1) * se.no)

#export data as csv
write.csv(snp_coverage2, "snpcoverage_grouped.csv")

##############
#pivot longer for hist
snp <- read.csv("snp_rate.csv")
hist1 <- snp %>% 
  pivot_longer(cols=c("coverage1", "coverage2", "coverage3", "coverage4",
                      "coverage5", "coverage6", "coverage7", "coverage8",
                      "coverage9"),
               names_to='type',
               values_to='value')

#create column d to group the data
hist1$d <- hist1$type
hist1$d[hist1$type == c("coverage1", "coverage2", "coverage3")] <- 1
hist1$d[hist1$type == c("coverage4", "coverage5", "coverage6")] <- 2
hist1$d[hist1$type == c("coverage7", "coverage8", "coverage9")] <- 3
hist1

#create overlaid hist
hist2 <- hist1 %>%
  filter(X10..f == "x") %>%
  ggplot(aes(x = value, fill = type)) +    # Draw overlaying histogram
  geom_histogram(position = "identity", alpha = 0.2, bins = 50)+
  scale_x_continuous(breaks=seq(0, 300, 20), limits = c(0, 300))+
  facet_wrap(~d)+
  theme_minimal()
print(hist2)

#create discrete hist
hist3 <- hist1 %>%
  filter(X10..f == "x") %>%
  ggplot(aes(x = value, fill = type)) +   
  geom_histogram(color="black", alpha = 0.4, bins = 50)+
  scale_x_continuous(name = "Coverage", breaks=seq(0, 300, 20), limits = c(0, 300))+
  scale_y_continuous(name = "Count", expand = c(0,0))+ 
  #expand() remove the space between plot and axis line
  theme_minimal()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_blank(),
        axis.text.x = element_text(size = 8, face="bold",color = "black"),
        axis.text.y = element_text(size = 8, face="bold",color = "black"),
        axis.title.x = element_text(size = 10, face="bold", color = "black"),
        axis.title.y = element_text(size = 10, face="bold", color = "black"),
        legend.position="none")+
  facet_wrap(~type)

print(hist3)

#export svg
svglite("overlaid_hist_coverage.svg", width = 10, height = 5)
hist2
dev.off()

svglite("disdrete_hist_coverage.svg", width = 10, height = 5)
hist3
dev.off()





