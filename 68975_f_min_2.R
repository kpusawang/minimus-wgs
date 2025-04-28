library(dplyr)
library(tidyverse)

## merge data variant
mrna_min <- read.csv("13056_mrna_min.csv")
var <- read.csv("68975_variant_min.csv") 
jointed_table_2 <- merge(x=var,y=mrna_min,by="name.r")

data3 <- read.csv("68975_g1vsg2.csv")
data5 <- read.csv("68975_g1vsg3.csv")
data7 <- read.csv("68975_g2vsg3.csv")

data3 <- merge(x=data3,y=jointed_table_2,by="X68975n")
data5 <- merge(x=data5,y=jointed_table_2,by="X68975n") 
data7 <- merge(x=data7,y=jointed_table_2,by="X68975n") 

## g1vsg2
data4 <- data3 %>%
  filter(coverage.1 <= 1000 & coverage.2 <= 1000)%>%
  filter(description != 'unspecified product')%>%
  rowwise() %>%
  mutate(
    fisher.statistic = fisher.test(matrix(c(mut.1, mut.1.x, mut.2, mut.2.x), nrow = 2))$statistic,
    fisher.p = fisher.test(matrix(c(mut.1, mut.1.x, mut.2, mut.2.x), nrow = 2))$p.value,
  )

## g1vsg3
data6 <- data5 %>%
  filter(coverage.1 <= 1000 & coverage.3 <= 1000) %>%
  filter(description != 'unspecified product') %>%
  rowwise() %>%
  mutate(
    fisher.statistic = fisher.test(matrix(c(mut.1, mut.1.x, mut.3, mut.3.x), nrow = 2))$statistic,
    fisher.p = fisher.test(matrix(c(mut.1, mut.1.x, mut.3, mut.3.x), nrow = 2))$p.value
  )

## g2vsg3
data8 <- data7 %>%
  filter(coverage.2 <= 1000 & coverage.3 <= 1000) %>%
  filter(description != 'unspecified product') %>%
  rowwise() %>%
  mutate(
    fisher.statistic = fisher.test(matrix(c(mut.2, mut.2.x, mut.3, mut.3.x), nrow = 2))$statistic,
    fisher.p = fisher.test(matrix(c(mut.2, mut.2.x, mut.3, mut.3.x), nrow = 2))$p.value
  )

## adjusted p-value
as_tibble(data4)
as_tibble(data6)
as_tibble(data8)
fp_bh1 <- p.adjust(data4$fisher.p, method = "BH")
fp_bh1_rmna <- na.omit(fp_bh1)
sum(fp_bh1_rmna < 0.001) #2629
fp_bh2 <- p.adjust(data6$fisher.p, method = "BH")
fp_bh2_rmna <- na.omit(fp_bh2)
sum(fp_bh2_rmna < 0.001) #2948
fp_bh3 <- p.adjust(data8$fisher.p, method = "BH")
fp_bh3_rmna <- na.omit(fp_bh3)
sum(fp_bh3_rmna < 0.001) #4369

## bind column
adj1 <- data4 %>%
  cbind(fp_bh1) 
adj2 <- data6 %>%
  cbind(fp_bh2) 
adj3 <- data8 %>%
  cbind(fp_bh3) 

## filter

sig1 <- adj1 %>%
  filter(fp_bh1 < 0.001) #n=2629

sig2 <- adj2 %>%
  filter(fp_bh2 < 0.001) #n=2948

sig3 <- adj3 %>%
  filter(fp_bh3 < 0.001) #n=4369

## GO
goj <- read.csv("jointed_go.csv")
all <- read.csv("68975_variant_min_go.csv")
sig01 <- left_join(sig1, goj, by = "gene_id")
sig02 <- left_join(sig2, goj, by = "gene_id")
sig03 <- left_join(sig3, goj, by = "gene_id")
allgo <- left_join(all, goj, by = "gene_id")


## merge sig1 & sing2
merge_sig_snp<- inner_join(sig1, sig2, by = "X68975n") #798
merge_sig_snp_go<- inner_join(sig01, sig02, by = "X68975n") #798

## export data
write.csv(sig01 , "go_sig_68975_g1vsg2_f.csv", row.names = FALSE)
write.csv(sig02 , "go_sig_68975_g1vsg3_f.csv", row.names = FALSE)
write.csv(sig03 , "go_sig_68975_g2vsg3_f.csv", row.names = FALSE)
write.csv(allgo , "go_68975.csv", row.names = FALSE)
write.csv(merge_sig_snp , "merge_sig_snp_f_68975.csv", row.names = FALSE)
write.csv(merge_sig_snp_go , "merge_sig_snp_f_68975_go.csv", row.names = FALSE)
write.csv(adj1 , "68975_1_f.csv", row.names = FALSE)
write.csv(adj2 , "68975_2_f.csv", row.names = FALSE)
write.csv(adj3 , "68975_3_f.csv", row.names = FALSE)
