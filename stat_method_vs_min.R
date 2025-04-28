library(tidyverse)
library(svglite)

### fisher method vs min

df <- data.frame("min" = c(119, 121, 63, 122,79), 
                 "non_min" = c(1281, 19, 25, 95, 73), 
                 row.names = c("cow", "hli", "hlo", "lti", "lto"))

mosaicplot(df, color = TRUE)

#perform fisher
fisher.test(df, simulate.p.value = TRUE)
#sig p<0.05

#posthoc
library(rstatix)
pairwise_fisher_test(as.matrix(df), p.adjust.method = "fdr")
#sig p<0.05, except lti vs lto

### fisher lineage vs method
df_2 <- data.frame("la" = c(45, 35, 32, 33, 36), 
                 "lb" = c(40, 15, 18, 17, 16), 
                 row.names = c("cow", "hli", "hlo", "lti", "lto"))
mosaicplot(df_2, color = TRUE)  

#perform fisher
fisher.test(df_2)

#posthoc
pairwise_fisher_test(as.matrix(df_2), p.adjust.method = "fdr")
## not sig p>0.05
###################################
### fisher method vs mac
df_3 <- data.frame("mac" = c(530, 10, 16, 23, 23), 
                 "non_mac" = c(870, 130, 72, 194, 129), 
                 row.names = c("cow", "hli", "hlo", "lti", "lto"))

mosaicplot(df_3, color = TRUE)

#perform fisher
fisher.test(df_3, simulate.p.value = TRUE)
#sig p<0.05

#posthoc
library(rstatix)
pairwise_fisher_test(as.matrix(df_3), p.adjust.method = "fdr")
#sig p<0.05 in cow vs others / hlio vs hlo















