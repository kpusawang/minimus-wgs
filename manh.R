library(qqman)
library(tidyverse)
library(svglite)

snpsOfInterest1 <- c("68082", "18702", "11122", "45965", "11415", "33032")
                    
snpsOfInterest2 <- c("1212", "18702", "64929", "68064", "11122", "33032") 

snpsOfInterest3 <- c("68082", "18702", "11122", "45965", "11415", "33032",
                     "1212", "64929", "68064")

svglite("manhat_g1.svg", width = 10, height = 5)
man <- read.csv("manplot_g1.csv")%>%
  filter(P >= 10^-15)%>%
  manhattan(genomewideline = -log10(0.001), suggestiveline = F,
            highlight = snpsOfInterest1, col = c("#636363", "#A5ADAF"))
dev.off()

svglite("manhat_g2.svg", width = 10, height = 5)
man_2 <- read.csv("manplot_g2.csv")%>%
  filter(P >= 10^-15)%>%
  manhattan(genomewideline = -log10(0.001), suggestiveline = F,
            highlight = snpsOfInterest2, col = c("#636363", "#A5ADAF"))
dev.off()

svglite("manhat_g3.svg", width = 10, height = 5)
man_3 <- read.csv("manplot_g3.csv")%>%
  filter(P >= 10^-15)%>%
  manhattan(genomewideline = -log10(0.001), suggestiveline = F,
            highlight = snpsOfInterest3, col = c("#636363", "#A5ADAF"))
dev.off()


#############
comp <- read_csv("mosq_comp_wgs.csv")
comp_plot<- ggplot(comp, aes(fill=method, y=no, x= reorder(species, no))) + 
  geom_bar(position="stack", stat="identity", color ="black")+
  theme_minimal()+
  scale_y_continuous(name = "Count",expand = c(0, 0))+
  labs(x= "Species")+
  scale_fill_manual(name= "Method",
                    labels = c("Cow", "HLI", "HLO", "LTI", "LTO"),
                    values = c("#F8766D","#A3A500", "#00BF7D", "#00B0F6", "#E76BF3"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(color = "black"),
        axis.text.y = element_text(size = 14, face="bold",color = "black"),
        axis.text.x = element_text(size = 14, face="bold", color = "black"), # fliped_y
        axis.title.x = element_text(size = 18, face="bold", color = "black"),
        axis.title.y = element_text(size = 18, face="bold", color = "black"),
        legend.title = element_text(size = 16, face="bold", color = "black"),
        legend.text = element_text(size = 14, face="bold", color = "black"))+
  coord_flip()

#export to svg
svglite("comp_gwas.svg", width = 10, height = 5)
comp_plot
dev.off()
###############



