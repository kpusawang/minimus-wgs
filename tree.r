## you need to install BiocManager before using it
install.packages("BiocManager")
library(BiocManager)
install("ggtree")
BiocManager::install("YuLab-SMU/treedataverse")

library(ggstance)
library(ggtree)
library(tidyverse)
library(cowplot)
library(ggplot2)
library(svglite)

## import data
treefile <- ("tree_newick_2")
tree <- read.tree(treefile)
df <- as_tibble(tree)
View(df)

info <- read.csv("haplotype.csv")
info <- as.data.frame(info)

info_2 <- read.csv("haplotype_count.csv")
info_3<- info_2 %>%
    pivot_longer(cow:LTO,
                 names_to = "method",
                 values_to = "count")

info_3 <- info_3 %>%
    select(haplotype, lineage, method, count) %>%
    filter(method != "NA")



## adjust the order
info$lineage <- factor(info$lineage, levels= c("A", "B", "N", "An. minimus", "An. harrisoni"), 
                       labels = c("A", "B", "H12, H23", "HQ877370, HQ877371, HQ877372, HQ877373",
                                  "HQ877375, HQ877376, HQ877377"))
info_3$method <- factor(info_3$method, levels = c("cow", "HLI", "HLO", "LTI", "LTO"), 
                        labels = c("Cow", "HLI", "HLO", "LTI", "LTO") )




## visualization


p <- ggtree(tree) 
p2 <- facet_plot(p %<+% info + 
                 geom_tippoint(aes(color=lineage), size = 2, alpha = 0.7),
                 panel = 'Collection Methods', 
                 data = info_3, geom = geom_barh,
                 mapping = aes(x= count, fill = as.factor(method)),
                 stat='identity')+ 
                 theme_tree2()+
                 theme(strip.background = element_blank(),
                       strip.text.x = element_blank())+
                 theme(plot.margin = unit(c(6,0,6,0), "pt"))+
                 theme(legend.background=element_rect(fill=NA),
                       axis.line = element_line(color = "black"),
                       axis.text.x = element_text(size = 14, face="bold",color = "black"),
                       legend.title=element_text(size= 16, face = "bold", color = "black"),
                       legend.text=element_text(size= 14, face = "bold", color = "black"))+
                       #legend.spacing.x = unit(0.5, "cm"),
                       #legend.key.size = unit(0.5,"cm"))+
                 scale_color_brewer(name= "Lineage",palette = "Set1")+
                 scale_fill_manual(name= "Method",
                                    values = c("#F8766D","#A3A500", "#00BF7D",
                                               "#00B0F6", "#E76BF3"))+
                 guides(color = guide_legend(order = 1),
                 fill = guide_legend(override.aes = 
                          list(colour = "black"), order = 2))
print(p2)

#export svg
svglite("lineage_gwas_2.svg", width = 10, height = 5)  
p2
dev.off()

info_4<- info_3 %>%
    select(lineage, method, count) %>%
               filter(lineage != "An. harrisoni") %>%
               filter(lineage != "An. minimus") %>%
               filter(lineage != "N") %>%
               group_by(lineage, method) %>%
               summarise(n=sum(count))

p3 <- ggplot(info_4, aes(x=method, y=n, fill = lineage, label = n))+
    geom_bar(stat='identity', position= "fill", color ="black", alpha = 0.7)+
    theme_minimal()+
    scale_x_discrete(name = "Method", expand = c(0, 0)) +
    scale_y_continuous(name = "%", expand = c(0, 0))+ 
    theme(
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(color = "black"),
        axis.text.x = element_text(size = 12, face="bold", color = "black" ),
        axis.text.y = element_text(size = 12, face="bold", color = "black"),
        axis.title.x = element_text(size = 14, face="bold", color = "black"),
        axis.title.y = element_text(size = 14, face="bold", color = "black"),
        legend.title=element_text(size= 14, face = "bold", color = "black"),
        legend.text=element_text(size= 12, face = "bold", color = "black"),
        legend.background=element_rect(fill=NA, color =NA))+
    #theme(plot.margin = unit(c(6,0,6,0), "pt"))+
    scale_fill_manual(name= "Lineage", values = c("#E41A1C", "#377EB8"))
    

print(p3)

#export svg
svglite("lineage_method.svg", width = 3.85, height = 3.25)  
p3
dev.off()

##create grid
prow <- plot_grid(p2+ theme(legend.position="none"), p3+ theme(legend.position="none"), labels=c("A", "B"), align = "h", axis = "bt", rel_widths=c(2, 1) )

legend_b <- get_legend(p2 + theme(legend.position="bottom"))## create common legend


q <- plot_grid( prow, legend_b, ncol = 1, rel_heights = c(1, .2))

print(q)


