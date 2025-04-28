library(tidyverse)
library(svglite)

dist <- read.csv("hap_distance_heatmap_column.csv")
dist <- as_tibble(dist)
#reorder axis test
order <- c("1H2", "2H133", "3H74", "4H58", "5H24", "6H85", "7H73", "8H25",
            "9H46", "10H19", "11H105", "12H43", "13H80", "14H55", "15H100", 
            "16H61", "17H81", "18H102", "19H45", "20H52", "21H51", "22H122", 
            "23H142", "24H15", "25H44", "26H91", "27H30", "28H29", "29H87", 
            "30H49", "31H11", "32H62", "33H95", "34H60", "35H121", "36H39", 
            "37H90", "38H32", "39H99", "40H4", "41H56", "42H125", "43H131", 
            "44H128", "45H130", "46H112", "47H21", "48H77", "49H31", "50H129",
            "51H75", "52H89", "53H110", "54H63", "55H27", "56H83", "57H107", 
            "58H53", "59H92", "60H88", "61H98", "62H20", "63H67", "64H84", 
            "65H97", "66H115", "67H138", "68H79", "69H34", "70H140", "71H120",
            "72H50", "73H28", "74H111", "75H65", "76H108", "77H139", "78H59", 
            "79H117", "80H5", "81H10", "82H136", "83H134", "84H135", "85H26", 
            "86H6", "87H33", "88H7", "89H17", "90H141", "91H1", "92H35", 
            "93H143", "94H116", "95H16", "96H137", "97H37", "98H101", 
            "99H41", "100H66", "101H13", "102H124", "103H72", "104H106", 
            "105H54", "106H64", "107H71", "108H113", "109H38", "110H132", 
            "111H70", "112H96", "113H18", "114H47", "115H36", "116H76", 
            "117H118", "118H94", "119H3", "120H22", "121H82", "122H78", 
            "123H114", "124H42", "125H14", "126H93", "127H86", "128H127", 
            "129H48", "130H119", "131H40", "132H68", "133H69", "134H104", 
            "135H8", "136H103", "137H57", "138H126", "139H123", "140H9", 
            "141H109", "142H12", "143H23")
dist$species_1 <- factor(dist$species_1, levels = order)   
dist$species_2 <- factor(dist$species_2, levels = order)   

#create heatmap
hmap <- dist %>%
  ggplot(aes(fill = dist, x = species_1, y= species_2))+
  geom_tile()+
  scale_fill_distiller(name= "Distance",
                       palette = "Spectral", 
                       limits = c(min(dist$dist), max(dist$dist)))+
  theme_minimal()+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.text.x = element_text(size = 5, angle = 90),
    axis.text.y = element_text(size = 5)
     )

print(hmap)

#export to svg
svglite("heatmap_coi_distance_min.svg", width = 10, height = 5)
hmap
dev.off()
