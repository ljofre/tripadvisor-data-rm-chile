# Required sources
library(tidyverse)
library(ggplot2)
library(sf)
library(viridis)
library(stringr)

chile <- st_read("Division/Division.shp")

# Extracting polygon data
stgo <- chile %>% filter(PROVINCIA=="Santiago")

stgo <- stgo %>% mutate(centroid=map(geometry, st_centroid), 
                        coords=map(centroid, st_coordinates),
                        coords_x=map_dbl(coords, 1), 
                        coords_y=map_dbl(coords, 2))

# Main communes
communes <- stgo %>% filter(COMUNA=="Las Condes" | COMUNA=="Ñuñoa" | COMUNA=="Providencia" |
                              COMUNA=="Recoleta" | COMUNA=="Santiago" | COMUNA=="Vitacura")


# Santiago map
stgo_map <- function(data){
  comnames <- expression(paste(bold("Communes:"), " \ \ 1 Santiago, \ \ 2 Ñuñoa, \ \ 3 Providencia, \ \ 4 Recoleta, \ \ 5 Vitacura, \ \ 6 Las Condes"))
  ggplot(data=communes) +
    geom_sf(fill="grey95") + geom_point(data=data, aes(x=lon, y=lat, colour=as.factor(rating)), alpha=1.5, size=1.25) +
    theme_minimal() + 
    labs(x="Longitude", y="Latitude", caption=comnames) + 
    theme(plot.title=element_text(hjust=0.5, color="black", size=12, face="bold"),
          plot.subtitle=element_text(hjust=0.5, face="bold", size=10),
          plot.caption=element_text(hjust=0.5, size=10),
          legend.position="top", legend.title=element_text(size=10, face="bold"),
          legend.text=element_text(size=10), 
          axis.text.x=element_text(size=8), axis.text.y=element_text(size=8), 
          legend.key=element_rect(fill="grey95")) + 
    guides(colour=guide_legend(nrow=1)) +
    scale_color_manual(name="Rating", labels=decimals(min(data$rating),max(data$rating)),
          values=inferno(length(seq(min(data$rating),max(data$rating),0.5)), begin=0, end=0.8)) +
    annotate("text", -70.669, -33.471, label="1") +
    annotate("text", -70.620, -33.469, label="2") +
    annotate("text", -70.590, -33.438, label="3") +
    annotate("text", -70.650, -33.383, label="4") +
    annotate("text", -70.553, -33.367, label="5") +
    annotate("text", -70.455, -33.445, label="6")
    
}

# Decimals
decimals <- function(begin=1, end=5, increase=0.5){
  sec <- seq(begin, end, increase)
  vector <- ifelse(begin%%ceiling(begin)==0, paste0(begin,".0"), as.character(begin))
  limit <- (end - begin)/increase + 1
  for(i in 2:limit){
    aux <- ifelse(sec[i]%%ceiling(sec[i])==0, paste0(sec[i],".0"), as.character(sec[i]))
    vector <- c(vector, aux)
  }
  return(vector)
}
