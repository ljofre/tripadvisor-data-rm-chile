library(tidyverse)
library(ggplot2)
library(sf)
library(viridis)
library(stringr)

setwd("C:/Users/dfran/OneDrive/PUC/Doctorado en Estadística UC/1º Semestre/Métodos Exploratorios/Proyecto")

chile =  st_read("Bases/DivisionPoliticaAdministrativa2019/DivisionPoliticaAdministrativa2019.shp")

# Extracting polygon data
chile =  chile %>% filter(PROVINCIA == "Santiago")

chile = chile %>% mutate(centroid = map(geometry, st_centroid), coords = map(centroid, st_coordinates),
                                    coords_x = map_dbl(coords, 1), coords_y = map_dbl(coords, 2))

# Main communes
Communes = c("Las Condes", "Ñuñoa", "Providencia", "Recoleta", "Santiago", "Vitacura")
chile.communes = chile %>% 
  filter(COMUNA == "Santiago" | COMUNA == "Providencia" | COMUNA == "Las Condes" |
           COMUNA == "Recoleta" |  COMUNA == "Ñuñoa" | COMUNA == "Vitacura")

#################
#   FUNCTIONS   #
#################

# Classification of points by communes (regions)
pmap = function(x){
  pnts_sf = do.call("st_sfc",c(lapply(1:nrow(x), function(i) {st_point(as.numeric(x[i, ]))}), list("crs" = 4326))) 
  pnts_trans = st_transform(pnts_sf, 2163) 
  tt1_trans = st_transform(chile, 2163)      
  regiones = apply(st_intersects(tt1_trans, pnts_trans, sparse = FALSE), 2, 
                   function(col) { 
                     tt1_trans[which(col), ]$COMUNA
                   })
  return(regiones)
}

# Function for points within the main communes and the respective coordinates.
zone = function(x){
  n = which(colnames(x) == "lon" | colnames(x) == "lat")
  cords = x[, n]
  colnames(cords) = c("y", "x")
  cords = data.frame("x" = cords$x, "y" = cords$y)
  return(list(x,cords))
}

# Invalid chars
invalid_char = function(x){
  a = c(0)
  for (i in 1:length(x)) {
    if(rlang::is_empty(x[[i]])){
      a = cbind(a,i)
    }
  }
  a = a[-1] 
  return(a)
}

# Filter by communes
main_communes = function(x){
  p = x[which(x$comuna == "Santiago" | x$comuna == "Providencia" | x$comuna == "Las Condes" |
                  x$comuna == "Recoleta" | x$comuna == "Ñuñoa" | x$comuna == "Vitacura"), ]
}

# Operate data base
operar.data = function(data){
  datazone = zone(data)
  data = as.data.frame(datazone[1])
  cords.data = as.data.frame(datazone[2])
  data.communes = pmap(cords.data)
  remove = invalid_char(data.communes)
  data = data[-remove,]
  data$comuna = unlist(data.communes)
  data.main = main_communes(data)
  return(data.main)
}

# Location map
loca_map = function(data, r){
  note = expression(paste(bold("Communes:"), " \ \ 1 Santiago, \ \ 2 Ñuñoa, \ \ 3 Providencia, \ \ 4 Recoleta, \ \ 5 Vitacura, \ \ 6 Las Condes"))
  ggplot(data = chile.communes) +
    geom_sf(fill = "grey95") + geom_point(data = data, aes(x = lon, y = lat, colour = rating), alpha = 1.5, size = 1.25) +
    theme_minimal() + 
    labs(x = "Longitude", y = "Latitude",
         caption = note) + 
    theme(plot.title = element_text(hjust = 0.5, color = "black", size = 12, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5, face = "bold", size = 10),
          plot.caption = element_text(hjust = 0.5, size = 10),
          legend.position = "top", legend.title = element_text(size = 10, face = "bold"),
          legend.text = element_text(size = 10), 
          axis.text.x = element_text(size = 8), axis.text.y = element_text(size = 8), 
          legend.key = element_rect(fill = "grey95")) + 
    guides(colour = guide_legend(nrow = 1)) +
    scale_color_manual(name = "Rating", labels = decimals(min(r), max(r)),
                       values = inferno(length(r), begin = 0, end = 0.8)) +
    annotate("text", -70.67, -33.47, label = "1") +
    annotate("text", -70.62, -33.468, label = "2") +
    annotate("text", -70.591, -33.44, label = "3") +
    annotate("text", -70.649, -33.39, label = "4") +
    annotate("text", -70.45, -33.44, label = "6") +
    annotate("text", -70.55, -33.37, label = "5")
}

# Decimals
decimals = function(begin = 1, end = 5, increase = 0.5){
  sec = seq(begin, end, increase)
  vector = ifelse(begin%%ceiling(begin) == 0, paste(begin, ".0", sep = ""), as.character(begin))
  limit = (end - begin)/increase + 1
  for(i in 2:limit){
    aux = ifelse(sec[i]%%ceiling(sec[i]) == 0, paste(sec[i], ".0", sep = ""), as.character(sec[i]))
    vector = c(vector, aux)
  }
  return(vector)
}


#############################
#          HOTELS
#############################

hotel_data = read.csv("Bases/hoteles.csv")

hotel_data.main_communes = operar.data(hotel_data)
tabla.hotel_data = table(hotel_data.main_communes$comun) 
colnames(hotel_data.main_communes)[colnames(hotel_data.main_communes) == "starclass"] = "rating"

# Habitaciones
ggplot(hotel_data.main_communes, aes(y = nbrooms, x = comuna, fill = comuna)) + 
  geom_boxplot() + theme_minimal() + 
  labs(x = "Communes", y = "Number of rooms") + 
  theme(plot.title = element_text(hjust = 0.5, color = "black", size = 12, face = "bold"), 
        plot.subtitle = element_text(hjust = 0.5, face = "bold", size = 10),
        legend.position = "none" , legend.title = element_text(size = 10), legend.text = element_text(size = 10), 
        axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10)) +
  scale_fill_manual(values = viridis(6, begin = 0.2, end = 0.6))

# Mapa de hotel_data por comunas main_communes
loca_map(hotel_data.main_communes, seq(3,5,0.5))

# Rainting todas las comunas
table.rating.h = table(hotel_data$starclass)
total = sum(table.rating.h); Total = sum
table.rating.h = addmargins(table(hotel_data$starclass), FUN = Total, quiet = F)
table.rating.h = rbind(table.rating.h, paste(round(table.rating.h/total*100,2), "%", sep = ""))
table.rating.h

# Rainting en comunas main_communes
#rating.comuna(hotel_data.main_communes, "Hotels Rating by Main Communes")

#############################
#         RESTAURANTS
#############################

restaurant_data = read.csv("Bases/restaurant.csv")

# Raiting de los restaurant_dataes (modificación de carácteres)
restaurant_data$rating = matrix(unlist(strsplit(as.character(restaurant_data$restaurant_rating), "Â")), ncol = 2, byrow = T)[,1]
restaurant_data$rating = as.numeric(gsub(",", ".", restaurant_data$rating))
table.rating.r = table(restaurant_data$rating)
total = sum(table.rating.r); Total = sum
table.rating.r = addmargins(table.rating.r, FUN = Total, quiet = F)
table.rating.r = rbind(table.rating.r, paste(round(table.rating.r/total*100,2), "%", sep = ""))
table.rating.r

restaurant_data.main_communes = operar.data(restaurant_data)
restaurant_data.main_communes$rating = as.factor(restaurant_data.main_communes$rating)
tabla.restaurant_data = table(restaurant_data.main_communes$comuna)

# Mapa de restaurant_dataes en comunas main_communes
loca_map(restaurant_data.main_communes, as.numeric(seq(3,5,0.5)))

#############################
#         Activities
#############################

tourist_activity_data = read.csv("Bases/panoramas.csv")

table.rating.ta = table(tourist_activity_data$rating)
total = sum(table.rating.ta); Total = sum
table.rating.ta = addmargins(table.rating.ta, FUN = Total, quiet = F)
table.rating.ta = rbind(table.rating.ta, paste(round(table.rating.ta/total*100,2), "%", sep = ""))
table.rating.ta

# Tourists language 
leng = apply(tourist_activity_data[, c(10:18)],2, FUN = function(x){
  sum(na.omit(x))
})

Data = list()
for (i in 1:length(names(leng))) {
  Data[[i]] = rep(names(leng)[i],leng[i])
}
Data = data.frame("Language" = unlist(Data))
Data$Language = as.character(Data$Language)

Data[Data == "portugues"] = "Portuguese"
Data[Data == "ingles"] = "English"
Data[which(Data != "Portuguese" & Data != "English"),1] = "Other"

table.leng = as.data.frame(as.matrix(round(prop.table(table(Data)),4)))
table.leng = data.frame("Language" = row.names(table.leng), "Freq" = table.leng$V1)
table.leng

ggplot(table.leng, aes(x = "", y = Freq, fill = Language)) + 
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0) +
  geom_text(aes(x = c(1.4,1.4,1.4), y = c(0.93,0.829,0.4),label = paste0(round(Freq*100), "%")), size = 5) +
  scale_fill_manual(name = "Language", limits = c("Portuguese", "English", "Other"),
                    values = magma(3, begin = 0.6, end = 0.8)) +
  labs(x = NULL, y = NULL, fill = NULL) + theme_classic() + 
  theme(axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, color = "black", size = 14, face = "bold"),
        legend.title = element_text(size = 11, face = "bold"), legend.text = element_text(size = 11),
        legend.position = "top")

tourist_activity_data.main_communes = operar.data(tourist_activity_data)
tabla.tourist_activity_data = table(tourist_activity_data.main_communes$comuna) 
tabla.tourist_activity_data

# Activity map
aux = tourist_activity_data.main_communes[-which(is.na(tourist_activity_data.main_communes$rating)),]
loca_map(aux, as.numeric(seq(1.5,5,0.5)))

# Variable presence of language
tourist_activity_data.main_communes$ presence.leng = apply(tourist_activity_data.main_communes[9:18], 1, FUN = function(x){
  x[is.na(x)] = 0
  if(x[2] > 0 | x[3] > 0){
    if(x[2] > 0 & x[3] > 0) return("Portuguese & English")
    else{
        if(x[2] > 0) return("Portuguese")
      if(x[3] > 0) return("English")
    }
  } else {
    if(sum(x) > 0) return("Other")
    else return("Unregistered") 
  }
})

aux = tourist_activity_data.main_communes[which(tourist_activity_data.main_communes$ presence.leng == "Portuguese & English" |
                                  tourist_activity_data.main_communes$ presence.leng == "Portuguese" | 
                                  tourist_activity_data.main_communes$ presence.leng == "English"),]

# Language presence map
note = expression(paste(bold("Communes:"), " \ \ 1 Santiago, \ \ 2 Ñuñoa, \ \ 3 Providencia, \ \ 4 Recoleta, \ \ 5 Vitacura, \ \ 6 Las Condes"))
ggplot(data = chile.communes) + geom_sf(fill = "grey95") + 
  geom_point(data = aux, aes(x = lon, y = lat, color =  presence.leng), alpha = 1, size = 1.25) + 
  theme_minimal() + 
  labs(x = "Longitude", y = "Latitude", caption = note) + 
  theme(plot.title = element_text(hjust = 0.5, color = "black", size = 12, face = "bold"), 
        plot.subtitle = element_text(hjust = 0.5, face = "bold", size = 10),
        plot.caption = element_text(hjust = 0.5, size = 10),
        legend.position = "top", legend.title = element_text(size = 10, face = "bold"), legend.text = element_text(size = 10), 
        legend.key = element_rect(fill = "grey95"),
        axis.text.x = element_text(size = 8), axis.text.y = element_text(size = 8)) +
  scale_color_manual(name = "Tourist language", values = inferno(3, begin = 0, end = 0.8),
                     limits = c("Portuguese", "English", "Portuguese & English")) +
  annotate("text", -70.67, -33.47, label = "1") +
  annotate("text", -70.62, -33.468, label = "2") +
  annotate("text", -70.591, -33.44, label = "3") +
  annotate("text", -70.649, -33.39, label = "4") +
  annotate("text", -70.45, -33.44, label = "6") +
  annotate("text", -70.55, -33.37, label = "5")




