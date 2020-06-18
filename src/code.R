# Loading Santiago map functions
source("stgo_map_functions.R")

#############################################################
#                        HOTEL DATA                         #
#############################################################
# Loading hotel data
hotel <- read.csv("hotel_data.csv", sep=";")
head(hotel)

# Number and percentage of hotels by rating
table(hotel$rating)
round(100*table(hotel$rating)/nrow(hotel),2)

# ADD MOSAIC PLOT

# Hotels of the main communes by rating
stgo_map(hotel)


#############################################################
#                     RESTAURANT DATA                       #
#############################################################
# Loading restaurant data
restaurant <- read.csv("restaurant_data.csv", sep=";")
head(restaurant)

# Number and percentage of restaurants by rating
table(restaurant$rating)
round(100*table(restaurant$rating)/nrow(restaurant),2)

# ADD MOSAIC PLOT

# Restaurants of the main communes by rating
stgo_map(restaurant)


#############################################################
#                 TOURIST ACTIVITY DATA                     #
#############################################################
tourist <- read.csv("tourist_activity_data.csv", sep=";")
head(tourist)

# Number and percentage of tourist activity by rating
table(tourist$rating)
round(100*table(tourist$rating)/nrow(tourist),2)

# ADD MOSAIC PLOT

# Tourist activity of the main communes by rating
stgo_map(tourist)

# ADD WORD CLOUD PLOT (tourist$type)

# ADD stgo_map() with the main activities according to the word cloud plot
