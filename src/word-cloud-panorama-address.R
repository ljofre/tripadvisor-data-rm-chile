library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")


pan <- read.csv("./data/tourist_activity_data.csv", sep = ";")


text <- pan$type
docs <- Corpus(VectorSource(text))
inspect(docs)

toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")
docs <- tm_map(docs, toSpace, "\\.")
docs <- tm_map(docs, removeWords, "santiago")
docs <- tm_map(docs, removeWords, "Santiago")
docs <- tm_map(docs, removeWords, "avenida")
docs <- tm_map(docs, removeWords, "Avenida")
docs <- tm_map(docs, removeWords, "av")
docs <- tm_map(docs, removeWords, "Av")


dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

wordcloud(words = d$word,width=450,height=450, freq = d$freq, min.freq = 1,
           random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

library(dplyr)
library(ggmosaic)
library(ggplot2)
data <- pan %>% 
  filter(type %in% c('Church', 'Museum',"Theater", "Gallery", "Square")) 

data$type <- as.character(data$type) 
ggplot(data = data) + geom_mosaic(aes(x = product(type, commune), fill = type)) + theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, color = "black", size = 12, face = "bold"),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1.3, size = 8),
        legend.position = "top", legend.title=element_text(size=10, face="bold")) + 
  guides(fill = guide_legend(nrow = 1)) 
  


