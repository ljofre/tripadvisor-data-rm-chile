library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")


rest <- read.csv("./data/restaurant.csv")


filePath <- "http://www.sthda.com/sthda/RDoc/example-files/martin-luther-king-i-have-a-dream-speech.txt"

text <- rest$address

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

wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))


