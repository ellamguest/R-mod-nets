library(tm)
library(SnowballC)
library(wordcloud)
library(magrittr)
library(RColorBrewer)

dir = '/Users/emg/Programming/GitHub/the_donald_project/raw_data/bigquery'
filenames = dir(dir, pattern =".csv")
cols <- rev(brewer.pal(6,"Dark2") # set col palette


png('td-monthly-word-clouds.png', width=210, height=297, units='mm', res=120)
par(mfrow=c(5,4))
for (filename in filenames[-1]){
  path = sprintf("/Users/emg/Programming/GitHub/the_donald_project/raw_data/bigquery/%s", filename)
  print(path)
  df <- read.csv(path, stringsAsFactors = FALSE)
  corpus <- Corpus(VectorSource(df$body))  # create a corpus from 'body' column
  corpus <- tm_map(corpus, PlainTextDocument) # make plain text
  corpus <- tm_map(corpus, removePunctuation) # clean data
  corpus <- tm_map(corpus, removeWords, stopwords('english'))
  corpus <- tm_map(corpus, removeWords, c('the', 'this', stopwords('english')))
  corpus <- tm_map(corpus, stemDocument)
  corpus <- Corpus(VectorSource(corpus)) # return to corpus

  wordcloud(corpus, max.words = 100, random.order = FALSE, color=cols) # plot
}
dev.off()

df <- read.csv('/Users/emg/Programming/GitHub/the_donald_project/raw_data/bigquery/td_comments_2017_03.csv', stringsAsFactors = FALSE)

corpus <- Corpus(VectorSource(df$body))  # create a corpus from 'body' column
corpus <- tm_map(corpus, PlainTextDocument) # make plain text
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, stemDocument)
corpus <- tm_map(corpus, removeWords, c('the', 'this', stopwords('english')))
corpus <- Corpus(VectorSource(corpus)) # return to corpus

cols <- rev(brewer.pal(6,"Dark2")) # set col palette

wordcloud(corpus, max.words = 100, random.order = FALSE, color=cols) # plot


#### document term matrix
dtm = DocumentTermMatrix(corpus)

inspect(dtm[1:2,1000:1005])
freq <- colSums(as.matrix(dtm))
ord <- order(freq,decreasing=TRUE)
freq[tail(ord)]
