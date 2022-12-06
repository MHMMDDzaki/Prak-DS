# import library vadersentiment
library(vader)
library(tidyverse)
library("readxl")

# ambil data dari file
dataClean = read_excel("/Users/macpro/Documents/work/prakDS/praktikum2020/output3.xls")
# ubah menjadi dataframe
df = data.frame(dataClean$cleaned_3)
colnames(df) <- c('tweet')
# copy dataframe untuk menghitung word yang sering muncul nanti
dfword = df
# dataframe sementara untuk menaruh hasil modeling dengan vader sentiment
df_temp = vader_df(df['tweet'])
# masukkan masing-masing value dari dataframe sementara ke dataframe tetap
df["positive"]<- df_temp['pos']
df["negative"] <- df_temp['neg']
df["neutral"] <- df_temp['neu']
df["compound"] = df_temp['compound']
# buat kolom kosong baru dengan nama label
df['label'] <- NA

# perulangan untuk memberi sentimen berdasarkan nilai compound
for(i in 1:nrow(df)){
 if (df[i,"compound"] >= 0.05){ 
   df[i,"label"] = "Positive"
 } else if (df[i,"compound"] < 0 ){
   df[i,"label"] = "Negative"
 } else {
   df[i,"label"] = "Netral"
 }
}

# membuat dataframe baru untuk counting labelnya
dfsentiment = data.frame(df["tweet"], df["label"])
sentimenCounter = count(dfsentiment, label)

# import library untuk text cleaning
library(dplyr)
library(tm)

# menghilangkan stopword
mycorpus <- Corpus(VectorSource(dfword))
mycorpus <- tm_map(mycorpus, removeWords, c(stopwords(),"ukrain","russian","russia","putin","war"))

# menghilangkan http
removw_url <- function(x) gsub("http[^[:space:]]*","",x)
mycorpus <- tm_map(mycorpus, content_transformer(removw_url))

# menghilangkan tanda baca
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*","",x)
mycorpus <- tm_map(mycorpus, content_transformer(removeNumPunct))
mycorpus <- tm_map(mycorpus, removePunctuation)
mycorpus <- tm_map(mycorpus, content_transformer(tolower))
mycorpus <- tm_map(mycorpus, stripWhitespace)
mycorpus <- tm_map(mycorpus, stemDocument)

# mengubah kata yang dibersihkan tadi untuk dibuat word yang sering muncul
tdm <- TermDocumentMatrix(mycorpus)
tdm <- removeSparseTerms(tdm, sparse = 0.98)
tdm <- as.matrix(tdm)
w = sort(rowSums(tdm), decreasing = T)

# import library untuk deploy aplikasi ke browser
library(wordcloud)
library(markdown)
library(DT)
library(shiny)

# untuk membuat UI shiny
ui <- fluidPage(
  titlePanel("Tweets Ukraine War"), #halaman judul
  mainPanel(
    tabsetPanel(type = "tabs",
                tabPanel("Data Twitter", DT::dataTableOutput('data')), 
                tabPanel("Sentiment Analysis", DT::dataTableOutput('sentiment')), 
                tabPanel("Classification", plotOutput('klasifikasi')), 
                tabPanel("Frequency Words", plotOutput('freqword')), 
                tabPanel("Wordcloud", plotOutput('wordcloud')) 
    )
  )
)

# untuk menjalankan server shiny
server <- function(input, output) {
  # untuk menampilkan data tweet
  output$data <- DT::renderDataTable({
    DT::datatable(df["tweet"], options = list(lengthChange = FALSE))
  })
  
  # untuk menampilkan data yang telah dianalisis sentimen 
  output$sentiment <- DT::renderDataTable({
     DT::datatable(dfsentiment, options = list(lengthChange = FALSE))
  })
  
  # untuk menunjukkan barplot banyaknya sentimen yang berbentuk positif, negatif, ataupun netral 
  output$klasifikasi <- renderPlot({
    barplot(sentimenCounter$n, 
          names.arg = c("Negative", "Neutral", "Positive"),
          main = "Sentiment",
          col= rainbow(3)) 
  })
  
  # untuk menunjukkan barplot tentang kata yang sering muncul  
  output$freqword<- renderPlot({
  barplot(w[c(3,6,9,15,26)],
          las=2,
          main = "Frequency of Words",
          col= rainbow(5))
  })
  
  # untuk menunjukkan wordcloud dari kata yang sering muncul tadi
  output$wordcloud<- renderPlot({
    wordcloud(mycorpus, min.freq = 3,
              max.words=100, random.order=FALSE, rot.per=0.40, 
              colors=brewer.pal(8, "Dark2"))
  })
}

# untuk menjalankan shiny
shinyApp(ui = ui, server = server)

