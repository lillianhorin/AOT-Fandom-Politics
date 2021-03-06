# Jarek Azim
# Thesis Keyword Twitter Analysis
# March 22, 2022
# Assistance by Lillian Horin

# Import library
library(tidyverse)
library(udpipe)
library(wordcloud)


# Import data
data <- read_csv("data/twitter10_text_time_keywords.csv")

---

# Keyword Analysis
## Set up Japanese language model

jl <- udpipe_download_model(language = "japanese")
str(jl)
udmodel_japanese <- udpipe_load_model(file = jl$file_model)
annotated_data <- data.frame(udpipe_annotate(udmodel_japanese, data$text))
saveRDS(annotated_data, file = "annotated_jpn_keyword_analysis.rds")


annotated_data <- readRDS(file = "data/annotated_jpn_keyword_analysis.rds")

## General keyword analysis of AOT dataset for Word Cloud
cleaned_data <- subset(annotated_data,
                       upos == "NOUN" |
                         upos == "PROPN")

cleaned_data <- subset(cleaned_data, 
                         !grepl('@', token) & # remove usernames
                         token != "RT" & # remove retweet reference
                         token != "_" &
                         token != "お" &
                         token != "阪神" & # remove baseball team reference
                         token != "/" &
                         token != "×" & 
                         token != "コラボ" &
                         token != "https" &
                         token != "giants" &
                         token != "日") 

frequency <- txt_freq(cleaned_data$token)
wc <- wordcloud(words = frequency$key, freq = frequency$freq, max.words=200)
wc 


cleaned_data <- subset(cleaned_data, 
                       token != "進撃" & 
                         token != "巨人" & 
                         token != "SNK" & 
                         token != "shingeki" & 
                         token != "No" & 
                         token != "Kyojin" & 
                         token != "ShingekiNoKyojin" & 
                         token != "AOT" & 
                         token != "Attack" & 
                         token != "on" & 
                         token != "Titan" & 
                         token != "AttackOnTitan" ) # CHECK FOR CASE SENSITIVITY

frequency <- txt_freq(cleaned_data$token)
wc <- wordcloud(words = frequency$key, freq = frequency$freq, max.words=100)
wc
                         
                         
## Specific keyword analysis of AOT dataset for Frequency Bar Graph

## Isolate specific tweets based on keyword
specific_kw <- data %>%
  filter(str_detect(text, "ファン") | 
           str_detect(text, "軍隊") | 
           str_detect(text, "調査兵団") |
           str_detect(text, "民族主義") |
           str_detect(text, "虐殺") |
           str_detect(text, "韓国") |
           str_detect(text, "政治") |
           str_detect(text, "兵士") |
           str_detect(text, "ネオー社会自由主義") |
           str_detect(text, "自由") |
           str_detect(text, "虚無") |
           str_detect(text, "香港,") |
           str_detect(text, "国粋") |
           str_detect(text, "ユダヤ人") |
           str_detect(text, "台湾") |
           str_detect(text, "中国") |
           str_detect(text, "歴史"))

write_excel_csv(specific_kw, 'tweet10_keywords.csv')

## Create list of keywords to get counts for
keywords <- list("ファン","軍隊","調査兵団","民族主義","虐殺","韓国","政治",
                 "兵士","ネオー社会自由主義","自由","虚無","香港","国粋","ユダヤ人",
                 "台湾","中国","歴史")

## Get counts for those keywords
keyword_count <- data.frame()
colnames(keyword_count) <- c("Keyword", "Count")
  
for (keyword in keywords) {
  key_count <- length(grep(keyword, specific_kw$text))
  key_table <- data.frame(keyword, key_count)
  colnames(key_table) <- c("Keyword", "Count")
  keyword_count <- rbind(keyword_count, key_table)
}

## Transform the counts for bettery visualization

keyword_count_transformed <- keyword_count
keyword_count_transformed$Count <- sqrt(keyword_count_transformed$Count)
  
## Visualize the data
require(gridExtra)

kw_bar <- ggplot(data=keyword_count, aes(x=reorder(Keyword, -Count), y = Count)) +
  geom_bar(stat="identity", fill = "steelblue3") +
  xlab("Keyword") + ylab("Count")
kw_bar

tr_kw_bar <- ggplot(data=keyword_count_transformed, aes(fill=Keyword,x=reorder(Keyword, Count), y = Count)) +
  geom_bar(stat="identity", show.legend=FALSE) +
  geom_text(aes(label=keyword_count$Count, hjust=-0.3)) +
  xlab("Keyword") + ylab("sqrt(Count)") + 
  theme(axis.text.y=element_text(size=14),
        axis.title=element_text(size=14)) +
  coord_flip()
tr_kw_bar


grid.arrange(kw_bar, tr_kw_bar, nrow=2)
