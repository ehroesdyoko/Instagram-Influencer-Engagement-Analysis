library(dplyr)
library(tidyr)
library(ggplot2)
library(caret)
library(readxl)
library(openxlsx)
library(cluster)
library(tidytext)
library(stringr)
library(textdata)
library(tidytext)
library(vader)
library(tm)
library(wordcloud2)
library(SnowballC)
library(stopwords)
library(corrplot)
library(tibble)
library(scales)
library(slam)
library(corrplot)

setwd("/Users/erlanggaroesdyoko/Documents/MADS/3rd Block/DMI/Assignment 2")
engagement <- read.xlsx("scrapping_dmi.xlsx")
colors <- read.xlsx("color_analysis_results.xlsx")
objects <- read.xlsx("detr_object_detection_results.xlsx")

# Check NA in caption
engagement$Caption.Words[is.na(engagement$Caption.Words)] <- 0
sum(is.na(engagement$Caption.Words))
sum(is.na(engagement$Likes))
engagement <- engagement[!is.na(engagement$Likes), ] # Remove NA
#engagement <- engagement[!is.na(engagement$Caption.Words), ] # Remove NA

# First, remove commas from Likes if necessary
engagement$Likes <- as.numeric(gsub(",", "", engagement$Likes))

engagement <- engagement %>%
  mutate(
    Likes = as.numeric(Likes),
    M_Likes = as.numeric(M_Likes),
    Comments = as.numeric(Comments),
    Word.count = as.numeric(Word.Count),
    Emoji.count = as.numeric(Emoji.Count)
  )

#Q1----
#engagement metrics
summary(engagement)
summary(engagement$Likes)
summary(engagement$Comments)
hist(engagement$Likes)
hist(engagement$Comments)

# Likes
# Calculate a 95th percentile threshold for Likes per Influencer
filtered_likes <- engagement %>%
  group_by(Influencer) %>%
  mutate(max_likes = quantile(Likes, 0.95, na.rm = TRUE)) %>%
  filter(Likes <= max_likes)

# Plot Likes histogram faceted by Influencer
ggplot(filtered_likes, aes(x = Likes)) +
  geom_histogram(bins = 30, fill = "#69b3a2", color = "white") +
  facet_wrap(~ Influencer, scales = "free_y") +
  theme_minimal() +
  labs(title = "Distribution of Likes per Influencer",
       x = "Likes", y = "Frequency") +
  scale_x_continuous(labels = label_number(scale = 1e-3, suffix = "K")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, face = "bold"))

# Comments
# Calculate a 95th percentile threshold per influencer
filtered_engagement <- engagement %>%
  group_by(Influencer) %>%
  mutate(max_comment = quantile(Comments, 0.95, na.rm = TRUE)) %>%
  filter(Comments <= max_comment)

# Plot
ggplot(filtered_engagement, aes(x = Comments)) +
  geom_histogram(bins = 30, fill = "#FFA07A", color = "white") +
  facet_wrap(~ Influencer, scales = "free_y") +
  theme_minimal() +
  labs(title = "Distribution of Comments per Influencer",
       x = "Comments", y = "Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, face = "bold"))


# Calculate a 95th percentile threshold for Likes per Influencer
filtered_likes <- engagement %>%
  group_by(Influencer) %>%
  mutate(max_likes = quantile(Likes, 0.95, na.rm = TRUE)) %>%
  filter(Likes <= max_likes)

# Plot Likes histogram faceted by Influencer
ggplot(filtered_likes, aes(x = Likes)) +
  geom_histogram(bins = 30, fill = "#69b3a2", color = "white") +
  facet_wrap(~ Influencer, scales = "free_y") +
  theme_minimal() +
  labs(title = "Distribution of Likes by Influencer (Excl. Outliers)",
       x = "Likes", y = "Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# metadata engagement summary
# Create a vector with manual values for Followers and Following
manual_followers <- c(enzoknol = 2000000, negin_mirsalehi = 7200000, juttaleerdam = 5000000, kalvijn = 1000000, jeremyfragrance = 3500000)  # Example values
manual_followings <- c(enzoknol = 157, negin_mirsalehi = 1008, juttaleerdam = 600, kalvijn = 676, jeremyfragrance = 0)  # Example values
engagement <- engagement %>%
  mutate(
    Followers = manual_followers[Influencer],
    Followings = manual_followings[Influencer]
  )

#likes/comments per follower
engagement$Likes_per_follower <- engagement$Likes/engagement$Followers
engagement$Comments_per_follower <- engagement$Comments/engagement$Followers

names(engagement)[names(engagement) == "Word Count"] <- "Word.Count"
names(engagement)[names(engagement) == "Emoji Count"] <- "Emoji.Count"

summary_stats <- engagement %>%
  group_by(Influencer) %>%
  summarise(
    mean_likes = mean(Likes, na.rm = TRUE),
    median_likes = median(Likes, na.rm = TRUE),
    mean_comments = mean(Comments, na.rm = TRUE),
    median_comments = median(Comments, na.rm = TRUE),
    mean_wordcount = mean(Word.Count, na.rm = TRUE),
    median_wordcount = median(Word.Count, na.rm = TRUE),
    mean_emojicount = mean(Emoji.Count, na.rm = TRUE),
    median_emojicount = median(Emoji.Count, na.rm = TRUE)
    #mean_LPF = mean(Likes_per_follower, na.rm = TRUE),
    #median_LPF = median(Likes_per_follower, na.rm = TRUE),
    #mean_CPF = mean(Comments_per_follower, na.rm = TRUE),
    #median_CPF = mean(Comments_per_follower, na.rm = TRUE)
  ) %>%
  arrange(desc(mean_likes))  # Optionally sort by mean likes

# add the following and followers
summary_stats <- summary_stats %>%
  mutate(
    followers = manual_followers[Influencer],  # Match influencer names
    followings = manual_followings[Influencer]
  )

print(summary_stats)

# Visualization
# Reshape for plotting
summary_long <- summary_stats %>%
  tibble::as_tibble() %>%
  dplyr::select(Influencer, mean_likes, mean_comments, mean_wordcount, mean_emojicount, followers, followings) %>%
  tidyr::pivot_longer(cols = -Influencer, names_to = "Metric", values_to = "Value")

# Plot
ggplot(summary_long, aes(x = reorder(Influencer, -Value), y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ Metric, scales = "free_y") +  # Separate panel per metric
  theme_minimal() +
  labs(title = "Influencer Comparison Across Key Metrics",
       x = "Influencer",
       y = "Value",
       fill = "Metric") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

#export for visualization in excel
# write.csv(summary_stats, "~/Downloads/summary_stats.csv", row.names = FALSE)


# Q2 ----
word_count_summary <- engagement %>%
  group_by(Influencer) %>%
  summarise(
    mean_wordcount = mean(Word.Count, na.rm = TRUE),
    median_wordcount = median(Word.Count, na.rm = TRUE),
    min_wordcount = min(Word.Count, na.rm = TRUE),
    max_wordcount = max(Word.Count, na.rm = TRUE)
  )
print(word_count_summary)

## Q4 - Emoji ----
emoji_count_summary <- engagement %>%
  group_by(Influencer) %>%
  summarise(
    mean_emojicount = mean(Emoji.Count, na.rm = TRUE),
    median_emojicount = median(Emoji.Count, na.rm = TRUE),
    min_emojicount = min(Emoji.Count, na.rm = TRUE),
    max_emojicount = max(Emoji.Count, na.rm = TRUE)
  )
print(emoji_count_summary)

# Create a box plot for Word_Count per Influencer
ggplot(engagement, aes(x = Influencer, y = Word.Count)) +
  geom_boxplot(fill = "lightblue", color = "black", outlier.colour = "red", outlier.shape = 16) +
  labs(title = "Distribution of Word Count per Influencer",
       x = "Influencer", y = "Word Count") +
  theme_minimal() + ylim(0,100)

# Create a box plot for Emoji_Count per Influencer
ggplot(engagement, aes(x = Influencer, y = Emoji.Count)) +
  geom_boxplot(fill = "lightblue", color = "black", outlier.colour = "red", outlier.shape = 16) +
  labs(title = "Distribution of Emoji Count per Influencer",
       x = "Influencer", y = "Emoji Count") +
  theme_minimal() + ylim(0,15)

#likes per follower
ggplot(engagement, aes(x = Influencer, y = Likes_per_follower)) +
  geom_boxplot(fill = "lightblue", color = "black", outlier.colour = "red", outlier.shape = 16) +
  labs(title = "Distribution of likes rate per Influencer",
       x = "Influencer", y = "Likes per follower") +
  theme_minimal()

#comments per follower
ggplot(engagement, aes(x = Influencer, y = Comments_per_follower)) +
  geom_boxplot(fill = "lightblue", color = "black", outlier.colour = "red", outlier.shape = 16) +
  labs(title = "Distribution of Comment rate per Influencer",
       x = "Influencer", y = "Comments per follower") +
  theme_minimal()

#Q3 ----
engagement$Caption[engagement$Caption == "nan"] <- NA

# Build corpus
engagement$Caption[engagement$Caption == "nan"] <- NA
influencer_data <- subset(engagement, Influencer == "negin_mirsalehi")
#corpus <- Corpus(VectorSource(influencer_data$Caption))
corpus <- Corpus(VectorSource(engagement$Caption))
inspect(corpus[1:5])

# Clean text
corpus <- tm_map(corpus, tolower)
inspect(corpus[1:5])

corpus <- tm_map(corpus, removePunctuation)
inspect(corpus[1:5])

corpus <- tm_map(corpus, removeNumbers)
inspect(corpus[1:5])

corpus_stemmed <- tm_map(corpus, content_transformer(stemDocument))
custom_stopwords <- c(",", "'",  "de", "niet","aan","doen","dan","die","maar","naar","diz","deze","als", "bij", "er","zijn", "zo","en", "ik", "het", "een", "met", "op", "van", "te", "je", "om", "voor", "dat", "dit", "wat", "ook", "al","1", "heb", "äôs","äç", "nog", "4" ,NA)
combined_stopwords <- c(stopwords("en"), stopwords("nl"),custom_stopwords)
cleanset <- tm_map(corpus_stemmed, removeWords, combined_stopwords)
inspect(cleanset[1:5])

removeURL <- function(x) gsub('http[[:alnum:]]*', '', x)
cleanset <- tm_map(cleanset, content_transformer(removeURL))
inspect(cleanset[1:5])

#cleanset <- tm_map(cleanset, gsub, 
#                   pattern = 'ambiance', 
#                   replacement = 'ambience')
#inspect(cleanset[1:5])

cleanset <- tm_map(cleanset, stripWhitespace)
inspect(cleanset[1:5])


# Tokenization: Create Term document matrix (Bag of words)
tdm <- TermDocumentMatrix(cleanset)
#tdm2 <- TermDocumentMatrix(doc$text) # this would use the non-cleaned dataset instead
tdm
tdm <- as.matrix(tdm)
tdm[1:10, 1:10]

word_freq <- rowSums(tdm)
word_freq_sorted <- sort(word_freq, decreasing = TRUE)
top_words <- head(word_freq_sorted, 10)
print(top_words)

# Bar plot
w <- rowSums(tdm)
w <- sort(w, decreasing = TRUE)
w <- subset(w, w > 10)

# Create barplot without x-axis labels
bp <- barplot(w,
              names.arg = rep("", length(w)),
              col = rainbow(20),
              ylim = c(0, max(w) + 5),
              main = "Most frequent words",
              cex.main = 1.6,   # Bigger title font
              cex.axis = 1.2)   # Bigger y-axis numbers

# Add 45-degree x-axis labels manually with bigger font
text(x = bp,
     y = par("usr")[3] - 1,
     labels = names(w),
     xpd = TRUE,
     srt = 45,
     adj = 1,
     cex = 1.2)  # Bigger x-axis label font

# Add frequency labels above bars with bigger font
text(x = bp,
     y = w,
     labels = w,
     pos = 3,
     cex = 1.2)  # Bigger number label font

# Create a wordcloud
w <- rowSums(tdm)
w <- data.frame(names(w), w)
colnames(w) <- c('word', 'freq')
wordcloud2(w[which(w$freq > 5),],
           size = 0.7,
           shape = 'circle',
           rotateRatio = 0.5,
           minSize = 1)

# Q4 - Lexicon - Sentiment Analysis ----
# Tokenize captions into words
# Remove Empty Captions
engagement <- engagement %>% filter(!is.na(Caption.Words) & Caption.Words != "")

# Tokenize Captions & Remove Stopwords
word_tokens <- engagement %>%
  unnest_tokens(word, Caption.Words) %>%
  anti_join(stop_words, by = "word")  # Remove common stopwords

# Apply AFINN Sentiment Scoring
#afinn_scores <- word_tokens %>%
  #inner_join(get_sentiments("afinn"), by = "word") %>%
  #group_by(Image.Name) %>%
  #summarise(afinn_sentiment = sum(value, na.rm = TRUE))  # Aggregate scores
 
# Apply Bing Lexicon (Positive/Negative Counts)
#bing_scores <- word_tokens %>%
  #inner_join(get_sentiments("bing"), by = "word") %>%
  #count(Image.Name, sentiment) %>%
  #spread(sentiment, n, fill = 0) %>%
  #rename(positive_words = positive, negative_words = negative)

# Apply NRC Lexicon (Emotion Analysis) - Handling Many-to-Many
word_sentiments <- get_sentiments("nrc") %>%
  count(word, sentiment) %>%
  group_by(word) %>%
  top_n(1, n)  # Select the most frequent sentiment for each word

nrc_scores <- word_tokens %>%
  inner_join(word_sentiments, by = "word", relationship = "many-to-many") %>%
  count(Image.Name, sentiment) %>%
  spread(sentiment, n, fill = 0)

# LIWC-Like Feature Extraction
# Word count = total number of tokens per document
dtm_data <- DocumentTermMatrix(corpus_stemmed)
word_count <- row_sums(dtm_data)

# Lexical diversity = number of unique words / total words
unique_terms <- apply(as.matrix(dtm_data), 1, function(row) sum(row > 0))
ttr <- unique_terms / word_count

liwc_features <- data.frame(
  Image.Name = engagement$Image.Name,
  word_count = word_count,
  lexical_diversity = ttr
)


# VADER Sentiment Analysis
# Run VADER sentiment analysis
vader_scores <- vader_df(engagement$Caption.Words)

# Add `Image.Name` from `engagement`
vader_scores <- engagement %>%
  dplyr::select(Image.Name) %>%
  bind_cols(vader_scores)

# Merge All Text-Based Features
textual_features <- engagement %>%
  select(Image.Name) %>%
  distinct() %>%
  #left_join(afinn_scores, by = "Image.Name") %>%
  #left_join(bing_scores, by = "Image.Name") %>%
  left_join(nrc_scores, by = "Image.Name")

LV_text_features <- engagement %>%
  left_join(vader_scores, by = "Image.Name") %>%
  left_join(liwc_features, by = "Image.Name") %>%
  left_join(nrc_scores, by = "Image.Name") #%>%
  #select(Image.Name, word_count, lexical_diversity, compound, pos, neu, neg)  

# Final Cleaned & Merged Dataset
#final_text_features <- engagement %>%
  #left_join(vader_scores, by = "Image.Name") %>%
  #left_join(afinn_scores, by = "Image.Name") %>%
  #left_join(bing_scores, by = "Image.Name") %>%
  #left_join(nrc_scores, by = "Image.Name") %>%
  #left_join(liwc_features, by = "Image.Name") %>%
  #select(Image.Name, word_count, lexical_diversity, compound, pos, neu, neg, 
         #afinn_sentiment, positive_words, negative_words, anger, anticipation, 
         #disgust, fear, joy, sadness, surprise, trust)

# Replace NA values with 0
LV_text_features[is.na(LV_text_features)] <- 0

# Check Final Data Structure
print(summary(LV_text_features))
print(head(LV_text_features))


# Corr
cor_matrix <- cor(LV_text_features[, c("compound", "lexical_diversity", "anger", 
                         "anticipation" , 'disgust', 'fear', 'joy'
                        , "negative", 'positive', 'sadness', 'surprise','trust')], use = "complete.obs")

# Display correlation heatmap
library(reshape2)

melted_cor <- melt(cor_matrix)

ggplot(melted_cor, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
  labs(title = "Correlation Heatmap of Sentiment Features") +
  theme_minimal()

## Average sentiment per influencer ----
# VADER
sentiment_features <- LV_text_features %>%
  tibble::rownames_to_column(var = "index")
engagement <- engagement %>%
  tibble::rownames_to_column(var = "index")

sentiment <- left_join(engagement, sentiment_features, by = "index")

VADER_sentiment <- sentiment %>%
  group_by(Influencer.x) %>%
  summarise(
    compound = mean(compound, na.rm = TRUE),
    positive = mean(pos, na.rm = TRUE),
    neutral = mean(neu, na.rm = TRUE),
    negative = mean(neg, na.rm = TRUE),
  )
VADER_sentiment

sentiment_long <- VADER_sentiment %>%
  pivot_longer(cols = -Influencer.x, names_to = "Topic", values_to = "Average")

sentiment_long$Topic <- factor(sentiment_long$Topic, 
                               levels = c("compound", "positive", "neutral", "negative"))

# Create the bar plot
ggplot(sentiment_long, aes(x = Influencer.x, y = Average, fill = Topic)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  scale_fill_manual(values = c(
    "compound" = "#6A9E6F",              # green
    "positive" = "#A8D5BA",              # light green
    "neutral" = "#F4B183",           # orange
    "negative" = "#E07A5F"   # red-orange
  )) +
  labs(title = "Average VADER Sentiment by Influencer",
       x = "Influencer",
       y = "Average Score",
       fill = "Sentiment") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

ggplot(sentiment_long, aes(x = Influencer.x, y = Average, fill = Topic)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  scale_fill_manual(values = c(
    "compound" = "#6A9E6F",      # green
    "positive" = "#A8D5BA",      # light green
    "neutral"  = "#F4B183",      # orange
    "negative" = "#E07A5F"       # red-orange
  )) +
  labs(
    title = "Average VADER Sentiment by Influencer",
    x = "Influencer",
    y = "Average Score",
    fill = "Sentiment"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    plot.title  = element_text(hjust = 0.5, face = "bold", size = 16),
    legend.title = element_text(size = 13),
    legend.text  = element_text(size = 11)
  )

# LIWC
sentiment <- left_join(engagement, sentiment_features, by = "index")

LIWC_sentiment <- sentiment %>%
  group_by(Influencer.x) %>%
  summarise(
    lexical_diversity = mean(compound, na.rm = TRUE),
  )
LIWC_sentiment

LIWC_sentiment_long <- LIWC_sentiment %>%
  pivot_longer(cols = -Influencer.x, names_to = "Topic", values_to = "Average")


# Create the bar plot
ggplot(LIWC_sentiment_long, aes(x = Influencer.x, y = Average, fill = Topic)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.4), width = 0.4) +  # slimmer bars
  geom_text(aes(label = round(Average, 2)), 
            vjust = -0.5, 
            size = 4,
            position = position_dodge(width = 0.4)) +  # match bar dodge width
  theme_minimal(base_size = 14) +
  scale_fill_manual(values = c(
    "lexical_diversity" = "#6A9E6F"
  )) +
  labs(title = "Average LIWC Sentiment by Influencer",
       x = "Influencer",
       y = "Average Score",
       fill = NULL) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "none"
  )

# NRC
NRC_sentiment <- sentiment %>%
  group_by(Influencer.x) %>%
  summarise(
    anger = mean(anger, na.rm = TRUE),
    anticipation = mean(anticipation, na.rm = TRUE),
    disgust = mean(disgust, na.rm = TRUE),
    fear = mean(fear, na.rm = TRUE),
    joy = mean(joy, na.rm = TRUE),
    negative = mean(negative, na.rm = TRUE),
    positive = mean(positive, na.rm = TRUE),
    sadness = mean(sadness, na.rm = TRUE),
    surprise = mean(surprise, na.rm = TRUE),
    trust = mean(trust, na.rm = TRUE)
  )
NRC_sentiment

NRC_sentiment_long <- NRC_sentiment %>%
  pivot_longer(cols = -Influencer.x, names_to = "Topic", values_to = "Average")

NRC_sentiment_long$Topic <- factor(NRC_sentiment_long$Topic, 
                                   levels = c("anger", "anticipation", "disgust", "fear", "joy",
                                              "negative", "positive", "sadness", "surprise", "trust"))

# Define colors for each NRC emotion
nrc_colors <- c(
  "anger" = "#D7263D",         # red
  "anticipation" = "#FFB400", # amber
  "disgust" = "#A05195",      # purple
  "fear" = "#58508D",         # deep blue
  "joy" = "#2CA02C",          # green
  "negative" = "#E07A5F",     # reddish orange
  "positive" = "#A8D5BA",     # light green
  "sadness" = "#1F77B4",      # medium blue
  "surprise" = "#FF7F0E",     # orange
  "trust" = "#17BECF"         # teal
)

# Create the NRC emotion bar plot
ggplot(NRC_sentiment_long, aes(x = Influencer.x, y = Average, fill = Topic)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  scale_fill_manual(values = nrc_colors) +
  labs(title = "Average NRC Emotion by Influencer",
       x = "Influencer",
       y = "Average Score",
       fill = "Emotion") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

# NRC: Only use top 4: positive, joy, anticipation, trust
# Filter to include only the selected positive emotions
positive_emotions <- c("positive", "joy", "anticipation", "trust")
nrc_filtered <- NRC_sentiment_long %>%
  filter(Topic %in% positive_emotions)

# Define custom colors for those emotions
nrc_colors_filtered <- c(
  "positive" = "#A8D5BA",     # light green
  "joy" = "#2CA02C",          # green
  "anticipation" = "#FFB400", # amber
  "trust" = "#17BECF"         # teal
)

# Plot
ggplot(nrc_filtered, aes(x = Influencer.x, y = Average, fill = Topic)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  scale_fill_manual(values = nrc_colors_filtered) +
  labs(title = "Average Positive NRC Emotions by Influencer",
       x = "Influencer",
       y = "Average Score",
       fill = "Emotion") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

ggplot(nrc_filtered, aes(x = Influencer.x, y = Average, fill = Topic)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  scale_fill_manual(values = nrc_colors_filtered) +
  labs(
    title = "Average Positive NRC Emotions by Influencer",
    x = "Influencer",
    y = "Average Score",
    fill = "Emotion"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 11)
  )

# Q5 ---- 
# Mentions
engagement$mention_count <- str_count(engagement$Caption, "@\\w+")
engagement$mentions <- str_extract_all(engagement$Caption, "@\\w+")

# Hashtags
engagement$hashtag_count <- str_count(engagement$Caption, "#\\w+")
engagement$hashtags <- str_extract_all(engagement$Caption, "#\\w+")

# Exclamation Point
engagement$exclamation_count <- str_count(engagement$Caption, fixed("!"))

# Question Mark
engagement$qm_count <- str_count(engagement$Caption, fixed("?"))

# Number of Sentence based on (".","!","?")
engagement$selected_punct_count <- str_count(engagement$Caption, "[.!?]")

hist(engagement$hashtag_count, main = "Hashtag Count Distribution", xlab = "# of Hashtags")
barplot(table(engagement$exclamation_count), main = "Exclamation Count", xlab = "!", ylab = "Frequency")

# plot
# Prepare long-form data
engagement_long <- engagement %>%
  select(mention_count, hashtag_count, exclamation_count, qm_count) %>%
  pivot_longer(cols = everything(), names_to = "Feature", values_to = "Count") %>%
  filter(!is.na(Count))

# Rename and reorder facet labels
engagement_long$Feature <- recode(engagement_long$Feature,
                                  "exclamation_count" = "Exclamation Point",
                                  "qm_count" = "Question Mark",
                                  "mention_count" = "Mention Count",
                                  "hashtag_count" = "Hashtag Count"
)

# Set desired facet order
engagement_long$Feature <- factor(engagement_long$Feature,
                                  levels = c("Exclamation Point", "Question Mark", "Mention Count", "Hashtag Count")
)

# Facetted plot
ggplot(engagement_long, aes(x = as.factor(Count))) +
  geom_bar(fill = "orange", color = "black") +
  facet_wrap(~ Feature, scales = "free_y") +
  labs(
    title = "Distributions of Caption Features",
    x = "Count",
    y = "Frequency"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5),
    strip.text = element_text(face = "bold")
  )

# emoji plot
# Filter out NA values from emoji count
emoji_data <- engagement %>%
  filter(!is.na(Emoji.Count))

# Create barplot
ggplot(emoji_data, aes(x = as.factor(Emoji.Count))) +
  geom_bar(fill = "orange", color = "black") +
  labs(
    title = "Distribution of Emoji Count in Captions",
    x = "Emoji Count",
    y = "Frequency"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
  )

# Q6 ----
#read data
data <- data.frame(text = engagement[["Caption"]])

data$text <- gsub("na", "", data$text) 
data$text <- gsub("na_na", "", data$text) 
data$text <- gsub("wow", "", data$text) 
data$text <- gsub("NA_NA", "", data$text) 
data$text <- gsub("NA", "", data$text) 
data$text <- gsub("dont", "", data$text) 
corpus <- Corpus(VectorSource(data$text))
corpus <- tm_map(corpus, removeWords, combined_stopwords)
data$text <- sapply(corpus, as.character)
head(data$text)

# Ensure textstem package is loaded
library(textstem)

# Create unique IDs for each document
data$ID <- seq.int(nrow(data))

# Tokenize the text into words
text_cleaning_tokens <- data %>% 
  tidytext::unnest_tokens(word, text)

# Remove digits and punctuation
text_cleaning_tokens$word <- gsub('[[:digit:]]+', '', text_cleaning_tokens$word)
text_cleaning_tokens$word <- gsub('[[:punct:]]+', '', text_cleaning_tokens$word)

# Remove empty words
tokens <- text_cleaning_tokens %>% filter(word != "")

# Lemmatize words
tokens$word <- lemmatize_words(tokens$word)
# Spread the words back into rows for each document
tokens <- tokens %>% 
  group_by(ID) %>% 
  mutate(ind = row_number()) %>%
  tidyr::spread(key = ind, value = word)

# Replace any NA values with an empty string
tokens[is.na(tokens)] <- ""

# Unite tokens back into a single text column per document
tokens <- tidyr::unite(tokens, text, -ID, sep = " ")
tokens$text <- trimws(tokens$text)



data$ID <- seq.int(nrow(data))

text_cleaning_tokens <- data %>%  tidytext::unnest_tokens(word, text)
text_cleaning_tokens$word <- gsub('[[:digit:]]+', '', text_cleaning_tokens$word)
text_cleaning_tokens$word <- gsub('[[:punct:]]+', '', text_cleaning_tokens$word)
tokens <- text_cleaning_tokens %>% filter(!(word==""))
tokens <- tokens %>% mutate(ind = row_number())
tokens <- tokens %>% group_by(ID) %>% mutate(ind = row_number()) %>%
  tidyr::spread(key = ind, value = word)
tokens [is.na(tokens)] <- ""
tokens <- tidyr::unite(tokens, text,-ID,sep =" " )
tokens$text <- trimws(tokens$text)

library(textmineR)

#create a document term matrix
dtm <- CreateDtm(doc_vec = tokens$text, # character vector of documents
                 doc_names = tokens$ID, # document names
                 ngram_window = c(1, 1), # minimum and maximum n-gram length
                 stopword_vec = c(stopwords::stopwords("en"), # stopwords from tm
                                  stopwords::stopwords(source = "smart")), # this is the default value
                 lower = TRUE, # lowercase - this is the default value
                 remove_punctuation = TRUE, # punctuation - this is the default
                 remove_numbers = TRUE, # numbers - this is the default
                 verbose = FALSE, # Turn off status bar for this demo
) 

#keep row number with >0 for later
rowsum = rowSums(dtm)
ID = data$ID
keep_row_id = as.data.frame(cbind(rowsum, ID))
keep_row_id = keep_row_id[rowsum>0,]

dtm <- dtm[,colSums(dtm) > 0]
dtm <- dtm[rowSums(dtm) > 0,]

#explore the basic frequency 
tf <- TermDocFreq(dtm = dtm)
original_tf <- tf %>% select(term, term_freq,doc_freq)
rownames(original_tf) <- 1:nrow(original_tf)
# Eliminate words appearing less than 2 times or in more than half of the
# documents
vocabulary <- tf$term[ tf$term_freq > 2 & tf$doc_freq < nrow(dtm) / 2 ]
dtm = dtm


#determine number of topics
library(ldatuning)
result1 <- FindTopicsNumber(
  dtm,
  topics = seq(from = 2, to = 8, by = 1),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 77),
  mc.cores = 2L,
  verbose = TRUE
)
FindTopicsNumber_plot(result1) # 4/5 topics

#run lda with 4 classes
# see extensions for more info

set.seed(12345)

model <- FitLdaModel(dtm = dtm, 
                     k = 4,   #number of topics
                     iterations = 500, # I usually recommend at least 500 iterations
                     burnin = 180,
                     alpha = 0.10,  #Original=0.10 High alpha => each doc has more topics
                     beta = 0.05,   #Original=0.05 High beta => topic contains many words
                     smooth = TRUE,
                     optimize_alpha = TRUE,
                     calc_likelihood = TRUE,
                     calc_coherence = TRUE,
                     calc_r2 = TRUE,
                     cpus = 2) 

model$r2
plot(model$log_likelihood) #, type = "l")


# Get the top terms of each topic
model$top_terms <- GetTopTerms(phi = model$phi, M = 30)
head(t(model$top_terms))
model[["top_terms"]]

#get topic probabilities per unit
theta_num <- model$theta   # these are the "scores" for each row on each topic.
topic_probabilities <- as.data.frame(theta_num)
colnames(topic_probabilities) <- c("lifestyle_probability", "dailyroutine_probability", "sentimental_religious_probability", "emotional_probability")

#topic probabilities per influencer
topic_probabilities <- topic_probabilities %>%
  tibble::rownames_to_column(var = "no")
engagement <- engagement %>%
  tibble::rownames_to_column(var = "no")

topic <- left_join(engagement, topic_probabilities, by = "no")
topic_probabilities <- topic %>%
  mutate(
    lifestyle_probability = ifelse(is.na(lifestyle_probability), 0.25, lifestyle_probability),
    dailyroutine_probability = ifelse(is.na(dailyroutine_probability), 0.25, dailyroutine_probability),
    sentimental_religious_probability = ifelse(is.na(sentimental_religious_probability), 0.25, sentimental_religious_probability),
    emotional_probability = ifelse(is.na(emotional_probability), 0.25, emotional_probability)
  )
topic_probabilities <- topic_probabilities[,c("lifestyle_probability","emotional_probability", "dailyroutine_probability", "sentimental_religious_probability",  "index", "Influencer", "Image.Name")]
colnames(topic_probabilities)
average_probabilities <- topic_probabilities %>%
  group_by(Influencer) %>%
  summarise(
    lifestyle_avg = mean(lifestyle_probability, na.rm = TRUE),
    dailyroutine_avg = mean(dailyroutine_probability, na.rm = TRUE),
    sentimental_religious_avg = mean(sentimental_religious_probability, na.rm = TRUE),
    emotional_avg = mean(emotional_probability, na.rm = TRUE)
  )
average_probabilities

average_probabilities_long <- average_probabilities %>%
  pivot_longer(cols = -Influencer, names_to = "Topic", values_to = "Probability")
average_probabilities_long$Topic <- factor(average_probabilities_long$Topic, 
                                           levels = c("lifestyle_avg", "emotional_avg", "dailyroutine_avg", "sentimental_religious_avg"))

ggplot(average_probabilities_long, aes(x = Influencer, y = Probability, fill = Topic)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  scale_fill_manual(values = c("#6A9E6F", "#A8D5BA", "#F4B183", "#E07A5F")) +  # Green → Yellow → Orange → Red
  labs(title = "Average Topic Probabilities by Influencer",
       x = "Influencer",
       y = "Average Probability",
       fill = "Topic") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate labels for readability

# topic probabilities per post
# Calculate average topic probabilities across ALL posts (all influencers)
overall_topic_probabilities <- topic_probabilities %>%
  summarise(
    lifestyle = mean(lifestyle_probability, na.rm = TRUE),
    emotional = mean(emotional_probability, na.rm = TRUE),
    dailyroutine = mean(dailyroutine_probability, na.rm = TRUE),
    sentimental_religious = mean(sentimental_religious_probability, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = everything(), names_to = "Topic", values_to = "Average_Probability")

# View
print(overall_topic_probabilities)

# plot
ggplot(overall_topic_probabilities, aes(x = Topic, y = Average_Probability, fill = Topic)) +
  geom_col() +
  theme_minimal() +
  labs(title = "Overall Average Topic Probabilities Across All Influencers",
       x = "Topic", y = "Average Probability") +
  theme(plot.title = element_text(hjust = 0.5))

#write.csv(topic_probabilities, "~/Downloads/topic_probabilities.csv", row.names = FALSE)
#topics <- read.csv("/Users/casbuitenhuis/Downloads/topic_probabilities.csv")

# Q8 ----
# Combine all the information into one dataframe
names(engagement)[names(engagement) == "Image Name"] <- "Image.Name"
names(colors)[names(colors) == "File Name"] <- "File.Name"
names(objects)[names(objects) == "File Name"] <- "File.Name"

merged_data <- engagement %>%
  left_join(colors, by = c("Image.Name" = "File.Name"))

merged_data <- merged_data %>%
  left_join(objects, by = c("Image.Name" = "File.Name"))

merged_data <- merged_data %>%
  left_join(LV_text_features, by = c("Image.Name"))

merged_data <- merged_data %>%
  left_join(topic_probabilities, by = c("Image.Name"))

# Extract the Hour from Date Time
# Load necessary package
library(lubridate)

# Convert Date.Time column to a proper datetime format
merged_data$Date.Time <- dmy_hms(merged_data$Date.Time.x)

# Extract the hour and store it in a new column
merged_data$Hour <- hour(merged_data$Date.Time.x)

# View the first few rows to confirm
unique(merged_data$Hour)

## EDA - Picture Analysis ----
# Scatter plot: Brightness vs. Likes
ggplot(merged_data, aes(x = Brightness.Contrast, y = Likes.x)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", col = "red") +
  labs(title = "Brightness.Contrast vs. Likes",
       x = "Brightness.Contrast",
       y = "Likes") +
  coord_cartesian(ylim = c(0, 500000)) +  # Set y-axis max to 500K
  theme_minimal()

# Scatter plot: Saturation vs. Likes
ggplot(merged_data, aes(x = Saturation, y = Likes.x)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", col = "blue") +
  labs(title = "Saturation vs. Likes",
       x = "Saturation",
       y = "Likes") +
  coord_cartesian(ylim = c(0, 500000)) +
  theme_minimal()

# Scatter plot: Warmth vs. Likes
ggplot(merged_data, aes(x = Warmth, y = Likes.x)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", col = "blue") +
  labs(title = "Warmth vs. Likes",
       x = "Warmth",
       y = "Likes") +
  coord_cartesian(ylim = c(0, 500000)) +
  theme_minimal()

# Scatter plot: Nimber of Detected Objects vs. Likes
ggplot(merged_data, aes(x = Total.Detected.Objects, y = Likes.x)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", color = "darkgreen") +
  labs(title = "Trend of Likes by Number of Detected Objects",
       x = "Total Detected Objects",
       y = "Likes") +
  coord_cartesian(ylim = c(0, 500000)) +  # optional: limit y-axis
  theme_minimal()

# Box plot: Number of Detected Objects vs. Likes
ggplot(merged_data, aes(x = as.factor(`Total.Detected.Objects`), y = Likes.x)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Engagement Distribution by Object Count", x = "Total Objects", y = "Likes") +
  theme_minimal()

### Second passed
# Convert Likes and Comments to numeric (if needed)
merged_data$Likes.x <- as.numeric(gsub(",", "", merged_data$Likes.x))  # Remove commas if present
merged_data$Comments <- as.numeric(merged_data$Comments.x)
names(merged_data)[names(merged_data) == "Seconds Passed"] <- "Seconds.Passed"

# Plot Likes vs. Seconds Passed with LOESS Smoothing
ggplot(merged_data, aes(x = Seconds.Passed.x, y = Likes.x)) +
  geom_point(alpha = 0.5) +  # Scatter plot
  geom_smooth(method = "loess", col = "red", se = TRUE) +  # LOESS smoothing
  labs(title = "Trend of Likes Over Time",
       x = "Seconds Passed Since Post",
       y = "Likes") +
  coord_cartesian(ylim = c(0, 500000)) +  # Limit y-axis to 500,000
  theme_minimal()

# Filter dataset to exclude extreme values (Likes > 350,000)
filtered_data <- merged_data %>% filter(Likes.x <= 350000)

# Plot Likes vs. Seconds Passed for multiple influencers (filtered data)
ggplot(filtered_data, aes(x = Seconds.Passed.x, y = Likes.x, color = Influencer.x)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", se = FALSE) +
  labs(title = "Trend of Likes Over Time by Influencer",
       x = "Seconds Passed Since Post", y = "Likes") +
  theme_minimal()

# Plot Comments vs. Seconds Passed for multiple influencers (filtered data)
ggplot(merged_data, aes(x = Seconds.Passed.x, y = Comments.x, color = Influencer.x)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", se = FALSE) +
  labs(title = "Trend of Comments Over Time by Influencer",
       x = "Seconds Passed Since Post", y = "Comments") +
  coord_cartesian(ylim = c(0, 7500)) +
  theme_minimal()


# Plot Comments vs. Seconds Passed with LOESS Smoothing
ggplot(merged_data, aes(x = Seconds.Passed.x, y = Comments.x)) +
  geom_point(alpha = 0.5) +  # Scatter plot
  geom_smooth(method = "loess", col = "blue", se = TRUE) +  # LOESS smoothing
  labs(title = "Trend of Comments Over Time",
       x = "Seconds Passed Since Post",
       y = "Comments") +
  coord_cartesian(ylim = c(0, 10000)) +  # Limit y-axis to 10,000
  theme_minimal()

#### Days Passed
# Convert Seconds Passed into Days
merged_data$Days.Passed <- merged_data$Seconds.Passed.x / 86400

# View the first few rows to confirm the new column
head(merged_data$Days.Passed)

# Plot Likes vs. Seconds Passed with LOESS Smoothing
ggplot(merged_data, aes(x = Days.Passed, y = Likes.x)) +
  geom_point(alpha = 0.5) +  # Scatter plot
  geom_smooth(method = "loess", col = "red", se = TRUE) +  # LOESS smoothing
  labs(title = "Trend of Likes Over Time", 
       x = "Days Passed Since Post", 
       y = "Likes") +
  coord_cartesian(ylim = c(0, 500000)) +  # Set max y-axis
  theme_minimal()

# Filter dataset to exclude extreme values (Likes > 350,000)
filtered_data <- merged_data %>% filter(Likes.x <= 350000)

# Plot Likes vs. Days Passed for multiple influencers (filtered data)
ggplot(filtered_data, aes(x = Days.Passed, y = Likes.x, color = Influencer.x)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", se = FALSE) +
  labs(title = "Trend of Likes Over Time by Influencer",
       x = "Days Passed Since Post", y = "Likes") +
  theme_minimal()

## Correlation Analysis - Picture Analysis ----

# Select only numeric columns
numeric_data <- merged_data %>%
  select(Likes.x, Comments.x, Brightness, Saturation, Brightness.Contrast, Warmth, Total.Detected.Objects)

# Compute correlation matrix
cor_matrix <- cor(numeric_data, use = "complete.obs")

# Plot correlation heatmap
corrplot(cor_matrix,
         method = "color",
         type = "upper",
         col = colorRampPalette(c("#67001F", "#F7F7F7", "#053061"))(200),  # Custom diverging palette
         tl.col = "black",         # Change axis labels to black
         tl.cex = 0.9,             # Slightly larger text
         number.cex = 0.8,         # Correlation coefficient font size
         addCoef.col = "black",   # Coefficients in black
         diag = FALSE             # Remove the diagonal
)

# Q9 - Regression Model ----
#add posting time variable
merged_data$Posting.Time <- as.POSIXct(merged_data$Date.Time, format = "%d/%m/%Y %H:%M:%S")
merged_data$Posting.Time <- format(merged_data$Posting.Time, "%H")
merged_data$Posting.Time <- as.numeric(merged_data$Posting.Time)

#### Likes ----
# Remove duplicated columns in merged_data
merged_data_clean <- merged_data[, !duplicated(as.list(merged_data))]

# Dummy for Influencers
merged_data_clean$Influencer.x <- as.factor(merged_data_clean$Influencer.x)

# Ensure Followers is numeric
merged_data_clean$Followers <- as.numeric(merged_data_clean$Followers.x)
merged_data_clean$Brightness.Contrast <- as.numeric(merged_data_clean$Brightness.Contrast)

# Create Engagement Rate variable
merged_data_clean$Likes_Rate <- merged_data_clean$Likes.x / merged_data_clean$Followers.x

# Remove all list-type columns
merged_data_clean <- merged_data_clean %>%
  select(where(~ !is.list(.)))

sapply(merged_data_clean, function(x) is.factor(x) && length(unique(x)) < 2)
sapply(merged_data_clean, is.numeric)

merged_data_clean <- merged_data_clean %>%
  select(where(~ is.numeric(.) || (is.factor(.) && length(unique(.)) > 1)))

# Drop rows with NA values only for the variables involved in the full model
merged_data_clean <- na.omit(merged_data_clean)

# Run the full model again
full_model_likes <- lm(Likes_Rate ~ .  - Likes.x - Likes_per_follower.x
                       - Comments_per_follower.x  - Likes_Rate - M_Likes.x,
                       data = merged_data_clean)

# Then do stepwise
stepwise_model_likes <- step(full_model_likes, direction = "both", trace = TRUE)
# Print regression results
summary(full_model_likes)
summary(stepwise_model_likes)
formula(stepwise_model_likes)

# Quadratic
quadratic_model_likes <- lm(Likes_Rate ~ 
                              Influencer.x +
                              Number.of.Comments.x + I(Number.of.Comments.x^2) +
                              Word.Count.x + I(Word.Count.x^2) +
                              Emoji.Count.x + I(Emoji.Count.x^2) +
                              hashtag_count + I(hashtag_count^2) +
                              qm_count + I(qm_count^2) +
                              Saturation + I(Saturation^2) +
                              Warmth + I(Warmth^2) +
                              Number.of.Unique.Objects + I(Number.of.Unique.Objects^2) +
                              compound + I(compound^2) +
                              but_count + I(but_count^2) +
                              word_count + I(word_count^2) +
                              anticipation + I(anticipation^2) +
                              fear + I(fear^2) +
                              positive + I(positive^2) +
                              emotional_probability + I(emotional_probability^2),
                            data = merged_data_clean)


summary(quadratic_model_likes)

# Interaction
interaction_model_likes <- lm(Likes_Rate ~ 
                                Influencer.x +
                                Number.of.Comments.x +
                                Word.Count.x +
                                Emoji.Count.x +
                                hashtag_count +
                                qm_count +
                                Warmth +
                                compound +
                                word_count +
                                anticipation +
                                positive +
                                emotional_probability +
                                
                                # Interaction terms
                                Influencer.x * compound +
                                Influencer.x * anticipation +
                                compound * Warmth +
                                hashtag_count * qm_count +
                                Emoji.Count.x * positive +
                                emotional_probability * anticipation,
                              
                              data = merged_data_clean)


summary(interaction_model_likes)

# Model Comparison - Likes
# Create a list of all models
models_likes <- list(
  Full = full_model_likes,
  Stepwise = stepwise_model_likes,
  Quadratic = quadratic_model_likes,
  Interaction = interaction_model_likes
)

# Extract metrics for each model
model_comparison_likes <- lapply(models_likes, function(model) {
  list(
    R2 = summary(model)$r.squared,
    Adjusted_R2 = summary(model)$adj.r.squared,
    AIC = AIC(model),
    BIC = BIC(model)
  )
})

# Convert the list to a tidy data frame
comparison_likes <- do.call(rbind, lapply(names(model_comparison_likes), function(name) {
  cbind(Model = name, as.data.frame(model_comparison_likes[[name]]))
}))

# Show as a data frame
comparison_likes <- as.data.frame(comparison_likes)

# Print comparison table
print(comparison_likes) # Quadratic is the best as it has the lowest value AIC/BIC

# Visualize Quadratic
# number of comment
ggplot(merged_data_clean, aes(x = log(Number.of.Comments.x), y = Likes_Rate)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), color = "blue", se = TRUE) +
  labs(
    title = "Quadratic Relationship: Number of Comments vs Likes Rate",
    x = "Number of Comments",
    y = "Likes Rate"
  ) +
  theme_minimal()

ggplot(merged_data_clean, aes(x = Number.of.Comments.x, y = Likes_Rate)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "loess", se = TRUE, color = "darkgreen") +
  coord_cartesian(xlim = c(0, 1000)) +
  labs(title = "LOESS Smoothing: Number of Comments vs Likes Rate") +
  theme_minimal()

# positive
ggplot(merged_data_clean, aes(x = positive, y = Likes_Rate)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), color = "blue") +
  labs(title = "Quadratic Relationship: Positive Sentiment vs Likes Rate",
       x = "Positive Sentiment Score",
       y = "Likes Rate") +
  theme_minimal()


### Comments ----
# Ensure Followers is numeric
merged_data_clean$Followers <- as.numeric(merged_data_clean$Followers)

# Create Engagement Rate variable
merged_data_clean$Comments_Rate <- merged_data_clean$Comments.x / merged_data_clean$Followers.x

# Run the full model again
full_model_comments <- lm(Comments_Rate ~ . - Comments.x - Number.of.Comments.x -  Number.of.Comments.y - Likes_per_follower.x
                       - Comments_per_follower.x  - Likes_Rate - M_Likes.x,
                       data = merged_data_clean)

# Then do stepwise
stepwise_model_comments <- step(full_model_comments, direction = "both", trace = TRUE)
# Print regression results
summary(full_model_comments)
summary(stepwise_model_comments)
formula(stepwise_model_comments)

# Quadratic
quadratic_model_comments <- lm(Comments_Rate ~ Influencer.x +
                                 Likes.x + I(Likes.x^2) +
                                 Word.Count.x + I(Word.Count.x^2) +
                                 pos + I(pos^2) +
                                 neu + I(neu^2) +
                                 neg + I(neg^2) +
                                 word_count + I(word_count^2) +
                                 disgust + I(disgust^2) +
                                 sentimental_religious_probability + I(sentimental_religious_probability^2),
                               data = merged_data_clean)

summary(quadratic_model_comments)

# Interaction
interaction_model_comments <- lm(Comments_Rate ~ 
                                   Influencer.x +
                                   Likes.x +
                                   Word.Count.x +
                                   word_count +
                                   pos + neu + neg +
                                   disgust +
                                   sentimental_religious_probability +
                                   
                                   # Interaction terms
                                   Influencer.x * pos +
                                   Influencer.x * Word.Count.x +
                                   pos * neg +
                                   Word.Count.x * sentimental_religious_probability +
                                   Likes.x * word_count +
                                   disgust * sentimental_religious_probability,
                                 
                                 data = merged_data_clean)

summary(interaction_model_comments)

# Model Comparison - Comments
# Create a list of all models for Comments_Rate
models_comments <- list(
  Full = full_model_comments,
  Stepwise = stepwise_model_comments,
  Quadratic = quadratic_model_comments,
  Interaction = interaction_model_comments
)

# Extract metrics for each model
model_comparison_comments <- lapply(models_comments, function(model) {
  list(
    R2 = summary(model)$r.squared,
    Adjusted_R2 = summary(model)$adj.r.squared,
    AIC = AIC(model),
    BIC = BIC(model)
  )
})

# Convert the list to a tidy data frame
comparison_df_comments <- do.call(rbind, lapply(names(model_comparison_comments), function(name) {
  cbind(Model = name, as.data.frame(model_comparison_comments[[name]]))
}))

# Convert to data frame
comparison_df_comments <- as.data.frame(comparison_df_comments)

# Print comparison table
print(comparison_df_comments)

# Visualize Quadratic
# Likes
ggplot(merged_data_clean, aes(x = Likes.x, y = Comments_Rate)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), color = "blue") +
  coord_cartesian(xlim = c(0, 300000), ylim = c(0, 0.005)) +  # Adjust as needed
  labs(
    title = "Quadratic Relationship: Likes vs Comments Rate (Zoomed)",
    x = "Number of Likes",
    y = "Comments Rate"
  ) +
  theme_minimal(base_size = 14)

# Word Count
ggplot(merged_data_clean, aes(x = Word.Count.x, y = Comments_Rate)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), color = "darkred") +
  labs(
    title = "Quadratic Relationship: Word Count vs Comments Rate",
    x = "Word Count",
    y = "Comments Rate"
  ) +
  theme_minimal()

#### Likes Significance Plot ---- 
# Comment
ggplot(merged_data_clean, aes(x = Comments.x, y = Likes_Rate)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", col = "red") +
  labs(title = "Effect of Comments on Likes Rate", 
       x = "Number of Comments", y = "Likes Rate") +
  theme_minimal()

# Word Count
ggplot(merged_data_clean, aes(x = Word.Count.x, y = Likes_Rate)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", col = "blue") +
  labs(title = "Effect of Word Count on Likes Rate", 
       x = "Word Count", y = "Likes Rate") +
  theme_minimal() + ylim(0,0.1)

# Lexical Diversity
ggplot(merged_data_clean, aes(x = lexical_diversity, y = Likes_Rate)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", col = "purple") +
  labs(title = "Effect of Lexical Diversity on Likes Rate", 
       x = "Lexical Diversity", y = "Likes Rate") +
  theme_minimal()

# Emotions
ggplot(merged_data_clean, aes(x = sadness, y = Likes_Rate, color = "Sadness")) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  
  geom_point(aes(x = anticipation, y = Likes_Rate, color = "Anticipation"), alpha = 0.5) +
  geom_smooth(aes(x = anticipation, y = Likes_Rate, color = "Anticipation"), method = "lm", se = FALSE) +
  
  geom_point(aes(x = joy, y = Likes_Rate, color = "Joy"), alpha = 0.5) +
  geom_smooth(aes(x = joy, y = Likes_Rate, color = "Joy"), method = "lm", se = FALSE) +
  
  geom_point(aes(x = surprise, y = Likes_Rate, color = "Surprise"), alpha = 0.5) +
  geom_smooth(aes(x = surprise, y = Likes_Rate, color = "Surprise"), method = "lm", se = FALSE) +
  
  labs(title = "Effect of Emotional Sentiment on Likes Rate", 
       x = "Emotion Score", y = "Likes Rate", color = "Emotion") +
  theme_minimal()

## Emotions - Facet
emotion_data <- merged_data_clean %>%
  pivot_longer(cols = c(sadness, anticipation, joy, surprise), 
               names_to = "Emotion", values_to = "Score")

ggplot(emotion_data, aes(x = Score, y = Likes_Rate)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", col = "blue", se = FALSE) +
  facet_wrap(~ Emotion, scales = "free_x") +  # Separate plot for each emotion
  labs(title = "Effect of Different Emotions on Likes Rate", 
       x = "Emotion Score", y = "Likes Rate") +
  theme_minimal() + ylim(0,0.05)

# Influencer Specific
ggplot(merged_data_clean, aes(x = Influencer.x, y = Likes_Rate, fill = Influencer.x)) +
  geom_boxplot() +
  labs(title = "Influencer Effect on Likes Rate",
       x = "Influencer", y = "Likes Rate") +
  theme_minimal()

ggplot(merged_data_clean, aes(x = Influencer.x, y = Likes_Rate, fill = Influencer.x)) +
  geom_boxplot() +
  labs(
    title = "Influencer Effect on Likes Rate",
    x = "Influencer",
    y = "Likes Rate"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    plot.title  = element_text(hjust = 0.5, face = "bold", size = 16),
    legend.position = "none"
  )

### New Models to capture non-linear effects - Likes ----
# Linear regression: Predicting Likes based on image features
lm_model_likes_non_linear <- lm(Likes_Rate ~ 
                                  Comments.x +I(Comments.x^2)+ 
                                  Word.Count.x + I(Word.Count.x^2) +  
                                  Brightness.Contrast + I(Brightness.Contrast^2) +  
                                  Warmth + I(Warmth^2) +  
                                  anticipation + I(anticipation^2) +  
                                  joy + I(joy^2) +  
                                  surprise + I(surprise^2) +  
                                  lexical_diversity + I(lexical_diversity^2) + 
                                  Influencer.x, 
                                data = merged_data_clean)

# Print regression results
summary(lm_model_likes_non_linear)

### Comments ----
# Ensure Followers is numeric
merged_data_clean$Followers <- as.numeric(merged_data_clean$Followers)

# Create Engagement Rate variable
merged_data_clean$Comments_Rate <- merged_data_clean$Comments.x / merged_data$Followers.x

# Linear regression: Predicting Likes based on image features
lm_model_comment <- lm(Comments_Rate ~ Likes.x + Brightness + Saturation + Brightness.Contrast + Warmth 
                     + Total.Detected.Objects + Seconds.Passed.x + Posting.Time + Word.Count.x + Emoji.Count.x 
                     + emotional_probability + lifestyle_probability + dailyroutine_probability
                     + lexical_diversity + compound + pos + neu + neg 
                     + anger + anticipation + disgust
                     + fear + joy + sadness + surprise + trust
                     + Influencer.x, data = merged_data_clean)

# Print regression results
summary(lm_model_comment)

# Word Count
ggplot(merged_data_clean, aes(x = Word.Count.x, y = Comments_Rate)) +
  geom_point(alpha = 0.6, color = "gray30", size = 2) +
  geom_smooth(method = "loess", color = "#1f77b4", se = TRUE, linewidth = 1) +
  labs(
    title = "Effect of Word Count on Comments Rate",
    x = "Caption Word Count",
    y = "Comment Rate"
  ) +
  scale_y_continuous(limits = c(0, 0.00005), labels = label_scientific()) +
  coord_cartesian(xlim = c(0, 60)) +  # optional: zoom in on x-axis
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text = element_text(color = "black"),
    axis.title = element_text(face = "bold")
  )

# Linear regression: Predicting Comments based on image features
lm_model_comments_non_linear <- lm(Comments_Rate ~ 
                                  Likes.x +I(Likes.x^2)+ 
                                  Word.Count.x + I(Word.Count.x^2) +  
                                  pos + I(pos^2) +  
                                  neu + I(neu^2) +  
                                  neg + I(neg^2) +  
                                  compound + I(compound^2) +  
                                  surprise + I(surprise^2) +  
                                  lexical_diversity + I(lexical_diversity^2) + 
                                  Influencer.x, 
                                data = merged_data_clean)

# Print regression results
summary(lm_model_comments_non_linear)

#### Interaction terms ----
# Add interaction terms
lm_interact_likes <- lm(Likes_Rate ~  Saturation + 
                          Brightness + Brightness.Contrast + Warmth +
                          Word.Count.x * emotional_probability +   Word.Count.x *lifestyle_probability +   Word.Count.x *dailyroutine_probability
                        +   Word.Count.x *lexical_diversity +   Word.Count.x *compound +   Word.Count.x * pos +   Word.Count.x * neu 
                        +   Word.Count.x *neg 
                        + Total.Detected.Objects + Seconds.Passed.x + Word.Count.x + Emoji.Count.x, data = merged_data_clean)

# Print the summary
summary(lm_interact_likes)

