library(tidyverse) # metapackage of all tidyverse packages

# Loading the dataset
raw_songs <- read.csv("C:/Users/10User/Desktop/R projects/Analyzing Spotify Songs/spotify_songs.csv")

# Now the dataset is loaded, I will begin cleaning the data for accurate analysis
songs_no_miss <- na.omit(raw_songs) # remove missing values
songs_no_dup <- distinct(songs_no_miss,track_id,.keep_all = TRUE) # remove duplicated values
# sum(duplicated(songs_no_dup$track_id)) will give 0

# For visualization purposes I will add a new column of the release year of these tracks 
songs_no_dup$release_year <- sapply(songs_no_dup$track_album_release_date, function(x) as.integer(substr(x, 1, 4)))

# For certain plots (4 till 13) I need a dataset with only numerical data
songs_num <- songs_no_dup[sapply(songs_no_dup, is.numeric)]



# Data preprocessing is done, now I will perform some exploratory data analysis (EDA)
# Graph 1: Pie chart(polar bar chart) showing the frequency of each genre
genre_data <- mutate(summarise(group_by(songs_no_dup, playlist_genre),count = n()),
  proportion = count / sum(count),
  ypos = cumsum(proportion) - 0.5 * proportion)
graph1 <- ggplot(genre_data, aes(x = "", y = proportion, fill = playlist_genre)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  theme_void() + theme(plot.background = element_rect(fill="white", color = NA)) + 
  scale_fill_brewer(palette="Spectral") +
  geom_text(aes(y = ypos, label = scales::percent(proportion)), color = "black", size = 6) +
  labs(title = "Genre Popularity")
ggsave ("genre_popularity.png", graph1, width = 10, height =8)

# Graph 2: Donut chart showing the frequency of each sub genre
subgenre_data <- mutate(summarise(group_by(songs_no_dup, playlist_subgenre),count = n()),
  proportion = count / sum(count),
  ypos = cumsum(proportion) - 0.5 * proportion)
graph2 <- ggplot(subgenre_data, aes(x = 2, y = proportion, fill = playlist_subgenre)) +
  geom_bar(width = 1, stat = "identity", color = "white") + coord_polar(theta = "y") +
  xlim(0.5, 2.5) + theme_void() + labs(title = "Sub Genre Popularity") +
  geom_text(aes(y = ypos, label = scales::percent(proportion)), color = "black", size = 2) +
  theme(legend.position = "right", plot.background = element_rect(fill="white", color = NA))
ggsave ("subgenre_popularity.png", graph2, width = 10, height =8)

# Graph 3: Bar plot showing the number of songs for each year
year_data <- summarise(group_by(songs_no_dup, release_year),count = n())
year_data <- year_data[order(-year_data$release_year), ] # descending order
top_years <- year_data[1:10, ] # last ten years (from 2011 till 2020)
graph3 <- ggplot(top_years, aes(x = as.factor(release_year), y = count, fill = as.factor(release_year))) +
  geom_bar(stat = "identity") + theme_minimal() + scale_fill_brewer(palette="Spectral") +
  labs(title = "Year Popularity", x = "Year", y = "Number of Songs") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.background = element_rect(fill="white", color = NA))
ggsave ("year_popularity.png", graph3, width = 10, height =8)

# Graph 4: Histogram showing the density of danceability for all songs
graph4 <- ggplot(songs_num, aes(x = danceability)) +
  geom_histogram(aes(y = after_stat(density)), bins = 45, fill = "green", color = "black", alpha = 0.6) +
  geom_density(color = "red", linewidth = 1) +
  theme_minimal() + labs(title = "Danceability Distribution", x = "Danceability", y = "Density") +
  theme(axis.text.x = element_text(angle = 0, hjust = 1), plot.background = element_rect(fill="white", color = NA))
ggsave ("danceability_distribution.png", graph4, width = 10, height =8)

# Graph 5: Histogram showing the density of energy for all songs
graph5 <- ggplot(songs_num, aes(x = energy)) +
  geom_histogram(aes(y = after_stat(density)), bins = 45, fill = "green", color = "black", alpha = 0.6) +
  geom_density(color = "red", linewidth = 1) +
  theme_minimal() + labs(title = "Energy Distribution", x = "Energy", y = "Density") +
  theme(axis.text.x = element_text(angle = 0, hjust = 1), plot.background = element_rect(fill="white", color = NA))
ggsave ("energy_distribution.png", graph5, width = 10, height =8)

# Graph 6: Histogram showing the density of loudness for all songs
graph6 <- ggplot(songs_num, aes(x = loudness)) +
  geom_histogram(aes(y = after_stat(density)), bins = 45, fill = "green", color = "black", alpha = 0.6) +
  geom_density(color = "red", linewidth = 1) +
  theme_minimal() + labs(title = "Loudness Distribution", x = "Loudness", y = "Density") +
  theme(axis.text.x = element_text(angle = 0, hjust = 1), plot.background = element_rect(fill="white", color = NA))
ggsave ("loudness_distribution.png", graph6, width = 10, height =8)

# Graph 7: Histogram showing the density of speechiness for all songs
graph7 <- ggplot(songs_num, aes(x = speechiness)) +
  geom_histogram(aes(y = after_stat(density)), bins = 45, fill = "green", color = "black", alpha = 0.6) +
  geom_density(color = "red", linewidth = 1) +
  theme_minimal() + labs(title = "Speechiness Distribution", x = "Speechiness", y = "Density") +
  theme(axis.text.x = element_text(angle = 0, hjust = 1), plot.background = element_rect(fill="white", color = NA))
ggsave ("speechiness_distribution.png", graph7, width = 10, height =8)

# Graph 8: Histogram showing the density of acoustiness for all songs
graph8 <- ggplot(songs_num, aes(x = acousticness)) +
  geom_histogram(aes(y = after_stat(density)), bins = 45, fill = "green", color = "black", alpha = 0.6) +
  geom_density(color = "red", linewidth = 1) +
  theme_minimal() + labs(title = "Acoustiness Distribution", x = "Acoustiness", y = "Density") +
  theme(axis.text.x = element_text(angle = 0, hjust = 1), plot.background = element_rect(fill="white", color = NA))
ggsave ("acoustiness_distribution.png", graph8, width = 10, height =8)

# Graph 9: Histogram showing the density of instrumentalness for all songs
graph9 <- ggplot(songs_num, aes(x = instrumentalness)) +
  geom_histogram(aes(y = after_stat(density)), bins = 45, fill = "green", color = "black", alpha = 0.6) +
  geom_density(color = "red", linewidth = 1) +
  theme_minimal() + labs(title = "Instrumentalness Distribution", x = "Instrumentalness", y = "Density") +
  theme(axis.text.x = element_text(angle = 0, hjust = 1), plot.background = element_rect(fill="white", color = NA))
ggsave ("instrumentalness_distribution.png", graph9, width = 10, height =8)

# Graph 10: Histogram showing the density of liveness for all songs
graph10 <- ggplot(songs_num, aes(x = liveness)) +
  geom_histogram(aes(y = after_stat(density)), bins = 45, fill = "green", color = "black", alpha = 0.6) +
  geom_density(color = "red", linewidth = 1) +
  theme_minimal() + labs(title = "Liveness Distribution", x = "Liveness", y = "Density") +
  theme(axis.text.x = element_text(angle = 0, hjust = 1), plot.background = element_rect(fill="white", color = NA))
ggsave ("liveness_distribution.png", graph10, width = 10, height =8)

# Graph 11: Histogram showing the density of valence for all songs
graph11 <- ggplot(songs_num, aes(x = valence)) +
  geom_histogram(aes(y = after_stat(density)), bins = 45, fill = "green", color = "black", alpha = 0.6) +
  geom_density(color = "red", linewidth = 1) +
  theme_minimal() + labs(title = "Valence Distribution", x = "Valence", y = "Density") +
  theme(axis.text.x = element_text(angle = 0, hjust = 1), plot.background = element_rect(fill="white", color = NA))
ggsave ("valence_distribution.png", graph11, width = 10, height =8)

# Graph 12: Histogram showing the density of tempo for all songs
graph12 <- ggplot(songs_num, aes(x = tempo)) +
  geom_histogram(aes(y = after_stat(density)), bins = 45, fill = "green", color = "black", alpha = 0.6) +
  geom_density(color = "red", linewidth = 1) +
  theme_minimal() + labs(title = "Tempo Distribution", x = "Tempo", y = "Density") +
  theme(axis.text.x = element_text(angle = 0, hjust = 1), plot.background = element_rect(fill="white", color = NA))
ggsave ("tempo_distribution.png", graph12, width = 10, height =8)

# Graph 13: Histogram showing the density of song duration for all songs
graph13 <- ggplot(songs_num, aes(x = duration_ms)) +
  geom_histogram(aes(y = after_stat(density)), bins = 45, fill = "green", color = "black", alpha = 0.6) +
  geom_density(color = "red", linewidth = 1) +
  theme_minimal() + labs(title = "Song Duration (ms) Distribution", x = "Duration (ms)", y = "Density") +
  theme(axis.text.x = element_text(angle = 0, hjust = 1), plot.background = element_rect(fill="white", color = NA))
ggsave ("songDuration_distribution.png", graph13, width = 10, height =8)

# Graph 14: Bar plot showing the frequency of each artist (from best to least  best)
artist_data <- summarise(group_by(songs_no_dup, track_artist),count = n())
artist_data <- artist_data[order(-artist_data$count), ] # descending from best to least
top_artists <- artist_data[1:10, ] # only top 10 artists
graph14 <- ggplot(top_artists, aes(x = reorder(track_artist, -count), y = count, fill = track_artist)) +
  geom_bar(stat = "identity") + theme_minimal() + 
  labs(title = "Most Popular Artists", x = "Artist", y = "Num of Appearances on Spotify") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.background = element_rect(fill="white", color = NA))
ggsave ("artists_popularity.png", graph14, width = 10, height =8)

# Graph 15: Correlation plot showing the correlation between column titles
library(corrplot)
co <- cor(songs_num) #created the correlation matrix
png("correlation_plot.png", width = 1000, height =800)
corrplot(co, method = 'circle') #created the correlation plot
dev.off()