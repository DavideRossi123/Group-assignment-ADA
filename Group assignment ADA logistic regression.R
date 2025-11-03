library(tidyverse)
hiphop <-read_csv("https://raw.githubusercontent.com/DavideRossi123/Group-assignment-ADA/b13f2ff2d7b53948b64cb4952cca622c65b4aec0/hiphop.csv")

#Data wrangling####
hiphop.mod<-hiphop

#track_id

#track_id is not of interest at the modeling stage and so it should be removed 
#from the tibble
hiphop.mod<-select(hiphop.mod,-track_id)


#popularity(range 0-100)   #continuos
hist(hiphop.mod$popularity,breaks=50)
c(min(hiphop.mod$popularity),max(hiphop.mod$popularity)) #there are no units with a value outside the range


#duration_ms(range>0)      #continuos
hist(hiphop.mod$duration_ms,breaks=50)
c(min(hiphop.mod$duration_ms),max(hiphop.mod$duration_ms)) #there are no units with a value outside the range



#danceability(range 0-1)   #continuos
hist(hiphop.mod$danceability,breaks=50)
c(min(hiphop.mod$danceability),max(hiphop.mod$danceability)) #there are no units with a value outside the range



#energy(range 0-1)         #continuos
hist(hiphop.mod$energy,breaks=50)
c(min(hiphop.mod$energy),max(hiphop.mod$energy)) #there are no units with a value outside the range

#key(categorical variable) #discrete
table(hiphop.mod$key) #no category contains a number of units that 
#is to low to allow for reliable estimation.
hiphop.mod$key <- factor(hiphop.mod$key, levels = 0:11)


#loudness(typical range -60-0)   #continuos
hist(hiphop.mod$loudness,breaks=50)
c(min(hiphop.mod$loudness),max(hiphop.mod$loudness)) #there are one unit that has a
#value that is slightly outside the typical range, yet there is no reeason to remove it

#mode(binary variable)     #discrete
table(hiphop$mode)
c(min(hiphop.mod$mode),max(hiphop.mod$mode)) #there are no units with a value outside the range
hiphop.mod$mode <- factor(hiphop.mod$mode ,levels = c(1,0),
                          labels=c("major","minor"))


#speechiness(range 0-1)   #contiuos
hist(hiphop.mod$speechiness,breaks=50)
c(min(hiphop.mod$speechiness),max(hiphop.mod$speechiness)) #there are no units with a value outside the range

#acousticness(range 0-1)  #continuos
hist(hiphop.mod$acousticness,breaks=50)
c(min(hiphop.mod$acousticness),max(hiphop.mod$acousticness)) #there are no units with a value outside the range

#speechiness(range 0-1)   #continuos
hist(hiphop.mod$speechiness,breaks=50)
c(min(hiphop.mod$speechiness),max(hiphop.mod$speechiness)) #there are no units with a value outside the range

#instrumentalness (range 0-1)  #continuos
hist(hiphop.mod$instrumentalness,breaks=50)
c(min(hiphop.mod$instrumentalness),max(hiphop.mod$instrumentalness)) #there are no units with a value outside the range

#liveness(range 0-1)   #continuos
hist(hiphop.mod$liveness,breaks=50)
c(min(hiphop.mod$liveness),max(hiphop.mod$liveness)) #there are no units with a value outside the range

#valence(range 0-1)   #continuos
hist(hiphop.mod$valence,breaks=50)
c(min(hiphop.mod$valence),max(hiphop.mod$valence)) #there are no units with a value outside the range

#tempo(range>0)    #continuos
hist(hiphop.mod$tempo,breaks=50)
c(min(hiphop.mod$tempo),max(hiphop.mod$tempo)) #one observations has tempo 0!!!!!!!!!#####
#We mght consoider merging them

#time_signature(categorical variable)  #discrete
table(hiphop.mod$time_signature)   #some of the categories have very few observations inside!!!!!!!!!######
#Let's merge some of the categories that are too sparse
hiphop.mod$time_signature <- ifelse(hiphop.mod$time_signature == 4, 
                                    "4", 
                                    "Other")
hiphop.mod$time_signature <- factor(hiphop.mod$time_signature, 
                                    levels = c("4", "Other"))
 
#is_genre
table(hiphop.mod$is_genre)
hiphop.mod$is_genre<-factor(hiphop.mod$is_genre,levels = c(1,0),labels=c("hiphop","other"))

#Missing values
sum(is.na(hiphop.mod))  #there are no NA values in the dataset


#Exploratory analysis

#Response variable
table(hiphop.mod$is_genre) #the response variable is balanced



#Bivariate relationships
###########################CONTINUOS VARIABLES##################################

# -------------------------
# Popularity
# - Density: distribution of popularity by genre
# - Boxplot: compare central tendency/spread of popularity between genres
# -------------------------
ggplot(hiphop.mod, aes(x = popularity, fill = is_genre)) +
  geom_density(alpha = 0.5) +
  theme_minimal() +
  labs(title = "Density of Popularity by Genre",
       x = "Popularity", y = "Density")
#1) The graph shows that hiphop songs tend to have either extremly low or extremly high 
#popularity values, suggesting that a polynomial relationship for the popularity 
#covariate may be necessary.

ggplot(hiphop.mod, aes(x = is_genre, y = popularity, fill = is_genre)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Popularity by Genre (boxplot)",
       x = "Genre", y = "Popularity") +
  theme(legend.position = "none")

#The intuition is confirmed by the following graph #CKHECKCKKKKK####
hiphop.mod %>%
  mutate(pop_bin = ntile(popularity, 20)) %>%            # 20 bins
  group_by(pop_bin) %>%
  summarise(pop_med = median(popularity),
            prob_hiphop = mean(is_genre == "hiphop"),
            n = n()) %>%
  ggplot(aes(x = pop_med, y = prob_hiphop)) +
  geom_point() +
  geom_line() +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE) +
  theme_minimal() +
  labs(title = "Binned empirical probability of hiphop vs popularity",
       x = "Median popularity in bin", y = "Proportion hiphop")




# -------------------------
# Duration (ms)
# - Density: distribution of track duration by genre
# - Boxplot: compare duration between genres
# -------------------------
ggplot(hiphop.mod, aes(x = duration_ms, fill = is_genre)) +
  geom_density(alpha = 0.5) +
  theme_minimal() +
  labs(title = "Density of Duration (ms) by Genre",
       x = "Duration (ms)", y = "Density")
#2) Distributions seems to be very similar suggesting that duration covariate
#may be non significant

ggplot(hiphop.mod, aes(x = is_genre, y = duration_ms, fill = is_genre)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Duration (ms) by Genre (boxplot)",
       x = "Genre", y = "Duration (ms)") +
  theme(legend.position = "none")


# -------------------------
# Danceability
# - Density: distribution of danceability by genre
# - Boxplot: compare danceability between genres
# -------------------------
ggplot(hiphop.mod, aes(x = danceability, fill = is_genre)) +
  geom_density(alpha = 0.5) +
  theme_minimal() +
  labs(title = "Density of Danceability by Genre",
       x = "Danceability", y = "Density")
#3) Hip Hop songs tend to have an higher danceability, suggesting that danceability
#should be included in the model

ggplot(hiphop.mod, aes(x = is_genre, y = danceability, fill = is_genre)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Danceability by Genre (boxplot)",
       x = "Genre", y = "Danceability") +
  theme(legend.position = "none")


# -------------------------
# Energy
# - Density: distribution of energy by genre
# - Boxplot: compare energy between genres
# -------------------------
ggplot(hiphop.mod, aes(x = energy, fill = is_genre)) +
  geom_density(alpha = 0.5) +
  theme_minimal() +
  labs(title = "Density of Energy by Genre",
       x = "Energy", y = "Density")
#4) It's uncleaar whether Energy shoud or not be included in the model

ggplot(hiphop.mod, aes(x = is_genre, y = energy, fill = is_genre)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Energy by Genre (boxplot)",
       x = "Genre", y = "Energy") +
  theme(legend.position = "none")


# -------------------------
# Loudness
# - Density: distribution of loudness by genre
# - Boxplot: compare loudness between genres
# -------------------------
ggplot(hiphop.mod, aes(x = loudness, fill = is_genre)) +
  geom_density(alpha = 0.5) +
  theme_minimal() +
  labs(title = "Density of Loudness by Genre",
       x = "Loudness (dB)", y = "Density")
#5) It's uncleaar whether Energy shoud or not be included in the model

ggplot(hiphop.mod, aes(x = is_genre, y = loudness, fill = is_genre)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Loudness by Genre (boxplot)",
       x = "Genre", y = "Loudness (dB)") +
  theme(legend.position = "none")


# -------------------------
# Speechiness
# - Density: distribution of speechiness by genre
# - Boxplot: compare speechiness between genres
# -------------------------
ggplot(hiphop.mod, aes(x = speechiness, fill = is_genre)) +
  geom_density(alpha = 0.5) +
  theme_minimal() +
  labs(title = "Density of Speechiness by Genre",
       x = "Speechiness", y = "Density")
#6) It's uncleaar whether Energy shoud or not be included in the model

ggplot(hiphop.mod, aes(x = is_genre, y = speechiness, fill = is_genre)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Speechiness by Genre (boxplot)",
       x = "Genre", y = "Speechiness") +
  theme(legend.position = "none")


# -------------------------
# Acousticness
# - Density: distribution of acousticness by genre
# - Boxplot: compare acousticness between genres
# -------------------------
ggplot(hiphop.mod, aes(x = acousticness, fill = is_genre)) +
  geom_density(alpha = 0.5) +
  theme_minimal() +
  labs(title = "Density of Acousticness by Genre",
       x = "Acousticness", y = "Density")
#7) It's uncleaar whether Energy shoud or not be included in the model

ggplot(hiphop.mod, aes(x = is_genre, y = acousticness, fill = is_genre)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Acousticness by Genre (boxplot)",
       x = "Genre", y = "Acousticness") +
  theme(legend.position = "none")


# -------------------------
# Instrumentalness
# - Density: distribution of instrumentalness by genre
# - Boxplot: compare instrumentalness between genres
# -------------------------
ggplot(hiphop.mod, aes(x = instrumentalness, fill = is_genre)) +
  geom_density(alpha = 0.5) +
  theme_minimal() +
  labs(title = "Density of Instrumentalness by Genre",
       x = "Instrumentalness", y = "Density")
#8) As the distibution of instrumentalness is extremly right skewed it' unclear
#which is the effect of instrumentalness on the genre of the song

ggplot(hiphop.mod, aes(x = is_genre, y = instrumentalness, fill = is_genre)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Instrumentalness by Genre (boxplot)",
       x = "Genre", y = "Instrumentalness") +
  theme(legend.position = "none")


# -------------------------
# Liveness
# - Density: distribution of liveness by genre
# - Boxplot: compare liveness between genres
# -------------------------
ggplot(hiphop.mod, aes(x = liveness, fill = is_genre)) +
  geom_density(alpha = 0.5) +
  theme_minimal() +
  labs(title = "Density of Liveness by Genre",
       x = "Liveness", y = "Density")
#9) The two distributions are extremly similar suggesting thatliveness should not
#be included in the model

ggplot(hiphop.mod, aes(x = is_genre, y = liveness, fill = is_genre)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Liveness by Genre (boxplot)",
       x = "Genre", y = "Liveness") +
  theme(legend.position = "none")


# -------------------------
# Valence
# - Density: distribution of valence by genre
# - Boxplot: compare valence between genres
# -------------------------
ggplot(hiphop.mod, aes(x = valence, fill = is_genre)) +
  geom_density(alpha = 0.5) +
  theme_minimal() +
  labs(title = "Density of Valence by Genre",
       x = "Valence", y = "Density")
#10) It's uncleaar whether Energy shoud or not be included in the model

ggplot(hiphop.mod, aes(x = is_genre, y = valence, fill = is_genre)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Valence by Genre (boxplot)",
       x = "Genre", y = "Valence") +
  theme(legend.position = "none")


# -------------------------
# Tempo
# - Density: distribution of tempo by genre
# - Boxplot: compare tempo between genres
# -------------------------
ggplot(hiphop.mod, aes(x = tempo, fill = is_genre)) +
  geom_density(alpha = 0.5) +
  theme_minimal() +
  labs(title = "Density of Tempo by Genre",
       x = "Tempo (BPM)", y = "Density")
#11) It's uncleaar whether Energy shoud or not be included in the model


ggplot(hiphop.mod, aes(x = is_genre, y = tempo, fill = is_genre)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Tempo by Genre (boxplot)",
       x = "Genre", y = "Tempo (BPM)") +
  theme(legend.position = "none")

###########################DISCRETE VARIABLES###################################

# -------------------------
# Mode
# - Bar plot: proportion of hiphop songs by mode
# -------------------------
ggplot(hiphop.mod, aes(x = mode, fill = is_genre)) +
  geom_bar(position = "fill") +
  theme_minimal() +
  labs(title = "Proportion of 'Hiphop' by Mode",
       x = "Mode", y = "Proportion") +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(values = c("grey70", "orange"),
                    name = "Genre", labels = c("Other", "Hiphop"))

#12) Major seems to be slightly more popular than minor in hiphop songs

# -------------------------
# Time Signature
# - Bar plot: proportion of hiphop songs for each time signature
# -------------------------
ggplot(hiphop.mod, aes(x = time_signature, fill = is_genre)) +
  geom_bar(position = "fill") +
  theme_minimal() +
  labs(title = "Proportion of 'Hiphop' by Time Signature",
       x = "Time Signature", y = "Proportion") +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(values = c("grey70", "orange"),
                    name = "Genre", labels = c("Other", "Hiphop"))



# -------------------------
# Key
# - Bar plot: proportion of hiphop songs for each key
# -------------------------
ggplot(hiphop.mod, aes(x = key, fill = is_genre)) +
  geom_bar(position = "fill") +
  theme_minimal() +
  labs(title = "Proportion of 'Hiphop' by Key",
       x = "Key (0–11)", y = "Proportion") +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(values = c("grey70", "orange"), name = "Genre",
                    labels = c("Other", "Hiphop"))
#14)


#############################Modelling##########################################


#is_genre (binary variable)
#let's split the data in in train 50%, validation 25% and test 25%.`
library(rsample)  # for data splitting

set.seed(123)  # for reproducibility

# 1. Initial split: 50% train, 50% temp (validation + test)
split_1 <- initial_split(hiphop.mod, prop = 0.5, strata = is_genre)
train <- training(split_1)
temp <- testing(split_1)

# 2. Second split: split temp into 25% validation, 25% test
split_2 <- initial_split(temp, prop = 0.5, strata = is_genre)
validation <- training(split_2)
test <- testing(split_2)

# Check proportions
nrow(train) / nrow(hiphop.mod)      #50%
nrow(validation) / nrow(hiphop.mod) #25%
nrow(test) / nrow(hiphop.mod)       #25%

# Verify class balance in each split
prop.table(table(train$is_genre))   #perfect balance
prop.table(table(validation$is_genre)) #perfect balance
prop.table(table(test$is_genre))    #perfect balance



model1<-glm(is_genre~.,family=binomial(link="logit"),data=train)
summary(model1)
validation_pred <- predict(model1, newdata = validation, type = "response")

#Consider several implementations!!!!!!!!!!!!!!!!!!!!!!!!
#Interactions resampling ecc...
