library(tidyverse)
library(mgcv)
library(pROC)
hiphop <-read_csv("https://raw.githubusercontent.com/DavideRossi123/Group-assignment-ADA/b13f2ff2d7b53948b64cb4952cca622c65b4aec0/hiphop.csv")

#consider resampling instead of merging categories
#Ask if it's appropriate to perform the EDA on the whole dataset
#Note that in our problem it's not necessay to set a certain treshold for the number of false negatuve ecc

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
#should be included in the model,possibly with a polynomial relationship

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
#4) Extreme values of energy appears to be strong predictors for the "other" category
#suggesting that energy should be included as a polynomial regression in the model,
#in order to allow the possibility of a strong effect of the covariate for extreme value only

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
#5)Low loudness values appear almost only in the "other" category, suggesting a nonlinear
#(possibly polynomial) relationship between loudness and genre.

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
# High values of speechiness are more common in hip-hop songs; however, the
#strength of this covariate's effect seems to be lower of the ones above

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
#7)Low values of acousticness are more popular in hiphop songs the opposit can be told 
#for small values suggesting a linear effect may be suitable here

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
#9) The two distributions are extremly similar suggesting that liveness should not
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
#10)Low values of valence are more popular in "other" songs whereas high values
#of valence are more popular in hiphop songs , a linear effect may be suitable here.


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
#11) It's uncleaar whether tempo shoud or not be included in the model


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

#12) Major seems to be slightly more popular than minor in hiphop songs,
#this may not be significant

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
#13) "other" time signature seems to be slightly more popular in  hiphop songs with respect to "4" time signature 
#this may appear non significant in the inal model

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
#14) Some keys are more popular in hip hop songs with respect to 
# other songs, for others the the frequencys for others the frequency is almost equally shared


#############################Modelling##########################################

###############################DATA SPLIT#######################################

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

#Let's converte the response variable intoa numerical variable
train$is_genre<-ifelse(train$is_genre=="hiphop",1,0)
validation$is_genre<-ifelse(validation$is_genre=="hiphop",1,0)
test$is_genre<-ifelse(test$is_genre=="hiphop",1,0)

# Check proportions
nrow(train) / nrow(hiphop.mod)      #50%
nrow(validation) / nrow(hiphop.mod) #25%
nrow(test) / nrow(hiphop.mod)       #25%

# Verify class balance in each split
prop.table(table(train$is_genre))   #perfect balance
prop.table(table(validation$is_genre)) #perfect balance
prop.table(table(test$is_genre))    #perfect balance

#From the exploratory analysis:

#1) The graph shows that hiphop songs tend to have either extremely low or        ♣♣♣
#extremely high popularity values, suggesting that a polynomial relationship for 
#the popularity covariate may be necessary.
#2) Distributions seems to be very similar suggesting that duration covariate may  ♣♣♣
#be non significant
#3) Hip Hop songs tend to have a higher danceability, suggesting that danceability
#should be included in the model, possibly with a polynomial relationship
#4) Extreme values of energy appears to be strong predictors for the "other" 
#category suggesting that energy should be included as a polynomial regression 
#in the model, in order to allow the possibility of a strong effect of the 
#covariate for extreme value only
#5) Low loudness values appear almost only in the "other" category, suggesting 
#a nonlinear (possibly polynomial) relationship between loudness and genre.
#6) High values of speechiness are more common in hip-hop songs; however, the 
#strength of this covariate's effect seems to be lower of the ones above
#7) Low values of acousticness are more popular in hiphop songs, the opposite 
#can be told for small values suggesting a linear effect may be suitable here
#8) As the distribution of instrumentalness is extremely right-skewed, it's 
#unclear what the effect of instrumentalness on the genre of the song is
#9) The two distributions of liveness are extremely similar, suggesting that      ♣♣♣
#liveness should not be included in the model
#10) Low values of valence are more popular in "other" songs, whereas high 
#values of valence are more popular in hiphop songs, a linear effect may be suitable here.
#11) It's unclear whether tempo should or not be included in the model
#12) Major seems to be slightly more popular than minor in hiphop songs, this 
#may not be significant
#13) "Other" time signature seems to be slightly more popular in hiphop songs 
#with respect to "4" time signature, this may appear non-significant in the final model
#14) Some keys are more popular in hip hop songs with respect to other songs, 
#for others the frequencies are almost equally shared












#Likely something better can be done for categorical variables
# try to log durations_ms and tempo that have a lot of extreme values
#tuning of the cutoff







model1<-gam(is_genre ~ popularity + duration_ms + danceability + energy + loudness + 
                             speechiness + acousticness + instrumentalness + liveness + 
                             valence + tempo + key + mode + time_signature, 
                           family = binomial(link = "logit"), 
                           data = train)
summary(model1)
#Let's use 0.5 as cutoff for the preliminary analysis
# Make predictions on the validation set
predictions<- predict(model1, newdata = validation, type = "response")
# Compute hit ratio
predicted_class <- ifelse(predictions > 0.5, 1, 0)
hit_ratio1 <- mean(predicted_class == validation$is_genre)
hit_ratio1

#Let's inspect the confusion matrix as well
confusion_matrix1<- table(Predicted = predicted_class, Actual = validation$is_genre)
confusion_matrix1







#As expected(point 2) duration_ms is not significant
#So let's fit a model without this covariate
model2<-gam(is_genre ~ popularity+ danceability + energy + loudness + 
              speechiness + acousticness + instrumentalness + liveness + 
              valence + tempo + key + mode + time_signature, 
            family = binomial(link = "logit"), 
            data = train)
summary(model2)
#Let's use 0.5 as cutoff for the preliminary analysis
# Make predictions on the validation set
predictions<- predict(model2, newdata = validation, type = "response")
# Compute hit ratio
predicted_class <- ifelse(predictions > 0.5, 1, 0)
hit_ratio2<- mean(predicted_class == validation$is_genre)
hit_ratio2

#As expected(point 9) liveness is not significant
#So let's fit a model without this covariate
model3<-gam(is_genre ~ popularity+ danceability + energy + loudness + 
              speechiness + acousticness + instrumentalness + 
              valence + tempo + key + mode + time_signature, 
            family = binomial(link = "logit"), 
            data = train)
summary(model3)
#Let's use 0.5 as cutoff for the preliminary analysis
# Make predictions on the validation set
predictions <- predict(model3, newdata = validation, type = "response")
# Compute hit ratio
predicted_class <- ifelse(predictions > 0.5, 1, 0)
hit_ratio3 <- mean(predicted_class == validation$is_genre)
hit_ratio3

#From point 1:
# 1) The graph shows that hiphop songs tend to have either extremely low or
#extremely high popularity values, suggesting that a polynomial relationship for the popularity covariate may be necessary.
model4<-gam(is_genre ~ s(popularity)+ danceability + energy + loudness + 
              speechiness + acousticness + instrumentalness + 
              valence + tempo + key + mode + time_signature, 
            family = binomial(link = "logit"), 
            data = train) 
summary(model4)
#As expected the smooth term is extremly significant and with high edf.
#A graphical inspection confirms that the effect is large at the extremes and low in the middle
plot(model4,residuals=TRUE)

#Let's use 0.5 as cutoff for the preliminary analysis
# Make predictions on the validation set
predictions <- predict(model4, newdata = validation, type = "response")
# Compute hit ratio
predicted_class <- ifelse(predictions > 0.5, 1, 0)
hit_ratio4 <- mean(predicted_class == validation$is_genre)
hit_ratio #Hit ratio increases significantly

#Let's inspect the confusion matrix as well
confusion_matrix4<- table(Predicted = predicted_class, Actual = validation$is_genre)
confusion_matrix4
confusion_matrix1


#From point 3)
#Let's try to fit a model with a smooth term for danceability
model5<-gam(is_genre ~ s(popularity)+s(danceability) + energy + loudness + 
              speechiness + acousticness + instrumentalness + 
              valence + tempo + key + mode + time_signature, 
            family = binomial(link = "logit"), 
            data = train) 
summary(model5)
#as edf is close to 1 i choose to prefer the more simplistic model with 
#danceability introduced as a linear term and so to prefer model4
#Let's use 0.5 as cutoff for the preliminary analysis
# Make predictions on the validation set
predictions <- predict(model5, newdata = validation, type = "response")
# Compute hit ratio
predicted_class <- ifelse(predictions > 0.5, 1, 0)
hit_ratio5 <- mean(predicted_class == validation$is_genre)
hit_ratio5  #Hit ratio increases only slightly



#From point 4)
#Let's try to fit a model with a smooth term for energy
model6<-gam(is_genre ~ s(popularity)+danceability + s(energy) + loudness + 
              speechiness + acousticness + instrumentalness + 
              valence + tempo + key + mode + time_signature, 
            family = binomial(link = "logit"), 
            data = train) 
summary(model6)
#the smooth term is highly signifcant and has high edf so it should be retained

predictions <- predict(model6, newdata = validation, type = "response")
# Compute hit ratio
predicted_class <- ifelse(predictions > 0.5, 1, 0)
hit_ratio6 <- mean(predicted_class == validation$is_genre)
hit_ratio6 
hit_ratiio4 #However hitratio does not increase at all


#From point 5)
model7<-gam(is_genre ~ s(popularity)+danceability + s(energy) + s(loudness) + 
              speechiness + acousticness + instrumentalness + 
              valence + tempo + key + mode + time_signature, 
            family = binomial(link = "logit"), 
            data = train) 
summary(model7) #The smooth term is only slightly significant

predictions <- predict(model7, newdata = validation, type = "response")
# Compute hit ratio
predicted_class <- ifelse(predictions > 0.5, 1, 0)
hit_ratio7 <- mean(predicted_class == validation$is_genre)
hit_ratio7 #the hitratio does not increase
#So we drop the smooth term for loudness and stick to model6
hit_ratio4


#From point 6)
#We had some hints that a polynomil regression may be suitable for speechiness
#Let's try to fit a model with a smooth trm for speechiness:
model8<-gam(is_genre ~ s(popularity)+danceability + s(energy) + loudness + 
             s(speechiness) + acousticness + instrumentalness + 
              valence + tempo + key + mode + time_signature, 
            family = binomial(link = "logit"), 
            data = train) 
summary(model8) #The smooth term is extremly significant

predictions <- predict(model8, newdata = validation, type = "response")
# Compute hit ratio
predicted_class <- ifelse(predictions > 0.5, 1, 0)
hit_ratio8 <- mean(predicted_class == validation$is_genre)
hit_ratio8 #the hitratio does increase
hit_ratio4


#From point 7)
#A linear effect seemsed to be appropriate however let's try to fit a model
#with a smooth term for acousticness instead
model9<-gam(is_genre ~ s(popularity)+danceability + s(energy) + loudness + 
              s(speechiness) +s(acousticness) + instrumentalness + 
              valence + tempo + key + mode + time_signature, 
            family = binomial(link = "logit"), 
            data = train) 
summary(model9) #As expected the smooth term is not sigificant


#from point 8)
#It was unclear form the exploratory anaalysis
model10<-gam(is_genre ~ s(popularity)+danceability + s(energy) + loudness + 
              s(speechiness) +acousticness + s(instrumentalness) + 
              valence + tempo + key + mode + time_signature, 
            family = binomial(link = "logit"), 
            data = train) 
summary(model10) #edf is really low, we should stick to model 8 for simplicity


#from point 10)
#A linear effect seemsed to be appropriate however let's try to fit a model
#with a smooth term for valence instead
model11<-gam(is_genre ~ s(popularity)+danceability + s(energy) + loudness + 
               s(speechiness) +acousticness + instrumentalness+ 
               s(valence) + tempo + key + mode + time_signature, 
             family = binomial(link = "logit"), 
             data = train) 
summary(model11)  #edf is really low, we should stick to model 8 for simplicity

#From point 11)
#tempo is higly unsignificant in model 8 so we shpould remove it 
model12<-gam(is_genre ~ s(popularity)+danceability + s(energy) + loudness + 
               s(speechiness) +acousticness + instrumentalness+ 
               valence +key + mode + time_signature, 
             family = binomial(link = "logit"), 
             data = train) 
summary(model12)
predictions <- predict(model12, newdata = validation, type = "response")
# Compute hit ratio
predicted_class <- ifelse(predictions > 0.5, 1, 0)
hit_ratio12 <- mean(predicted_class == validation$is_genre)
hit_ratio12 #The hit ratio does not decrease even if tempo was removed
hit_ratio8

#from point 12)
#From model12 mode is not significant so we should remove it
model13<-gam(is_genre ~ s(popularity)+danceability + s(energy) + loudness + 
               s(speechiness) +acousticness + instrumentalness+ 
               valence +key + time_signature, 
             family = binomial(link = "logit"), 
             data = train) 
summary(model13)
predictions <- predict(model13, newdata = validation, type = "response")
# Compute hit ratio
predicted_class <- ifelse(predictions > 0.5, 1, 0)
hit_ratio13 <- mean(predicted_class == validation$is_genre)
hit_ratio13 #The hit ratio does increase(even if slightly) after tempo was removed,suggesting thta model13
#should be preferred
hit_ratio12

plot(model13,residuals=TRUE)   #the last 2 graphs are almost flat!!!!!!!!!!!

#From point 13)
#From model 13 time signature is not significant so we should remove it
model14<-gam(is_genre ~ s(popularity)+danceability + s(energy) + loudness + 
               s(speechiness) +acousticness + instrumentalness+ 
               valence +key, 
             family = binomial(link = "logit"), 
             data = train) 
summary(model14)
predictions <- predict(model14, newdata = validation, type = "response")
# Compute hit ratio
predicted_class <- ifelse(predictions > 0.5, 1, 0)
hit_ratio14 <- mean(predicted_class == validation$is_genre)
hit_ratio14 #The hit ratio decreases slighltly 
hit_ratio13



#From point 14)
#Som of the levels of keynare signifcant soi key shouldn't ptobably be removed
#Let's try to remove key from the model
model15<-gam(is_genre ~ s(popularity)+danceability + s(energy) + loudness + 
               s(speechiness) +acousticness + instrumentalness+ 
               valence, 
             family = binomial(link = "logit"), 
             data = train) 
summary(model15)
predictions <- predict(model14, newdata = validation, type = "response")
# Compute hit ratio
predicted_class <- ifelse(predictions > 0.5, 1, 0)
hit_ratio15 <- mean(predicted_class == validation$is_genre)
hit_ratio15 #The hit ratio does not decreae from model 14 
hit_ratio13

#Consider several:
#Interactions resampling ecc...




#testing####################
#compute hit ratio 
# Load required package
library(pROC)

# Create a helper function to evaluate models
evaluate_model <- function(model, validation, cutoff = 0.5) {
  # Predicted probabilities
  preds <- predict(model, newdata = validation, type = "response")
  
  # Predicted classes based on cutoff
  pred_class <- ifelse(preds > cutoff, 1, 0)
  
  # Hit ratio (accuracy)
  hit_ratio <- mean(pred_class == validation$is_genre)
  
  # Confusion matrix
  confusion <- table(Predicted = pred_class, Actual = validation$is_genre)
  
  # ROC curve and AUC
  roc_obj <- roc(validation$is_genre, preds)
  auc_value <- auc(roc_obj)
  
  # Plot ROC curve
  plot(roc_obj, main = paste("ROC Curve for", deparse(substitute(model))), col = "blue")
  abline(a = 0, b = 1, lty = 2, col = "gray")
  
  # Return results
  list(
    Hit_Ratio = hit_ratio,
    Confusion_Matrix = confusion,
    AUC = auc_value
  )
}

# Now evaluate all models
results_model1  <- evaluate_model(model1, validation)
results_model13 <- evaluate_model(model13, validation)
results_model14 <- evaluate_model(model14, validation)
results_model15 <- evaluate_model(model15, validation)

# Print results
results_model1
results_model13
results_model14
results_model15


















