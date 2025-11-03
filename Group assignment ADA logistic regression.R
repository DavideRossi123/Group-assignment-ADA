`library(tidyverse)
hiphop <-read_csv("https://raw.githubusercontent.com/DavideRossi123/Group-assignment-ADA/b13f2ff2d7b53948b64cb4952cca622c65b4aec0/hiphop.csv")

#Data wrangling####
hiphop.mod<-hiphop

#track_id

#track_id is not of interest at the modeling stage and so it should be removed 
#from the tibble
hiphop.mod<-select(hiphop.mod,-track_id)


#popularity(range 0-100)
hist(hiphop.mod$popularity,breaks=50)
c(min(hiphop.mod$popularity),max(hiphop.mod$popularity)) #there are no units with a value outside the range


#duration_ms(range>0)
hist(hiphop.mod$duration_ms,breaks=50)
c(min(hiphop.mod$duration_ms),max(hiphop.mod$duration_ms)) #there are no units with a value outside the range



#danceability(range 0-1)
hist(hiphop.mod$danceability,breaks=50)
c(min(hiphop.mod$danceability),max(hiphop.mod$danceability)) #there are no units with a value outside the range



#energy(range 0-1)
hist(hiphop.mod$energy,breaks=50)
c(min(hiphop.mod$energy),max(hiphop.mod$energy)) #there are no units with a value outside the range

#key(categorical variable)
table(hiphop.mod$key) #no category contains a number of units that 
#is to low to allow for reliable estimation.

#loudness(typical range -60-0)
hist(hiphop.mod$loudness,breaks=50)
c(min(hiphop.mod$loudness),max(hiphop.mod$loudness)) #there are one unit that has a
#value that is slightly outside the typical range, yet there is no reeason to remove it

#mode(binary variable)
table(hiphop$mode)
c(min(hiphop.mod$mode),max(hiphop.mod$mode)) #there are no units with a value outside the range

#speechiness(range 0-1)
hist(hiphop.mod$speechiness,breaks=50)
c(min(hiphop.mod$speechiness),max(hiphop.mod$speechiness)) #there are no units with a value outside the range

#acousticness(range 0-1)
hist(hiphop.mod$acousticness,breaks=50)
c(min(hiphop.mod$acousticness),max(hiphop.mod$acousticness)) #there are no units with a value outside the range

#speechiness(range 0-1)
hist(hiphop.mod$speechiness,breaks=50)
c(min(hiphop.mod$speechiness),max(hiphop.mod$speechiness)) #there are no units with a value outside the range

#instrumentalness (range 0-1)
hist(hiphop.mod$instrumentalness,breaks=50)
c(min(hiphop.mod$instrumentalness),max(hiphop.mod$instrumentalness)) #there are no units with a value outside the range

#liveness(range 0-1)
hist(hiphop.mod$liveness,breaks=50)
c(min(hiphop.mod$liveness),max(hiphop.mod$liveness)) #there are no units with a value outside the range

#valence(range 0-1)
hist(hiphop.mod$valence,breaks=50)
c(min(hiphop.mod$valence),max(hiphop.mod$valence)) #there are no units with a value outside the range

#tempo(range>0)
hist(hiphop.mod$tempo,breaks=50)
c(min(hiphop.mod$tempo),max(hiphop.mod$tempo)) #one observations has tempo 0!!!!!!!!!#####
#We mght consoider merging them

#time_signature(categorical variable)
table(hiphop.mod$time_signature)   #some of the categories have very few observations inside!!!!!!!!!######
#We might consider merging them

#is_genre (binary variable)
#let's split the data in in train 50%, validation 25% and test 25%.`






