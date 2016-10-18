# 1.1
songs = read.csv("songs.csv")
str(songs)
songsFrom2010 = subset(songs, year >= 2010)
str(songsFrom2010)

# 1.2
MichaelJackson = subset(songs, artistname == "Michael Jackson")
str(MichaelJackson)

# 1.3
MichaelTop10 = subset(MichaelJackson, Top10 == 1)
summary(MichaelTop10)

# 1.4
sort(songs$timesignature)

# 1.5
songs[which.max(songs$tempo), "songtitle"]

# 2.1
SongsTest = songsFrom2010
SongsTrain = subset(songs, year < 2010)
str(SongsTrain)

# 2.2 - 2.5
nonvars = c("year", "songtitle", "artistname", "songID", "artistID")
SongsTrain = SongsTrain[,!(names(SongsTrain)%in%nonvars)]
SongsTest = SongsTest[,!(names(SongsTest)%in%nonvars)]
SongsLog1 = glm(Top10 ~ ., data=SongsTrain, family=binomial)
summary(SongsLog1)

# 3.1
cor(SongsTrain$loudness, SongsTrain$energy)

# 3.2
SongsLog2 = glm(Top10 ~ . - loudness, data=SongsTrain, family=binomial)
summary(SongsLog2)

# 3.3
SongsLog3 = glm(Top10 ~ . - energy, data=SongsTrain, family=binomial)
summary(SongsLog3)

# 4.1
predictMod3 = predict(SongsLog3, type="response", newdata=SongsTest)
table(SongsTest$Top10, predictMod3 >= 0.45)
(309+19)/(309+19+40+5)

# 4.2
table(SongsTest$Top10)
314/(314+59)

# 4.3
table(SongsTest$Top10, predictMod3 >= 0.45)

# 4.4
19/(40+19)
309/(309+5)