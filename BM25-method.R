###BM25 methodology
#### Environment setup ####
# Load packages.
packages <- c("gdata", "ggplot2", "plyr", "reshape2", "tm", "dplyr")
packages <- lapply(packages, FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
        install.packages(x)
        library(x, character.only = TRUE)
    }
})

angle <- function(x,y){
    dot.prod <- x%*%y 
    norm.x <- norm(x,type="2")
    norm.y <- norm(y,type="2")
    theta <- acos(dot.prod / (norm.x * norm.y))
    as.numeric(theta)
}

bm25weight <- function(artistusers, idf, average_plays, B, K1){
    length_norm <- (1-B)+B*artistusers/average_plays
    
}

# Set working directory
workingdir<-paste("C:\\Users", Sys.getenv("USERNAME"), "Documents\\GitHub\\Recommendations", sep = "\\")
setwd(workingdir)

lastfmdata<-read.csv("lastfm-dataset-as-csv.csv", header = FALSE)
names(lastfmdata)<-c("User", "ArtistId", "Artist","Plays")

for(i in 1:length(lastfmdata)){
    lastfmdata$Artist_clean[i]<-removePunctuation(as.character(lastfmdata$Artist[i]))
    lastfmdata$Artist_clean[i]<-tolower(as.character(lastfmdata$Artist[i]))
    lastfmdata$Artist_clean[i]<-stripWhitespace(as.character(lastfmdata$Artist[i]))
}

#takes a long time
splitlastfm<-split(lastfmdata$User, lastfmdata$Artist, drop=TRUE)
splitlastfm_keep<-splitlastfm

#setuserlimit
userlimit<-500 # set to 1000 for faster running
pb<-txtProgressBar(1, length(splitlastfm), style=3)


for(i in length(splitlastfm):1){
    pbi<-pbi+1
    setTxtProgressBar(pb, pbi)
    if(length(splitlastfm[[i]])<userlimit){
        splitlastfm[[i]]<-NULL
    }
}

idf<-rep(0,length(splitlastfm))
pbi<-0
for(i in 1:length(splitlastfm)){
    pbi<-pbi+1
    setTxtProgressBar(pb, pbi)
    idf[i]<-log1p(length(splitlastfm)/(1+length(splitlastfm[[i]])))
}


a<-melt(splitlastfm)
lastfmdatasubset<-lastfmdata[lastfmdata$Artist %in% unique(a$L1),] # gives me only artist with more than the user limit of users
rm(a,i,packages, pb,pbi)

