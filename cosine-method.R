## cosine method

## Environment setup
# Load packages.
packages <- c("gdata", "ggplot2", "plyr", "reshape2", "tm", "dplyr")
packages <- lapply(packages, FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
        install.packages(x)
        library(x, character.only = TRUE)
    }
})

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
userlimit<-500
pb<-txtProgressBar(1, length(splitlastfm), style=3)
pbi<-0

for(i in length(splitlastfm):1){
    pbi<-pbi+1
    setTxtProgressBar(pb, pbi)
    if(length(splitlastfm[[i]])<userlimit){
        splitlastfm[[i]]<-NULL
    }
}

a<-melt(splitlastfm)

lastfmdatasubset<-lastfmdata[lastfmdata$Artist %in% unique(a$L1),] # gives me only artist with more than the user limit of users

#now create data table for cosine
lastfmdatasubset<-lastfmdatasubset[,c(1,3,4)]
dcastsubset<-dcast(lastfmdatasubset, Artist~User)

dcastsubset[is.na(dcastsubset)]<-0

matchup <- data.frame(artist1=character(),
                      artist2=character(),
                      angle= numeric(),
                      stringsAsFactors=FALSE)

for(i in 1:length(dcastsubset)){
    v1<-as.vector(dcastsubset[i,-1])
    for(j in 1:length(dcastsubset)){
        v2<-as.vector(dcastsubset[j,-1])
        theta <- ( sum(v1*v2) / ( sqrt(sum(v1 * v1)) + sqrt(sum(v2 * v2)) ) )
        theta <- acos(theta)
        matchup<-rbind(matchup,
                       data.frame(
                           artist1=as.character(names(dcastsubset[i,1])), 
                           artist2=as.character(names(dcastsubset[j,1])), 
                           angle=theta
                       ))
    }
}





