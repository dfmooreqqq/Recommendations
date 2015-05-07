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

angle <- function(x,y){
    dot.prod <- x%*%y 
    norm.x <- norm(x,type="2")
    norm.y <- norm(y,type="2")
    theta <- acos(dot.prod / (norm.x * norm.y))
    as.numeric(theta)
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
                      anglesmoothed = numeric(),
                      stringsAsFactors=FALSE)


pb<-txtProgressBar(1, dim(dcastsubset)[1]^2, style=3)
pbi<-0

for(i in 1:dim(dcastsubset)[1]){
    v1<-as.matrix(dcastsubset[i,-1])
    v1binarize<-ifelse(v1>0,1,0)
    for(j in 1:dim(dcastsubset)[1]){
        pbi<-pbi+1
        setTxtProgressBar(pb, pbi)
        if(i!=j){
            v2<-as.matrix(dcastsubset[j,-1])
            v2binarize<-ifelse(v2>0,1,0)
            theta<-angle(v1, t(v2))
            theta<-theta*(180/pi)
            overlap<-(v1binarize%*%t(v2binarize))
            smoothedcos<-(overlap / (SMOOTHING + overlap))*cos(theta*pi/180)
            thetasmoothed<-acos(smoothedcos) * (180/pi)
        }
        else {            
            theta<-0
            thetasmoothed<-0
        }
        matchup<-rbind(matchup,
                       data.frame(
                           artist1=as.character(dcastsubset$Artist[i]), 
                           artist2=as.character(dcastsubset$Artist[j]), 
                           angle = theta,
                           anglesmoothed = thetasmoothed[1]
                       ))  

    }
}




# Now, give me an artist name and I'll give the top 5 similar artists
artist = "radiohead"
artistlist = matchup[matchup$artist1==artist,]

#by angle
artistlist = arrange(artistlist, anglesmoothed) #from plyr
artistlisttop5 = artistlist[2:6,2:3] # because the first one will be the artist


