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
    data<-([(plays*(K1+1)/(K1*length_norm+plays))*idf])
}

# Set working directory
workingdir<-paste("C:\\Users", Sys.getenv("USERNAME"), "Documents\\GitHub\\Recommendations", sep = "\\")
setwd(workingdir)

lastfmdata<-read.csv("lastfm-dataset-as-csv.csv", header = FALSE)
names(lastfmdata)<-c("User", "ArtistId", "Artist","Plays")

# for(i in 1:length(lastfmdata)){
#     lastfmdata$Artist_clean[i]<-removePunctuation(as.character(lastfmdata$Artist[i]))
#     lastfmdata$Artist_clean[i]<-tolower(as.character(lastfmdata$Artist[i]))
#     lastfmdata$Artist_clean[i]<-stripWhitespace(as.character(lastfmdata$Artist[i]))
# }

#takes a long time
splitlastfm<-split(lastfmdata$User, lastfmdata$Artist, drop=TRUE)
splitlastfm_keep<-splitlastfm

#setuserlimit
userlimit<-200 # set to 1000 for faster running
pb<-txtProgressBar(1, length(splitlastfm), style=3)
pbi<-0

for(i in length(splitlastfm):1){
    pbi<-pbi+1
    setTxtProgressBar(pb, pbi)
    if(length(splitlastfm[[i]])<userlimit){
        splitlastfm[[i]]<-NULL
    }
}

idf<-rep(0,length(splitlastfm))
numusers<-rep(0,length(splitlastfm))
pbi<-0
for(i in 1:length(splitlastfm)){
    pbi<-pbi+1
    setTxtProgressBar(pb, pbi)
    idf[i]<-log1p(length(splitlastfm)/(1+length(splitlastfm[[i]])))
    numusers[i]<-length(splitlastfm[[i]])
}

a<-melt(splitlastfm)
lastfmdatasubset<-lastfmdata[lastfmdata$Artist %in% unique(a$L1),] # gives me only artist with more than the user limit of users

plays = ddply(lastfmdatasubset, .(Artist), function(x) avgplays = mean(x$Plays))
normalizedfreq<-0.5+(0.5*numusers)/(max(numusers)) # see https://en.wikipedia.org/wiki/Tf%E2%80%93idf - double normalization of TF
idfdatatable<-data.frame(artist=names(splitlastfm), idf=idf, avgplays = plays$V1, numusers=numusers, normalizedTF = normalizedfreq)


## now we'll create the data table for cosine based on idf, normalizedTF, and avg plays
#now create data table for cosine
matchup <- data.frame(artist1=character(),
                      artist2=character(),
                      angle= numeric(),
                      stringsAsFactors=FALSE)


pb<-txtProgressBar(1, dim(idfdatatable)[1]^2, style=3)
pbi<-0

for(i in 1:dim(idfdatatable)[1]){
    v1<-as.matrix(idfdatatable[i,2:3])
    for(j in 1:dim(idfdatatable)[1]){
        pbi<-pbi+1
        setTxtProgressBar(pb, pbi)
        if(i!=j){
            v2<-as.matrix(idfdatatable[j,2:3])
            theta<-angle(v1, t(v2))
            theta<-theta*(180/pi)
        }
        else {            
            theta<-0
        }
        matchup<-rbind(matchup,
                       data.frame(
                           artist1=as.character(idfdatatable$artist[i]), 
                           artist2=as.character(idfdatatable$artist[j]), 
                           angle = theta
                       ))  
        
    }
}

# Now, give me an artist name and I'll give the top 5 similar artists
artist = "the beatles"
artistlist = matchup[matchup$artist1==artist,]

#by angle
artistlist = arrange(artistlist, angle) #from plyr
artistlisttop5 = artistlist[2:6,2:3] # because the first one will be the artist





rm(a,packages, pb,pbi, plays, numusers, normalizedfreq,v1,v2,artist,i,j,theta)
