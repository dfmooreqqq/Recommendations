## Environment setup
# Load packages.
packages <- c("gdata", "ggplot2", "plyr", "reshape2", "tm")
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
userlimit<-250
pb<-txtProgressBar(1, length(splitlastfm), style=3)
pbi<-0

for(i in length(splitlastfm):1){
    pbi<-pbi+1
    setTxtProgressBar(pb, pbi)
    if(length(splitlastfm[[i]])<userlimit){
        splitlastfm[[i]]<-NULL
    }
}

matchup <- data.frame(artist1=character(),
                      artist2=character(),
                      intersection=numeric(),
                      jaccardintersection=numeric(),
                      stringsAsFactors=FALSE)

commonusersthreshold <- 1

# This takes way to long
pb<-txtProgressBar(1, length(splitlastfm), style=3)
pbi<-0
for(i in 1:length(splitlastfm))
{
    pbi<-pbi+1
    setTxtProgressBar(pb, pbi)
    
    for(j in 1:length(splitlastfm))
    {
        if((length(intersect(splitlastfm[[i]], splitlastfm[[j]])) > commonusersthreshold) & i!=j) # the i!= j removes self-similarity
        {
            matchup<-rbind(matchup, 
                        data.frame(
                            artist1=as.character(names(splitlastfm[i])), 
                            artist2=as.character(names(splitlastfm[j])), 
                            intersection=length(intersect(splitlastfm[[i]], splitlastfm[[j]])),
                            jaccardintersection=length(intersect(splitlastfm[[i]], splitlastfm[[j]]))/(length(splitlastfm[[i]])+length(splitlastfm[[j]]))
                        )
                        )
        }
    }
}


# Now, give me an artist name and I'll give the top 5 similar artists
artist = "3 doors down"
artistlist = matchup[matchup$artist1==artist,]

#by naive intersection
artistlist = arrange(artistlist, desc(intersection)) #from plyr
artistlisttop5 = artistlist[1:5,2:3]

#by jaccard intersection
artistlistjaccard = arrange(artistlist, desc(jaccardintersection)) #from plyr
artistlisttop5jaccard = artistlistjaccard[1:5,c(2,4)]
