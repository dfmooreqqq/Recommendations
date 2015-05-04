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
splitlastfm<-split(lastfmdata$User, lastfmdata$Artist)
splitlastfm_keep<-splitlastfm