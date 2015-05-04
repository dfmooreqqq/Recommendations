## Environment setup
# Load packages.
packages <- c("gdata", "ggplot2", "plyr")
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



#takes a long time
splitlastfm<-split(lastfmdata$User, lastfmdata$Artist)

matchup <- data.frame(artist1=character(),
                      artist2=character(),
                      intersection=numeric())

commonusersthreshold <- 100

# This takes way to long
for(i in 1:length(splitlastfm))
{
    for(j in 1:length(splitlastfm))
    {
        if(length(intersect(splitlastfm[[i]], splitlastfm[[j]])) > commonusersthreshold) 
        {
            matchup<-rbind(matchup, 
                    c(names(splitlastfm[i]), 
                    names(splitlastfm[j]), 
                    length(intersect(splitlastfm[[i]], splitlastfm[[j]]))))
        }
    }
}