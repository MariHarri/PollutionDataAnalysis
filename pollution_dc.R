rm(list=ls())
library(stringr)
#recording directory name
specdata<-'C:/Users/chris/OneDrive/Desktop/School Docs/Coursera Assignments/specdata'

pollution_dc<-function(directory,id,threshold=0){
    #changing some graphical parameters for visibility
    par(mfrow=c(1,2),mar=c(5,5,5,5),bg='light gray',lwd=2)
    #initiating variables with empty vectors
    sulfate.mean<-c()
    nitrate.mean<-c()
    id.list<-c()
    cex<-1
    for(i in id){
        #used str_pad to deal with the leading zeros in the file names
        x<-str_pad(i,3,pad='0')
        filename<-paste0(directory,'/',x,'.csv')
        file<-read.csv(filename)
        #this if/else determines if the NA threshold level is reached
        nas.removed<-complete.cases(file)
        z<-file[nas.removed,]
        if(nrow(z)>threshold){
            sulfate.mean<-c(sulfate.mean,mean(z$sulfate))
            nitrate.mean<-c(nitrate.mean,mean(z$nitrate))
            id.list<-c(id.list,i)}
        else{print(paste('ID #',i,' has less than the maximum allowable NAs',sep=''))}}
    #if statements dealing with scaling and different quantities of location IDs
    if(length(id)>20){cex<-.7}
    if(length(id)>40){print('Recommended:use no more than 40 ID#s to ensure data points are distinct')}
    #statements for producing the two charts
    dotchart(sulfate.mean,col='blue',pch=16, lcolor='dimgray',labels=id.list,cex=cex)
    title('Sulfate Levels by Location',xlab='Mean Level of Sulfate',ylab='Monitor IDs')
    dotchart(nitrate.mean,col='red',pch=16, lcolor='dimgray',labels=id.list,cex=cex)
    title('Nitrate Levels by Location',xlab='Mean Level of Nitrate',ylab='Monitor IDs')
}

pollution_dc(specdata,id=1:20,threshold=100)
