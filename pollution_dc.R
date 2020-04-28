rm(list=ls())
library(stringr)

specdata<-'C:/Users/chris/OneDrive/Desktop/School Docs/Coursera Assignments/specdata'

par(mfrow=c(1,2))
pollution_dc<-function(directory,id=1:332,threshold=0){
    sulfate.mean<-c()
    nitrate.mean<-c()
    id.list<-c()
    for(i in id){
        #used str_pad to deal with the leading zeros in the id name
        x<-str_pad(i,3,pad='0')
        filename<-paste(directory,'/',x,'.csv',sep='')
        file<-read.csv(filename,header=T)
        #this determines if the NA threshold level is reached
        nas.removed<-complete.cases(file)
        z<-file[nas.removed,]
        if(nrow(z)>threshold){
        sulfate.mean<-c(sulfate.mean,mean(z$sulfate))
        nitrate.mean<-c(nitrate.mean,mean(z$nitrate))
        id.list<-c(id.list,i)
        }
        else{print(paste('ID #',id,' has less than the maximum allowable NAs',sep=''))}
    }
    dotchart(sulfate.mean,main='Sulfate Levels by Location',
        xlab='Mean Level of Sulfate',labels=id.list,ylab='Monitor IDs')
    dotchart(nitrate.mean,main='Nitrate Levels by Location',
             xlab='Mean Level of Nitrate',labels=id.list,ylab='Monitor IDs')
}

pollution_dc(specdata,id=1:20,threshold=100)
