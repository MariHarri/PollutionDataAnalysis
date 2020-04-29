rm(list=ls())
library(stringr)

specdata<-'C:/Users/chris/OneDrive/Desktop/School Docs/Coursera Assignments/specdata'

par(mfrow=c(1,2),mar=c(5,5,5,5),bg='light gray',lwd=2)
pollution_dc<-function(directory,id=1:332,threshold=0){
    sulfate.mean<-c()
    nitrate.mean<-c()
    id.list<-c()
    for(i in id){
        #used str_pad to deal with the leading zeros in the id name
        x<-str_pad(i,3,pad='0')
        filename<-paste0(directory,'/',x,'.csv')
        file<-read.csv(filename)
        #this determines if the NA threshold level is reached
        nas.removed<-complete.cases(file)
        z<-file[nas.removed,]
        if(nrow(z)>threshold){
            sulfate.mean<-c(sulfate.mean,mean(z$sulfate))
            nitrate.mean<-c(nitrate.mean,mean(z$nitrate))
            id.list<-c(id.list,i)
        }
        else{print(paste('ID #',i,' has less than the maximum allowable NAs',sep=''))}
    }
    if(length(id)<10){
        print('Recommended:use at least 10 ID#s')
    }
    if(length(id)>40&length(id)<40){
        print('Recommended:use less than 40 ID#s')
        par(cex=.7)
    }
    dotchart(sulfate.mean,main='Sulfate Levels by Location',col='blue',pch=16,
        lcolor='dimgray',xlab='Mean Level of Sulfate',labels=id.list,ylab='Monitor IDs')
    dotchart(nitrate.mean,main='Nitrate Levels by Location', col='red',pch=16,
        lcolor='dimgray',xlab='Mean Level of Nitrate',labels=id.list,ylab='Monitor IDs')

}

pollution_dc(specdata,id=1:39,threshold=100)
