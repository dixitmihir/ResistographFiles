#PeaksData reads a directory of text files and Generates feed
#and Drill curve data files in Excel

#Read Files and Generated Adjusted Data
peaksData<-function(Directory){
library(xlsx)
getwd()
setwd(Directory)
files<-list.files(path=Directory, pattern="*.txt")
for(fileName in files){
importdata <- read.table(fileName, skip=129, sep=";")
colnames(importdata)<-c("Drill Raw", "Feed Raw")
Adjustdata <- importdata/10
Adjustdata["Drill Peak"]<-NA
Adjustdata["Feed Peak"]<-NA
colnames(Adjustdata)<-c("Drill Adjusted", "Feed Adjusted", "Drill Peak", "Feed Peak")

#Create Table of Drill Peaks
DrillPeakData<-data.frame(PointNumber=integer(), DrillAdjusted=double(), RingNumber=integer(), RingSize=double())
colnames(DrillPeakData)<-c("Point Number", "Drill Adjusted", "Ring Number", "Ring Size")
DrillPeakData[1,1]=1
DrillPeakData[1,2]=Adjustdata[1,1]
DrillPeakData[1,3]=0
DrillPeakData[1,4]=0

#Create Table of Feed Peaks
FeedPeakData<-data.frame(PointNumber=integer(), FeedAdjusted=double(), RingNumber=integer(), RingSize=double())
colnames(FeedPeakData)<-c("Point Number", "Feed Adjusted", "Ring Number", "Ring Size")
FeedPeakData[1,1]=1
FeedPeakData[1,2]=Adjustdata[1,2]
FeedPeakData[1,3]=0
FeedPeakData[1,4]=0

#Populate DrillPeakData And Label Starting Point in DrillPeak Columnn
from<-2
to<-nrow(Adjustdata)-1
Adjustdata[1,3]<-"START"
rcount<-2

for(i in from:to){
  x<-i-1
  y<-i+1
  if(Adjustdata[i,1]>Adjustdata[x, 1]&&Adjustdata[i,1]>Adjustdata[y,1]){
    Adjustdata[i,3]="PEAK"
    DrillPeakData[rcount,1]=i
    DrillPeakData[rcount,2]=Adjustdata[i, 1]
    DrillPeakData[rcount, 3]=rcount-1
    rcount=rcount+1
  }
  else{
    Adjustdata[i,3]="*"
  }
  Adjustdata[nrow(Adjustdata), 3]="END"
  DrillPeakData[rcount, 1]=nrow(Adjustdata)
  DrillPeakData[rcount, 2]=Adjustdata[nrow(Adjustdata), 1]
  DrillPeakData[rcount, 3]=rcount-1
}

#populate Drill Peak Ring Size
LastDrillPeak<-nrow(DrillPeakData)
for(j in from:LastDrillPeak){
  DrillPeakData[j, 4]=(DrillPeakData[j, 1]-DrillPeakData[j-1,1])
}

#Populate Feed Peak Data
start<-2
end<-nrow(Adjustdata)-1
Adjustdata[1,4]<-"START"
scount<-2

for(i in start:end){
  x<-i-1
  y<-i+1
  if(Adjustdata[i,2]>Adjustdata[x, 2]&&Adjustdata[i,2]>Adjustdata[y,2]){
    Adjustdata[i,4]="PEAK"
    FeedPeakData[scount,1]=i
    FeedPeakData[scount,2]=Adjustdata[i, 2]
    FeedPeakData[scount, 3]=scount-1
    scount=scount+1
    
  }
  else{
    Adjustdata[i,4]="*"
  }
  Adjustdata[nrow(Adjustdata), 4]="END"
  FeedPeakData[scount, 1]=nrow(Adjustdata)
  FeedPeakData[scount, 2]=Adjustdata[nrow(Adjustdata), 2]
  FeedPeakData[scount, 3]=scount-1
  
}

#populate Feed Peak Ring Sizae
LastFeedPeak<-nrow(FeedPeakData)
for(j in from:LastFeedPeak){
  FeedPeakData[j, 4]=(FeedPeakData[j, 1]-FeedPeakData[j-1,1])
}

#generate ouptut file
outfile1<-sprintf("%s_Drill.xlsx",fileName)
outfile2<-sprintf("%s_Feed.xlsx", fileName)
write.xlsx(DrillPeakData, outfile1)
write.xlsx(FeedPeakData, outfile2)
dev.off()
}
}

