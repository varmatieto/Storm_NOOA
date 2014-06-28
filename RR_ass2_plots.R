#############
# ADDITIONAL PLOTS 

topSD <- read.table("data/topSD.txt", sep=";", stringsAsFactors = F)



str(topSD)
dim(topSD)

topSD

str(SD_post)

library (ggplot2)

topSD5 <- topSD[topSD$qFA>500,]

ggplot( topSD5, aes(x=qFA, y=qIN, color= EVTYPE) ) +
    geom_point(size=15, show_guide = F ) +
    scale_color_brewer(palette="Set1") +
    ggtitle("most dangerous events")  + 
    xlab("fatalities") + ylab( "injuries") +
    geom_text(aes(label=EVTYPE), size=4, hjust=0.5, vjust=0.5, color="black") 
#ggsave ("plot/dangerous.png", width = 8, height = 6)



topSDc <- topSD[topSD$qCR>1e+9,]

ggplot( topSDc, aes(x=qPR/1e+9, y=qCR/1e+9, color= EVTYPE) ) +
  geom_point(size=15, show_guide = F  ) +
  scale_color_brewer(palette="Set1") +
  ggtitle("most damaging  events")  + 
  xlab("properties in B$") + ylab( "crops in B$") +
  geom_text(aes(label=EVTYPE), size=5, hjust=0.5, vjust=0.5, color="black") 
#ggsave ("plot/damaging.png", width = 8, height = 6)


#########################################
## how much those 16 events weight?
##########################################

SD_post <- read.table("data/SD_post.txt", sep=";", stringsAsFactors = F)

topevent <- read.table("data/topevent.txt")

str(SD_post)
topevent

top <-as.matrix(topevent)
topx<-as.vector(top)


SDET<-ddply(SD_post, .(EVTYPE), summarise,
            nobs= length(EVTYPE),
            qFA= sum(FATALITIES),
            qIN= sum(INJURIES),
            qPR= sum(PROPDMG),
            qCR= sum(CROPDMG)    )


str(SDET)

mycol<-colnames(SDET)[3:6]

sum16<-sapply(mycol, function(x) sum(SDET[,x][SDET$EVTYPE%in%topx]) )
sumtot<-sapply(mycol, function(x) sum(SDET[,x]))


round(sum16/sumtot,2)






########################################################

SDy<-ddply(SD_post, .(EVTYPE,BGN_DATE ), summarise,
            nobs= length(EVTYPE),
            qFA= sum(FATALITIES),
            qIN= sum(INJURIES),
            qPR= sum(PROPDMG),
            qCR= sum(CROPDMG)    )

str(SDy)

SDyy<-cbind(SDy, top=0)
SDyy$top[SDy$EVTYPE%in%topx]<-1
colnames(SDyy)<- c("event", "year", "nobs" ,"qFA" , "qIN", "qPR","qCR", "top")
SDyy$top<- as.factor(SDyy$top)
str(SDyy)

SDyy<-SDyy[order(SDyy$top),]

ggplot(SDyy, aes(y=qFA, x=year, fill=top)) + 
  geom_bar(stat="identity")

ggplot(SDyy, aes(y=fIN, x=year, fill=top)) + 
  geom_bar(stat="identity")


ggplot(SDyy, aes(y=qPR, x=year, fill=top)) + 
  geom_bar(stat="identity")

ggplot(SDyy, aes(y=qCR, x=year, fill=top)) + 
  geom_bar(stat="identity")


###############################################

SDs<-ddply(SD_post, .(EVTYPE,STATE ), summarise,
           nobs= length(EVTYPE),
           qFA= sum(FATALITIES),
           qIN= sum(INJURIES),
           qPR= sum(PROPDMG),
           qCR= sum(CROPDMG)    )

str(SDs)

SDss<-cbind(SDs, top=0)
SDss$top[SDs$EVTYPE%in%topx]<-1

colnames(SDss)<- c("event", "state", "nobs" ,"qFA" , "qIN", "qPR","qCR", "top")
SDss$top<- as.factor(SDss$top)
str(SDss)

SDss<-SDss[order(SDss$top),]

ggplot(SDss, aes(y=qFA, x=state, fill=top)) + 
  geom_bar(stat="identity")+ coord_flip()

ggplot(SDss, aes(y=qIN, x=state, fill=top)) + 
  geom_bar(stat="identity")+ coord_flip()


ggplot(SDss, aes(y=qPR, x=state, fill=top)) + 
  geom_bar(stat="identity")+ coord_flip()

ggplot(SDss, aes(y=qCR, x=state, fill=top)) + 
  geom_bar(stat="identity")+ coord_flip()




