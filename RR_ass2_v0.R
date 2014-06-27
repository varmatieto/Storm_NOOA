
#download.file("http://archive.ics.uci.edu/ml/machine-learning-databases/00275/
#Bike-Sharing-Dataset.zip", "ProjectData/Bike-Sharing-Dataset.zip")


url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"

dir("data")

if (!file.exists("data/StormData.cvs.bz2")) {
    download.file(url, "data/StormData.cvs.bz2", method = "internal")  
}

#SD <- read.csv('data/stormdata.csv.bz2', stringsAsFactors = F, strip.white = TRUE, na.strings = c("NA","?",""))


SD <- read.csv('data/stormdata.csv.bz2', stringsAsFactors = F)

sum(is.na(SD$CROPDMG))
sum(is.na(SD$PROPDMG))

sum(is.na(SD$CROPDMGEXP))
sum(is.na(SD$PROPDMGEXP))


#################################################################################
# dataset with 902k obs.

str(SD)
names(SD)

##########first operation : choose only relevant columns


rel_cols = c("EVTYPE", "BGN_DATE", "STATE", "FATALITIES", "INJURIES", "PROPDMG", "PROPDMGEXP", 
                  "CROPDMG", "CROPDMGEXP")
SD_clean <- SD[, which(names(SD) %in% rel_cols)]

str(SD_clean)
dimclean<-dim(SD_clean)[1]

##########and take year only 
sum(is.na(SD_clean$CROPDMG))
sum(is.na(SD_clean$PROPDMG))
summary(SD_clean$CROPDMG)


SD_clean$BGN_DATE <- as.Date(SD_clean$BGN_DATE, "%m/%d/%Y ")

SD_clean$BGN_DATE <- format (SD_clean$BGN_DATE, "%Y")

table(SD_clean$BGN_DATE )

##########second operation : recalculate damages using magnifier 


SD_clean$PROPDMGEXP <-as.factor(SD_clean$PROPDMGEXP)
SD_clean$CROPDMGEXP <-as.factor(SD_clean$CROPDMGEXP)

levels(SD_clean$PROPDMGEXP)
levels(SD_clean$CROPDMGEXP)

conversion_factor = list(`0` = c("", "-", "?", "+", "0"),
                    `1` = "1", 
                    `2` = c("2", "h", "H"),
                    `3` = c("3", "K", "k"), 
                    `4` = "4", 
                    `5` = "5", 
                    `6` = c("6", "M", "m"),
                    `7` = "7",
                    `8` = "8",
                    `9` = c("9","B", "b"))

levels(SD_clean$PROPDMGEXP) <- conversion_factor
levels(SD_clean$CROPDMGEXP) <- conversion_factor

table(SD_clean$PROPDMGEXP)
table(SD_clean$CROPDMGEXP)

SD_clean$PROPDMG = SD_clean$PROPDMG * (10^(as.integer(as.character(SD_clean$PROPDMGEXP))))
SD_clean$CROPDMG = SD_clean$CROPDMG * (10^(as.integer(as.character(SD_clean$CROPDMGEXP))))

summary (SD$PROPDMG )
summary (SD_clean$PROPDMG )
summary (SD$CROPDMG )
summary (SD_clean$CROPDMG )

str(SD_clean)

names(SD_clean)


##########third operation : look at event type and partially clean it
# there are 985different EVENT TYPE 

levtype<-length(unique(SD$EVTYPE))


headevents<-head(sort(table(SD_clean$EVTYPE), decreasing=TRUE), n=30)
kevents<-names(headevents)
headevents
kevents

# Non-informative "summary": only 3 obs.
  
SD_clean[(grepl("SUMMARY .+", SD_clean$EVTYPE)),]

sumh<-sum(headevents)
(sumh)/dimclean # 978% obs in first 20 labels

################## new test on the 985 events types
summary(SD_clean$FATALITIES)

fatByYearType <- tapply(SD_clean$FATALITIES,interaction(SD_clean$BGN_DATE,
                                                        SD_clean$EVTYPE),sum)
class(fatByYearType)

library (plyr)

SDxx<-ddply(SD_clean, .(EVTYPE), summarise,
            nobs= length(EVTYPE),
            qFA= sum(FATALITIES),
            qIN= sum(INJURIES),
            qPR= sum(PROPDMG),
            qCR= sum(CROPDMG)    
)

str(SDxx)

########## PROBLEM
# how to evaluate distribution?
#
summary (SDxx$qIN )
hist (log(SDxx$qIN ))
hist (SDxx$qIN, breaks=20)



limitIN <- quantile(SDxx$qIN, probs= c(0.97))[[1]]
dim (SDxx[SDxx$qIN>limitIN,])



summary (SDxx$qPR )
summary (SDxx$qCR  )


SDxx<- SDxx[order(SDxx$qFA, decreasing = T),]
hFA<-SDxx[1:30,1]


SDxx<- SDxx[order(SDxx$qIN, decreasing = T),]
hIN<-SDxx[1:30,1]

SDxx<- SDxx[order(SDxx$qPR, decreasing = T),]
hPR<-SDxx[1:30,1]

SDxx<- SDxx[order(SDxx$qCR, decreasing = T),]
hCR<-SDxx[1:30,1]

nkevents<-unique (append (hFA, append(hIN,append(hPR, hCR))))


kkevents<-unique (append (nkevents, kevents))
kkevents<-kkevents [order(kkevents)]
kkevents
ltevents<- length(kkevents)

####################################################################
kevent_tranf <- list( "heat"=c(8,12,23,24),
                      "hurricane" = c(31,32,33,34,48,49,50,53),
                      "cold" =c(3,4,10,11,18,19,20),
                      "wind" = c(29,30,45,46,47,51,52,42),
                      "rain" = c(25,26),
                      "fog" = c(5,17),
                      "waterspour" = c(55,21),
                      "tide" = c(43,44,28),
                      "ripcurrent" = c(39,40),
                      "flood" =c(13,14,15,16,41,54),
                      "winter" =c(2,35,59,60,61),
                      "marinestorm" =c(37,38),
                      "fire" = c(56,57,58))

sapply(kevent_tranf, function(x) kkevents[x])

length(names(kevent_tranf))

sum(sapply( kevent_tranf, length))


mylist<-c() 

for (i in names(kevent_tranf))
{
  mylist<-append(mylist, kevent_tranf[[i]])
}

length(mylist)

kkevents [!kkevents%in%kkevents[mylist]]

mylist[duplicated (mylist)] 

####################
SD_pre <- SD_clean # safe side
# SD_clean <- SD_pre

kkevents_post <- kkevents
######################


for (i in 1:length(names(kevent_tranf))){
  
  ivents<-kkevents[kevent_tranf[[i]]]
  kkevents_post [kkevents %in% ivents ]<-names (kevent_tranf[i])
}

postevents<-unique(kkevents_post)


for (i in 1:length(names(kevent_tranf))){
  
  ivents<-kkevents[kevent_tranf[[i]]]
  SD_clean$EVTYPE [SD_clean$EVTYPE %in% ivents ]<-names (kevent_tranf[i])
}

# write.table(SD_clean,"data/SD_clean.txt", sep=";")
#########################################



SDET<-ddply(SD_clean, .(EVTYPE), summarise,
            nobs= length(EVTYPE),
            qFA= sum(FATALITIES),
            qIN= sum(INJURIES),
            qPR= sum(PROPDMG),
            qCR= sum(CROPDMG)    )

###############################################

SDET<- SDET[order(SDET$qFA, decreasing = T),]
topFA<-SDET[1:10,1]

SDET[1:30,c(1,2,3)]


SDET<- SDET[order(SDET$qIN, decreasing = T),]
topIN<-SDET[1:10,1]

SDET[1:30,c(1,2,4)]


SDET<- SDET[order(SDET$qPR, decreasing = T),]
SDET[1:30,c(1,2,5)]

topPR<-SDET[1:10,1]

SDET<- SDET[order(SDET$qCR, decreasing = T),]
SDET[1:30,c(1,2,6)]

topCR<-SDET[1:10,1]


topevents<-unique (append (topFA, append(topIN,append(topPR, topCR))))

topevents<-topevents [order(topevents)]
topevents

topSD<-SDET[SDET$EVTYPE%in%topevents,]

write.table(topSD,"data/topSD.txt", sep=";", stringsAsFactors = F)

topSD <- read.table("data/topSD.txt", sep=";", stringsAsFactors = F)

str(topSD)


l1<-topSD[,c(1,2,3)]
l2<-topSD[,c(1,2,4)]
l3<-topSD[,c(1,2,5)]
l4<-topSD[,c(1,2,6)]

l1 <- cbind(l1, accident=c("F"))
l2 <- cbind(l2, accident=c("I"))
colnames(l1)<-c("event","n.events","n.accidents","accident")
colnames(l2)<-c("event","n.events","n.accidents","accident")
l12<-rbind(l1,l2)

l12<-l12[order (l12$event),]
l12

l3 <- cbind(l3, damage=c("P"))
l4 <- cbind(l4, damage=c("C"))
colnames(l3)<-c("event","n.events","tot.damage","damage")
colnames(l4)<-c("event","n.events","tot.damage","damage")
l34<-rbind(l3,l4)

l34<-l34[order (l34$event),]
l34



SDfinal<-cbind(l12,l34)
SDfinal


SDfinal<-SDfinal[order(SDfinal$event),]

SDfinal<-SDfinal[,c(-5,-6)]

write.table(SDfinal,"data/SDfinal.txt", sep=";")
#############################################
SDfinal <- read.table("data/SDfinal.txt", sep=";")

##########################################

## PRINTING

colnames(SDfinal)<-c("event","nevents",
                     "n.accidents","accident",
                     "tot.damage","damage")

SDfinal$accident <-as.factor(SDfinal$accident)
SDfinal$damage <-as.factor(SDfinal$damage)

levels(SDfinal$accident) <- c("fatalities", "injuries")
levels(SDfinal$damage) <- c("properties", "crops")


str(SDfinal)

library (ggplot2)

ggplot( SDfinal, aes(x=event, y=nevents/2000, fill=event) ) +
    geom_bar(stat="identity", show_guide = F  ) + coord_flip() +
    ggtitle("total number of event by type")  + 
    xlab("event") + ylab( "total number in thousands") 
#ggsave ("plot/aplot1.png")


SDfinal<- SDfinal[order(SDfinal$accident),]

ggplot( SDfinal, aes(x=event, y=n.accidents, fill=accident ) ) +
    geom_bar(stat="identity" ) + coord_flip() +
    ggtitle("total accidents by event type")  
#ggsave ("plot/aplot2.png")

SDfinal<- SDfinal[order(SDfinal$damage),]

ggplot( SDfinal, aes(x=event, y=tot.damage/1000000000, fill=damage ) ) +
    geom_bar(stat="identity" ) + coord_flip() +
    ggtitle("total damages by event type")  + 
    xlab("event") + ylab( "in Bl. dollars")
#ggsave ("plot/aplot3.png")


#