#############
# ADDITIONAL PLOTS 

topSD <- read.table("data/topSD.txt", sep=";", stringsAsFactors = F)

str(topSD)
topSD

library (ggplot2)

topSD5 <- topSD[topSD$qFA>500,]

ggplot( topSD5, aes(x=qFA, y=qIN) ) +
    geom_point(size=12, color= "gold" ) +
    ggtitle("most dangerous events")  + 
    xlab("fatalities") + ylab( "injuries") +
    geom_text(aes(label=EVTYPE), size=5, hjust=0.5, vjust=0.5, color="red") 


