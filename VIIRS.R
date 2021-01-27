
V.archive<-read.csv("VIIRS_archive.csv", stringsAsFactors = F)
V.nrt<-read.csv("VIIRS_nrt.csv", stringsAsFactors = F)

head(V.archive)
tail(V.archive)
str(V.archive)

table(V.archive$INSTRUMENT)
table(V.archive$TYPE)
#0      2      3 
#879929  23440  39752 
#missing type 1, so no volcano
summary(V.archive)

summary(V.archive$BRIGHT_TI4)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#208.0   315.3   333.7   330.8   341.8   367.0 
V.archive[which.max(V.archive$BRIGHT_TI4),]
#max is at 21/01/2012


summary(V.archive$BRIGHT_TI5)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#217.9   289.3   293.1   292.7   297.6   380.0 

V.archive[which.max(V.archive$BRIGHT_TI5),]
# max is at 20/10/2015



#'data.frame':	943121 obs. of  16 variables:
#  $ LATITUDE  : num  2.417 -3.884 -0.498 -0.499 -2.201 ...
#$ LONGITUDE : num  118 115 109 109 113 ...
#$ BRIGHT_TI4: num  297 307 330 334 367 ...
#$ SCAN      : num  0.77 0.43 0.57 0.57 0.34 0.34 0.34 0.34 0.34 0.34 ...
#$ TRACK     : num  0.77 0.62 0.69 0.69 0.56 0.56 0.56 0.56 0.56 0.56 ...
#$ YEAR      : int  2012 2012 2012 2012 2012 2012 2012 2012 2012 2012 ...
#$ MONTH     : int  1 1 1 1 1 1 1 1 1 1 ...
#$ DAY       : int  20 20 21 21 21 21 21 21 21 21 ...
#$ ACQ_TIME  : int  1639 1821 520 520 520 520 520 520 520 520 ...
#$ SATELLITE : chr  "N" "N" "N" "N" ...
#$ INSTRUMENT: chr  "VIIRS" "VIIRS" "VIIRS" "VIIRS" ...
#$ CONFIDENCE: chr  "n" "n" "n" "n" ...
#$ VERSION   : int  1 1 1 1 1 1 1 1 1 1 ...
#$ BRIGHT_TI5: num  278 277 282 283 285 ...
#$ FRP       : num  0.9 1 7.9 7.4 10.7 10.7 10.1 10.1 3.4 3 ...
#$ TYPE      : int  0 2 0 0 0 0 0 0 0 0 ...


#CUTE PLOT #1
par(mfrow=c(1,1))
barplot(table(V.archive$FRP, V.archive$YEAR), 
        ylim=c(0,500000),
        col="darkorange",
        main="Barplot of Fire Radiative Power by Year",
        xlab="Year",
        ylab="FRP")
grid(col="lightblue")

#hyothesis: I will try to see how the type(2 or 3) is associated with FRP and brightness

V.archiveT2<-V.archive[V.archive$TYPE==2, ]
V.archiveT3<-V.archive[V.archive$TYPE==3, ]
V.archiveT0<-V.archive[V.archive$TYPE==0, ]



#PLOT #2
#FRP and YEAR



#all levels of confidence
barplot(table(V.archiveT0$FRP, V.archiveT0$YEAR), 
        ylim=c(0,400000),
        col="red",
        main="Fire Radiative Power in Presumed Forest Fires by Year (all levels of confidence)",
        xlab="Year",
        ylab="FRP")


par(mfrow=c(1,2))


barplot(table(V.archiveT0$FRP[V.archive$CONFIDENCE=="h"], V.archiveT0$YEAR[V.archive$CONFIDENCE=="h"]), 
        ylim=c(0,17000),
        col="red",
        main="FRP in Presumed Forest Fires by Year (HIGH CONFIDENCE)",
        xlab="Year",
        ylab="FRP")

barplot(table(V.archiveT0$FRP[V.archive$CONFIDENCE=="l"], V.archiveT0$YEAR[V.archive$CONFIDENCE=="l"]), 
        ylim=c(0,17000),
        col="red",
        main="FRP in Presumed Forest Fires by Year (LOW CONFIDENCE)",
        xlab="Year",
        ylab="FRP")




#brightness
barplot(table(V.archiveT0$BRIGHT_TI4, V.archiveT0$YEAR), 
        ylim=c(0,400000),
        col="red",
        main="Barplot of Satellite Perceived Brigthness in Presumed Forest Fires by Year",
        xlab="Year",
        ylab="FRP")
grid(col="red")

par(mfrow=c(1,2))
barplot(table(V.archiveT0$BRIGHT_TI4[V.archiveT0$CONFIDENCE=='h'], V.archiveT0$YEAR[V.archiveT0$CONFIDENCE=='h']), 
        ylim=c(0,15000),
        col="darkred",
        main="(HIGH CONF) Sat.Perceived Brigthness in Presumed Forest Fires by Year",
        xlab="Year",
        ylab="FRP")
grid(col="brown1")

barplot(table(V.archiveT0$BRIGHT_TI4[V.archiveT0$CONFIDENCE=='l'], V.archiveT0$YEAR[V.archiveT0$CONFIDENCE=='l']), 
        ylim=c(0,15000),
        col="darkred",
        main="(LOW CONF) Sat.Perceived Brigthness in Presumed Forest Fires by Year",
        xlab="Year",
        ylab="FRP")
grid(col="brown1")

#subset the data to  view only 2015, type 0, with 2 types of confidences (low and high)

par(mfrow=c(1,1))
V2015<-V.archiveT0[V.archiveT0$YEAR==2015, ]
boxplot(log(V2015$FRP, 10)~V2015$MONTH,
        col="firebrick1",
        xlab="Month",
        ylab="Log 10 of Fire Radiative Power",
        main="2015: Fire Radiative Power by Month")


V2015<-V.archiveT0[V.archiveT0$YEAR==2015, ]

boxplot(log(V2015$FRP[V2015$CONFIDENCE=='h'], 10)~V2015$MONTH[V2015$CONFIDENCE=='h'],
       
        col="firebrick1",
        xlab="Month",
        ylab="Log 10 of Fire Radiative Power",
        main="2015: Fire Radiative Power by Month")


plot(log(V2015$FRP, 10)~V2015$MONTH)
