
library(nycflights13)
library(ggplot2)

data <- flights
na<- complete.cases(data)
data_na<- data[na,]                                      ##Data set without NA                     
data_mean<- data_na[ ,c(4,5,6,7,13,14,15,16)]            ##Exact onle needed variables
head(data)


## Departure Delay satistic(best and worst airline)


delay<- tapply(data_na$dep_delay, data_na$carrier, mean)              ##Averrage delay for each airline
delay1<- data.frame(ID=airlines$carrier, Name=airlines$name, Averrage.departure.delay=delay)  ##Create new data frame and fill it
delay1<- delay1[order(delay1$Averrage.departure.delay),]              ##Sort data frame by averrage delay
rownames(delay1)<-NULL                                                ##After sorting id were mixed, here i assign them new RIGHT id
delay
  
##Arriving delay statistic
##Here I've made everythibg the same that before, exept change departure delay on arriving delay
 
delay_ar<-tapply(data_na$arr_delay, data_na$carrier, mean)
del<-data.frame(ID=airlines$carrier, Name=airlines$name, Averrage.arriving.delay=delay_ar)
del<-del[order(del$Averrage.arriving.delay), ]
rownames(del)<-NULL
del

##After analizing previous two tables we can clearly see best and worst airlines groups. "Frontier Airlines" is the worst airline. As we can see this airline has in averrage the longest departure and arriving delays. The best are "Alaska Airlines" and "Hawaiian Airlines"

##Problem 1
##Which airline is the most expeditious for the longest distances?**
##We should find out airline than spends the least time to overcome the longest distance.
##The problem is for every airline to find the longest route and then exact the minimum time that goes for overcoming it. 
##For example, the longest route for "American airlines" is 3500, but at different times of years spends different time for overcoming it. So, our job is to find the least time.
##And later we will compare speed for longest routes with general speed for each airline, result will be very interesting.

library(gridExtra)
require(reshape)
library(maps)
library(geosphere)
library(plyr)
library(splines)
library(MASS)
airports1<-read.csv("airports.csv", sep="\t")

##First, let's see how looks routes.
##Southwest Airlines
##Data for mamping

dani <- list()
dist <- list()
time<- list()
data<- list()
level <- levels(factor(data_na$carrier))                                 
for (i in 1:length(level)) {
  dani[[i]] <- data_na[data_na$carrier==level[i], c("carrier","air_time","origin","dest", "distance")] 
  dist[[i]] <- max(dani[[i]]$distance)                                 
  time[[i]] <- min(dani[[i]]$air_time[dani[[i]]$distance== dist[i]])
  data[[i]]<- dani[[i]][which(dani[[i]]$air_time==time[[i]] & dani[[i]]$distance==dist[[i]])[1],]
} 
good1<- data.frame(t(data.frame( sapply(data,'['))))
rownames(good1)=c(1:16)


##Origin airport coordinates
lev_or<-levels(factor(data_na$origin))
cord1<- c()
cord2<-c()
for (i in 1:length(lev_or)) {
 cord1[i] <- airports$lat[airports$faa==lev_or[i]]
 cord2[i] <- airports$lon[airports$faa==lev_or[i]]
   }
coord_orig<- data.frame(airports=lev_or, lat=cord1, lon=cord2)

##Destination airports coordinates
lev_des<-factor(do.call(rbind,good1$des))
cord1<- c()
cord2<-c()
for (i in 1:length(lev_des)) {
  cord1[i] <- airports$lat[airports$faa==lev_des[i]]
  cord2[i] <- airports$lon[airports$faa==lev_des[i]]
   }
coord_des<- data.frame(airports=lev_des, lat=cord1, lon=cord2)

##Mapping
xlim <- c(-171.738281, -56.601563)
ylim <- c(12.039321, 71.856229)

map("world", col="#192027", fill=TRUE, bg="#000000", lwd=0.05, xlim=xlim, ylim=ylim)
pal <- colorRampPalette(c("steelblue", "skyblue", "#1292db"))
colors <- pal(100)
##colors<- c("steelblue", colors()[grep("sky",colors())])
fsub <- data_na[data_na$carrier == "WN",]
maxf <- max(fsub$flight)
for (j in 1:length(fsub$carrier)) {
  air1 <- airports1[airports1$iata == fsub[j,]$origin,]
  air2 <- airports1[airports1$iata == fsub[j,]$dest,]
  inter <- gcIntermediate(c(air1[1,]$lon, air1[1,]$lat), c(air2[1,]$lon, air2[1,]$lat), n=100, addStartEnd=T)
  colindex <- round( (fsub[j,]$flight / maxf) * length(colors) )
  lines(inter, col=colors[colindex], lwd=0.6)
points(air2$lon, air2$lat, col="steelblue", pch=19, cex=0.4)
   }

##SkyWest Airlines

map("world", col="#192027", fill=TRUE, bg="#000000", lwd=0.05, xlim=xlim, ylim=ylim)
pal <- colorRampPalette(c("steelblue", "skyblue", "#1292db"))
colors <- pal(100)
##colors<- c("steelblue", colors()[grep("sky",colors())])
fsub <- data_na[data_na$carrier == "OO",]
maxf <- max(fsub$flight)
for (j in 1:length(fsub$carrier)) {
  air1 <- airports1[airports1$iata == fsub[j,]$origin,]
  air2 <- airports1[airports1$iata == fsub[j,]$dest,]
  inter <- gcIntermediate(c(air1[1,]$lon, air1[1,]$lat), c(air2[1,]$lon, air2[1,]$lat), n=100, addStartEnd=T)
  colindex <- round( (fsub[j,]$flight / maxf) * length(colors) )
  lines(inter, col=colors[colindex], lwd=0.6)
points(air2$lon, air2$lat, col="steelblue", pch=19, cex=0.4)
}

##Here in column "Distance" we have the longest distance for each airline, in column "Time" the least time to overcome it.

dani <- data.frame()
dist <- vector()
time<- vector()
level <- levels(factor(data_na$carrier))                                 ##Amout of airlines
for (i in 1:length(level)) {
  dani <- data_na[data_na$carrier==level[i], c("carrier","air_time", "distance")] 
  dist[i] <- max(dani$distance)                                  ##Max distance for each airline
  time[i] <- min(dani$air_time[dani$distance== max(dani$distance)]) }  ##Min time for overcoming it
faster <- data.frame(ID=level, NAme=airlines$name, Time= time, Distance=dist, Speed= dist/time)  ##New data frame, were i obtain speed for better estimate
faster_od<- faster[order(faster$Speed), ]                             ##Sorting data           
rownames(faster_od) <- NULL                                           ##Right id numbers
faster_od

 
##Lets see it on the plot.

mt1 <- mt <- data.frame(Id= rownames(faster_od), faster_od, row.names= NULL)
mt1$ID <- mt$ID<- as.factor(mt$ID)
mt1$ID <- factor(mt$ID, levels= mt[order(faster_od$Speed, decreasing= T), "ID"])   ##Ordering bars
theme_set(theme_gray(base_size = 20))         
ggplot(mt1, aes(x= ID, y= Speed)) + 
                  geom_bar(color="black", fill="#333341",stat="identity", ylim=c(0,15))+ylab("Speed")+ggtitle("Speed on long routes")+xlab(c(NULL))


##After analyzing our table and plot, clearly visible the best and worst airlines in this competition. Undoubted leaders are "United Air Lines" and "AirTran Airways Corporation". Loser is: "SkyWest Airlines".
##Display this:
##Longest routes for each airline

m<- mutate(data_na, speed= distance/air_time)
carriers<- unique(m$carrier)
pal <- colorRampPalette(c("#333333", "white", "#1292db"))
##colors <- pal(17000)
##colors<-c("red","blue","white","yellow", "green","violet","pink","brown","grey","orange","violetred2","deepskyblue1","palevioletred1","indianred1","#FFFF00","red4")
colors<- c("steelblue", colors()[grep("sky",colors())])
map("world", col="#192027", fill=TRUE, bg="#000009", lwd=0.005, xlim=xlim, ylim=ylim)
for (i in 1:length(carriers)) {
  fsub <- good1[good1$carrier == carriers[i],]
  colindex <- which(good1$carrier==carriers[i])
    air1 <- coord_orig[coord_orig$airports == fsub[1,]$origin,]
    air2 <- coord_des[coord_des$airports == fsub[1,]$dest,]
    inter <- gcIntermediate(c(air1[1,]$lon, air1[1,]$lat), c(air2[1,]$lon, air2[1,]$lat), n=100, addStartEnd=T
    lines(inter, col=colors[i], lwd=.4)
      }
points(coord_des$lon, coord_des$lat, col="steelblue", pch=19, cex=0.4)

##Now let's see, how speed changes for each airline in general case.
##General speed

t<-tapply(data_na$distance, data_na$carrier, mean)
t1<-tapply(data_na$air_time , data_na$carrier, mean)
genSpeed<- data.frame(Id=levels(factor(m$carrier)), Name=airlines$name, speed=t/t1)
genSpeed<- genSpeed[order(genSpeed$speed),]
rownames(genSpeed)=NULL
genSpeed

theme_set(theme_gray(base_size = 20))
ggplot(m, aes(x=reorder(carrier,speed, FUN=median), y=speed))+ geom_boxplot(fill="darkseagreen4")+
  ggtitle("General speed")+xlab(c(NULL))+ylab("Speed")+ggtitle("Airlines speed")

 
##This plot and table shows completely different results then before. Here, the best, fastest and the most comfortable is Hawaian airlines. We can make a conclusion, that estimating airlines only on long routes isn't right.

##Dependence between speed and distance.
##Classification on groups. Clustering.

clust<- data.frame(speed=m$speed, distance=m$distance, orig=m$origin)
d<-clust
d$orig=NULL
k<- kmeans(d,3)
m$cluster<- k$cluster
x1<- mean(m$distance[which(m$cluster==1)])
x2<- mean(m$distance[which(m$cluster==2)])
x3<- mean(m$distance[which(m$cluster==3)])
dat<- data.frame(clust=c(1,2,3), v=c(x1,x2,x3)) 
dat<-dat[order(dat$v, decreasing=F),]
dat1<- data.frame(dat, lev=c("short","medium", "long")) 
k$cluster<-replace(k$cluster, k$cluster== dat1$clust[1], levels(dat1$lev)[3])
k$cluster<-replace(k$cluster, k$cluster== dat1$clust[2], levels(dat1$lev)[2])
k$cluster<-replace(k$cluster, k$cluster== dat1$clust[3], levels(dat1$lev)[1])
m$cluster<- k$cluster
table(m$clust, m$carrier)
 
##Table shows, which of the category of routes airline more prefer.
theme_set(theme_gray(base_size = 20))
ggplot(m, aes(distance, speed))+geom_point(size=3, aes(color=factor(m$cluster)))+
  theme(legend.title = element_text(colour="steelblue", size=16, face="bold"))+
  scale_color_discrete(name="Routes:")+ stat_smooth(method = rlm, formula= y ~ ns(x,3), lwd=0.9)+ ggtitle("Classification")

 ##As we see, at the plot are visible three categories of routes. I tried to define, on which routes specialises each airline, long, medium or local.  

##Test on normality
library(nortest)
data <- flights
na<- complete.cases(data)
data_na<- data[na,]
m<- mutate(data_na, speed= distance/air_time)
  
shapiro.test(sample(m$speed, 5000))
ad.test(sample(m$speed, 5000))

hist(m$speed, prob=TRUE, breaks=5000, col="steelblue", main= "Test on normality")
curve(dnorm(x, mean= mean(m$speed), sd= sd(m$speed)), col="red",lwd=2, add=TRUE)


##Problem 2

##Arriving delay trend
 
   ##The problem is to figure out which airline makes steps for decreasing arriving delay.
   ## We will be comparing average delay at the beginning year with delaying at the end of the year.

dani<- data.frame()
level<- levels(factor(data_na$carrier))
month<-list()
new<- data.frame()
for (i in 1:length(level)) {
   dani<- data_na[data_na$carrier==level[i], c("month","carrier","arr_delay")]
   month[[i]]<- tapply(dani$arr_delay, dani$month, FUN=function(x)(round(mean(x), 2)))} ##Averrage delay for each month for each airline
good<- data.frame( sapply(month,'[',c(mean(1:3), mean(10:12))))  ##New data where i choose first 3 month at the beginning years and last 3 month
good[is.na(good)]<- 0                                        ##NA's substitute on zero
colnames(good)<- level; good$Month<- c("1:3","10:12")        ##New columns and rows names
good<- good[ ,c(17, 1:16)]                              ##Put on first place numeration of months
```
 
##First row it is the averrage arriving delay for each airline during first three months, second - during last three months.
good
df <- melt(good , id = 'Month', variable_name = "Airline")  ##Reshape data set 
head(df)
group1<- df$value[df$Month=="1:3"]                          ##Delay for first three months
group2<- df$value[df$Month=="10:12"]                        ##For last three
diff<- t(data.frame(Delay_change= group2-group1, row.names=level))  ##Here i obtain a difference between group1 and group2, it's gives a value that characterizes how changed arrive Delay during year

## Under each airlines ID we have the value that describe how changed delay during whole year.
diff
 
## Let's see it visually.
height<- rbind(group1, group2)
mp <- barplot(height, col=c("steelblue", rgb(0, 1, 0, 0.4)),beside = TRUE, names.arg= level, main="Arriving delay at the begining and end of the year")
legend("topright", 
       legend = c("First 3 months", "Last 3 months"), 
       fill = c("steelblue", rgb(0, 1, 0, 0.4)))

 
##So, what we can say here.
##We can see that "SkyWest Airlines" shows us very good and positive result. Here this airline show us the biggest decreasing in arriving delay. Should also be noted "ExpressJet Airlines".


##Problem 3
## We have three airports. I'm going to make analysis for both arriving and departure delaying, to see trend of delaying, and how it changed during the year.
##Origin departure delay

n1<- airports$name[airports$faa=="EWR"]        ##Exact a description for each of the airports id
n2<-airports$name[airports$faa=="JFK"]
n3<-airports$name[airports$faa=="LGA"]
##origin_name<- data.frame(id=c("EWR","JFK","LGA"), name=c(n1,n2,n3))
g1<-data_na[data_na$origin=="EWR",]         ##Exact from our data set variables only for "EWR" airport
g2<-data_na[data_na$origin=="JFK",]         ##Similitary only for "JFK"
g3<-data_na[data_na$origin=="LGA",]         ##For "LGA"
x1<- tapply(g1$dep_delay, g1$month, mean)   ##Calculate mean of depature delay for each month for first airport
x2<- tapply(g2$dep_delay, g2$month, mean)   ##For second
x3<- tapply(g3$dep_delay, g3$month, mean)   ##For third
delay_data <- data.frame(month=1:12, EWR=x1, JFK=x2, LGA=x3)  ##New data set where we have averrage of depature delay for each airport for each month
delay_data
 
## Let's look how it looks on plots.
s<-melt (delay_data, id = "month", variable_name = "airports")
levels(s$airports)<-c(n1,n2,n3)
theme_set(theme_gray(base_size = 20))
ggplot(s,aes(factor(month), value))+geom_point(size=3, color="steelblue")+facet_grid(.~airports)+xlab("Month")+ylab("Delay")+ggtitle("Airports delaying trend during year")+stat_smooth(method = rlm, formula= y ~ ns(x,3), lwd=0.9,(aes(group=1)))


##Origin arriving delay**
 
##Now let's calculate all the same for arriving delay.

x1<- tapply(g1$arr_delay, g1$month, mean)
x2<- tapply(g2$arr_delay, g2$month, mean)
x3<- tapply(g3$arr_delay, g3$month, mean)
delay_data2 <- data.frame(month=1:12, EWR=x1, JFK=x2, LGA=x3)
delay_data2

theme_set(theme_gray(base_size = 20))
s2<-melt (delay_data2, id = "month", variable_name = "airports")
levels(s2$airports)<-c(n1,n2,n3)
theme_set(theme_gray(base_size = 16))
ggplot(s2,aes(factor(month), value))+geom_point(size=3, color="steelblue")+facet_grid(.~airports)+xlab("Month")+ylab("Delay")+ggtitle("Airports delaying trend during year")+
  stat_smooth(method = rlm, formula= y ~ ns(x,3), lwd=0.9,(aes(group=1)))

 
##Conclusion.
##We can see that in both cases in summer period of year we have a big increasing of delays. What causes this? 
##Answer very simple. In this period of year in several times growing amount of tourist, and it's causes delays. What's interesting that in both cases present big decreasing of delays in autumn, it can be explained that  this period of year is not so interesting for tourists to have trips and travelling, thats why airports isn't so loaded in autumn, and stuff have enough time to prepare passanges for flying.
##So, it may be useful for summer time increase number of stuff in every airports and decrease it's for autumn, it must be profitable.


