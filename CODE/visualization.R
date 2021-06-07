data <- read.table("dataSem1.txt", sep=",", header=TRUE, stringsAsFactors = T)
head(data)
nrow(data)
summary(data)

data$datum <- as.Date(data$datum)
data$ura <- as.factor(data$ura)
yTitle <- "Electricity usage(kWh)"
sel <- data$ura == "11" & data$stavba == "1"
plot(data$datum[sel], data$poraba[sel], type = "l", 
     xlab="Date", ylab=yTitle, 
     main="Usage of electricity throughout the year 2016 at building '1'")
#higher air temperature cuses more energy use
plot(data$temp_zraka[sel], data$poraba[sel], xlab="Air temperature (C)",
     ylab=yTitle, 
     main="Usage of elecrticty depending on the air temperature")

cor(data$temp_zraka[sel], data$poraba[sel], method = "pearson")
#cor 0.7956


selSummer <- data$namembnost == "izobrazevalna" & 
            data$datum >= as.Date("2016-06-01") & 
            data$datum <= as.Date("2016-09-01")
            
selWinter <- data$namembnost == "izobrazevalna" & 
             data$datum <= as.Date("2016-05-30") | 
             data$datum >=as.Date("2016-09-02")

selSchools <- data$namembnost == "izobrazevalna"

schools <- data[selSchools,]


#we select a sample from schools and plot them to see if there is any correlation btw the summer break and the school year
sample <- sample(1:nrow(schools), 4)
selH <-schools$ura == "11"
sel <- schools$stavba == "17" & selH
#sample = 17 33 39 174
schoolsStavbe <- schools[sample,"stavba"]

plot(schools$datum[sel], schools$poraba[sel], 
     type="l", xlab="Date", ylab="Electricity usage(kWh)",
     main="Educational building 17")

sel <- schools$stavba == "33" & selH
plot(schools$datum[sel], schools$poraba[sel], type="l", 
     xlab="Date", ylab=yTitle, main="Educational building 33")

sel <- schools$stavba == "39" & selH
plot(schools$datum[sel], schools$poraba[sel], type="l",
     xlab="Date", ylab = yTitle, main ="Educational building 39")

sel <- schools$stavba == "174" & selH
plot(schools$datum[sel], schools$poraba[sel], type="l", 
     xlab="Date",ylab=yTitle, main="Educational building 174")

boxplot(data$poraba[selSummer], data$poraba[selWinter], 
        names = c("Summer break", "Scholl working"))

#checking if there is any corr btw the weekend and working days
selWeekend <- weekdays(data$datum) %in% c("sobota", "nedelja")
selWeekDays <- weekdays(data$datum) %in% 
  c("ponedeljek", "torek", "sreda", "četrtek", "petek")
  
boxplot(data$poraba[selWeekend] / 2, data$poraba[selWeekDays] / 5,
        names=c("Weekend", "Weekdays"))
abline(h=median(data$poraba[selWeekend] / 2), col="green")
abline(h=median(data$poraba[selWeekDays] / 5), col="blue")

selWeekendHouses <- data$namembnost == "stanovanjska" & selWeekend
selWeekendJavno <- data$namembnost == "javno_storitvena" & selWeekend

boxplot(data$poraba[selWeekendHouses], data$poraba[selWeekendOffice], 
        ylab = "Poraba elektrike(kWh)" ,
        names=c("Stanovanjska", "Javno storitvena"), 
        main="Elektrika, porabljena ob koncu tedna.")
        
abline(h=median(data$poraba[selWeekendHouses]), col="red")
abline(h=median(data$poraba[selWeekendOffice]), col="blue")
legend("topleft", legend=c("Medijana stanovanj", "Medijana javno storitvena"),
       col=c("red", "blue"), lty=1:1, cex=0.8,
       box.lty=2, box.lwd=2, box.col="green")

selWeekHouses <- data$namembnost == "stanovanjska" & selWeekDays
#doesn't make sence bc there are multiple weekdays and 2 days for weekend
boxplot(data$poraba[selWeekendHouses] / 2, data$poraba[selWeekHouses] / 5, 
        ylab = yTitle, names =c("Weekend", "Weekdays"), main="Stanovanja")
abline(h=mean(data$poraba[selWeekendHouses] / 2), col="red")
abline(h=mean(data$poraba[selWeekHouses] / 5), col="blue")


tab <- table(data$datum, data$norm_poraba)
tabAll <- sum(colSums(tab))
ratioNizka <- (tab[,1] + tab[,4]) / tabAll
barplot(ratioNizka, xlab = "Date", ylab=Ratio(N))

ratioVZeloV <- (tab[,3] + tab[,5]) / tabAll
barplot(ratio)
plot(x=seq(as.Date("2015-12-31"), as.Date("2016-12-31"), by="days"), 
     y=ratioVZeloV, type="l",
     xlab="Dates", ylab="(VISOKA + ZELOVISOKA) / ALL",
     main = "Ratio of HIGH(VISOKA, ZELO_VISOKA) electricty 
     usage throughout the year ")
plot(x=seq(as.Date("2015-12-31"), as.Date("2016-12-31"), by="days"), y=ratioNizka, type="l",
     xlab="Dates", ylab="(NIZKA) / ALL", main="The ratio of LOW(NIZKA, ZELONIZKA) electricity usage 
     through the year")
#suggesting that we should add an atribute for the season

