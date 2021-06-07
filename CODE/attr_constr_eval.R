
############################
###CONSTRUCTION OF ATRIBUTES######
###########################
data$vikend <- weekdays(data$datum) %in% c("sobota", "nedelja")

letniCas <- function(data){
  v <- vector()
  for(datum in data$datum){
    if(datum == as.Date("2015-12-31") | datum >= as.Date("2016-12-21") 
       | datum < as.Date("2016-03-21")){
      v <- c(v, "ZIMA")
    }
    else if(datum >= as.Date("2016-03-21") & datum < as.Date("2016-06-21")){
      v <- c(v, "POMLAD")
    }
    else if(datum >= as.Date("2016-06-21") & datum < as.Date("2016-09-23")){
      v <- c(v, "LETO")
    }
    else{
      v <- c(v, "JESEN")
    }
  }
  v
}

data$letni_cas <- as.factor(letniCas(data))



#CHECKING IF ALL THE BUILDINGS HAVE MESUREMENTS FOR 2015-12-31
nlevels(as.factor(data$stavba)) == sum(data$datum == "2015-12-31")

#AVERAGE TEMPERATURE OF THE PREVIOUS DAY
sel <- data$datum > min(data$datum)
df <- data.frame(datum = vector(), stavba = integer(), 
                 pov_t_zr_prejsni_dan = double(), min_t_zr_prejsni_dan = double(), 
                 max_t_zr_prejsni_dan = double(), pov_por_prejsni_dan = double(),
                 min_por_prejsni_dan = double(), max_por_prejsni_dan = double(),
                 stringsAsFactors = T)
                 
for(i in 1:nrow(data)) {
  tempStavba <- data$stavba[i]
  tempDatum <- as.Date(data$datum[i])
  if(as.Date(data$datum[i]) == as.Date(min(data$datum))) {
    avgZrak <- data$temp_zraka[i]
    minZrak <- data$temp_zraka[i]
    maxZrak <- data$temp_zraka[i]
    
    avgPoraba <- data$poraba[i]
    minPoraba <- data$poraba[i]
    maxPoraba <- data$poraba[i]
  }
  else{
    sel <- data$datum == as.Date(tempDatum - 1) & data$stavba == tempStavba
    
    avgZrak <- mean(data$temp_zraka[sel])
    
    minZrak <- min(data$temp_zraka[sel])
    
    maxZrak <- max(data$temp_zraka[sel])
    
    avgPoraba <- mean(data$poraba[sel])
    
    minPoraba <- min(data$poraba[sel])
    
    maxPoraba <- max(data$poraba[sel])
    
  }
  df[i,] <- c(as.character(tempDatum), tempStavba, avgZrak, minZrak, maxZrak,
                                                avgPoraba, minPoraba, maxPoraba)
}

df$datum <- as.Date(df$datum)
df$stavba <- as.factor(df$stavba)
for(i in 3:8){
  df[, i] <- as.double(df[, i])
}
sum(is.nan(df$povp_temp_z_prejsni_dan))
sum(is.nan(df$pov_por_prejsni_dan))
levels(factor(df$datum[is.nan(df$pov_por_prejsni_dan)]))
#WE SEE THAT THERE ARE MISSING VALUES.
sel <- is.nan(df2$pov_por_prejsni_dan)

temp <- df[sel,]


for(i in 1:nrow(temp)){
  tempDatum <- as.Date(temp$datum[i])
  tempStavba <- temp$stavba[i]
  tempSel <- data$datum == tempDatum - 2 & data$stavba == tempStavba
  
  tempPoraba <- data$poraba[tempSel]
  tempZrak <- data$temp_zraka[tempSel]
  if(is.nan(mean(tempPoraba))){
    tempSel <- data$datum == tempDatum - 3 & data$stavba == tempStavba
    tempPoraba <- data$poraba[tempSel]
    tempZrak <- data$temp_zraka[tempSel]
  }
  temp$pov_por_prejsni_dan[i] <- mean(tempPoraba)
  temp$min_por_prejsni_dan[i] <- min(tempPoraba)
  temp$max_por_prejsni_dan[i] <- max(tempPoraba)
  
  temp$pov_t_zr_prejsni_dan[i] <- mean(tempZrak)
  temp$max_t_zr_prejsni_dan[i] <- max(tempZrak)
  temp$min_t_zr_prejsni_dan[i] <- min(tempZrak)
}
  
df$pov_t_zr_prejsni_dan[sel] <- temp$pov_t_zr_prejsni_dan
df$max_t_zr_prejsni_dan[sel] <- temp$max_t_zr_prejsni_dan
df$min_t_zr_prejsni_dan[sel] <- temp$min_t_zr_prejsni_dan

df$pov_por_prejsni_dan[sel] <- temp$pov_por_prejsni_dan
df$max_por_prejsni_dan[sel] <- temp$max_por_prejsni_dan
df$min_por_prejsni_dan[sel] <- temp$min_por_prejsni_dan


sum(is.nan(df$pov_t_zr_prejsni_dan))
#We minimised the NAN to 1467
dataFinal <- cbind(data, df)
dataFinal <- dataFinal[, !duplicated(colnames(dataFinal))]

#We delete the rows that are not complete
dataFinal <- dataFinal[complete.cases(dataFinal), ]





######################
##ATTRIBUTE EVALUATION###
#####################
#divide into regression and classification subsets
dataReg <- subset(dataFinal, select = -c(norm_poraba))
dataKlas <- subset(dataFinal, select = -c(poraba, pov_por_prejsni_dan, min_por_prejsni_dan, max_por_prejsni_dan))


#Information gain
sort(attrEval(norm_poraba ~ .,dataKlas,"InfGain"),decreasing = TRUE)

#Gini
sort(attrEval(norm_poraba ~ .,dataKlas,"Gini"),decreasing = TRUE)

#Gain Ratio
sort(attrEval(norm_poraba ~ .,dataKlas,"GainRatio"),decreasing = TRUE)

#Information Gain and Gini both over value the multivalued variable leto_izgrajanje,gain ratio clear;y lowers that overvalueing

#MDL
sort(attrEval(norm_poraba ~ .,dataKlas,"MDL"),decreasing = TRUE)

#MDL correctly values multivalued attributes, with letni_cas and vikend being the most valued.
#But these are all short-sighted, that is why we will use ReliefF

#ReliefF
sort(attrEval(norm_poraba ~ .,dataKlas,"ReliefFequalK"),decreasing = TRUE)

#Evaluation of attributes for Regression
sort(attrEval(poraba ~ .,dataReg,"MSEofMean"),decreasing = TRUE)
sort(attrEval(poraba ~ .,dataReg,"RReliefFexpRank"),decreasing = TRUE)

