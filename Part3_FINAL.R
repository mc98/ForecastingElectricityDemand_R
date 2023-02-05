# Team C: Part 1
#   Michel Chamoun 
#   Camille Duchesne
#   Mitchel Karkheck
#   Michael Gimple
#   Gurpreet Singh
#--------------------------------------------------------------------
#=================70 characters per line constraint==================
#--------------------------------------------------------------------
getwd()

setwd(choose.dir())

load("ercotdata.RData")
head(ercotdata)


library(timeSeries)
library(forecast)

#convert rows to CST
row_val = 
  as.character(as.POSIXct(rownames(ercotdata),
                          format="%Y-%m-%d %H")- 6*60*60)

missing = which(is.na(row_val))
#Fill in missing times
for(i in missing){
  row_val[i] = 
    as.character(as.POSIXct(row_val[i-1],
      format = "%Y-%m-%d %H" ) + 60*60)
}
#Remove first 5 and last 19 hours to only
#                       get complete days

row_val = row_val[6:87653]


North = ercotdata[,4][6:87653]

missing_col = which(is.na(North))

North[missing_col] = (North[missing_col + 1]
              + North[missing_col - 1]) / 2

#Combine to make new time series
North_Energy = timeSeries(North, row_val)
colnames(North_Energy) = "Energy"

## --- Max Daily Values ---

by <-timeSequence(from=start(North_Energy),
                  to=end(North_Energy), by="day")
num_days = length(North_Energy) / 24
#Account for daylight savings time
#The last day in each is a filler
nov_days = c(309,673,1037,1401,1772,2136,2500,2864,3228,3599,3800)
march_days = c(71,435,799,1163,1534,1898,2262,2626,2990,3361,3700)

daily_max = rep(0,num_days)

#For each day loop through the 24 hours of each
#day to find max daily load

#Day light savings days will have 23 or 25 hours

#Adjust for hour missing in days between March and November

for(i in 1: length(daily_max)){
  local_max = 0
  if(march_days[1]< nov_days[1]){
    #Between November and March
    if(i %in% nov_days){
      for(j in 1:25){
        if(North_Energy[((i-1) *24) + j ]> local_max){
          local_max = North_Energy[((i-1)*24) + j]
        }
      }
      nov_days = nov_days[-1]
    }
    
    if(i %in% march_days){
      for(j in 1:23){
        if(North_Energy[((i-1) *24) + j ]> local_max){
          local_max = North_Energy[((i-1)*24) + j]
        }
      }
      march_days = march_days[-1]
    } else{
      for(j in 1:24){
        if(i == 1){
          if(North_Energy[j] > local_max){
            local_max = North_Energy[j]
          }
        }
        if(i>1){
          if(North_Energy[((i-1) *24) + j ] 
             > local_max){
            local_max = 
              North_Energy[((i-1)*24) + j]
          }
        }
        
      }
    }
  }else{

    if(i %in% nov_days){
      for(j in 1:25){
        if(North_Energy[((i-1) *24) - 1 + j ] > local_max){
          local_max = North_Energy[((i-1)*24) - 1 + j]
        }
      }
      nov_days = nov_days[-1]
    }
    if(i %in% march_days){
      for(j in 1:23){
        if(North_Energy[((i-1) *24) - 1 + j ] > local_max){
          local_max =North_Energy[((i-1)*24) - 1 + j]
        }
        print(row_val[((i-1) *24) + j ])
        
      }
      march_days = march_days[-1]
    } else{
      for(j in 1:24){
        if(i == 1){
          if(North_Energy[j] > local_max){
            local_max = North_Energy[j]
          }
        }
        if(i>1){
          if(North_Energy[((i-1) *24) + j - 1 ]> local_max){
            local_max = North_Energy[((i-1)*24) + j - 1]
          }
        }
        
      }
    }
  }
  
  daily_max[i] = local_max
}
max_usage = timeSeries(daily_max, by)
colnames(max_usage) = "Energy Usage"

#--------------------------------------------------------------------
#=================70 characters per line constraint==================
#--------------------------------------------------------------------

#=====================================================
#dummy variables for each month
mon_var <- outer(rep(month.abb, length = 10*12),
                 month.abb, "==") + 0
## the "+ 0" will convert logical to numeric
dim(mon_var)
dimnames(mon_var)[[2]] <- month.abb #mon_var is a list of 1 & 0
head(mon_var)

#=====================================================
#dummy variables for public holidays
#list of all public holidays in US from 2012 to 2021
holiday_var <- holidayNERC(2012:2021) 

#dummy variables for weekend
tS = timeSequence(from = paste("2012", "-01-01", sep = ""),
                  to = paste("2021", "-12-30", sep = ""))
tS
weknd_var <- isWeekend(tS)

#=====================================================
#--------------------------------------------------------------------
#=================70 characters per line constraint==================
#--------------------------------------------------------------------
max_usage1 = timeSeries(daily_max, by)
max_usage1.ts <- ts(max_usage1, start=c(2012,1), end=c(2020,1),
                    frequency = (365))
#---------------------------#NAIVE METHODS---------------------------
fcastS <- c(2018,1)
fcastE <- c(2020,1)
## Generate Naive forecast
naive_forecast <-naive(max_usage1.ts,h=1)
forecast <- window(naive_forecast$fitted, start=fcastS, end=fcastE)
observed <- window(naive_forecast$x, start=fcastS, end=fcastE)
bias1 <- mean(forecast-observed)
pbias1 <- mean((forecast-observed)/observed)*100
mape1 <- mean(abs((forecast-observed)/observed)*100)

#Mooving Average over 3 days
naive3d <- zoo::rollmean(max_usage1.ts, 3, align="right")
naive3 <- naive(naive3d, h=1)
forecast3 <- window(naive3$fitted, start=fcastS, end=fcastE)
observed3 <- window(max_usage1.ts, start=fcastS, end=fcastE)
bias3 <- mean(forecast3-observed3)
pbias3 <- mean((forecast3-observed3)/observed3)*100
mape3 <- mean(abs((forecast3-observed3)/observed3)*100)

cat("Using function <accuracy>:","\n")
print(accuracy(forecast, observed)[,1:5])
print(accuracy(forecast3, observed3)[,1:5])

# ======Diebold Mariano======
# All p-values are small ==> reject the null hypothesis

# Naive Vs moving average 3 days
print(dm.test((forecast-observed),(forecast3-observed3)))

#====================================================================
#-------------------------------Part2--------------------------------
#====================================================================

var_lub <- read.csv('Whichita.csv')   
head(var_lub)
var_lub<-var_lub[var_lub$STATION=="USW00013966",]

#sub-setting: keeping only relevant data points
myvars <- c("DATE","STATION","NAME","TMAX","TMIN","AWND","PRCP")
var_lub <- var_lub[myvars]

PreProcess <- function(input_dataset){
  #calculating HDD
  input_dataset$TAVG = ((input_dataset$TMAX)+(input_dataset$TMIN))/2
  input_dataset$HDD <- pmax(10-input_dataset$TAVG,0) 
  #base_T = 11 deg Celsius, (explained in literature)
  #calculating CDD
  input_dataset$CDD <- pmax(input_dataset$TAVG-18,0) 
  #base_T = 21 deg Celsius, (explained in literature)
  
  #calculating Wind Chill
  input_dataset$WindChill = rep(0,nrow(input_dataset))
  for(i in 1: length(input_dataset$WindChill)){
    CPt = 0
    if(input_dataset$TAVG[i] < 15){
      CPt =((input_dataset$AWND[i])^0.5)*(15-input_dataset$TAVG[i])
    }
    else{
      CPt = 0
    }
    input_dataset$WindChill[i] = CPt
  }
  return(input_dataset)
}

#=====================================================
#dummy variables for each month
mon_var <- outer(rep(month.abb, length = 10*12),
                 month.abb, "==") + 0
## the "+ 0" will convert logical to numeric
dim(mon_var)
dimnames(mon_var)[[2]] <- month.abb #mon_var is a list of 1 & 0
head(mon_var)

#=====================================================
#dummy variables for public holidays
#list of all public holidays in US from 2012 to 2021
holiday_var <- holidayNERC(2012:2021) 

#dummy variables for weekend
tS = timeSequence(from = paste("2012", "-01-01", sep = ""),
                  to = paste("2021", "-12-30", sep = ""))
tS
weknd_var <- isWeekend(tS)

#=====================================================

#====================================================================
#-------------With days of the week instead  of weekend--------------
#====================================================================

processed_data = PreProcess(var_lub)
processed_data$DATE=as.Date(processed_data$DATE,"%Y-%m-%d")
processed_data = processed_data[1:length(processed_data$DATE)-1,]
summary(processed_data)

Yt = timeSeries(max_usage$`Energy Usage`,processed_data$DATE)
Yt = window(Yt,start=timeDate("2012-01-01",format="%Y-%m-%d"),
            end=timeDate("2017-12-31",format="%Y-%m-%d"))

train_df <- processed_data[processed_data$DATE <=
                             as.Date("2017-12-31"),]

DATE = train_df$DATE
  
  # choosing the variables
vars = c("TMAX","TMIN","PRCP","HDD","CDD","WindChill")
  
  # HDDt
HDDt = timeSeries(train_df$HDD, train_df$DATE)
  # CDDt
CDDt = timeSeries(train_df$CDD,train_df$DATE)
  # isHolidayt
holiday_var <- as.Date(holidayNERC(2012:2021),format="%Y-%m-%d") 
df_holiday = data.frame(DATE)
df_holiday$isHoliday = ifelse(DATE %in% holiday_var, 1, 0)
isHolidayt = timeSeries(df_holiday$isHoliday ,DATE)

  # include month variable
asdate = as.Date(DATE,format = "%Y-%m-%d")
x_month = format(asdate,"%m")
train_df$January <- ifelse(x_month == "01", 1, 0)
train_df$February <- ifelse(x_month == "02", 1, 0)
train_df$March <- ifelse(x_month == "03", 1, 0)
train_df$April <- ifelse(x_month == "04", 1, 0)
train_df$May <- ifelse(x_month == "05", 1, 0)
train_df$June <- ifelse(x_month == "06", 1, 0)
train_df$July <- ifelse(x_month == "07", 1, 0)
train_df$August <- ifelse(x_month == "08", 1, 0)
train_df$September <- ifelse(x_month == "09", 1, 0)
train_df$October <- ifelse(x_month == "10", 1, 0)
train_df$November <- ifelse(x_month == "11", 1, 0)
train_df$December <- ifelse(x_month == "12", 1, 0)
  
train_df$weekday_var = weekdays(asdate)
train_df$Monday <- ifelse(train_df$weekday_var == "Monday", 1, 0)
train_df$Tuesday <- ifelse(train_df$weekday_var == "Tuesday", 1, 0)
train_df$Wednesday <- 
  ifelse(train_df$weekday_var == "Wednesday", 1, 0)
train_df$Thrusday <- ifelse(train_df$weekday_var == "Thursday", 1, 0)
train_df$Friday <- ifelse(train_df$weekday_var == "Friday", 1, 0)
train_df$Saturday <- ifelse(train_df$weekday_var == "Saturday", 1, 0)
train_df = subset(train_df, select = -c(weekday_var))
  
complete_train_df = data.frame(train_df$WindChill, HDDt$TS.1,
                               CDDt$TS.1,lag(HDDt, 1)$TS.1,
                               lag(HDDt, 2)$TS.1,
                               lag(CDDt, 1)$TS.1,lag(CDDt, 2)$TS.1,
                               isHolidayt$TS.1,
                               lag(isHolidayt, 1)$TS.1,
                               train_df$January,
                               train_df$February,train_df$March,
                               train_df$April,train_df$May,
                               train_df$June,train_df$July,
                               train_df$August,train_df$September,
                               train_df$October,train_df$November,
                               train_df$Monday,train_df$Tuesday,
                               train_df$Wednesday,train_df$Thrusday,
                               train_df$Friday,train_df$Saturday,
                               Yt$TS.1)

complete_train_df$lag.HDDt..1..TS.1[is.na(
  complete_train_df$lag.HDDt..1..TS.1)] <- 	3.35
complete_train_df$lag.HDDt..2..TS.1[is.na(
  complete_train_df$lag.HDDt..2..TS.1)] <- 	3.35
complete_train_df$lag.CDDt..1..TS.1[is.na(
  complete_train_df$lag.CDDt..1..TS.1)] <- 	0
complete_train_df$lag.CDDt..2..TS.1[is.na(
  complete_train_df$lag.CDDt..2..TS.1)] <- 	0
complete_train_df$lag.isHolidayt..1..TS.1[is.na(
  complete_train_df$lag.isHolidayt..1..TS.1)] <- 	0
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# same thing as above but for validation datasets
Yt = timeSeries(max_usage$`Energy Usage`,processed_data$DATE)
Yt_val = window(Yt,start=timeDate("2018-01-01",format="%Y-%m-%d"),
            end=timeDate("2019-12-31",format="%Y-%m-%d"))

val_df <- processed_data[processed_data$DATE >= 
                           as.Date("2018-01-01")& 
                           processed_data$DATE <= 
                           as.Date("2019-12-31"),]

DATE = val_df$DATE

# choosing the variables
vars = c("TMAX","TMIN","PRCP","HDD","CDD","WindChill")

# HDDt
HDDt = timeSeries(val_df$HDD, val_df$DATE)
# CDDt
CDDt = timeSeries(val_df$CDD,val_df$DATE)
# isHolidayt
holiday_var <- as.Date(holidayNERC(2012:2021),format="%Y-%m-%d") 
df_holiday = data.frame(DATE)
df_holiday$isHoliday = ifelse(DATE %in% holiday_var, 1, 0)
isHolidayt = timeSeries(df_holiday$isHoliday ,DATE)

# include month variable
asdate = as.Date(DATE,format = "%Y-%m-%d")
x_month = format(asdate,"%m")
val_df$January <- ifelse(x_month == "01", 1, 0)
val_df$February <- ifelse(x_month == "02", 1, 0)
val_df$March <- ifelse(x_month == "03", 1, 0)
val_df$April <- ifelse(x_month == "04", 1, 0)
val_df$May <- ifelse(x_month == "05", 1, 0)
val_df$June <- ifelse(x_month == "06", 1, 0)
val_df$July <- ifelse(x_month == "07", 1, 0)
val_df$August <- ifelse(x_month == "08", 1, 0)
val_df$September <- ifelse(x_month == "09", 1, 0)
val_df$October <- ifelse(x_month == "10", 1, 0)
val_df$November <- ifelse(x_month == "11", 1, 0)
val_df$December <- ifelse(x_month == "12", 1, 0)

val_df$weekday_var = weekdays(asdate)
val_df$Monday <- ifelse(val_df$weekday_var == "Monday", 1, 0)
val_df$Tuesday <- ifelse(val_df$weekday_var == "Tuesday", 1, 0)
val_df$Wednesday <- ifelse(val_df$weekday_var == "Wednesday", 1, 0)
val_df$Thrusday <- ifelse(val_df$weekday_var == "Thursday", 1, 0)
val_df$Friday <- ifelse(val_df$weekday_var == "Friday", 1, 0)
val_df$Saturday <- ifelse(val_df$weekday_var == "Saturday", 1, 0)
val_df = subset(val_df, select = -c(weekday_var))

complete_val_df = data.frame(val_df$WindChill, HDDt$TS.1,CDDt$TS.1,
                             lag(HDDt, 1)$TS.1,
                               lag(HDDt, 2)$TS.1,lag(CDDt, 1)$TS.1,
                             lag(CDDt, 2)$TS.1,
                               isHolidayt$TS.1,
                             lag(isHolidayt, 1)$TS.1,val_df$January,
                               val_df$February,val_df$March,
                             val_df$April,val_df$May,
                               val_df$June,val_df$July,
                             val_df$August,val_df$September,
                               val_df$October,val_df$November,
                             val_df$Monday,val_df$Tuesday,
                               val_df$Wednesday,val_df$Thrusday,
                             val_df$Friday,val_df$Saturday,
                               Yt_val$TS.1)

complete_val_df$lag.HDDt..1..TS.1[is.na(
  complete_val_df$lag.HDDt..1..TS.1)] <- 	17.95
complete_val_df$lag.HDDt..2..TS.1[is.na(
  complete_val_df$lag.HDDt..2..TS.1)] <- 	17.95
complete_val_df$lag.CDDt..1..TS.1[is.na(
  complete_val_df$lag.CDDt..1..TS.1)] <- 	0
complete_val_df$lag.CDDt..2..TS.1[is.na(
  complete_val_df$lag.CDDt..2..TS.1)] <- 	0
complete_val_df$lag.isHolidayt..1..TS.1[is.na(
  complete_val_df$lag.isHolidayt..1..TS.1)] <- 	0

#____________________________________________________________________
# same thing as above but for test datasets
Yt = timeSeries(max_usage$`Energy Usage`,processed_data$DATE)
Yt_test = window(Yt,start=timeDate("2020-01-01",format="%Y-%m-%d"),
                end=timeDate("2021-12-31",format="%Y-%m-%d"))

test_df <- processed_data[processed_data$DATE >= 
                            as.Date("2020-01-01")& 
                            processed_data$DATE <= 
                            as.Date("2021-12-31"),]

DATE = test_df$DATE

# choosing the variables
vars = c("TMAX","TMIN","PRCP","HDD","CDD","WindChill")

# HDDt
HDDt = timeSeries(test_df$HDD, test_df$DATE)
# CDDt
CDDt = timeSeries(test_df$CDD,test_df$DATE)
# isHolidayt
holiday_var <- as.Date(holidayNERC(2012:2021),format="%Y-%m-%d") 
df_holiday = data.frame(DATE)
df_holiday$isHoliday = ifelse(DATE %in% holiday_var, 1, 0)
isHolidayt = timeSeries(df_holiday$isHoliday ,DATE)

# include month variable
asdate = as.Date(DATE,format = "%Y-%m-%d")
x_month = format(asdate,"%m")
test_df$January <- ifelse(x_month == "01", 1, 0)
test_df$February <- ifelse(x_month == "02", 1, 0)
test_df$March <- ifelse(x_month == "03", 1, 0)
test_df$April <- ifelse(x_month == "04", 1, 0)
test_df$May <- ifelse(x_month == "05", 1, 0)
test_df$June <- ifelse(x_month == "06", 1, 0)
test_df$July <- ifelse(x_month == "07", 1, 0)
test_df$August <- ifelse(x_month == "08", 1, 0)
test_df$September <- ifelse(x_month == "09", 1, 0)
test_df$October <- ifelse(x_month == "10", 1, 0)
test_df$November <- ifelse(x_month == "11", 1, 0)
test_df$December <- ifelse(x_month == "12", 1, 0)

test_df$weekday_var = weekdays(asdate)
test_df$Monday <- ifelse(test_df$weekday_var == "Monday", 1, 0)
test_df$Tuesday <- ifelse(test_df$weekday_var == "Tuesday", 1, 0)
test_df$Wednesday <- ifelse(test_df$weekday_var == "Wednesday", 1, 0)
test_df$Thrusday <- ifelse(test_df$weekday_var == "Thursday", 1, 0)
test_df$Friday <- ifelse(test_df$weekday_var == "Friday", 1, 0)
test_df$Saturday <- ifelse(test_df$weekday_var == "Saturday", 1, 0)
test_df = subset(test_df, select = -c(weekday_var))

complete_test_df = data.frame(test_df$WindChill,HDDt$TS.1,CDDt$TS.1,
                              lag(HDDt,1)$TS.1,
                             lag(HDDt,2)$TS.1,lag(CDDt,1)$TS.1,
                             lag(CDDt,2)$TS.1,
                             isHolidayt$TS.1,lag(isHolidayt,1)$TS.1,
                             test_df$January,
                             test_df$February,test_df$March,
                             test_df$April,test_df$May,
                             test_df$June,test_df$July,
                             test_df$August,test_df$September,
                             test_df$October,test_df$November,
                             test_df$Monday,test_df$Tuesday,
                             test_df$Wednesday,test_df$Thrusday,
                             test_df$Friday,test_df$Saturday,
                             Yt_test$TS.1)

complete_test_df$lag.HDDt..1..TS.1[is.na(
  complete_test_df$lag.HDDt..1..TS.1)] <- 	17.95
complete_test_df$lag.HDDt..2..TS.1[is.na(
  complete_test_df$lag.HDDt..2..TS.1)] <- 	17.95
complete_test_df$lag.CDDt..1..TS.1[is.na(
  complete_test_df$lag.CDDt..1..TS.1)] <- 	0
complete_test_df$lag.CDDt..2..TS.1[is.na(
  complete_test_df$lag.CDDt..2..TS.1)] <- 	0
complete_test_df$lag.isHolidayt..1..TS.1[is.na(
  complete_test_df$lag.isHolidayt..1..TS.1)] <- 	0

#####################################################################
#--------------------------------------------------------------------
#--------------------------------------------------------------------
#=============================== PART III ===========================
#--------------------------------------------------------------------
#--------------------------------------------------------------------
#####################################################################

#==============BOX-COX TRANSFO & DIFF ANALYSIS  =====================
d_train = window(max_usage, start=time(max_usage)[1], 
                      end=time(max_usage)[2192])
d_valid = window(max_usage, start=time(max_usage)[2193], 
                      end=time(max_usage)[2922])
d_test = Yt_test
            
plot(d_train,
     ylab="Peak Demand for North Texas (training set) (in MW)",
     xlab="2012-01-01 to 2017-12-31", at="pretty")
#From this plot we can clearly 

#Plot of tha ACF to show non stationnarity
acf(max_usage,max.lag=50,main="ACF plot of the time series")

#Do we require BoxCox transformation?

lambda = BoxCox.lambda(max_usage)
lambda_train = BoxCox.lambda(d_train)

YMt.mean = applySeries(max_usage,FUN=colMeans)
YMt.sd   = applySeries(max_usage,FUN=colSds)
YMt.mean.bc = applySeries(BoxCox(max_usage,lambda),FUN=colMeans)
YMt.sd.bc = applySeries(BoxCox(max_usage,lambda),FUN=colSds)
par(mfrow=c(2,2))

plot(series(YMt.mean),series(YMt.sd), 
     xlab=" Average monthly peak of the demand",
     ylab="Standard Deviation",
     main="Demand Peak Before Box-Cox Transfo")
BCB = lm(YMt.sd~YMt.mean)
abline(BCB, col="red", lwd=2)
plot(series(YMt.mean.bc),series(YMt.sd.bc),
     xlab="Average monthly peak of the deman",
     ylab="Standard Deviation",
     main=paste("Demand Peak After Box-Cox Transfo"))
BCA = lm(YMt.sd.bc~YMt.mean.bc)
abline(BCA, col="red", lwd=2)

hist(max_usage,main="Before Box-Cox Transfo",
     xlab="Demand Peak")
hist(BoxCox(max_usage,lambda),
     main="After Box-Cox Transfo",
     xlab="BoxCox of the Demand Peak")

# Showing the impact of the series (training when we differentiate 
#at lag 7!)

diffl7 = diff(d_train, lag=7)
plot(diffl7,main="Series with Lag 7 differencing",
     ylab="Difference in MW",xlab="Time", type="l")
abline(h=0, col="red", lwd=2)

diffl7BC = diff(BoxCox(d_train,lambda_train), lag=7)
plot(diffl7BC,main="Series with Lag 7 differencing 
     and Box-Cox Transformation",ylab="Difference in MW"
     ,xlab="Years", type="l")
abline(h=0, col="red", lwd=2)

library(astsa)
par(mfrow=c(1,1))
acf2(diffl7, main="ACF & PACF of the differenced series at lag 7",
     max.lag = 50)
acf2(diffl7BC,main="ACF & PACF of the differenced series at lag 7 
     with BoxCox Transfo")

#So both ACF & PACF plots show very similar results with and without 
#the BoxCox Transformation With diff at lag 7, we have stationnary 
#data!

#####################################################################
#--------------------------------------------------------------------
#=============================== SARIMA =============================
#--------------------------------------------------------------------
#####################################################################

#iterative procedure to find the optimal SARIMA model

#NOT GOOD
SARIMA_1 = sarima(as.numeric(
  BoxCox(d_train,lambda_train)),1,0,1,1,0,1,7)
SARIMA_1

#STILL NOT GOOD
SARIMA_2 = sarima(as.numeric(
  BoxCox(d_train,lambda_train)),1,1,1,1,0,1,7)
SARIMA_2

#Looks Good
SARIMA_3 = sarima(as.numeric(
  BoxCox(d_train,lambda_train)),2,0,2,1,0,1,7)
SARIMA_3 #AIC:-6.583238 ; BIC:-6.562462

#Looks Good
SARIMA_4 = sarima(as.numeric(
  BoxCox(d_train,lambda_train)),2,0,2,0,1,1,7)      
SARIMA_4 #AIC:-6.578119 ; BIC:-6.559892                

#Looks Good
SARIMA_5 = sarima(as.numeric(
  BoxCox(d_train,lambda_train)),2,0,2,1,1,1,7)
SARIMA_5 #AIC:-6.577268 ; BIC:-6.556438

#Also looks good
SARIMA_6 = sarima(as.numeric(
  BoxCox(d_train,lambda_train)),2,0,3,1,1,1,7) #SELECTED MODELS!
SARIMA_6 
#AIC:-6.576353 ; BIC:-6.552919
#(the model works (see output), and is the model 
#which minimises AIC& BIC)

SARIMA_7 = sarima(as.numeric(
  BoxCox(d_train,lambda_train)),2,0,3,1,2,1,7)  #BAD
SARIMA_7

SARIMA_8 = sarima(as.numeric(
  BoxCox(d_train,lambda_train)),2,0,3,1,3,1,7)   #BAD                                  
SARIMA_8

SARIMA_9 = sarima(as.numeric(
  BoxCox(d_train,lambda_train)),2,0,3,1,2,2,7)    
SARIMA_9 #AIC: -6.531866 ; BIC:-6.50837    # it didnt work
#although respects tests, its only runs for 17 iterations

#(the following were to test, as get grow the model, 
#but the results werent conclusive!)
SARIMA_11 = sarima(as.numeric(
  BoxCox(d_train,lambda_train)),2,0,3,1,1,2,7)   #BAD
SARIMA_11

SARIMA_12 = sarima(as.numeric(
  BoxCox(d_train,lambda_train)),2,0,3,1,2,1,7)   #BAD
SARIMA_12

SARIMA_10 = sarima(as.numeric(
  BoxCox(d_train,lambda_train)),2,0,4,1,2,2,7)   #BAD
SARIMA_10

#Overall performance of the SARIMA MODELS
cat("ARIMA(2,0,2)(1,0,1)[7] - AIC:",
    SARIMA_3 $AIC," BIC:",SARIMA_3 $BIC,"\n")
cat("ARIMA(2,0,2)(0,1,1)[7] - AIC:",
    SARIMA_4 $AIC," BIC:",SARIMA_4 $BIC,"\n")
cat("ARIMA(2,0,2)(1,1,1)[7] - AIC:",
    SARIMA_5 $AIC," BIC:",SARIMA_5 $BIC,"\n")
cat("ARIMA(2,0,3)(0,1,1)[7] - AIC:",
    SARIMA_6 $AIC," BIC:",SARIMA_6 $BIC,"\n")
cat("ARIMA(2,0,3)(1,2,2)[7] - AIC:",
    SARIMA_9 $AIC," BIC:",SARIMA_9 $BIC,"\n")

#Predictions for SARIMA                                                   
d_all =  rbind(d_train, d_valid)                   

#=============== SARIMA-- Expanding window ==========================
#=============== SARIMA-- Expanding window ==========================
#The sarima fixed value come from the coefficients from the SARIMA_6
#model above
SARIMA_exp = NULL
pred_sarima_exp = NULL
se_sarima_exp_1 = NULL
nrow(d_train)

lower = rep(0,730)
upper = rep(0,730)
val = rep(0,730)

for (i in (nrow(d_train)+1):nrow(d_all))
{
  SARIMA_exp = sarima.for(BoxCox(d_all[1:i-1],lambda_train), 
                          n.ahead = 1,
                          p=2,d=0,q=3, P=1,D=1,Q=1,S=7,
                          fixed=c(1.4515,-0.4587, -0.5638,-0.2448,
                                  -0.0013,-0.0088,-0.9899),
                          no.constant = TRUE,plot = FALSE)                                             
  
  val[i-2192] =InvBoxCox(SARIMA_exp$pred[1],lambda_train)
  
  lower[i - 2192] = InvBoxCox(SARIMA_exp$pred[1] - 
                                (1.96*SARIMA_exp$se[1]),lambda_train)
  
  upper[i - 2192] = InvBoxCox(SARIMA_exp$pred[1] + 
                                (1.96*SARIMA_exp$se[1]),lambda_train)
}



#Get accuracy
Pred_SARIMA_exp<-ts(val)
acc_sarima_exp = accuracy(Pred_SARIMA_exp,d_valid)[,1:5]
print(acc_sarima_exp)


match = 0

for(i in 1:730){
  if(d_valid[i] <= upper[i] && d_valid[i] >= lower[i]){
    match = match + 1
  }
}

perc_match  = match / 730
perc_match # 94.8%


#================= SARIMA-- Rolling window ==========================
lower_rol = rep(0,730)
upper_rol = rep(0,730)
val_rol = rep(0,730)

for (i in (nrow(d_train)+1):nrow(d_all))
{
  SARIMA_roll = sarima.for(BoxCox(d_all[(i-nrow(d_train)):i-1],
                                  lambda_train), n.ahead = 1,
                           p=2,d=0,q=3, P=1,D=1,Q=1,S=7,
                           fixed=c(1.4515,-0.4587, -0.5638,         
                                   -0.2448,-0.0013,-0.0088,-0.9899),
                           no.constant = TRUE,plot = FALSE)                                             
  
  val_rol[i-2192] =InvBoxCox(SARIMA_roll$pred[1],lambda_train)
  
  lower_rol[i - 2192] = InvBoxCox(SARIMA_roll$pred[1] - 
                              (1.96*SARIMA_roll$se[1]),lambda_train)
  
  upper_rol[i - 2192] = InvBoxCox(SARIMA_roll$pred[1] + 
                              (1.96*SARIMA_roll$se[1]),lambda_train)
}




Pred_SARIMA_rolling<-ts(val_rol)
acc_sarima_roll = accuracy(Pred_SARIMA_rolling,d_valid)[,1:5]
print(acc_sarima_roll)

match1 = 0

for(i in 1:730){
  if(d_valid[i] <= upper_rol[i] && d_valid[i] >= lower_rol[i]){
    match1 = match1 + 1
  }
}

perc_match1  = match1 / 730
perc_match1 # 94.9%

#####################################################################
#--------------------------------------------------------------------
#================================ ARX ===============================
#--------------------------------------------------------------------
#####################################################################

library(dynlm)

# Function to produce diagnostic plots for dynlm and lm
#This function was borrowed from the class notes on R
#from the arx.R file
plot.diag <- function(o) { 
  
  par(mfrow=c(2,2))
  plot(o$fitted, o$residuals)
  for (k in 2:dim(o$x)[2]) { 
    plot(o$x[,k], o$residuals, xlab=dimnames(o$x)[2][[1]][k]) }
  qqnorm(o$residuals); abline(a=0, b=1, col="blue")
  acf(o$residuals)
  
}

#MAPE, RMSE, etc
#Recal complete_train_df or complete_val_df contain the expl variables
#variables d_valid & d_train contain the peak daily values

#=============== ARX-- Expanding window ==========================
#Created new df and changed the names to merge them
train_df_arx<-complete_train_df
names(train_df_arx)<-c('WindChill','HDD','CDD','l1HDD','l2HDD',
                       'l1CDD','l2CDD','isHoliday','l1isHoliday',
                       'January','February','March','April','May',
                       'June','July','August','September','October',
                       'November','Monday','Tuesday','Wednesday',
                       'Thrusday','Friday','Saturday','Yt')

val_df_arx<-complete_val_df
names(val_df_arx)<-c('WindChill','HDD','CDD','l1HDD','l2HDD',
                       'l1CDD','l2CDD','isHoliday','l1isHoliday',
                       'January','February','March','April','May',
                       'June','July','August','September','October',
                       'November','Monday','Tuesday','Wednesday',
                       'Thrusday','Friday','Saturday','Yt')
test_df_arx<-complete_test_df
names(test_df_arx)<-c('WindChill','HDD','CDD','l1HDD','l2HDD',
                        'l1CDD','l2CDD','isHoliday','l1isHoliday',
                        'January','February','March','April','May',
                        'June','July','August','September','October',
                        'November','Monday','Tuesday','Wednesday',
                        'Thrusday','Friday','Saturday','Yt')

df_Tot<-rbind(train_df_arx,val_df_arx)
df_t_v = as.zoo(ts(rbind(train_df_arx,val_df_arx)))

#Using LM to train on the training set and evaluate on the 
#validation set here I added a variable for lag1 of the time series
#(Yt)
#--------------------------------------------------------------------
#========================== ARX-Vanilla =============================
#--------------------------------------------------------------------
train_df_arx$lag1_Yt<- lag(d_train,1)
train_df_arx$lag1_Yt[1]<-mean(d_train)
val_df_arx$lag1_Yt<- lag(d_valid,1)
val_df_arx$lag1_Yt[1]<-mean(d_valid)

oo.lm <- lm(Yt ~ ., data=train_df_arx, x=T)
print(oo.lm)

plot.diag(oo.lm) 

pred0.lm <- predict(oo.lm,newdata=val_df_arx,interval="prediction")
pred_arx_1<-predict(oo.lm,newdata=val_df_arx)

cat("\n Forecast for y_{n+1|n} using lm fitted model:\n")
print(pred0.lm)

pred_arx_1<-ts(pred_arx_1)
acc_arx_1 = accuracy(pred_arx_1,d_valid)[,1:5]
print(acc_arx_1)  #this gives us a good MAPE of 4.06

#====================================================================
#--------------------------------------------------------------------
#==============ARX-Expanding window, retrain after 1 year============
#----------------------using up to 1 lags of Yt----------------------
#====================================================================

val_df_arx_TOP_1_YRS<-val_df_arx[1:365,]
val_df_arx_last_yrs<-tail(val_df_arx,365)

P1_exp_arx_1 <- lm(Yt ~ ., data=train_df_arx, x=T) #first phase 
PI_arx_exp_1_1 = predict(P1_exp_arx_1,newdata=val_df_arx_TOP_1_YRS,
                         interval="prediction")

exp_window_arx_1<-rbind(train_df_arx,val_df_arx_TOP_1_YRS) 
P2_exp_arx_1 <- lm(Yt ~ ., data=exp_window_arx_1, x=T) #2nd phase
PI_arx_exp_1_2 = predict(P2_exp_arx_1,newdata=val_df_arx_last_yrs,
                         interval="prediction")
plot.diag(P1_exp_arx_1)
plot.diag(P2_exp_arx_1)

PI_arx_exp_1_1<-as.data.frame(PI_arx_exp_1_1)
PI_arx_exp_1_2<-as.data.frame(PI_arx_exp_1_2)
pred_tot_exp_arx_1<-rbind(PI_arx_exp_1_1,PI_arx_exp_1_2)
pred_tot_exp_arx_1<-ts(pred_tot_exp_arx_1$fit)

acc_arx_exp_1 = accuracy(pred_tot_exp_arx_1,d_valid)[,1:5]
print(acc_arx_exp_1)
#     ME       RMSE        MAE        MPE       MAPE 
#10.8993454 52.6355322 40.2763737  0.7787443  4.0338562

#====================================================================
#--------------------------------------------------------------------
#==============ARX-Expanding window, retrain after 1 year============
#----------------------using up to 7 lags of Yt----------------------
#====================================================================

#Preping new DF for this 7day lag
train_df_arx_7<-train_df_arx
train_df_arx_7$lag2_Yt<- lag(d_train,2)
train_df_arx_7$lag3_Yt<- lag(d_train,3)
train_df_arx_7$lag4_Yt<- lag(d_train,4)
train_df_arx_7$lag5_Yt<- lag(d_train,5)
train_df_arx_7$lag6_Yt<- lag(d_train,6)
train_df_arx_7$lag7_Yt<- lag(d_train,7)

for(i in 1:ncol(train_df_arx_7)){
  train_df_arx_7[is.na(train_df_arx_7[,i]), i] <-
    mean(train_df_arx_7[,i], na.rm = TRUE)
}
#validation
val_df_arx_7<-val_df_arx
val_df_arx_7$lag2_Yt<- lag(d_valid,2)
val_df_arx_7$lag3_Yt<- lag(d_valid,3)
val_df_arx_7$lag4_Yt<- lag(d_valid,4)
val_df_arx_7$lag5_Yt<- lag(d_valid,5)
val_df_arx_7$lag6_Yt<- lag(d_valid,6)
val_df_arx_7$lag7_Yt<- lag(d_valid,7)
for(i in 1:ncol(val_df_arx_7)){
  val_df_arx_7[is.na(val_df_arx_7[,i]), i] <- 
    mean(val_df_arx_7[,i], na.rm = TRUE)
}

#test set
test_df_arx_7<-test_df_arx
test_df_arx_7$lag1_Yt<- lag(d_test,1)
test_df_arx_7$lag2_Yt<- lag(d_test,2)
test_df_arx_7$lag3_Yt<- lag(d_test,3)
test_df_arx_7$lag4_Yt<- lag(d_test,4)
test_df_arx_7$lag5_Yt<- lag(d_test,5)
test_df_arx_7$lag6_Yt<- lag(d_test,6)
test_df_arx_7$lag7_Yt<- lag(d_test,7)
for(i in 1:ncol(test_df_arx_7)){
  test_df_arx_7[is.na(test_df_arx_7[,i]), i] <-
    mean(test_df_arx_7[,i], na.rm = TRUE)
}
#ARX-expanding window 7 day lag
val_df_arx_TOP_1_YRS_7<-val_df_arx_7[1:365,]
val_df_arx_last_yrs_7<-tail(val_df_arx_7,365)

P1_exp_arx_7 <- lm(Yt ~ ., data=train_df_arx_7, x=T) #first phase 
PI_arx_exp_1_7 = predict(P1_exp_arx_7,newdata=val_df_arx_TOP_1_YRS_7,
                         interval="prediction")

exp_window_arx_1_7<-rbind(train_df_arx_7,val_df_arx_TOP_1_YRS_7) 
P2_exp_arx_7 <- lm(Yt ~ ., data=exp_window_arx_1_7, x=T) #2nd phase
PI_arx_exp_2_7 = predict(P2_exp_arx_7,newdata=val_df_arx_last_yrs_7,
                         interval="prediction")
plot.diag(P1_exp_arx_7)
plot.diag(P2_exp_arx_7)

PI_arx_exp_1_7<-as.data.frame(PI_arx_exp_1_7)
PI_arx_exp_2_7<-as.data.frame(PI_arx_exp_2_7)
pred_tot_exp_arx_7<-rbind(PI_arx_exp_1_7,PI_arx_exp_2_7)
pred_exp_arx_7<-ts(pred_tot_exp_arx_7$fit)

acc_arx_exp_7 = accuracy(pred_exp_arx_7,d_valid)[,1:5]
print(acc_arx_exp_7)
#        ME       RMSE        MAE        MPE       MAPE 
#9.2602515 51.5979988 39.8511414  0.6169949  4.0089042 


PI_95=NULL

for(i in 1:730)
{
  PI_95[i] = 
    ifelse(as.numeric(d_valid[i])>= 
             as.numeric(pred_tot_exp_arx_7$lwr[i]) && 
             as.numeric(d_valid[i])<= 
             as.numeric(pred_tot_exp_arx_7$upr[i]),1,0)
}

sum(PI_95, na.rm = T)/730
sum(PI_95, na.rm = T)

#92.73% fall into our prediciton interval (or 677 values)

#Get the PI length
col1<-as.numeric(d_valid) #actual validation data
col2<-as.numeric(pred_tot_exp_arx_7$lwr) # lower bound of PI
col3<-as.numeric(pred_tot_exp_arx_7$upr) # Upper bound of the PI
matpi<-cbind(col1,col2,col3)

#This code was borrowed from p.17 of Slides 7 on Predicitons
#from the Advance Statistical Class
#This function evaluated the prediction interval 
covlen=function(matpi) {
  list(mean(apply(matpi,1,function(a){(a[1]>=a[2])*(a[1]<=a[3])})),
       mean(matpi[,3]-matpi[,2]))}

arx_exp_7_fit=covlen(matpi)
arx_exp_7_fit
#the lenght on avg of the prediction interval is 181.99

#====================================================================
#--------------------------------------------------------------------
#==============ARX-Rollign window, retrain after 1 year============
#----------------------using up to 1 lags of Yt----------------------
#====================================================================
val_df_arx_TOP_1_YRS<-val_df_arx[1:365,]
val_df_arx_last_yrs<-tail(val_df_arx,365)
train_df_arx_last_yrs<-tail(train_df_arx,1827)

P1_roll_arx_1 <- lm(Yt ~ ., data=train_df_arx, x=T) #first phase 
PI_arx_roll_1_1 = predict(P1_roll_arx_1,newdata=val_df_arx_TOP_1_YRS,
                          interval="prediction")

roll_window_arx_1<-rbind(train_df_arx_last_yrs,val_df_arx_TOP_1_YRS)
P2_roll_arx_1 <- lm(Yt ~ ., data=roll_window_arx_1, x=T) #2nd phase
PI_arx_roll_1_2 = predict(P2_roll_arx_1,newdata=val_df_arx_last_yrs,
                          interval="prediction")
plot.diag(P1_roll_arx_1)
plot.diag(P2_roll_arx_1)

PI_arx_roll_1_1<-as.data.frame(PI_arx_roll_1_1)
PI_arx_roll_1_2<-as.data.frame(PI_arx_roll_1_2)
pred_tot_exp_arx_1<-rbind(PI_arx_roll_1_1,PI_arx_roll_1_2)
pred_tot_exp_arx_1<-ts(pred_tot_exp_arx_1$fit)

acc_arx_roll_1 = accuracy(pred_tot_exp_arx_1,d_valid)[,1:5]
print(acc_arx_roll_1) # we get poorer MAPE

#        ME       RMSE        MAE        MPE       MAPE 
#10.3018778 52.5070004 40.1003642  0.7191284  4.0198339

#====================================================================
#--------------------------------------------------------------------
#==============ARX-Rollign window, retrain after 1 year============
#----------------------using up to 7 lags of Yt----------------------
#====================================================================
val_df_arx_TOP_1_YRS_7<-val_df_arx_7[1:365,]
val_df_arx_last_yrs_7<-tail(val_df_arx_7,365)
train_df_arx_last_yrs<-tail(train_df_arx_7,1827)

P1_roll_arx_7 <- lm(Yt ~ ., data=train_df_arx_7, x=T) #first phase 
PI_arx_roll_1_7 = predict(P1_roll_arx_7,
                          newdata=val_df_arx_TOP_1_YRS_7,
                          interval="prediction")

roll_window_arx_1_7<-rbind(train_df_arx_last_yrs,
                           val_df_arx_TOP_1_YRS_7)
P2_roll_arx_7 <- lm(Yt ~ ., data=roll_window_arx_1_7, x=T)
PI_arx_roll_2_7 = predict(P2_roll_arx_7,
                          newdata=val_df_arx_last_yrs_7,
                          interval="prediction")
plot.diag(P1_roll_arx_7)
plot.diag(P2_roll_arx_7)

PI_arx_roll_1_7<-as.data.frame(PI_arx_roll_1_7)
PI_arx_roll_2_7<-as.data.frame(PI_arx_roll_2_7)
pred_tot_roll_arx_7<-rbind(PI_arx_roll_1_7,PI_arx_roll_2_7)
pred_roll_arx_7<-ts(pred_tot_roll_arx_7$fit)

acc_arx_roll_7 = accuracy(pred_roll_arx_7,d_valid)[,1:5]
print(acc_arx_roll_7)

#ME       RMSE        MAE        MPE       MAPE 
#8.7248063 51.4704878 39.7174621  0.5652624  3.9997566 

PI_95=NULL

for(i in 1:730)
{
  PI_95[i] = 
    ifelse(as.numeric(d_valid[i])>= 
             as.numeric(pred_tot_roll_arx_7$lwr[i]) && 
             as.numeric(d_valid[i])<= 
             as.numeric(pred_tot_roll_arx_7$upr[i]),1,0)
}

sum(PI_95, na.rm = T)/730
sum(PI_95, na.rm = T)
#92.73% fall into our prediciton interval (or 677 values)

#Get the PI length
col1<-as.numeric(d_valid) #actual validation data
col2<-as.numeric(pred_tot_roll_arx_7$lwr) # lower bound of PI
col3<-as.numeric(pred_tot_roll_arx_7$upr) # Upper bound of the PI
matpi<-cbind(col1,col2,col3)

#This code was borrowed from p.17 of Slides 7 on Predicitons
#from the Advance Statistical Class
#This function evaluated the prediction interval 
covlen=function(matpi) {
  list(mean(apply(matpi,1,function(a){(a[1]>=a[2])*(a[1]<=a[3])})),
       mean(matpi[,3]-matpi[,2]))}

arx_roll_7_fit=covlen(matpi)
arx_roll_7_fit
#182.3952

acf2(P2_roll_arx_7$residuals,
     main="ACF and PACF Plots of the Residuals")

plot(P2_roll_arx_7$residuals, 
     ylab="Residuals of the model", 
     xlab="Time",
     main="Residuals through time")

qqnorm(P2_roll_arx_7$residuals, 
       ylab="Residuals", 
       xlab="Normal Scores", 
       main="QQ Plot") 
qqline(P2_roll_arx_7$residuals)


#--------------------------------------------------------------------
#==============ARX-Rollign window, retrain after 1 year============
#----------------------using up to 7 lags of Yt----------------------
#testing our best ARM model against noise
noisy_var_lub = var_lub
noisy_var_lub$TMAX = noisy_var_lub$TMAX + 
  rnorm(length(noisy_var_lub$TMAX),mean=0,sd=0.5) 
noisy_var_lub$TMIN = noisy_var_lub$TMIN + 
  rnorm(length(noisy_var_lub$TMIN),mean=0,sd=1)

noisy_var_lub = PreProcess(noisy_var_lub)
noisy_var_lub$DATE=as.Date(noisy_var_lub$DATE,"%Y-%m-%d")
noisy_var_lub = noisy_var_lub[1:length(noisy_var_lub$DATE)-1,]
summary(noisy_var_lub)

Yt = timeSeries(max_usage$`Energy Usage`,noisy_var_lub$DATE)
Yt = window(Yt,start=timeDate("2012-01-01",format="%Y-%m-%d"),
            end=timeDate("2017-12-31",format="%Y-%m-%d"))

noisy_train_df <- noisy_var_lub[noisy_var_lub$DATE <=
                             as.Date("2017-12-31"),]

DATE = noisy_train_df$DATE

# choosing the variables
vars = c("TMAX","TMIN","PRCP","HDD","CDD","WindChill")

# HDDt
HDDt = timeSeries(noisy_train_df$HDD, noisy_train_df$DATE)
# CDDt
CDDt = timeSeries(noisy_train_df$CDD,noisy_train_df$DATE)

# isHolidayt
holiday_var <- as.Date(holidayNERC(2012:2021),format="%Y-%m-%d") 
df_holiday = data.frame(DATE)
df_holiday$isHoliday = ifelse(DATE %in% holiday_var, 1, 0)
isHolidayt = timeSeries(df_holiday$isHoliday ,DATE)

# include month variable
asdate = as.Date(DATE,format = "%Y-%m-%d")
x_month = format(asdate,"%m")
noisy_train_df$January <- ifelse(x_month == "01", 1, 0)
noisy_train_df$February <- ifelse(x_month == "02", 1, 0)
noisy_train_df$March <- ifelse(x_month == "03", 1, 0)
noisy_train_df$April <- ifelse(x_month == "04", 1, 0)
noisy_train_df$May <- ifelse(x_month == "05", 1, 0)
noisy_train_df$June <- ifelse(x_month == "06", 1, 0)
noisy_train_df$July <- ifelse(x_month == "07", 1, 0)
noisy_train_df$August <- ifelse(x_month == "08", 1, 0)
noisy_train_df$September <- ifelse(x_month == "09", 1, 0)
noisy_train_df$October <- ifelse(x_month == "10", 1, 0)
noisy_train_df$November <- ifelse(x_month == "11", 1, 0)
noisy_train_df$December <- ifelse(x_month == "12", 1, 0)

noisy_train_df$weekday_var = weekdays(asdate)
noisy_train_df$Monday <- ifelse(
  noisy_train_df$weekday_var == "Monday", 1, 0)
noisy_train_df$Tuesday <- ifelse(
  noisy_train_df$weekday_var == "Tuesday", 1, 0)
noisy_train_df$Wednesday <- 
  ifelse(noisy_train_df$weekday_var == "Wednesday", 1, 0)
noisy_train_df$Thrusday <- ifelse(
  noisy_train_df$weekday_var == "Thursday", 1, 0)
noisy_train_df$Friday <- ifelse(
  noisy_train_df$weekday_var == "Friday", 1, 0)
noisy_train_df$Saturday <- ifelse(
  noisy_train_df$weekday_var == "Saturday", 1, 0)
noisy_train_df = subset(
  noisy_train_df, select = -c(weekday_var))

complete_train__noisy_df = data.frame(noisy_train_df$WindChill,
                                      HDDt$TS.1,
                               CDDt$TS.1,lag(HDDt, 1)$TS.1,
                               lag(HDDt, 2)$TS.1,
                               lag(CDDt, 1)$TS.1,lag(CDDt, 2)$TS.1,
                               isHolidayt$TS.1,
                               lag(isHolidayt, 1)$TS.1,
                               noisy_train_df$January,
                               noisy_train_df$February,
                               noisy_train_df$March,
                               noisy_train_df$April,
                               noisy_train_df$May,
                               noisy_train_df$June,
                               noisy_train_df$July,
                               noisy_train_df$August,
                               noisy_train_df$September,
                               noisy_train_df$October,
                               noisy_train_df$November,
                               noisy_train_df$Monday,
                               noisy_train_df$Tuesday,
                               noisy_train_df$Wednesday,
                               noisy_train_df$Thrusday,
                               noisy_train_df$Friday,
                               noisy_train_df$Saturday,
                               Yt$TS.1)

complete_train__noisy_df$lag.HDDt..1..TS.1[is.na(
  complete_train__noisy_df$lag.HDDt..1..TS.1)] <- 	3.35
complete_train__noisy_df$lag.HDDt..2..TS.1[is.na(
  complete_train__noisy_df$lag.HDDt..2..TS.1)] <- 	3.35
complete_train__noisy_df$lag.CDDt..1..TS.1[is.na(
  complete_train__noisy_df$lag.CDDt..1..TS.1)] <- 	0
complete_train__noisy_df$lag.CDDt..2..TS.1[is.na(
  complete_train__noisy_df$lag.CDDt..2..TS.1)] <- 	0
complete_train__noisy_df$lag.isHolidayt..1..TS.1[is.na(
  complete_train__noisy_df$lag.isHolidayt..1..TS.1)] <- 	0

###similarly for the validation set
Yt = timeSeries(max_usage$`Energy Usage`,processed_data$DATE)
Yt_val = window(Yt,start=timeDate("2018-01-01",format="%Y-%m-%d"),
                end=timeDate("2019-12-31",format="%Y-%m-%d"))

val_noisy_df <- noisy_var_lub[noisy_var_lub$DATE >= 
                           as.Date("2018-01-01")& 
                           noisy_var_lub$DATE <= 
                           as.Date("2019-12-31"),]

DATE = val_noisy_df$DATE

# choosing the variables
vars = c("TMAX","TMIN","PRCP","HDD","CDD","WindChill")

# HDDt
HDDt = timeSeries(val_noisy_df$HDD, val_noisy_df$DATE)
# CDDt
CDDt = timeSeries(val_noisy_df$CDD,val_noisy_df$DATE)
# isHolidayt
holiday_var <- as.Date(holidayNERC(2012:2021),format="%Y-%m-%d") 
df_holiday = data.frame(DATE)
df_holiday$isHoliday = ifelse(DATE %in% holiday_var, 1, 0)
isHolidayt = timeSeries(df_holiday$isHoliday ,DATE)

# include month variable
asdate = as.Date(DATE,format = "%Y-%m-%d")
x_month = format(asdate,"%m")
val_noisy_df$January <- ifelse(x_month == "01", 1, 0)
val_noisy_df$February <- ifelse(x_month == "02", 1, 0)
val_noisy_df$March <- ifelse(x_month == "03", 1, 0)
val_noisy_df$April <- ifelse(x_month == "04", 1, 0)
val_noisy_df$May <- ifelse(x_month == "05", 1, 0)
val_noisy_df$June <- ifelse(x_month == "06", 1, 0)
val_noisy_df$July <- ifelse(x_month == "07", 1, 0)
val_noisy_df$August <- ifelse(x_month == "08", 1, 0)
val_noisy_df$September <- ifelse(x_month == "09", 1, 0)
val_noisy_df$October <- ifelse(x_month == "10", 1, 0)
val_noisy_df$November <- ifelse(x_month == "11", 1, 0)
val_noisy_df$December <- ifelse(x_month == "12", 1, 0)

val_noisy_df$weekday_var = weekdays(asdate)
val_noisy_df$Monday <- ifelse(
  val_noisy_df$weekday_var == "Monday", 1, 0)
val_noisy_df$Tuesday <- ifelse(
  val_noisy_df$weekday_var == "Tuesday", 1, 0)
val_noisy_df$Wednesday <- ifelse(
  val_noisy_df$weekday_var == "Wednesday", 1, 0)
val_noisy_df$Thrusday <- ifelse(
  val_noisy_df$weekday_var == "Thursday", 1, 0)
val_noisy_df$Friday <- ifelse(
  val_noisy_df$weekday_var == "Friday", 1, 0)
val_noisy_df$Saturday <- ifelse(
  val_noisy_df$weekday_var == "Saturday", 1, 0)
val_noisy_df = subset(val_noisy_df, select = -c(weekday_var))

complete_val_noisy_df = data.frame(val_noisy_df$WindChill,HDDt$TS.1,
                                      CDDt$TS.1,lag(HDDt, 1)$TS.1,
                                      lag(HDDt, 2)$TS.1,
                                      lag(CDDt, 1)$TS.1,
                                   lag(CDDt,2)$TS.1,isHolidayt$TS.1,
                                      lag(isHolidayt,1)$TS.1,
                                   val_noisy_df$January,
                                   val_noisy_df$February,
                                   val_noisy_df$March,
                                   val_noisy_df$April,
                                   val_noisy_df$May,
                                   val_noisy_df$June,
                                   val_noisy_df$July,
                                   val_noisy_df$August,
                                   val_noisy_df$September,
                                   val_noisy_df$October,
                                   val_noisy_df$November,
                                   val_noisy_df$Monday,
                                   val_noisy_df$Tuesday,
                                   val_noisy_df$Wednesday,
                                   val_noisy_df$Thrusday,
                                   val_noisy_df$Friday,
                                   val_noisy_df$Saturday,
                                      Yt_val$TS.1)

complete_val_noisy_df$lag.HDDt..1..TS.1[is.na(
  complete_val_noisy_df$lag.HDDt..1..TS.1)] <- 	17.95
complete_val_noisy_df$lag.HDDt..2..TS.1[is.na(
  complete_val_noisy_df$lag.HDDt..2..TS.1)] <- 	17.95
complete_val_noisy_df$lag.CDDt..1..TS.1[is.na(
  complete_val_noisy_df$lag.CDDt..1..TS.1)] <- 	0
complete_val_noisy_df$lag.CDDt..2..TS.1[is.na(
  complete_val_noisy_df$lag.CDDt..2..TS.1)] <- 	0
complete_val_noisy_df$lag.isHolidayt..1..TS.1[is.na(
  complete_val_noisy_df$lag.isHolidayt..1..TS.1)] <- 	0

#adjust the name
train_df_arx<-complete_train__noisy_df
names(train_df_arx)<-c('WindChill','HDD','CDD','l1HDD','l2HDD',
                       'l1CDD','l2CDD','isHoliday','l1isHoliday',
                       'January','February','March','April','May',
                       'June','July','August','September','October',
                       'November','Monday','Tuesday','Wednesday',
                       'Thrusday','Friday','Saturday','Yt')

val_df_arx<-complete_val_noisy_df
names(val_df_arx)<-c('WindChill','HDD','CDD','l1HDD','l2HDD',
                     'l1CDD','l2CDD','isHoliday','l1isHoliday',
                     'January','February','March','April','May',
                     'June','July','August','September','October',
                     'November','Monday','Tuesday','Wednesday',
                     'Thrusday','Friday','Saturday','Yt')

train_df_arx$lag1_Yt<- lag(d_train,1)
train_df_arx$lag1_Yt[1]<-mean(d_train)
val_df_arx$lag1_Yt<- lag(d_valid,1)
val_df_arx$lag1_Yt[1]<-mean(d_valid)

#Preping new DF for this 7day lag
train_df_arx_7<-train_df_arx
train_df_arx_7$lag2_Yt<- lag(d_train,2)
train_df_arx_7$lag3_Yt<- lag(d_train,3)
train_df_arx_7$lag4_Yt<- lag(d_train,4)
train_df_arx_7$lag5_Yt<- lag(d_train,5)
train_df_arx_7$lag6_Yt<- lag(d_train,6)
train_df_arx_7$lag7_Yt<- lag(d_train,7)

for(i in 1:ncol(train_df_arx_7)){
  train_df_arx_7[is.na(train_df_arx_7[,i]), i] <-
    mean(train_df_arx_7[,i], na.rm = TRUE)
}
#validation
val_df_arx_7<-val_df_arx
val_df_arx_7$lag2_Yt<- lag(d_valid,2)
val_df_arx_7$lag3_Yt<- lag(d_valid,3)
val_df_arx_7$lag4_Yt<- lag(d_valid,4)
val_df_arx_7$lag5_Yt<- lag(d_valid,5)
val_df_arx_7$lag6_Yt<- lag(d_valid,6)
val_df_arx_7$lag7_Yt<- lag(d_valid,7)
for(i in 1:ncol(val_df_arx_7)){
  val_df_arx_7[is.na(val_df_arx_7[,i]), i] <- 
    mean(val_df_arx_7[,i], na.rm = TRUE)
}

#Model
val_df_arx_TOP_1_YRS_7<-val_df_arx_7[1:365,]
val_df_arx_last_yrs_7<-tail(val_df_arx_7,365)
train_df_arx_last_yrs<-tail(train_df_arx_7,1827)

P1_roll_arx_7 <- lm(Yt ~ ., data=train_df_arx_7, x=T) #first phase
PI_arx_roll_1_7 = predict(P1_roll_arx_7,
                          newdata=val_df_arx_TOP_1_YRS_7,
                          interval="prediction")

roll_window_arx_1_7<-rbind(train_df_arx_last_yrs,
                           val_df_arx_TOP_1_YRS_7)
P2_roll_arx_7 <- lm(Yt ~ ., data=roll_window_arx_1_7, x=T)
PI_arx_roll_2_7 = predict(P2_roll_arx_7,
                          newdata=val_df_arx_last_yrs_7,
                          interval="prediction")

PI_arx_roll_1_7<-as.data.frame(PI_arx_roll_1_7)
PI_arx_roll_2_7<-as.data.frame(PI_arx_roll_2_7)
pred_tot_roll_arx_7<-rbind(PI_arx_roll_1_7,PI_arx_roll_2_7)
pred_roll_arx_7<-ts(pred_tot_roll_arx_7$fit)

acc_arx_roll_7 = accuracy(pred_roll_arx_7,d_valid)[,1:5]
print(acc_arx_roll_7) 

#trained on not noisy data and validated on noisy date
#       RMSE        MAE        MPE       MAPE 
#8.2607567 51.9987232 40.2254114  0.5284764  4.0365849 

#trained on noisy data and validated on noisy data
#       RMSE        MAE        MPE       MAPE 
#8.1580065 52.0804256 40.1601952  0.5134965  4.0307711 
#Compare to the previous accuracy it only goes up slightly!






#====================================================================
#--------------------------------------------------------------------
#==========================TEST SET!!!!!=============================
#--------------------------------------------------------------------
#====================================================================
#so the best performing model's assumptions could not be validated 
#so we went in the family of ARX models as the accuracy
#we all in the same waters anyways

#Rolling window with lag 7 days
test_df_arx_TOP_1_YRS_7<-test_df_arx_7[1:365,]
test_df_arx_last_yrs_7<-tail(test_df_arx_7,365)
new_train <-rbind(train_df_arx_7,val_df_arx_7)
train_df_arx_last_yrs<-tail(new_train,2192)

P1_test <- lm(Yt ~ ., data=new_train, x=T) #first phase 
PI_1_test = predict(P1_test,
                    newdata=test_df_arx_TOP_1_YRS_7,
                    interval="prediction")

roll_window_test<-rbind(train_df_arx_last_yrs,
                        test_df_arx_TOP_1_YRS_7)
P2_test <- lm(Yt ~ ., data=roll_window_test, x=T) #2nd phase
PI_2_test = predict(P2_test,newdata=test_df_arx_last_yrs_7,
                    interval="prediction")
plot.diag(P1_test)
plot.diag(P2_test)

PI_1_test<-as.data.frame(PI_1_test)
PI_2_test<-as.data.frame(PI_2_test)
pred_tot_test<-rbind(PI_1_test,PI_2_test)
pred_tot_arx_7<-ts(pred_tot_test$fit)

acc_test = accuracy(pred_tot_arx_7,d_test)[,1:5]
print(acc_test)

#######################EXPANDING WITH YT LAG 7
val_df_arx_TOP_1_YRS_7<-test_df_arx_7[1:365,]
val_df_arx_last_yrs_7<-tail(test_df_arx_7,365)

P1_exp_arx_7 <- lm(Yt ~ ., data=train_df_arx_7, x=T) #first phase 
PI_arx_exp_1_7 = 
  predict(P1_exp_arx_7,newdata=val_df_arx_TOP_1_YRS_7,
          interval="prediction")

exp_window_arx_1_7<-rbind(train_df_arx_7,val_df_arx_TOP_1_YRS_7)
P2_exp_arx_7 <- lm(Yt ~ ., data=exp_window_arx_1_7, x=T) #2nd phase
PI_arx_exp_2_7 = predict(P2_exp_arx_7,
                         newdata=val_df_arx_last_yrs_7,
                         interval="prediction")
plot.diag(P1_exp_arx_7)
plot.diag(P2_exp_arx_7)

PI_arx_exp_1_7<-as.data.frame(PI_arx_exp_1_7)
PI_arx_exp_2_7<-as.data.frame(PI_arx_exp_2_7)
pred_tot_exp_arx_7<-rbind(PI_arx_exp_1_7,PI_arx_exp_2_7)
pred_exp_arx_7<-ts(pred_tot_exp_arx_7$fit)

acc_arx_exp_7 = accuracy(pred_exp_arx_7,d_test)[,1:5]
print(acc_arx_exp_7)


#====================================================================
#--------------------------------------------------------------------
#==========================TEST SET!! FINALLLL=======================
#--------------------------------------------------------------------
#====================================================================

TEST_fit <- lm(Yt ~ ., data=new_train, x=T)
PI_test = predict(TEST_fit,newdata=test_df_arx_7,
                  interval="prediction")
plot.diag(TEST_fit)

PI_test<-as.data.frame(PI_test)
PI_test<-ts(PI_test$fit)

acc_test_final = accuracy(PI_test,d_test)[,1:5]
print(acc_test_final)

#performance on the full test set
#ME       RMSE        MAE        MPE       MAPE 
#-1.5851227 59.9552209 44.0180783 -0.6287978  4.7492601 

#we also checked the performance on the smaller test set (from)
#jan 2020 to march 2020 which we can consider as pre-covid
#we simply changed the window manually and re-ran everything
#here are the results
#the MAPE we got was 4.33384% 

#====================================================================
#--------------------------------------------------------------------
#==========================TEST SET!!!!!== FINALLLL==================
#--------------------------------------------------------------------
#====================================================================
winter_pred = c(PI_test[1:59],PI_test[335:424],PI_test[700:730])
winter_true = c(d_test[1:59],d_test[335:424],d_test[700:730])

spring_pred = c(PI_test[60:120],PI_test[425:485])
spring_true = c(d_test[60:120],d_test[425:485])

summer_pred = c(PI_test[121:273],PI_test[486:638])
summer_true = c(d_test[121:273],d_test[486:638])

fall_pred = c(PI_test[274:334],PI_test[639:699])
fall_true = c(d_test[274:334],d_test[639:699])

acc_winter = accuracy(winter_pred,winter_true)[,1:5]
acc_spring = accuracy(spring_pred,spring_true)[,1:5]
acc_summer = accuracy(summer_pred,summer_true)[,1:5]
acc_fall = accuracy(fall_pred,fall_true)[,1:5]

print(acc_winter)
print(acc_spring)
print(acc_summer)
print(acc_fall)


#====================================================================
#====================================================================
#====================================================================
#====================================================================
#====================================================================


#=================================ARMA model=========================
#somehow this doesnt work for me anymore :(

fit <- auto.arima(complete_train_df$Yt.TS.1, 
                  xreg=as.matrix(
                    complete_train_df[,1:length(complete_train_df)-1]
                    ))
print(fit) 

par(mfrow=c(1,1))
acf2(residuals(fit)[-(1:2)],
     main="With proper error structure (using auto.arima)")

# prediction of the 1st of january
pred.arima = predict(fit, n.ahead=1, newxreg=as.matrix(
  complete_val_df[,1:length(complete_val_df)-1]))

hist(pred.arima$pred, xlab="demand for 1 January 2017",
     main="Histogram of arima model predictions",
)

abline(v=pred.arima$pred[1], col='red')
abline(v=pred.arima$pred[1]+2*pred.arima$se[1], col="red", lty=3)
abline(v=pred.arima$pred[1]-2*pred.arima$se[1], col="red", lty=3)

abline(v=complete_val_df$Yt.TS.1[1], lwd=3)

# evaluate the prediction
acc1 = accuracy(pred.arima$pred,d_valid)[,1:5]
print(acc1)