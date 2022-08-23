library(MASS)
library(ggplot2)
library(tidyverse) 
library(lubridate) 
library(reshape2)
library(stringr)
library(zoo)
library(TTR)
library(tseries)
library(gridExtra)

##Exploratory Analysis 
orig_dat <- read_csv("~/Desktop/MSc-Imperial/Term 2/Data Visualisation/Assessments/Assessment_3/data.csv")

##Export table
png("orig_data.png", height = 1000, width = 200*ncol(orig_dat))
grid.table(head(orig_dat,5))
dev.off()

##Data Types
str(orig_dat )
##New data frame
dat<-data.frame(orig_dat)

##Extract codes and names
code<-data.frame(Detail=unique(orig_dat$MSN),Desc=unique(orig_dat$Description))

##Export table
png("code.png", height = 40*nrow(code), width = 550*ncol(code))
grid.table(code)
dev.off()

##Individual Values
ind<-code %>% 
  filter(!grepl('Total', Desc))
ind<-ind[1:7,]

##Total Values 
tot<-code %>% 
  filter(grepl('Total', Desc))

#Drop columns
dat<-subset(orig_dat, select = -c(Unit,Description,Column_Order))
##convert MSN to factor
dat$MSN<-as.factor(dat$MSN)

###Missing values
dat$Value[dat$Value=='Not Available']<-NA

##Dates
dat$YYYYMM<-as.character(dat$YYYYMM)
dat <- dat %>%
  add_column(year = substr(dat$YYYYMM, start = 1, stop = 4),.after = "YYYYMM") 
dat <- dat %>%
  add_column(mon = substr(dat$YYYYMM, start = 5, stop = 6),.after = "year")  
dat <- subset(dat, select = -YYYYMM )
## Drop rows with a date before january 2000
ec<-subset(dat,year>=2000)
## Drop rows with month 13 which corresponds to total
ec<-subset(ec,mon!=13)

#Combine dates in the right format
ec$year<-as.numeric(ec$year)
ec$mon<-as.numeric(ec$mon)
ec$Value<-as.numeric(ec$Value)
##Add column date for visualisation purposes 
ec$Date <- as.yearmon(paste(ec$year, ec$mon), "%Y %m")

#Find number a missing values per group
gg_miss_fct(ec, MSN) 
ec %>% group_by(MSN) %>% summarise(
  non_na = sum(!is.na(Value)),na = sum(is.na(Value)))
#Delete NAs and get final data frame
f.data <- na.omit(ec)

##Visualisation
ggplot(data = f.data, aes(x = Date,y = Value,color=MSN))+
  geom_line()+
  ggtitle("Residential Sector Natural Gas Energy Consumption") +
  xlab("Date") +
  ylab("Trillion Btu")

##Individual time series
ind.dat<-f.data %>% filter(MSN %in% ind$Detail)
ind.dat<-subset(ind.dat,select=c(MSN,Value,Date))

png("ind_data.png", height = 1000, width = 200*ncol(ind.dat))
grid.table(head(ind.dat,5))
dev.off()

ggplot(data = ind.dat, aes(x = Date,y = Value,color=MSN))+
  geom_line()+
  ggtitle("Residential Sector Energy Consumption per source") +
  xlab("Date") +
  ylab("Trillion Btu")

##Recent data
ind.dat %>% filter(Date>="Jan 2018") %>% 
  ggplot(aes(x = Date,y = Value,color=MSN))+
  geom_line()+
  ggtitle("Residential Sector Natural Gas Energy Consumption") +
  xlab("Date") +
  ylab("Trillion Btu")

##Fossil fuels
ind.dat %>% filter(MSN== 'CLRCBUS' | MSN=='NNRCBUS' | MSN=='PARCBUS' | MSN=='ESRCBUS') %>% 
  ggplot(aes(x = Date,y = Value,color=MSN))+
  geom_line()+
  ggtitle("Fossil fuel Energy Consumption") +
  xlab("Date") +
  ylab("Trillion Btu")+geom_smooth(method = "loess")

##renewable energy
ind.dat %>% filter(MSN %in% ind$Detail[4:7]) %>% 
  ggplot(aes(x = Date,y = Value,color=MSN))+
  geom_line()+
  ggtitle("Renewable Energy Consumption") +
  xlab("Date") +
  ylab("Trillion Btu") +geom_smooth(method = "loess")

ind.dat %>% filter(MSN %in% ind$Detail[4:6]) %>% 
ggplot(aes(x = Date,y = Value,color=MSN))+geom_line()+
  ggtitle("Renewable Energy Consumption") +xlab("Date") +
  ylab("Trillion Btu")+geom_smooth(method = "loess")

##Total per group all time
ind.sums<-ind.dat %>%filter(MSN != 'ESRCBUS') %>% group_by(MSN) %>% summarise(sum=sum(Value))
ggplot(ind.sums, aes(x="", y=sum, fill=MSN))+
  geom_bar(width = 1, stat = "identity")+coord_polar("y", start=0)+scale_fill_brewer(palette="Dark2")+
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))

##Group by mean
ind.dat %>% group_by(year=year(Date),MSN) %>% summarise(mean=mean(Value)) %>% 
  ggplot(aes(x = year,y = mean,color=MSN))+
  geom_line()+
  ggtitle("Total Energy Consumption per source each year") +
  xlab("Date") +
  ylab("Trillion Btu")

ind.dat %>% group_by(MSN) 
ggplot(ind.dat,aes(x = MSN,y = Value,color=MSN))+
  geom_boxplot()+
  ggtitle("Total Energy Consumption per source each year") +
  xlab("Date") +
  ylab("Trillion Btu")

##Summary statistics
stats<-ind.dat[,1:2] %>% group_by(MSN) %>% summarise(mean=mean(Value),sd=sd(Value), max=max(Value),min= min(Value))

##Export table
png("stats.png", height = 40*nrow(stats), width = 80*ncol(stats))
grid.table(stats)
dev.off()

##Total values
tot.dat<-f.data %>% filter(MSN %in% tot$Detail)
tot.dat<-subset(tot.dat,select=c(MSN,Value,Date))
stats.tot<-tot.dat[,1:2] %>% group_by(MSN) %>% summarise(mean=mean(Value),sd=sd(Value), max=max(Value),min= min(Value))

png("stats.tot.png", height = 40*nrow(stats.tot), width = 80*ncol(stats.tot))
grid.table(stats.tot)
dev.off()

ggplot(data = tot.dat, aes(x = Date,y = Value,color=MSN))+
  geom_line()+
  ggtitle("Residential Sector Natural Gas Energy Consumption") +
  xlab("Date") +
  ylab("Trillion Btu")

##Recent data
tot.dat %>% filter(Date>="Jan 2018") %>% 
  ggplot(aes(x = Date,y = Value,color=MSN))+
  geom_line()+
  ggtitle("Residential Sector Natural Gas Energy Consumption") +
  xlab("Date") +
  ylab("Trillion Btu")
 
##Total per group all time
tot.sums<-tot.dat %>% group_by(MSN) %>% summarise(sum=sum(Value))
ggplot(tot.sums, aes(x="", y=sum, fill=MSN))+
  geom_bar(width = 1, stat = "identity")+coord_polar("y", start=0)+
  ggtitle("Proportion of Total Energy Consumption per source")+ scale_fill_brewer(palette="Dark2")+
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))

##Per year data
year.data.tot<-tot.dat %>% group_by(year=year(Date),MSN) %>% summarise(sum=sum(Value)) 
ggplot(year.data.tot,aes(x = year,y = sum,color=MSN))+
geom_line()+
ggtitle("Total Energy Consumption") +
xlab("Date") +
ylab("Trillion Btu")

biomass<-subset(f.data , MSN=='BMRCBUS')
ggplot(data = biomass, aes(x =as.Date(Date) , y = Value))+
  geom_line(color = "black", size = 0.5)+
  geom_smooth(method = "loess") +
  ggtitle("Residential Sector Natural Gas Energy Consumption") +
  xlab("Date") +
  ylab("Trillion Btu")

##Univariate data -----------------------------
ec.un_data<-subset(f.data , MSN=='NNRCBUS')
ec.un_data<-subset(ec.un_data, select = -MSN ) 
ec.un<-ts(ec.un_data$Value,start=2000, frequency=12)

## EDA and visualisation 
plot.ts(ec.un)
ggplot(data = ec.un_data, aes(x = Date, y = Value))+
  geom_line(color = "black", size = 0.5)+
  geom_smooth(method = "loess") +
  ggtitle("Residential Sector Natural Gas Energy Consumption") +
  xlab("Date") +
  ylab("Trillion Btu")

##Moving average for each year
ec.un_data %>%
  mutate(seven_avg= rollmean(Value, 12,align="left",  fill=0)) %>%
  ggplot(aes(x=Date,y=Value)) +
  geom_col(fill="black")+
  geom_line(aes(y = seven_avg), color = "red",size = .75)+
  labs(title="Residential Sector Natural Gas Energy Consumption",
       y="Trillion Btu")

##Consumption per year
ec.un_data %>% group_by(year) %>% 
ggplot(aes(x = year,y = Value,color=year))+
  geom_boxplot()+
  ggtitle("Energy Consumption per year") +
  xlab("Date") +
  ylab("Trillion Btu")

ggplot(ec.un_data)+geom_boxplot(aes(y=Value, x=year, fill=format(year))) +
  xlab('Year') + guides(fill=guide_legend(title="Year")) +theme_bw()

ggplot(ec.un_data, aes(x=Value,  y=year, fill=year)) + geom_tile()

##Autocorrelation
par(mfrow=c(2,1))
n<-36
acf(ec.un, lag.max=n, xaxt="n", xlab="Lag (months)")
axis(1, at=0:n/12, labels=0:n)

##Partial-Autocorrelation
pacf(ec.un, lag.max=n, xaxt="n", xlab="Lag (months)")
axis(1, at=0:n/12, labels=0:n)

## Decompose
ec_dec <- decompose(ec.un)
plot(ec_dec)

## Trend 
time <- 1:length(ec.un)
lm_fit <- lm(ec.un ~ time)
trend <- lm_fit$fitted.values
ec_lr <- ec.un - trend
ec.un_data$Detrend<-as.numeric(ec_lr)

##Time series after removing the trend
ggplot(data = ec.un_data, aes(x = Date))+
  geom_line(aes(y = Value,color = 'Original series'))+
  geom_line(aes(y = Detrend,color = 'Detrend series'))+
  ggtitle("Residential Sector Natural Gas Energy Consumption") +
  xlab("Date") +ylab("Trillion Btu")

## Seasonality
ec_dlr <- diff(ec_lr, lag = 12)
n<-48
acf(ec_dlr,lag.max = 48,xaxt="n", xlab="Lag (months)")
axis(1, at=0:n/12, labels=0:n)
pacf(ec_dlr, lag.max=n)
axis(1, at=0:n/12, labels=0:n)

fin.ecun<-data.frame(Season=as.matrix(ec_dlr), date=as.Date(time(ec_dlr)))

##Time series after removing the seasonality
ggplot(data = fin.ecun, aes(x = date,y = Season))+
  geom_line()+
  ggtitle("Residential Sector Natural Gas Energy Consumption") +
  xlab("Date") +ylab("Trillion Btu")

##Multivariate data | Total fossil and total renewable energy consumption ------------------------
ec.mult_data<-data.frame(Date=f.data$Date,Val=f.data$Value,Code=f.data$MSN)
ec.mult_data<-subset(ec.mult_data,Code=='FFRCBUS'|Code=='RERCBUS')
## Reshape data frame 
ec.mult_data<-spread(ec.mult_data,Code,Val)

colnames(ec.mult_data)[2] <- 'Fos'
colnames(ec.mult_data)[3] <- 'Ren'
ec.mult_data<-select(ec.mult_data ,-Date) 
ec.mult<-ts(ec.mult_data,start=2000, frequency=12)


## Multivariate EDA and visualisation 
##Multivariate data |Total fossil and Total renewable 
ec.mult_data<-data.frame(Date=ec$Date,Val=ec$Value,Code=ec$MSN)
ec.mult_data<-subset(ec.mult_data,Code=='FFRCBUS'|Code=='RERCBUS')
##Reshape dataframe
ec.mult_data<-spread(ec.mult_data,Code,Val)
##Rename columns
colnames(ec.mult_data)[2] <- 'Fos'
colnames(ec.mult_data)[3] <- 'Ren'
## Remove date column
ec.mult<-subset(ec.mult_data, select = -Date ) 
##Create time series
ec.mult<-ts(ec.mult,start=2000, frequency=12)

##Plot variables
ggplot()+
  geom_line(data = ec.mult_data, aes(x = Date, y = Fos,color = "Fos"))+
  geom_line(data = ec.mult_data, aes(x = Date, y = Ren,color = "Ren"))+
  ggtitle("Total fossil and renewable energy consuption (01/2000-10/2021)") +
  xlab("Date") +
  ylab("Trillion Btu")


##Autocorrelation, Partial-Autocorrelation and Cross correlation
acfRes<-acf(ec.mult)
pacfRes<-pacf(ec.mult)

##Fossil data
##Separate time series
fos.ser<-ec.mult[,1]
##Time series visualization
ggplot(ec.mult_data, aes(x=Date, y=Fos)) +geom_line() +
  labs(title="Total Fossil fuel Consumption",x='Date',y="Trillion Btu")
##Moving Average 
ec.mult_data %>%
  mutate(seven_avg= rollmean(Fos, 12,align="left",fill=0)) %>%
  ggplot(aes(x=Date,y=Fos)) +
  geom_col(fill="pink")+
  geom_line(aes(y = seven_avg),color = "red",size = .75)+
  labs(title="Total Fossil fuel Consumption",y="Trillion Btu")

##Seasonality
data.spec<-spectrum(fos.ser)
max_freq <- data.spec$freq[data.spec$spec == max(data.spec$spec)]/12
print(c(max_freq,1/max_freq))
period_fos <- round(1/max_freq)

fos_diff<-diff(fos.ser,lag=period_fos)
acf(fos_diff,lag.max=n)
pacf(fos_diff,lag.max=n)

fos.dat<-data.frame(dif12=as.matrix(fos_diff), date=as.Date(time(fos_diff)))

ggplot(data = fos.dat, aes(x = date,y = dif12))+
  geom_line()+
  ggtitle("Residential Sector Natural Gas Energy Consumption") +
  xlab("Date") +ylab("Trillion Btu")

##Renewable data
ren.ser<-ec.mult[,2]

ggplot(ec.mult_data, aes(x=Date, y=Ren)) +geom_line() + 
  xlab("")+ylab("Fossil consumption(Trillion Btu)")

ec.mult_data %>%
  mutate(seven_avg= rollmean(Ren, 12,align="left",fill=0)) %>%
  ggplot(aes(x=Date,y=Ren)) +geom_col(fill="pink")+
  geom_line(aes(y = seven_avg),color = "red", size = .75)+
  labs(title="Total Renewable energy consumption", y="Trillion Btu")

##  Remove trend from series
time <- 1:length(ren.ser)
lm_ren <- lm(ren.ser ~ time)
trend <- lm_ren$fitted.values
ren.dt <- ren.ser - trend

ren.dat<-data.frame(dif1=as.matrix(ren.dt), date=as.Date(time(ren.dt)))

ggplot(data = ren.dat, aes(x = date,y = dif1))+
  geom_line()+
  ggtitle("Total Renewable Energy Consumption") +
  xlab("Date") +ylab("Trillion Btu")


##Volatility
##Standard deviation of every year
an_ren<-ec.mult_data %>% group_by(yr=year(ec.mult_data$Date)) %>% summarise(sd_Ren=sd(Ren),sd_Fos=sd(Fos))

an_ren_vol<-ec.mult_data %>% group_by(yr=year(ec.mult_data$Date))%>%   
  group_map(~mutate(., sd.by.year = sd(Ren)), .keep = T) %>%  
  bind_rows()

ren.vol<-ren.dt/an_ren_vol$sd.by.year

ren.dat$vol<-as.numeric(ren.vol)

ggplot()+
  geom_line(data = ren.dat, aes(x = date,y = dif1,color='dif1'))+
  geom_line(data = ren.dat, aes(x = date,y = vol,color='vol'))+
  ggtitle("Total Renewable Energy Consumption") +
  xlab("Date") +ylab("Trillion Btu")

##Seasonality
data.spec<-spectrum(ren.vol)
max_freq <- data.spec$freq[data.spec$spec == max(data.spec$spec)]/12
print(c(max_freq,1/max_freq))
period.ren <- round(1/max_freq)

ren_diff<-diff(ren.vol,lag=period.ren)
acf(ren_diff,lag.max=n)
pacf(ren_diff,lag.max=n)

re.dat.seas<-data.frame(dif12=as.matrix(ren_diff), date=as.Date(time(ren_diff)))

ggplot(data = re.dat.seas, aes(x = date,y = dif12))+
  geom_line()+
  ggtitle("Residential Sector Natural Gas Energy Consumption") +
  xlab("Date") +ylab("Trillion Btu")

