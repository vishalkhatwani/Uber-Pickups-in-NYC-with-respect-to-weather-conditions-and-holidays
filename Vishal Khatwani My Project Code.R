# Project Title:  NYC Uber Pickups with Weather and Holidays
# NAME: VISHAL KHATWANI
# EMAIL: VISHAL.KHATWANI1892@GMAIL.COM
# COLLEGE / COMPANY: University of Texas at Dallas 


uber <- read.csv(file.choose()) #reading the dataset in R
str(uber) #checking the classes of all the attributes
uber <- uber[!is.na(uber$borough),] #removing missing values
uber1 <- uber #creating a backup
uber1$hday <- as.numeric(uber1$hday) #converting to numeric for analysis
uber1$borough <- as.numeric(uber1$borough) #converting to numeric for analysis

describe(uber)#checking the attribute characterstics

newdate <- as.Date(uber$Date) #converting data to make time series analysis easy
levels(uber$Time)
par(mfrow=c(1,1))
#checking attributed distibutions
hist(uber$pickups,breaks = 30,xlim = c(0,2000)) 
plot(uber$Time,uber$pickups,type="s") #ploting time versus the number of pickups
t <- aggregate(uber$pickups,by=list(uber$Time), sum) #contigency tables for time versus pickups
t
prop.table(t[,2]) #checking propotions of pickups on wach time

hist(uber$Wind.Speed) #histogram for the wind speed vattribute
hist(uber$Visibility)#histogram for the visibility attribute
hist(uber$temp)#histogram for the temprature variable
hist(uber$DewPoint)#histogram for the dewpoint variable
hist(uber$SnowDepth)#histogram for the snowdepth variable
hist(as.numeric(uber$hday),breaks = 3)#histohram for the holiday variable

hdaypk <- aggregate(uber$pickups,by=list(uber$hday),mean)#number of pickups aggregated by holidays
hdaypk
nhday <- table(uber$hday)#checking the number of holdays in last 6 months
nhday
prop.table(hdaypk[,2])
prop.table(nhday)
plot(uber$Wind.Speed,uber$pickups,type = 's',main = "Wind Speed VS number of Pickups",xlab = "Wind Speed",ylab = "Pickups")#Wind speed SV number of pickups
plot(uber$Visibility,uber$pickups)# visibility VS number of pickups
plot(uber$temp,uber$pickups)# temprature versus the number of pickups
plot(uber$SnowDepth,uber$pickups)#snowdepthe versus the number of pickups
plot(uber$hday,uber$pickups)#holiday vwesus the number of pickups

corrgram(uber1,lower.panel = panel.pie)#corrogram for the variables 
cor(uber1[,4:12]) #correlation between all the attributes
plot(uber$borough,uber1$pickups,ylab="Pickups")#pickup plave VS number of pickups 

aggregate(uber1$pickups,by=list(uber$borough),sum) #sum of the pickups from each spot

#Our main objective here was to check what is the effect of various environment factors on Uber Pickups
#thus our target variable is "Number of Pickups"
#We run Leniar Regression first by considering all the variables and the removing all the variables that are least significant one by one

training <- uber1
#first regression
reg1 <- lm(training$pickups~training$borough+training$Wind.Speed+training$Visibility+training$temp+training$DewPoint+training$Sea.Press+training$SnowDepth+training$hday,data = training)
summary(reg1)
#second regresson
reg2 <- lm(training$pickups~training$borough+training$Wind.Speed+training$Visibility+training$temp+training$DewPoint+training$Sea.Press+training$SnowDepth,data = training)
summary(reg2)
#third regression and the best model obtained
reg3 <- lm(training$pickups~training$borough+training$Wind.Speed+training$Visibility+training$temp+training$DewPoint+training$SnowDepth,data = training)
summary(reg3)
plot(reg3)

#checking correlation between the variables that have a significant effect on our target variable i.e. "Number of Pickups"
subset.uber1 <- uber1[,4:12]
cor.plot(subset.uber1)#correlation plot

cov2cor(cov(subset.uber1)) #covariance
corrgram(subset.uber1,lower.panel = panel.pie)#corrogram

#pplotting Time VS number of pickups by highlighting the pickup place
#so that we get a better ideas as to where the pickups happern the most at what time
pl.pik <- ggplot(data = uber1,aes(x=uber1$Time,y=uber1$pickups))
pl.pik+geom_jitter(aes(colour= uber$borough))#greatjob
class(uber1$Time)

class(uber1$borough)
#plotting temprature VS number of pickups
temp.pick <- ggplot(data = uber1,aes(x=uber1$temp,y=uber1$pickups))
temp.pick+geom_point()
aggregate(uber1$pickups,by=list(uber1$temp),sum)

#plotting Visibility VS number of pickups
vis.pick <- ggplot(data = uber1,aes(x=uber1$Visibility,y=uber1$pickups))
vis.pick+geom_jitter()
aggregate(uber1$pickups,by=list(uber1$Visibility),sum)

#plotting holiday VS number of pickups from various locations
hldy.pick <- ggplot(data = uber1,aes(x=uber1$hday,y=uber1$pickups))
hldy.pick+geom_jitter(aes(colour=uber$borough),alpha=0.4)

#plotting snowdepth VS number of pickups
snowd.pick <- ggplot(data = uber1,aes(x=uber1$SnowDepth,y=uber1$pickups))
snowd.pick+geom_jitter(aes(colour=uber$borough))

#running T-test for target variable with all the independent variable
t.test(uber1$pickups,uber1$borough, type='pearson')
t.test(uber1$pickups,uber$Wind.Speed, type='pearson')
t.test(uber1$pickups,uber$Visibility, type='pearson')
t.test(uber1$pickups,uber$temp, type='pearson')
t.test(uber1$pickups,uber$DewPoint, type='pearson')
t.test(uber1$pickups,uber$Sea.Press, type='pearson')
t.test(uber1$pickups,uber$SnowDepth, type='pearson')
t.test(uber1$pickups,as.numeric(uber$hday), type='pearson')
t.test(uber1$pickups,as.numeric(uber$Time), type='pearson')

#------------------------ Random Forest Model------------------------

# random forest on uber dataset
uber.frst <- read_excel("E:/Data Analytics Internship/capstone project/random forest/uber_nyc_enriched.xlsx")
#reading the updated excel file withe a new catagorical attribute for number of pickups
#analysing the data summary
str(uber.frst)
describe(uber.frst)
#remong the number of pickups column to avaoid confusion
uber.frst <- uber.frst[,c(2:4,6:13)]
describe(uber.frst)
uber.frst$Date <- as.Date(uber.frst$Date)
uber.frst$Cat_Pick <- as.factor(uber.frst$Cat_Pick)
uber.frst$borough <- as.factor(uber.frst$borough)
uber.frst$hday <- as.factor(uber.frst$hday)

uber.frst1 <- uber.frst
uber.frst1$Time <- as.numeric(uber.frst1$Time) 
uber.frst1$Date <- as.numeric(uber.frst1$Date)
uber.frst1$borough <- as.numeric(uber.frst1$borough)

uber.frst1$hday <- as.numeric(uber.frst1$hday)
uber.frst1$`Wind Speed` <- as.numeric(uber.frst1$`Wind Speed`)
str(uber.frst1)
uber.frst1$borough <- as.factor(uber.frst1$borough)
uber.frst1$Cat_Pick <- as.factor(uber.frst1$Cat_Pick)
uber.frst1$hday <- as.factor(uber.frst1$hday)
uber.frst1$Time <- as.factor(uber.frst1$Time)
#splitting the data as 25% for training and 75% percent for validation
rndfrst <- sample.split(Y=uber.frst1$Cat_Pick,SplitRatio = 0.25)
train <- uber.frst1[rndfrst,]
test <- uber.frst1[!rndfrst,]
str(train)
train <- train[,2:13]#removing the dates column
describe(train)
colnames(train)
colnames(train)[colnames(train)=="Wind Speed"] <- c("wspeed")#updating column names
colnames(train)[colnames(train)=="Sea Press"] <- c("seapress")#updating column names
#implementing random forest model
rfrst.model <- randomForest(train$Cat_Pick~.,data = train)
importance(rfrst.model)#checking the importance of variables
varImpPlot(rfrst.model)#ploting importance of variables

test <- test[,2:13] 
colnames(test)[colnames(test)=="Wind Speed"] <- c("wspeed")
colnames(test)[colnames(test)=="Sea Press"] <- c("seapress")
#predicting the target variable in the test dataset
prdictclass <- predict(rfrst.model, test,type = 'class')
#confusion matrix for predicted vs target variable
a <- table(prediction=prdictclass,actual=test$Cat_Pick)
a
#accuracy of the model
accuracy <- sum(diag(a))/sum(a)
accuracy


