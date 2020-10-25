##To import my dataset directly from the internet
data<- read.csv('https://raw.githubusercontent.com/shiranli/ECO494-Project-I/main/raw%20data.csv')

##Checks the dimensions
dim(data)

##Shows the first 6 observations
head(data)

##Shows the last six observations.
tail(data)

##See the data in a summarized spreadsheet format in a new tab.
View(data)

##Generate the "six number summary" statistics
summary(data) 
summary(data$Merchandise.Trade....of.GDP.)
summary(data$Income.Person.GDP.capita..inflation.adjusted)
summary(data$countries..in.2019.)
summary(data$Inflation..annual...)
summary(data$GDP.Capita.Growth...per.year)
summary(data$Gini.Coefficient.for.Inequality)
summary(data$Population.Growth.Annual..)
summary(data$Developed.Country)

##check the object class
class(data)




##data cleaning process
##rename variables 
names(data)[1:8] <- c("countries", "inflation", "GDP", "trade", "income", "gini"
                      , "populationgrowth", "developedcountry")
str(data)

##create new dataset from starting point
##convert % to decimals
cleandata<-data
cleandata[,2] <- as.numeric(sub("%","",cleandata[,2]))/100
cleandata[,3] <- as.numeric(sub("%","",cleandata[,3]))/100
cleandata[,4] <- as.numeric(sub("%","",cleandata[,4]))/100
cleandata[,7] <- as.numeric(sub("%","",cleandata[,7]))/100
View(cleandata)

##clean data that are out of range
cleandata$inflation[cleandata$inflation>0.2 | cleandata$inflation< -0.1]<-NA
cleandata$GDP[cleandata$GDP>0.075 | cleandata$GDP<(-0.05)]<-NA
cleandata$trade[cleandata$trade<0 | cleandata$trade>1.5]<-NA
cleandata$income[cleandata$income<0 | cleandata$income>70000]<-NA
cleandata$gini[cleandata$gini<20 | cleandata$gini>60]<-NA
cleandata$populationgrowth[cleandata$populationgrowth>0.04 | cleandata$populationgrowth < (-0.01)]<-NA
##delete 2 columns that don't belong to this dataset
cleandata$X<-NULL 
cleandata$X.1<-NULL
View(cleandata)

##no data lost - same as dim(data)
dim(cleandata) 
str(cleandata)






##Plots
##install GGPLOT2 package
install.packages('ggplot2')

##load the GGPLOT2 library (MUST BE DONE EVERY TIME)
library(ggplot2)
library(plyr) 

##LOADING PREINSTALLED DATA FROM GGPLOT2 LIBRARY
data(cleandata) #LOADS THE DATA
cleandata #VIEW DATA IN CONSOLE OUTPUT
View(cleandata) #VIEW THE DATA IN LONG FORM
class(cleandata) #CHECK CLASS OF THE DATA

#LOADS THE MASS LIBRARY
library(MASS) 
library(mgcv)

##Distribution Graphs
#PLOTS A NONPARAMETRIC DENSITY CURVE
plot_a<-ggplot(cleandata,aes(x=inflation)) + geom_density()+
  xlab('inflation (annual %)') +  
  ylab('Probability Density') +
  ggtitle('Probability Density for Inflation')
plot_a
plot_c<-ggplot(cleandata,aes(x=GDP)) + geom_density()+
  xlab('GDP (per capita growth % per year)') +  
  ylab('Probability Density') +
  ggtitle('Probability Density for GDP')
plot_c
plot_d<-ggplot(cleandata,aes(x=income)) + geom_density()+
  xlab('income per person') +  
  ylab('Probability Density') +
  ggtitle('Probability Density for Income')
plot_d
plot_f<-ggplot(cleandata,aes(x=populationgrowth)) + geom_density()+
  xlab('population growth (annual %)') +  
  ylab('Probability Density') +
  ggtitle('Probability Density for Population Growth')
plot_f

##HISTOGRAMS AND FREQUENCY POLYGONS
ggplot(cleandata, aes(gini)) + geom_histogram(binwidth = 2)
ggplot(cleandata, aes(trade)) + geom_histogram(binwidth = 0.05)

##create a new data set 
##and delete outliers in order to see relationship between GDP and inflation
cleandata1<-cleandata
cleandata1$inflation[cleandata1$inflation>0.1 | cleandata1$inflation< -0.025]<-NA
cleandata1$GDP[cleandata1$GDP>0.06 | cleandata1$GDP<(-0.03)]<-NA
cleandata1$income[cleandata1$income<0 | cleandata1$income>70000]<-NA
#CREATE A SCATTER PLOT USING THE POINT GEOMETRY
ggplot(cleandata1, aes(x = GDP, y = inflation))+
  geom_point(aes(color = "label"))+
  geom_smooth(method = "gam", formula = y ~ s(x))
ggplot(cleandata1, aes(income, inflation))+ 
  geom_point(aes(color = "label"))+
  geom_smooth(method = "gam", formula = y ~ s(x))

##Plot the scatter plot with developing country and developed country
## compare the relationship
ggplot(cleandata1, aes(x = populationgrowth, y = inflation))+
  geom_point(aes(color = "label"))+
  geom_smooth(method = "gam", formula = y ~ s(x))+
  facet_grid(.~developedcountry, labeller = label_both)

#other graphs
ggplot(cleandata, aes(x = inflation, y = trade)) + geom_violin(aes(fill=inflation)) 
ggplot(cleandata, aes(x = inflation, y = populationgrowth)) + 
  geom_violin(aes(fill=inflation)) 


write.csv(cleandata, file = "TIDYDATA.csv")







