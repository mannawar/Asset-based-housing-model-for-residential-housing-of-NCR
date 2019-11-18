#1.libraries used
library(psych)
library(dplyr)
library(corrr)
library(corrplot)
library(RColorBrewer)
library(PerformanceAnalytics)
library(GGally)
#2.Data acquisition
#Reading the data set in csv files
house1<-read.csv(file.choose())
#3.Cleaning the dataset from NA if any
house<-na.omit(house1)
#4. Listing the dataset
house<-as.data.frame(house)
house
#5. cleaning the dataset from NA, if any
house<-na.omit(house)
#6. Data preparation and exploration
str(house)
summary(house)
names(house)
head(house, n=5)
tail(house, n=5)
#7. Splitting the data set into study population and analysis dataset
house_n<-dplyr::select(house,1:3)
house_n<-as.data.frame(house_n)
house_n
house_Model<-dplyr::select(house,-1:-3)
house_Model<-as.data.frame(house_Model)
house_Model
#8. Checking the study dataset
str(house_n)
names(house_n)
psych::headTail(house_n)
#9. Visualization of variables
plot(house_n$Name_B, col="Green", main="Real estate agencies from where data was collected")
pie(1:4,labels=house_n$proff,main="Distribution of profession of study population")
#10.converting the variables into the factors
house_n$Name_B<-factor(house_n$Name_B)
house_n$Proff<-factor(house_n$Proff, levels=1:4, labels=c("Business","Private-Service","Government-Service","NRI"))
house_n$Proff
#11.Visualization of the variables
plot(house_n$Name_B, col= "blue", main= "The agency from where data is collected")
pie(1:4, labels= house_n$Proff, main = "DISTRIBUTION OF PROFESSIONS OF STUDY POPULATION")
#12. Summary of study population
table(house_n$Name_B)
table<-ftable(house_n$Name_B, house_n$Proff)
table
#13. Data exploration of analysis variables datsets
# converting all the variables as in data dictionary
house_Model$Price<-as.numeric(house_Model$Price)
house_Model$Price
house_Model$Location<-factor(house_Model$Location, levels=1:4, labels= c("GURGAON","DELHI","NIODA","FARIDABAD"))
house_Model$Location
barplot(table(house_Model$Location), col=rainbow(4), main="Property location wise slected for study")
house_Model$Prop_type<-factor(house_Model$Prop_type, level = 1:4, labels =c("1BHK","2BHK","3BHK","4BHK"))
house_Model$Prop_type
barplot(table(house_Model$Prop_type), col=rainbow(4), main="Distribution of property analyzed")
house_Model$Carpet_area<as.numeric(house_Model$Carpet_area)
house_Model$Carpet_area
house_Model$Year<-factor(house_Model$Year,levels=1:4,labels=c("2019","2018","2017","2016 and before"))
house_Model$Year
barplot(table(house_Model$Year), col=rainbow(4), main="Property Analyzed with respect to year of purchase")
house_Model$Lease_Per<-factor(house_Model$Lease_Per,levels=1:4, labels=c("0-5","6-10","11-15","15 and above"))
house_Model$Lease_Per
barplot(table(house_Model$Lease_Per), col=rainbow(4), main="Expected year of leasing the property")
house_Model$Prop_purchase<-factor(house_Model$Prop_purchase,levels=1:2,labels=c("Loan","Purchased"))
house_Model$Prop_purchase
house_Model$Payoff<-as.numeric(house_Model$Payoff)
house_Model$Payoff
house_Model$Rental_increment<-as.numeric(house_Model$Rental_increment)
house_Model$Rental_increment
house_Model$Main_Amount<-as.numeric(house_Model$Main_Amount)
house_Model$Main_Amount
house_Model$Social_infra<-sample(c(house_Model$Social_infra),2248, replace=TRUE)
house_Model$Social_infra
barplot(table(house_Model$Social_infra), col=rainbow(2), xlab="house_Model$Social_infra", ylab= "Percent of Total", main="Description of availability of social infrastructure")
#14. checking the analysis dataset
str(house_Model)
names(house_Model)
psych::headTail(house_Model)
#15. Descriptive statistics of the variables of the analysis dataset
plot(house_Model, col="red")
psych::describe(house_Model)
#16. Descriptive statistics of the continuous variable and plotting the variables
summary(house_Model$Price)
boxplot(house_Model$Price, col="red", xlab= "Price", main="BOX PLOT OF THE IDICATIVE SELLING PRICE IN CRORE")
summary(house_Model$Carpet_area)
boxplot(house_Model$Carpet_area)
summary(house_Model$Main_Amount)
boxplot(house_Model$Main_Amount)
summary(house_Model$Rental_increment)
boxplot(house_Model$Rental_increment)
#17.Frequency Statistics of the Qualitative Variables and Plotting the Variables
house_Model$Location<-factor(house_Model$Location,levels=1:4,labels=c("GURGAON","DELHI","NIODA","FARIDABAD"))
house_Model$Location
table(house_Model$Location)
#barplot missing
barplot(table(house_Model$Location), col=rainbow(4), main="Property location wise slected for study")
house_Model$Prop_type<-factor(house_Model$Prop_type,levels=1:4,labels=c("1BHK","2BHK","3BHK","4BHK"))
house_Model$Prop_type
table(house_Model$Prop_type)
#barplot missing
barplot(table(house_Model$Prop_type), main="Property location wise slected for study", xlab="house_Model$Prop_type", ylab="count", col=rainbow(4))
barplot(table(house_Model$Prop_type),col=rainbow(4),main="DISTRIBUTION OF PROPERTIES ANALYSED")
house_Model$Year<-factor(house_Model$Year,levels=1:4,labels=c("2019","2018","2017","2016 and beynd"))
house_Model$Year
plot(table(house_Model$Year),col=rainbow(4),main="DISTRIBUTION OF PROPERTIES PURCHASED(INYEAR) ANALYSED")
#18. Statistical Model development and evaluation multiple linear regression
Model1<-lm(Price~.,data=house_Model)
summary(Model1)
step(Model1,direction = "both",trace=1)
anova(Model1)
predict<-predict(Model1)
#19. Diagnostic analysis of plots
plot(predict(Model1),col="blue")
#20. Transfering the output in CSV FILE
write.csv(output, file= "Predicted.csv", row.names=FALSE)
#21. Testing the correlation between the contributing varaibles
corr
# It can also be called using the traditional method
# network_plot(correlate(mydata), min_cor=0.5)
mydata<-house_Model[,c('Price','Carpet_area')]
mydata
chart.Correlation(mydata,histrogram="TRUE",pch=10)
pairs.panels(mydata, scale=TRUE)
ggpairs(mydata)
ggcorr(mydata, nbreaks=8, palette='RdGy', label=TRUE, label_size=5, label_color='red')



