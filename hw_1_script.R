#install.packages('ISLR')
#install.packages('MASS')
library(ISLR)
library(lattice)
data_ = Auto
dim(data_)
attach(data_)


# 1st Question---------------------------------------------------
#------------Pairwise scatter plots only across the numeric variables-----------
pairs(data_[, 1:7], main='Pair wise Scatter Plots for Auto data')

#-------------Separate Scatter Plots between mpg (response variable) and other features-------------
par(mfrow = c(6, 1))
par(mar=c(1,1,1,1))
plot(mpg~cylinders, data = data_, main='mpg vs cylinders')
plot(mpg~displacement, data = data_, main='mpg vs displacement')
plot(mpg~horsepower, data = data_, main='mpg vs horsepower')
plot(mpg~weight, data = data_, main='mpg vs weight')
plot(mpg~acceleration, data = data_, main='mpg vs acceleration')
plot(mpg~year, data = data_, main='mpg vs year')


#-----------Density plots for mpg and weight for distinct labels of origin------------
par(mfrow = c(2, 1))
densityplot(~mpg | origin, data = data_, auto.key = list(space = "right"), 
            groups = origin, main='MPG vs Origin') # Used for Outlier Removal too

densityplot(~weight | origin, data = data_, auto.key = list(space = "right"),
            groups = origin, main='Weight vs Origin')


#-----------Density plots for mpg, weight, acceleration and horsepower for different values of cylinders------------
densityplot(~mpg | cylinders, data = data_, auto.key = list(space = "right"), 
            groups = cylinders, main='MPG vs Cylinders') # Used for Outlier Removal too

densityplot(~weight | cylinders, data = data_, auto.key = list(space = "right"),
            groups = cylinders, main='Weight vs Cylinders')

densityplot(~acceleration | cylinders, data = data_, auto.key = list(space = "right"), 
            groups = cylinders, main='Acceleration vs Cylinders')

densityplot(~horsepower | cylinders, data = data_, auto.key = list(space = "right"),
            groups = cylinders, main='Horsepower vs Cylinders')


#----------Correlation matrix across the numeric variables------------
cor_data_auto = cor(data_[, 1:7])


#----------Additional Scatter plots----------
par(mfrow = c(2, 1))
plot(weight~year, data = data_, main='weight vs year')
plot(acceleration~horsepower, data = data_, main='acceleration vs horsepower')


#------------Box Plots Analysis------------
par(mfrow = c(3, 1))
boxplot(mpg, horizontal = TRUE, main='MPG Boxplot')
boxplot(horsepower, horizontal = TRUE, main='Horsepower Boxplot') # Outlier found at horsepower > 200
boxplot(acceleration, horizontal = TRUE, main='Acceleration Boxplot') # Outlier found at acceleration (>=22 and <=8)


#---------Outliers Identified Below-----------
subset(data_, horsepower > 200)
subset(data_, acceleration <= 8 | acceleration >= 22)
subset(data_, mpg>40 & origin==2)
subset(data_, mpg>41 & origin==3)
subset(data_, mpg>=38 & cylinders==6)

print(dim(data_)[1]) # 392 rows

#----------Removal of Outliers-----------
data_ = subset(data_, horsepower <= 200)
print(dim(data_)[1])
data_ = subset(data_, acceleration > 8 & acceleration < 22)
print(dim(data_)[1])
data_ = subset(data_, mpg<=40 | origin!=2)
print(dim(data_)[1])
data_ = subset(data_, mpg<=41 | origin!=3)
print(dim(data_)[1])
data_ = subset(data_, mpg<38 | cylinders!=6)
print(dim(data_)[1]) # 368 rows

#--------------Writing the cleaned Pre-Processed Data--------------
write.table(data_, file='/Users/iamchetry/Documents/UB_files/506/hw_1/auto_pre_processed.RData')
#-----------Please Use read.table to read the RData--------------


# 2nd Question -----------Model Training------------
data_$origin = as.factor(data_$origin)
model_ = lm(mpg~., data = data_[1:8]) # Multiple Regression
summary_model = summary(model_)
print(summary_model)

model_interaction = lm(mpg~(cylinders+displacement+horsepower+weight+acceleration+year+
                  origin)^2, data = data_[1:8]) # Multiple Regression by incorporating the interactions
summary_model_interaction = summary(model_interaction)
print(summary_model_interaction)


# 3rd Question -------------------------------------------------------

library('MASS')
dim(Boston)
head(Boston)
attach(Boston)

#----------Univariate Scatter Plots---------
par(mfrow = c(9, 1))
par(mar=c(1,1,1,1))
plot(crim, main='Crime Rate') #Skewed near 400th index
plot(zn, main='zn') #Skewed at 0
plot(indus, main='indus') # Skewed at 18.16
plot(chas, main='chas') # Large population at Label 0 only
plot(age, main='age')
plot(dis, main='dis')
plot(ptratio, main='ptratio') # Skewed at 20.2
plot(black, main='black') # Skewed at 400
plot(lstat, main='lstat')

#-----------Pairwise Scatter Plots------------
pairs(Boston, main='Pair wise Scatter Plots fo Boston data')

#----------Correlation matrix across variables------------
cor_data_boston = cor(Boston)
print(cor_data_boston)

#-----------Rows 400-450 have high values for Crime Rate, Tax Rate and Pupil-Teacher Ratio----------
par(mfrow = c(3, 1))
plot(crim, main='Crime Rate Scatter Plot')
plot(tax, main='Tax rate Scatter Plot')
plot(ptratio, main='Pupil-Teacher Ratio Scatter Plot')

#-----------Calculated Ranges-----------
range(crim)
range(tax)
range(ptratio)

#----------Suburbs Counts having average number of rooms per dwelling more than 7 or 8----------
dim(subset(Boston, rm>7)) # 64 rows (12.6% of total)
dim(subset(Boston, rm>8)) # 13 rows (2.5% of total)

#---------Comparison of overall data vs data with rm > 8----------
par(mfrow = c(4, 2))
hist(Boston$crim, main='Overall Crime Rate')
hist(subset(Boston, rm>8)$crim, main='Crime Rate for rm > 8')
# Crime rate is significantly low with range from 0.02009 to 3.47428

hist(Boston$zn, main='Overall Zn') # mean is 11.3
hist(subset(Boston, rm>8)$zn, main='Zn for rm > 8') # mean is 13.6

hist(Boston$age, main='Overall Age') # mean is 68.5
hist(subset(Boston, rm>8)$age, main='Age for rm > 8') # mean is 71.5

hist(Boston$medv, main='Overall Median Value of homes') # median is 21.2
hist(subset(Boston, rm>8)$medv,main='Median Value of homes for rm > 8') # median is 48.3
