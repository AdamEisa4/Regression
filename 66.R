install.packages("PerformanceAnalytics", repos = "http://cran.us.r-project.org")
install.packages('vtree')
install.packages("visreg")
library("PerformanceAnalytics")
library(ggplot2)
library(vtree)
library(visreg)

dataset_product <-read.csv(file.choose(),header = T)
names(dataset_product)[7] <- "average_review" #this function help us to change the name of the col name
names(dataset_product)[8] <- "main_sale_country"
names(dataset_product)[9] <- "price"

#----------------------------------------------------------------------------------------------------------
#Q2.1
dataset <- dataset_product$number_available_in_stock
dataset <- transform(dataset,number_of_reviews = dataset_product$number_of_reviews)
dataset <- transform(dataset,number_of_answered_questions =  dataset_product$number_of_answered_questions)
dataset <- transform(dataset,average_review = dataset_product$average_review)
dataset <- transform(dataset,price = dataset_product$price)
names(dataset)[1]  <- "number_available_in_stock"
cor(dataset)
#The new table (datasetUpdate) after deleting the variable from the previous section
datasetUpdate <- dataset_product$product_name
datasetUpdate <- transform(datasetUpdate,manufacturer = dataset_product$manufacturer)
datasetUpdate <- transform(datasetUpdate,number_available_in_stock = dataset_product$number_available_in_stock)
datasetUpdate <- transform(datasetUpdate,number_of_reviews = dataset_product$number_of_reviews)
datasetUpdate <- transform(datasetUpdate,ebay_category = dataset_product$ebay_category)
datasetUpdate <- transform(datasetUpdate,average_review = dataset_product$average_review)
datasetUpdate <- transform(datasetUpdate,main_sale_country = dataset_product$main_sale_country)
datasetUpdate <- transform(datasetUpdate,price = dataset_product$price)
names(datasetUpdate)[1]  <- "product_name"

#----------------------------------------------------------------------------------------------------------
#Q2.2
hist(dataset_product$number_available_in_stock, main = "Number Available In Stock", xlab = "Number Available In Stock")
hist(dataset_product$number_of_reviews, main = "Number Of Reviews", xlab = "Number Of Reviews")
hist(dataset_product$average_review, main = "Average Review", xlab = "Average Review")

#----------------------------------------------------------------------------------------------------------
#Q2.4
#1
fit11 <- lm(formula = datasetUpdate$price ~ datasetUpdate$number_available_in_stock * datasetUpdate$manufacturer)
summary(fit11)
plot(datasetUpdate$number_available_in_stock[datasetUpdate$manufacturer=="A Kent & Cleal game"],datasetUpdate$price[datasetUpdate$manufacturer=="A Kent & Cleal game"],col="blue",lwd=2,xlab="Number Available In Stock",ylab="Price",main="Price vs. Number Available In Stock")
points(datasetUpdate$number_available_in_stock[datasetUpdate$manufacturer=="A.S.PUPPETS"],datasetUpdate$price[datasetUpdate$manufacturer=="A.S.PUPPETS"],col="green",lwd=2)
points(datasetUpdate$number_available_in_stock[datasetUpdate$manufacturer=="AFV Club"],datasetUpdate$price[datasetUpdate$manufacturer=="AFV Club"],col="red",lwd=2)
points(datasetUpdate$number_available_in_stock[datasetUpdate$manufacturer=="Airfix"],datasetUpdate$price[datasetUpdate$manufacturer=="Airfix"],col="purple",lwd=2)
points(datasetUpdate$number_available_in_stock[datasetUpdate$manufacturer=="Alex"],datasetUpdate$price[datasetUpdate$manufacturer=="Alex"],col="black",lwd=2)
points(datasetUpdate$number_available_in_stock[datasetUpdate$manufacturer=="Amscan"],datasetUpdate$price[datasetUpdate$manufacturer=="Amscan"],col="orange",lwd=2)
legend(1,95,legend=c("A Kent & Cleal game","A.S.PUPPETS","AFV Club","Airfix","Alex","Amscan"),col=c("blue","green","red","purple","black","orange"),pch=c(1,1,1,1,1,1),bty="n" )
abline(28.427,-1.455,lwd=3,col="blue")
abline(28.427-6.020,-1.455,lwd=3,col="green")
abline(28.427-11.092,-1.455+4.120,lwd=3,col="red")
abline(28.427-9.603,-1.455+1.122,lwd=3,col="purple")
abline(28.427-6.578,-1.455+1.290,lwd=3,col="black")
abline(28.427-23.582,-1.455+1.365,lwd=3,col="orange")

fit12 <- lm(formula = datasetUpdate$price ~ datasetUpdate$number_available_in_stock * datasetUpdate$ebay_category)
summary(fit12)
plot(datasetUpdate$number_available_in_stock[datasetUpdate$ebay_category=="Balloons"],datasetUpdate$price[datasetUpdate$ebay_category=="Balloons"],col="orange",lwd=2,xlab="Number Available In Stock",ylab="Price",main="Price vs. Number Available In Stock")
points(datasetUpdate$number_available_in_stock[datasetUpdate$ebay_category=="Children's Craft Kits"],datasetUpdate$price[datasetUpdate$ebay_category=="Children's Craft Kits"],col="gray",lwd=2)
points(datasetUpdate$number_available_in_stock[datasetUpdate$ebay_category=="Hand Puppets"],datasetUpdate$price[datasetUpdate$ebay_category=="Hand Puppets"],col="green",lwd=2)
points(datasetUpdate$number_available_in_stock[datasetUpdate$ebay_category=="Military"],datasetUpdate$price[datasetUpdate$ebay_category=="Military"],col="purple",lwd=2)
points(datasetUpdate$number_available_in_stock[datasetUpdate$ebay_category=="Streamers"],datasetUpdate$price[datasetUpdate$ebay_category=="Streamers"],col="black",lwd=2)
points(datasetUpdate$number_available_in_stock[datasetUpdate$ebay_category=="Tanks"],datasetUpdate$price[datasetUpdate$ebay_category=="Tanks"],col="red",lwd=2)
points(datasetUpdate$number_available_in_stock[datasetUpdate$ebay_category=="Toy"],datasetUpdate$price[datasetUpdate$ebay_category=="Toy"],col="blue",lwd=2)
abline(4.53558,-0.07024,lwd=3,col="orange")
abline(4.53558+17.31363,-0.07024-0.09514,lwd=3,col="gray")
abline(4.53558+17.87183,-0.07024,lwd=3,col="green")
abline(4.53558+12.15109,-0.07024-0.18643,lwd=3,col="purple")
abline(4.53558-0.72109,-0.07024+0.03214,lwd=3,col="black")
abline(4.53558+14.68890,-0.07024+0.78291,lwd=3,col="red")
abline(4.53558+20.60509,-0.07024-0.88708,lwd=3,col="blue")

fit13 <- lm(formula = datasetUpdate$price ~ datasetUpdate$number_available_in_stock * datasetUpdate$main_sale_country)
summary(fit13)
#2
fit21 <- lm(formula = datasetUpdate$price ~ datasetUpdate$number_of_reviews * datasetUpdate$manufacturer)
summary(fit21)
fit22 <- lm(formula = datasetUpdate$price ~ datasetUpdate$number_of_reviews * datasetUpdate$ebay_category)
summary(fit22)

fit23 <- lm(formula = datasetUpdate$price ~ datasetUpdate$number_of_reviews * datasetUpdate$main_sale_country)
summary(fit23)
plot(datasetUpdate$number_of_reviews[datasetUpdate$main_sale_country=="Israel"],datasetUpdate$price[datasetUpdate$main_sale_country=="Israel"],col="orange",lwd=2,xlab="Number Of Reviews",ylab="Price",main="Price vs. Number Of Reviews")
points(datasetUpdate$number_of_reviews[datasetUpdate$main_sale_country=="China"],datasetUpdate$price[datasetUpdate$main_sale_country=="China"],col="green",lwd=2)
points(datasetUpdate$number_of_reviews[datasetUpdate$main_sale_country=="Italy"],datasetUpdate$price[datasetUpdate$main_sale_country=="Italy"],col="purple",lwd=2)
points(datasetUpdate$number_of_reviews[datasetUpdate$main_sale_country=="Japan"],datasetUpdate$price[datasetUpdate$main_sale_country=="Japan"],col="black",lwd=2)
points(datasetUpdate$number_of_reviews[datasetUpdate$main_sale_country=="NY"],datasetUpdate$price[datasetUpdate$main_sale_country=="NY"],col="red",lwd=2)
#points(datasetUpdate$number_of_reviews[datasetUpdate$main_sale_country==NULL],datasetUpdate$price[datasetUpdate$main_sale_country==NULL],col="blue",lwd=2)
legend(12,25,legend=c("Israel","China","Italy","Japan","NY","CountryIsNull"),col=c("orange","green","purple","black","red","blue"),pch=c(1,1,1,1,1,1),bty="n" )
abline(-73.29,31.53,lwd=3,col="orange")
abline(-73.29-70.19,31.53+31.07,lwd=3,col="green")
abline(-73.29-68.91,31.53+31.10,lwd=3,col="purple")
abline(-73.29-70.79,31.53+30.87,lwd=3,col="black")
abline(-73.29-56.60,31.53+27.55,lwd=3,col="red")
#abline(-73.29+84.14,31.53-31.17,lwd=3,col="blue")

#3
fit31 <- lm(formula = datasetUpdate$price ~ datasetUpdate$average_review * datasetUpdate$manufacturer)
summary(fit31)
fit32 <- lm(formula = datasetUpdate$price ~ datasetUpdate$average_review * datasetUpdate$ebay_category)
summary(fit32)
fit33 <- lm(formula = datasetUpdate$price ~ datasetUpdate$average_review * datasetUpdate$main_sale_country)
summary(fit33)

#----------------------------------------------------------------------------------------------------------
#Q3.1
Emp <- lm(datasetUpdate$price ~ 1, data = datasetUpdate)
Full <- lm(datasetUpdate$price ~ datasetUpdate$manufacturer + datasetUpdate$number_available_in_stock + datasetUpdate$number_of_reviews + datasetUpdate$ebay_category + datasetUpdate$average_review + datasetUpdate$main_sale_country)
fwd.model <- step(Emp, direction='forward', scope = ~ datasetUpdate$manufacturer + datasetUpdate$number_available_in_stock + datasetUpdate$number_of_reviews + datasetUpdate$ebay_category + datasetUpdate$average_review + datasetUpdate$main_sale_country)
bw.model <- step(Full, direction='backward', scope = ~ 1)
sw.model <- step(Emp, direction='both', scope = ~ datasetUpdate$manufacturer + datasetUpdate$number_available_in_stock + datasetUpdate$number_of_reviews + datasetUpdate$ebay_category + datasetUpdate$average_review + datasetUpdate$main_sale_country)
AIC(fwd.model)
AIC(bw.model)
AIC(sw.model)
BIC(fwd.model)
BIC(bw.model)
BIC(sw.model)
summary(fwd.model)
summary(bw.model)
summary(sw.model)

#----------------------------------------------------------------------------------------------------------
#Q3.2
model <- fwd.model
summary(model)
datasetUpdate$fitted <- fitted(model) # predicted values
datasetUpdate$residuals <- residuals(model) # residuals- errors
s.e_res <- sqrt(var(datasetUpdate$residuals)) # Calculating the standard deviation of the errors
datasetUpdate$stan_residuals <- (residuals(model)/s.e_res) # Saving the datasetUpdate after the calculation in a new row
plot(datasetUpdate$fitted, datasetUpdate$stan_residuals, xlab="Predicted Value", ylab="Normalized error")
abline(0,0,col="red",lwd=3)
qqnorm(datasetUpdate$stan_residuals)
abline(a=0, b=1,col="blue",lwd=3)
hist(datasetUpdate$stan_residuals ,prob=TRUE, xlab ="Normalized error", main="Histogram of normalized error")
lines(density(datasetUpdate$stan_residuals),col="blue",lwd=2)
ks.test(x=datasetUpdate$stan_residuals,y="pnorm",alternative = "two.sided",exact=NULL) 
shapiro.test(datasetUpdate$stan_residuals)

#----------------------------------------------------------------------------------------------------------
#Q4
#Full Model with all variable
Emp2 <- lm(dataset_product$price ~ 1, data = dataset_product)
Full2 <- lm(dataset_product$price ~ dataset_product$manufacturer + dataset_product$number_available_in_stock + dataset_product$number_of_reviews + dataset_product$number_of_answered_questions + dataset_product$ebay_category + dataset_product$average_review + dataset_product$main_sale_country)
fwd.model2 <- step(Emp2, direction='forward', scope = ~ dataset_product$manufacturer + dataset_product$number_available_in_stock + dataset_product$number_of_reviews + dataset_product$number_of_answered_questions + dataset_product$ebay_category + dataset_product$average_review + dataset_product$main_sale_country)
bw.model2 <- step(Full2, direction='backward', scope = ~ 1)
sw.model2 <- step(Emp2, direction='both', scope = ~ dataset_product$manufacturer + dataset_product$number_available_in_stock + dataset_product$number_of_reviews + dataset_product$number_of_answered_questions + dataset_product$ebay_category + dataset_product$average_review + dataset_product$main_sale_country)
AIC(fwd.model2)
AIC(bw.model2) 
AIC(sw.model2)
summary(fwd.model2)
summary(bw.model2)
summary(sw.model2)

#Transformation on the explained variable (price^2)
dataset_product$poly_model <- (dataset_product$price)^2
#Transformation on the explained variable (sqrt(price))
dataset_product$Sqrt_Price_Model <- sqrt(dataset_product$price)
#Transformation on the explained variable (ln(Price))
dataset_product$ln_model <- log(dataset_product$price)

#Analyzing the data for price^2
Emp3 <- lm(dataset_product$poly_model ~ 1, data = dataset_product)
Full3 <- lm(dataset_product$poly_model ~ dataset_product$manufacturer + dataset_product$number_available_in_stock + dataset_product$number_of_reviews + dataset_product$ebay_category + dataset_product$average_review + dataset_product$main_sale_country)
fwd.model3 <- step(Emp3, direction='forward', scope = ~ dataset_product$manufacturer + dataset_product$number_available_in_stock + dataset_product$number_of_reviews + dataset_product$ebay_category + dataset_product$average_review + dataset_product$main_sale_country)
bw.model3 <- step(Full3, direction='backward', scope = ~ 1)
sw.model3 <- step(Emp3, direction='both', scope = ~ dataset_product$manufacturer + dataset_product$number_available_in_stock + dataset_product$number_of_reviews + dataset_product$ebay_category + dataset_product$average_review + dataset_product$main_sale_country)
AIC(fwd.model3)
AIC(bw.model3)
AIC(sw.model3)
summary(fwd.model3)
summary(bw.model3)
summary(sw.model3)

#Analyzing the data for price^0.5
Emp4 <- lm(dataset_product$Sqrt_Price_Model ~ 1, data = dataset_product)
Full4 <- lm(dataset_product$Sqrt_Price_Model ~ dataset_product$manufacturer + dataset_product$number_available_in_stock + dataset_product$number_of_reviews + dataset_product$ebay_category + dataset_product$average_review + dataset_product$main_sale_country)
fwd.model4 <- step(Emp4, direction='forward', scope = ~ dataset_product$manufacturer + dataset_product$number_available_in_stock + dataset_product$number_of_reviews + dataset_product$ebay_category + dataset_product$average_review + dataset_product$main_sale_country)
bw.model4 <- step(Full4, direction='backward', scope = ~ 1)
sw.model4 <- step(Emp4, direction='both', scope = ~ dataset_product$manufacturer + dataset_product$number_available_in_stock + dataset_product$number_of_reviews + dataset_product$ebay_category + dataset_product$average_review + dataset_product$main_sale_country)
AIC(fwd.model4)
AIC(bw.model4)
AIC(sw.model4)
summary(fwd.model4)
summary(bw.model4)
summary(sw.model4)

#Analyzing the data for ln(price)
Emp5 <- lm(dataset_product$ln_model ~ 1, data = dataset_product)
Full5 <- lm(dataset_product$ln_model ~ dataset_product$manufacturer + dataset_product$number_available_in_stock + dataset_product$number_of_reviews + dataset_product$ebay_category + dataset_product$average_review + dataset_product$main_sale_country)
fwd.model5 <- step(Emp5, direction='forward', scope = ~ dataset_product$manufacturer + dataset_product$number_available_in_stock + dataset_product$number_of_reviews + dataset_product$ebay_category + dataset_product$average_review + dataset_product$main_sale_country)
bw.model5 <- step(Full5, direction='backward', scope = ~ 1)
sw.model5 <- step(Emp5, direction='both', scope = ~ dataset_product$manufacturer + dataset_product$number_available_in_stock + dataset_product$number_of_reviews + dataset_product$ebay_category + dataset_product$average_review + dataset_product$main_sale_country)
AIC(fwd.model5)
AIC(bw.model5)
AIC(sw.model5)
summary(fwd.model5)
summary(bw.model5)
summary(sw.model5)

model2 <- fwd.model5
summary(model2)
datasetUpdate$fitted2 <- fitted(model2) # predicted values
datasetUpdate$residuals2 <- residuals(model2) # residuals- errors
s.e_res2 <- sqrt(var(datasetUpdate$residuals2)) # Calculating the standard deviation of the errors
datasetUpdate$stan_residuals2 <- (residuals(model2)/s.e_res2) # Saving the datasetUpdate after the calculation in a new row
plot(datasetUpdate$fitted2, datasetUpdate$stan_residuals2, xlab="Predicted Value", ylab="Normalized error")
abline(0,0,col="red",lwd=3)
qqnorm(datasetUpdate$stan_residuals2)
abline(a=0, b=1,col="blue",lwd=3)
hist(datasetUpdate$stan_residuals2 ,prob=TRUE, xlab ="Normalized error", main="Histogram of normalized error")
lines(density(datasetUpdate$stan_residuals2),col="blue",lwd=2)
ks.test(x=datasetUpdate$stan_residuals2,y="pnorm",alternative = "two.sided",exact=NULL) 
shapiro.test(datasetUpdate$stan_residuals2)

