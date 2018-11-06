#real data analysis - see C:/Users/lmcleary/Documents/test data/thesis/gdpa/gdpa.R

file = "C:/Users/lmcleary/Documents/test data/thesis/gdpa/data/post.txt"

post <- read.table(file , sep = ",", header=T)
#post <- subset(post, region.x =="Asia" | region.x=="Africa")

##############################reg
#we dont need to subset year already subsetted for cont
reg <- lm(log(rgdpnapc) ~ I(log(x)) +  I(log(x)^2), 
          subset(post, year >1947  & year < 1991  & post$region=="Africa"))

reg1 <- lm(log(rgdpnapc) ~ I(log(x)) +  I(log(x)^2) + sub.region
    +  I(landl*log(x)) , subset(post, year >1947 & year < 1991 & post$region=="Africa"))


summary(reg)
summary(reg1)


# several other useful functions
coefficients(reg1) # show coefficients
anova(reg1) # show anova table
vcov(reg1) # show covariance matrix for model parameters
confint(reg1, level=0.95) # CIs for model parameters
#regted(reg1) # show fitted values
res <- residuals(reg1) # show residuals
influence(reg1) # show diagnostics


library(ggplot2)

ggplot(subset(post, year>"1949" &
                region=="Africa" &year <1991), aes(log(rgdpnapc), log(x), color = log(pop))) +
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ I(log(x)) +  I(log(x)^2), size = 1) 
#+ geom_text(aes(label=alpha.3,hjust=0, vjust=0))


af <- subset(post, region=="Africa")
# Predict on full data: p
af$p <- predict(reg1, af, type="response")
af$predict <- exp(af$p)

#countries with data before 1900
af %>% 
  subset(!is.na(rgdpnapc), select=c(year, rgdpnapc, alpha.3)) %>%
  filter(year<1900) %>%
  distinct(alpha.3) 



#prediction vs data
ggplot(subset(af, alpha.3=="GHA"& year>1700 & year<2001), aes(year)) +
  geom_line(aes(y=rgdpnapc, colour=rgdpnapc))+
  geom_line(aes(y=predict))

ggplot(subset(af, alpha.3=="TUN"& year>1700 & year<2001), aes(year)) +
  geom_line(aes(y=rgdpnapc, colour=rgdpnapc))+
  geom_line(aes(y=predict))

ggplot(subset(af, alpha.3=="EGY"& year>1700 & year<2001), aes(year)) +
  geom_line(aes(y=rgdpnapc, colour=rgdpnapc))+
  geom_line(aes(y=predict))

ggplot(subset(af, alpha.3=="ZAF"& year>1700 & year<2001), aes(year)) +
  geom_line(aes(y=rgdpnapc, colour=rgdpnapc))+
  geom_line(aes(y=predict))





# Compute errors: error
af$error = af$predict-af$rgdpnapc
af$e2=af$error^2
af$rmse = sqrt(af$e2)
# Calculate RMSE
print(sqrt(mean(af$error^2, na.rm=T)))

af$subs <- af$pop *300
af$percentageexp <- af$exp/af$subs
#are larger countries more likely to be predicted better? or countries with a larger % exports?           
summary(lm(rmse ~ pop, af)) #no signif
summary(lm(rmse ~ rgdpnapc, af)) #higher gdppc LOWER performance - USE PRMSE?
#is this because of time bias?
summary(lm(rmse ~ year, af))  #sig
summary(lm(rmse ~ year + I(year^2), af))
summary(lm(rmse ~ rgdpnapc + year, af)) #now year not sig  
#split dataset into estmate
summary(lm(rmse ~ rgdpnapc + year, subset(af, year >1947, year <1991))) #now year sig  
summary(lm(rmse ~ rgdpnapc + year, subset(af, year <1947| year >1991))) #now year not sig  

summary(lm(rmse ~ percentageexp + I(percentageexp^2), af)) 

ggplot(af, aes(rmse, percentageexp, color = rmse)) +
  geom_point() +
  stat_smooth(method = "lm", formula = y ~  percentageexp, size = 1) +
  geom_text(aes(label=alpha.3,hjust=0, vjust=0))           
           
ggplot(af, aes(percentageexp, rmse , color = rmse)) +
  geom_point() 
           
#we should use percentage rmse of gdp
af$prmse = af$rmse / af$rgdpnapc
summary(lm(prmse ~ percentageexp + I(percentageexp^2), af)) 

ggplot(af, aes( percentageexp,prmse, color = rmse)) +
  geom_point()        

#evolution over time
ggplot(subset(af, year>1800), aes(year, rmse , color = rmse)) +
  geom_point() +
  geom_text(aes(label=alpha.3,hjust=0, vjust=0))           


#are larger countries more likely to be predicted better? or countries with a larger % exports?           
summary(lm(prmse ~ pop, af)) #higher pop lower error
summary(lm(prmse ~ pop + landl, af))
summary(lm(prmse ~ rgdpnapc, af)) #not sig
# time bias?
summary(lm(prmse ~ year, af))  #
summary(lm(prmse ~ year + I(year^2), af))
summary(lm(prmse ~ rgdpnapc + year, af)) # 
#split dataset into estmate
summary(lm(prmse ~ rgdpnapc + year, subset(af, year >1947, year <1991))) #  
summary(lm(prmse ~ rgdpnapc + year, subset(af, year <1947| year >1991))) #

summary(lm(rmse ~ percentageexp + I(percentageexp^2), af))



#evolution over time
ggplot(subset(af, year>1800), aes(year, prmse , color = prmse)) +
  geom_point() +
  geom_text(aes(label=alpha.3,hjust=0, vjust=0)) 




#try to split all data
#time fixed effects

reg2 <- lm(log(rgdpnapc) ~ I(log(x)) +  I(log(x)^2) + sub.region
           +  I(landl*log(x)) , subset(post, region=="Asia" | region=="Africa"))

summary(reg2)


reg3 <- lm(log(rgdpnapc) ~ I(log(x)) +  I(log(x)^2) + sub.region
           +  I(landl*log(x)) + year , subset(post, region=="Asia" | region=="Africa"))
summary(reg3)

#proportion 
percentage <- prop.table(table(post$region)) * 100
cbind(freq=table(post$region), percentage=percentage)
summary(subset(post, region=="Asia" | region=="Africa"&year<"1948"))



p <- subset(post, !is.na(post$rgdpnapc)& !is.na(post$logx)&
              region=="Asia" | region=="Africa")

p <- subset(p1, select= c(1,  5, 14, 15))
# create a list of 80% of the rows in the original dataset we can use for training
validation_index <- createDataPartition(p$rgdpnapc, p=0.80, list=FALSE)
# select 20% of the data for validation
validation <- p[-validation_index,]
# use the remaining 80% of data to training and testing the models
p <- p[validation_index,]




# Run algorithms using 10-fold cross validation
control <- trainControl(method="cv", number=10)
metric <- "Accuracy"




# a) linear algorithms
set.seed(7)
fit.lda <- train(log(rgdpnapc) ~., data=p, method="lda", metric=metric, trControl=control)
# b) nonlinear algorithms
# CART
set.seed(7)
fit.cart <- train(log(rgdpnapc) ~., data=p, method="rpart", metric=metric, trControl=control)
# kNN
set.seed(7)
fit.knn <- train(log(rgdpnapc) ~., data=p, method="knn", metric=metric, trControl=control)
# c) advanced algorithms
# SVM
set.seed(7)
fit.svm <- train(log(rgdpnapc) ~., data=p, method="svmRadial", metric=metric, trControl=control)
# Random Forest
set.seed(7)
fit.rf <- train(log(rgdpnapc) ~., data=p, method="rf", metric=metric, trControl=control)


# a) linear algorithms
set.seed(7)
fit.lda <- train(log(rgdpnapc) ~., data=p, method="lda", metric=metric, trControl=control)
# b) nonlinear algorithms
# CART
set.seed(7)
fit.cart <- train(log(rgdpnapc) ~., data=p, method="rpart", metric=metric, trControl=control)
# kNN
set.seed(7)
fit.knn <- train(log(rgdpnapc) ~., data=p, method="knn", metric=metric, trControl=control)
# c) advanced algorithms
# SVM
set.seed(7)
fit.svm <- train(log(rgdpnapc) ~., data=p, method="svmRadial", metric=metric, trControl=control)
# Random Forest
set.seed(7)
fit.rf <- train(log(rgdpnapc) ~., data=p, method="rf", metric=metric, trControl=control)









