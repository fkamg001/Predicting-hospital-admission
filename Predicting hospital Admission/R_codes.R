
require('car')
require('nnet')
require('caret')
# Loading Admission dataset into R 
dat <- read.csv('../../Desktop/Practical Four/admissions (3).csv', header = T)
head(dat)
dim(dat)

# Identifying baseline
levels(dat$admission)


mn.is <- multinom(admission ~ died + white + los + age + age80, data = dat)
summary(mn.is)
Anova(mn.is)
str(mn.is)

mn.is.interaction <- multinom(admission ~ died + white + los + age + age80, white*died + 
                                died*los + los*white, data = dat)


glm.is.1 <- glm(admission ~ died, data = dat)
plot(mn.is.1)

step(mn.is.interaction)

library(VGAM) 
vglm.is <- vglm(admission ~ died + white + los, family=multinomial,data = dat)
summary(vglm.is)
Anova(vglm.is)
str(vglm.is)
hdeff(vglm.is)

mn.is.VIF <- multinom(admission ~ died + white + los + age8, data = dat)

vif(mn.is.VIF) > 10
step.VIF.mn <- step(mn.is.VIF)

AIC(step.VIF.mn)

# They are non-negative and a rule of thumb is that VIFs >10 is problematic
library(mctest)
vif(Step.mn.is) > 10
plot(admission ~ white + died + los, data=dat, col = 'red')
coplot(admission ~ provnum | died, data=dat, col = 'red')
coplot(admission ~ provnum | los, data=dat, col = 'red')
coplot(admission ~ provnum | age, data=dat, col = 'red')
coplot(admission ~ provnum | age80, data=dat, col = 'red')
coplot(admission ~ white | provnum, data=dat, col = 'red')

coplot(admission ~ los | age, data=dat, col = 'red')
coplot(admission ~ los | white, data=dat, col = 'red')
coplot(admission ~ los | died, data=dat, col = 'red')
coplot(admission ~ los | provnum, data=dat, col = 'red')
coplot(admission ~ los | age80, data=dat, col = 'red')

# checking for collineairty
fit.mn.is <- update(mn.is, . ~ . -died)
Anova(fit.mn.is)
AIC(fit.mn.is, mn.is)

# Model selection 
Anova(mn.is)
# Removing age variable
P.value.mn.is1 <- update(mn.is, . ~ . -age)
Anova(P.value.mn.is1)

# Removing age80 variable
P.value.mn.is2 <- update(P.value.mn.is1, . ~ . -age80)
Anova(P.value.mn.is2)

P.value.mn.model <- P.value.mn.is2
Anova(P.value.mn.model)

P.value.mn.is4 <- update(mn.is, . ~ . -provnum)
P.value.mn.is4 <- update(P.value.mn.is4, . ~ . -age)
P.value.mn.is4 <- update(P.value.mn.is4, . ~ . -age80)
Anova(P.value.mn.is4)

P.value.mn.is3 <- update(P.value.mn.is2, . ~ . -died)
AIC(P.value.mn.is3, P.value.mn.model, P.value.mn.is4,Step.P.value,Step.mn.is)

# Runnig BIC selection
Step.P.value <- step(P.value.mn.model)
Step.mn.is <- step(mn.is, direction="both", criterion = "AIC")
BIC.mn.is <- step(mn.is, direction="both", criterion = "BIC")
vif(Step.mn.is) > 10


# Performing all possible subsets selection
require('MuMIn')
options(na.action = "na.fail")
dredge.model <- dredge(mn.is, rank = "AICc")
subset(dredge.model, delta < 4)
get.models(dredge.model, subset = T)

dredge.fit <- multinom(formula = admission ~ died + los + white, data = dat)

summary(dredge.fit)
Anova(dredge.fit)

# Running Step on Vglm model 
step.vglm <- step(vglm.is, criterion = "AIC")

require('knitr')
# All returning same AICs
aic <- AIC(Step.mn.is, BIC.mn.is, P.value.mn.model, dredge.fit)
aic$DeltaAIC <- with(aic,AIC-min(AIC))

# Selecting Final Model 
final.model <- Step.mn.is
summary(final.model)
Anova(final.model)

# Calculating Z score and p-Value for the variables in the model.
# 2-tailed z test
z <- summary(final.model)$coefficients/summary(final.model)$standard.errors

# Calculating p-values of multinomial model
p <- (1 - pnorm(abs(z), 0, 1))*2

# Extracting the coefficients (odds) from the model and exponentiate
kable(coef(final.model))
kable(exp(coef(final.model)))
(1 - 0.7574024) * 100
(1 - 0.4779891) * 100

barplot(table(dat$admission), col = 'skyblue', xlab = 'Admission Type', ylab = 'counts', 
        main = 'Original counts')
barplot(count, col = 'slateblue', xlab = 'Admission Type', ylab = 'counts', 
        main = 'Predicted counts')

diff <- table(dat$admission) - count
table(dat$admission)
count

barplot(diff, col = 'black', xlab = 'Admission Type', ylab = 'counts', 
        main = 'Difference in counts' )

plot(,pred[,1],type="l",lty=1,ylim=range(pred),xlab ='Length of Stay', 
     ylab="Probability", main = "Predicted Length of Stay of non-white Patients when not died") 
lines(los,pred[,2],lty=2, col = 'red') 
lines(los,pred[,3],lty=3, col = 'blue')

# Getting the actual odds I antilog by the call 
exp(coef(final.model))
odds <- exp(coef(final.model))
odds * 100
coeffecits <- odds * coef(final.model)

coeffecits * 100

head(pp <- fitted(final.model))

# Predicitng probability
alpha <- coef(final.model)[,1] 
alpha

beta <- coef(final.model)[,-1]
beta

barplot(beta, col=c('darkblue', 'red'))

# confidence intervals
confint(mn.is)
confint(vglm.is)
confint(Step.mn.is)
confint(final.model)

count.1 <- predict(final.model,type="probs")[1,] * 1495
count.2 <- predict(mn.is,type="probs")[1,] * 1495


barplot(count, pred, col = 'skyblue')


# Making Predictions
par(mfrow=c(2,2))
pred <- predict(final.model, newdata = data.frame(died=0, white=0, los = los), type = "probs")
pred
par(bg="grey")

plot(los,pred[,1],type="l",lty=1,ylim=range(pred),xlab ='Length of Stay', 
ylab="Probability", main = "Predicted Length of Stay of non-white Patients given not died") 
lines(los,pred[,2],lty=2, col = 'red', label = 'Emergency') 
lines(los,pred[,3],lty=3, col = 'blue')

barplot(pred, main = 'Predicted Proportion for died=0, White=0')
# 
# hist <- ggplot(data = dat, aes(x=dat$admission, fill= admission)) +
#   geom_bar() + scale_fill_brewer(palette = 'Set2') + ylab('Counts') +
#   theme_light()
# print(hist)

# died=0, white=1
pred <- predict(final.model,newdata=data.frame(died = 0, white = 1,los = los), type="probs") 

barplot(pred.1)


par(bg="grey")
plot(los,pred[,1],type="l",lty=1,ylim=range(pred),xlab = 'Length of Stay', 
ylab="Probability", main = "Predicted Length of Stay of white Patients given not died") 
lines(los,pred[,2],lty=2, col = 'red') 
lines(los,pred[,3],lty=3, col = 'blue')



# white = 0, died=1
pred <- predict(final.model,newdata=data.frame(died = 1, white=0, los = los), type="probs") 
plot(los,pred[,1],type="l",lty=1,ylim=range(pred),xlab ='Length of Stay', 
ylab="Probability", main = "Predicted Length of Stay of non-white Patients given died") 
lines(los,pred[,2],lty=2, col = 'red') 
lines(los,pred[,3],lty=3, col = 'blue')

# white = 1 and Died = 1
pred <- predict(final.model,newdata=data.frame(died=1,
                                               white=1,
                                               los=los), type="probs") 
par(bg="grey")
plot(los,pred[,1],type="l",lty=1,ylim=range(pred),xlab ='Length of Stay', 
ylab="Probability", main = "Predicted Length of Stay of white Patients given died") 
lines(los,pred[,2],lty=2, col = 'red', labels = "Urgent") 
lines(los,pred[,3],lty=3, col = 'blue')


par(mfrow=c(1,1))
#--------------------------------------------------------------------------------------------------
bar.Plot.1 <- ggplot(data = dat, aes(x=as.factor(died), fill = admission)) + 
  scale_fill_brewer(palette = 'Set2') +
  geom_bar(position = 'fill') + ylim(c(0, 1)) + labs(x='Not Died and Died', y='Proportion') + theme_grey()
print(bar.Plot.1)

bar.Plot.2 <- ggplot(data = dat, aes(x=as.factor(white), fill = admission)) + 
  scale_fill_brewer(palette = 'Set2') + 
  geom_bar(position = 'fill') + ylim(c(0, 1)) +labs(x='Non-White and White', y='Proportion', 
                                                                         colour="blue") + theme_grey()
print(bar.Plot.2)

ggplot(data = dat, aes(x=count.1, fill = admission)) + 
  scale_fill_brewer(palette = 'Set2') +
  geom_bar(position = 'fill') + labs(x='Not Died and Died', y='Proportion') + theme_grey()



bar.Plot.3 <- ggplot(data = dat, aes(x= los, fill = admission)) + 
  scale_fill_brewer(palette = 'Set2') +
  geom_bar(position = 'fill') + ylim(c(0, 1)) + labs(x='Not Died and Died', y='Proportion') + theme_grey()
print(bar.Plot.3)

bar.Plot.4 <- ggplot(data = dat, aes(x= count, fill = admission)) + 
  scale_fill_brewer(palette = 'Set2') +
  geom_bar(position = 'fill') + labs(x='Not Died and Died', y='Proportion') + theme_grey()
print(bar.Plot.4)

predict(final.model,newdata=data.frame(died=x[1],white=x[2], los=x[3]), type="probs") 
predict(vglm.is,newdata=data.frame(died=x[1],white=x[2], los=x[3]), type="response") 

# Predicting counts
totcount <- with(dat,as.numeric(admission)) 
count <- predict(final.model,type="probs")[1,] * 1495 


max(dat$los)
min(dat$los)

# showing the predicted probability of each at median income, 
# but over the range of scottish from 0 to 116:
x <- c(median(dat$died), median(dat$white), median(dat$los)) 
x
par(mfrow=c(1,3))
los <- 0:116 
pred <- predict(final.model,newdata=data.frame(died=x[1],
                                               white=x[2],
                                               los=los), type="probs") 

plot(los,pred[,1],type="l",lty=1,ylim=range(pred),ylab="Probability", main = 'Median Predicted of Length of Stay') 
lines(los,pred[,2],lty=2, col = 'red') 
lines(los,pred[,3],lty=3, col = 'blue')

boxplot(pred,main = 'Median Predicted of los', ylab='Probability', col = 'skyblue1' )
died <- 0:1 
pred <- predict(final.model,newdata=data.frame(died = died, white = x[2],
                                                    los = x[3]), type="probs") 

barplot(pred, main = 'Median Predicted of Died')
plot(died,pred[,1],type="l",lty=1,ylim=range(pred),ylab="Probability", main = 'Median Predicted of Died') 
lines(died,pred[,2],lty=2, col = 'red') 
lines(died,pred[,3],lty=3, col = 'blue')
boxplot(pred,main = 'Median Predicted for Died', ylab='Probability', col = 'grey' )

# Obtaining Confidence Intervals for myprediction1

white <- 0:1
pred <- predict(final.model,newdata=data.frame(died = x[1],
                                               los = x[3], white = white), type="probs") 

plot(white,pred[,1],type="l",lty=1,ylim=range(pred),ylab="p") 
lines(white,pred[,2],lty=2, col = 'red') 
lines(white,pred[,3],lty=4, col = 'blue')

boxplot(pred,main = 'Median Predicted for white', ylab='Probability', col = 'skyblue4' )

max(dat$provnum)
min(dat$provnum)

# plot(provnum,pred[,1],type="l",lty=1,ylim=range(pred),ylab="p") 
# lines(provnum,pred[,2],lty=2, col = 'red') 
# lines(provnum,pred[,3],lty=3, col = 'blue')


# Showing effect Plots
require('effects')
effect.plot <- allEffects(final.model) 
plot(effect.plot, ask = FALSE)
plot(effect(Multinom.Simple), style = "stacked", colors = c("darkblue", "blue", "skyblue"), rug = FALSE)

# Bar Effects plots
dat$adm <- as.factor(dat$admission)
dat$di <- as.factor(dat$died)
dat$wh <- as.factor(dat$white)
dat$lo <- as.factor(dat$los)

# Effect Plots
require('MASS')
require('interplot')

Multinom.Simple <- multinom(adm ~ di + wh + los, data = dat)

par(mfrow=c(1,3))
plot(effect("di", Multinom.Simple), lwd = 2, 
     xlab= 'Survived and Died', ylab= 'Probability', main = 'Died Effect Plot')
plot(effect("wh", Multinom.Simple), lwd = 2, xlab= 'non-White and White', ylab= 'Probability', main = 'White Effect Plot')
plot(effect("los", Multinom.Simple), lwd = 2, xlab= 'Length of stay', ylab= 'Probability', 
     main = 'Length of Stay Effect Plot')
par(mfrow=c(1,1))

# Plotting observed vs predicted proportions:
mn.is.resid <- residuals(final.model) 
mn.is.fitted <- fitted(final.model) 
mn.is.observed <- mn.is.fitted + mn.is.resid 

par(mfrow=c(1,3)) 
for(i in 1:3){ 
  plot(mn.is.fitted[,i],mn.is.observed[,i], main=colnames(mn.is.fitted)[i],
       xlab="fitted p",ylab="observed p") 
abline(a=0,b=1) 
lines(smooth.spline(mn.is.fitted[,i], mn.is.observed[,i],df=4),col="red",lty=2) 
}

# Checking Assumptions
Model.Resid <- resid(final.model)
plot(fitted(final.model), Model.Resid, ylab = 'residuals', xlab = 'Fitted values')
plot(Step.mn.is$fitted.values, Model.Resid, ylab = 'residuals', xlab = 'Mother Weight')
plot(Step.mn.is$terms, Model.Resid, ylab = 'residuals', xlab = 'Mother Weight')


# Independence: 
# Checking serial correlation of residuals
# Testing with Durbin-Watson (H0 the errors are uncorrelated)
require(lmtest)
dwtest(as.numeric(admission) ~ died + white + los, data=dat)
res <- resid(final.model)
#---------------------------------------
Box.test(res[,1], lag=1, type = "Ljung")
Box.test(res[,2], lag=1, type = "Ljung")
Box.test(res[,3], lag=1, type = "Ljung")

acf(residuals(Step.mn.is))
acf(residuals(final.model))
#---------------------------------------

mn.sat<-multinom(admission ~ .,dat=dat) 
# weights: 99 (64 variable) initial value 4703291.041263 ... final value 4280785.380183 converged
dev <- deviance(final.model)- deviance(mn.sat) 
n <- dim(dat)[1] 
p <- length(coef(final.model)) 
1 - pchisq(dev,n-p)

# Checking fitness of the multinomial model 
mn.null <- multinom(admission ~ 1, dat = dat) 
# weights: 6 (2 variable) 
mcfadden <- 1 - (deviance(final.model) - deviance(mn.sat)) / + (deviance(mn.null) - deviance(mn.sat)) 
mcfadden


 
# Checking fitness of the vglm model 
vglm.null <- vglm(admission ~ 1, family=multinomial(refLevel="Elective"), dat = dat) 
logLik(vglm.is)
# weights: 6 (2 variable) 
mcfadden <- 1 - logLik(vglm.is)/ logLik(vglm.null) 
mcfadden

# Hauseman-McFadden test of independence of irrelevant alternatives 
m1 <- multinom(admission ~ ., data = dat)

## Estimate the same model for ground modes only (the variable avinc
## must be dropped because it is 0 for every observation

m2 <- multinom(admission ~ ., data = dat,reflevel="Elective",
            alt.subset=c("Elective","Urgent","Emergency"))

library('mlogit')
## Compute the test
hmftest(m1,m2)

# # calculating z value
# z.value <- (19-18)/(3/sqrt(20))
# 1 -pnorm(z.value)
# 
# pnorm(19, mean =18, sd=3/sqrt(20), lower.tail = F)
# 
# # Measure Possion likelihood
# x <- c(0,1,1,2,1,4,4)
# y <- dpois(x, lambda = 2.5)
# sum(log(y))

# sum(log(dexp(x, 2.5)))

# # Intercept plus origin 3 
# # adding horsepower interaction term to horsepower multiplied by 100 
# 34.476496 + 14.339718 + (-0.108723 + -0.121320)* 100 
# 2.5 + 4 +2 * (-2.5)


dat$Elective <- ifelse(dat$admission =='Elective', 1, 0)
dat$Urgent <- ifelse(dat$admission =='Urgent', 1, 0)
dat$Emergency <- ifelse(dat$admission == 'Emergency', 1, 0)

library('auditor')
dat$Elective
model.bin1 <- glm(Elective ~ died + white + los, family = binomial, data = dat)
model.bin2 <- glm(Urgent ~ died + white + los, family = binomial, dat = dat)
model.bin3 <- glm(Emergency ~ died + white + los, family = binomial, dat = dat)

model <- glm(Elective + Urgent ~ died, data = dat)
plot(model)

bin_au1 <- audit(model.bin1, data = dat, y=dat$Elective)
bin_au2 <- audit(model.bin2, data = dat, y=dat$Urgent)
bin_au3 <- audit(model.bin3, data = dat, y=dat$Emergency)

par(mfrow=c(1,3))

plotHalfNormal(bin_au1, sim=500, main = 'Half Normal Plot:Elective', 
               xlab = 'Half-Normal Quantiles', ylab = 'Residuals')
               
plotHalfNormal(bin_au2, sim=500, main = 'Half Normal Plot:Urgent',
               xlab = 'Half-Normal Quantiles', ylab = 'Residuals')

plotHalfNormal(bin_au3, sim=500, main = 'Half Normal Plot:Emergency',
               xlab = 'Half-Normal Quantiles', ylab = 'Residuals')

par(mfrow=c(1,1))

library('stargazer')

stargazer(final.model, coef=list(final.model), type = "html", title = "", style = "default")



par(mfrow=c(3,1))
termplot(model.bin1)
termplot(model.bin2)
termplot(model.bin3)
par(mfrow=c(1,1))
