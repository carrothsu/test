library(corrplot)
library(ggplot2)
library(reshape)
library(ROCR)
library(mgcv)
library(MASS)
library(tree)
library(e1071)
library(randomForest)
library(glmnet)
library(boot)
library(hmeasure)
#######################part 1##################
setwd("C:/Users/hongyixu/Downloads")
loan<-read.csv("cs-training.csv")
loan<-na.omit(loan)
loanp<-loan[,-(1:2)]
head(loanp)
str(loanp)
dim(loanp)

loanp.train<-loanp[1:60269,]
library(ggplot2)
library(reshape)
train.melt<-melt(loanp.train)
ggplot(train.melt,aes(x=value))+
  geom_density()+
  facet_wrap(~variable,scales="free")

cor(loanp.train)
str(loanp.train)
library(corrplot)
corrplot(cor(loanp.train))

#logistic
setwd("C:/Users/hongyixu/Downloads")
loan<-read.csv("cs-training.csv")
loan<-na.omit(loan)
loanp<-loan[,-1]


x<-matrix(1,120269,1)
Num30<-as.numeric(loanp$NumberOfTime30.59DaysPastDueNotWorse)
for(i in 1:120269)
{if(Num30[i]==0)
  x[i]=0}
loanp$factor30<-x

x<-matrix(1,120269,1)
Num60<-as.numeric(loanp$NumberOfTime60.89DaysPastDueNotWorse)
for(i in 1:120269)
{if(Num60[i]==0)
  x[i]=0}
loanp$factor60<-x

x<-matrix(1,120269,1)
Num90<-as.numeric(loanp$NumberOfTimes90DaysLate)
for(i in 1:120269)
{if(Num90[i]==0)
  x[i]=0}
loanp$factor90<-x
loanp$SeriousDlqin2yrs<-as.factor(loanp$SeriousDlqin2yrs)
loanp.train<-loanp[1:60629,]
loanp.test<-loanp[-(1:60629),]



subset3<-subset(loanp.train,select=c(-NumberOfTime30.59DaysPastDueNotWorse,-NumberOfTime60.89DaysPastDueNotWorse,-NumberOfTimes90DaysLate))
subset33<-subset(loanp.test,select=c(-NumberOfTime30.59DaysPastDueNotWorse,-NumberOfTime60.89DaysPastDueNotWorse,-NumberOfTimes90DaysLate))

str(subset3)
lr.fit<-glm(SeriousDlqin2yrs~.,data=subset3,family=binomial)
summary(lr.fit)
lr.fit2<-glm(SeriousDlqin2yrs~.-RevolvingUtilizationOfUnsecuredLines,data=subset3,family=binomial)
summary(lr.fit2)
deviance(lr.fit2)
#[1] 24582.19

templr<-glm(SeriousDlqin2yrs~1,data=subset3,family=binomial)
tempScope<-formula(glm(SeriousDlqin2yrs~.,subset3,family=binomial))
step<-stepAIC(templr,scope=tempScope,
              direction="forward")
step$anova

templr<-glm(SeriousDlqin2yrs~.,data=subset3,family=binomial)
tempScope<-formula(glm(SeriousDlqin2yrs~1,subset3,family=binomial))
step<-stepAIC(templr,scope=tempScope,
              direction="backward")
step$anova

library(bestglm)
temp<-subset3
names(temp)<-c("y",paste0("X",1:10))
bestglm(temp,family=binomial,IC="BIC",method="forward")

cv.glm(data=subset33,glmfit=lr.fit2, K=20)$delta
# 0.07629258 0.05450913

lr.pred<-prediction(predictions=predict(lr.fit2,subset33),labels=subset33$SeriousDlqin2yrs)
performance(lr.pred,"auc")@y.values[[1]]
#0.8115768
lr.ROC<-performance(lr.pred,"tpr","fpr")
plot(lr.ROC)

#logistic with spline
library(mgcv)
gam.1<-gam(SeriousDlqin2yrs~s(RevolvingUtilizationOfUnsecuredLines)+s(age)+s(MonthlyIncome)+s(NumberOfOpenCreditLinesAndLoans)+s(NumberRealEstateLoansOrLines)+s(NumberOfDependents)+s(DebtRatio)+factor30+factor60+factor90   ,data=subset3,family=binomial)
summary(gam.1)

gam.probs <- predict(gam.1,type="response") 
contrasts<-(loanp.train$SeriousDlqin2yrs)
gam.pred=rep(0,60629)
gam.pred[gam.probs>0.5]=1
table(gam.pred,loanp.train$SeriousDlqin2yrs)
#gam.pred     0     1
#0 55844  3434
#1   598   753
mean(gam.pred==loanp.train$SeriousDlqin2yrs)
#[1] 0.9334972

gam.probs <- predict(gam.1,type="response") 
contrasts<-(loanp.test$SeriousDlqin2yrs)
gam.pred=rep(0,60000)
gam.pred[gam.probs>0.5]=1
table(gam.pred,loanp.train$SeriousDlqin2yrs)

#gam.pred     0     1
#0 55259  3405
#1   598   753

cv.glm(data=subset33,glmfit=gam.1)

library(gamclass)
CVgam(SeriousDlqin2yrs~s(RevolvingUtilizationOfUnsecuredLines)+s(age)+s(MonthlyIncome)+s(NumberOfOpenCreditLinesAndLoans)+s(NumberRealEstateLoansOrLines)+s(NumberOfDependents)+s(DebtRatio)+factor30+factor60+factor90,subset3, nfold=10)

pred=predict(gam.1,type='response')
r=ROCEmpiric(group,1-pred)
#LDA
lda.fit<-lda(SeriousDlqin2yrs~.,data=subset3)
lda.fit
lda.class<-predict(lda.fit)$class
table(subset3$SeriousDlqin2yrs, lda.class)
mean(lda.class==loanp.train$SeriousDlqin2yrs)

lda.pred<-predict(lda.fit,subset33)
lda.class.test<-lda.pred$class
table(lda.class.test,subset33$SeriousDlqin2yrs)
mean(lda.class.test==loanp.test$SeriousDlqin2yrs)


X.lda <- lda(formula=SeriousDlqin2yrs~., data=subset3)
out.lda <- predict(X.lda,newdata=subset3)  
scores.lda <- out.lda$posterior[,2]
result.lda <- HMeasure(subset3$SeriousDlqin2yrs, scores.lda)
summary.hmeasure(result.lda)
plotROC(result.lda)

out.lda <- predict(X.lda,newdata=subset33)  
scores.lda <- out.lda$posterior[,2]
result.lda <- HMeasure(subset33$SeriousDlqin2yrs, scores.lda)
summary.hmeasure(result.lda)
plotROC(result.lda)
#AUC: train 0.8054577/ test 0.8102717

#QDA
qda.fit<-qda(SeriousDlqin2yrs~.,data=subset3)
summary(qda.fit)
qda.class<-predict(qda.fit)$class
table(subset3$SeriousDlqin2yrs,qda.class)
#0.89635
qda.pred<-predict(qda.fit,subset33)
qda.class.test<-qda.pred$class
table(qda.class.test, subset33$SeriousDlqin2yrs)

X.qda <- qda(formula=SeriousDlqin2yrs~., data=subset3)
out.qda <- predict(qda,newdata=subset3)  
scores.qda <- out.qda$posterior[,2]
result.qda <- HMeasure(subset3$SeriousDlqin2yrs, scores.qda)
summary.hmeasure(result.qda)
plotROC(result.qda)
#train auc 0.8041398

#svm
credit.svm<-svm(SeriousDlqin2yrs~.,data=subset3,scale=TRUE, method="C-classification", cost=0.05,kernel="linear")
svm.class<-predict(credit.svm,subset3,type="class")
table(svm.class,subset3$SeriousDlqin2yrs)
#svm.class     0     1
#0 56442  4187
#1     0     0
credit.svm2<-svm(SeriousDlqin2yrs~.,data=subset3,scale=TRUE, method="C-classification", cost=0.05,kernel="polynomial")
svm.class<-predict(credit.svm2,type="class")
table(subset3$SeriousDlqin2yrs, svm.class)
#svm.class
#0     1
#0 56095   347
#1  3708   479
svm.class<-predict(credit.svm2,newdata=subset33,type="class")
table(subset33$SeriousDlqin2yrs, svm.class)
#svm.class
#0     1
#0 55156   314
#1  3733   437

credit.svm3<-svm(SeriousDlqin2yrs~.,data=subset3,scale=TRUE, method="C-classification", cost=0.05,kernel="radial")
svm.class<-predict(credit.svm3,type="class")
table(subset3$SeriousDlqin2yrs, svm.class)
#svm.class
#0     1
#0 56108   334
#1  3729   458

#lasso and ridge
X=as.matrix(subset3[,-1])
y=as.matrix(subset3[,1])
ridge.fit<-glmnet(x=X,y=y,family="binomial", alpha=0,)
plot(ridge.fit, xvar="lambda")
ridge.class<-predict(ridge.fit,newx=X, type="class")
#all zero, not fit~

lasso.fit<-glmnet(x=X,y=y,family="binomial", alpha=1)
plot(lasso.fit,xvar="lambda")
lasso.class<-predict(lasso.fit,newx=X, type="class")
lasso.class
#all zero, not fit~

tempX<-as.matrix(subset3[,-1])
tempY<-as.matrix(subset3[,1])
cv=cv.glmnet(x=tempX, y=tempY)
model=glmnet(x=tempX, y=tempY,type.gaussian="covariance",lambda=cv$lambda.min)
predict(model, type="coefficients")


#random forest tree
# not avaliable for default setting
default<-ifelse(subset3$SeriousDlqin2yrs==1,"yes","no")
table(default=="no")
credit<-data.frame(subset3,default)
tree.credit<-tree(default~.-SeriousDlqin2yrs,data=credit)
summary(tree.credit)
tree.credit
--#test data
  default2<-ifelse(subset33$SeriousDlqin2yrs==1,"yes","no")
credit.test<-data.frame(subset33,default2)
tree.pred=predict(tree.credit,credit.test,type="class")
table(tree.pred,default2)

rf1 = randomForest( SeriousDlqin2yrs~.,subset3,ntree=500, sampsize=5000)
rf2 = randomForest( SeriousDlqin2yrs~.,subset3,ntree=4000,sampsize=c(12000,4000),strata=subset3$ SeriousDlqin2yrs)
rf3 = randomForest( SeriousDlqin2yrs~.,subset3,ntree=4000,sampsize=c(8000,4000),strata=subset3$ SeriousDlqin2yrs)
rf4 = randomForest( SeriousDlqin2yrs~.,subset3,ntree=4000,sampsize=c(4000,4000)  ,strata=subset3$ SeriousDlqin2yrs)

tree.pred=predict(rf1,subset33,type="class")
table(tree.pred,subset33$SeriousDlqin2yrs)
#92.77
#tree.pred     0     1
#0 55773  3737
#1   288   471
tree.pred=predict(rf2,subset33,type="class")
table(tree.pred,subset33$SeriousDlqin2yrs)
#90.73
#tree.pred     0     1
#0 53111  2309
#1  2950  1899
tree.pred=predict(rf3,subset33,type="class")
table(tree.pred,subset33$SeriousDlqin2yrs)
#88.995
#tree.pred     0     1
#0 51730  1981
#1  4331  2227
tree.pred=predict(rf4,subset33,type="class")
table(tree.pred,subset33$SeriousDlqin2yrs)
#83.003
#tree.pred     0     1
#0 47423  1307
#1  8638  2901

#############part2############
#diviance plot
subset3.melt<-melt(subset3)
subset3.melt2 <- cbind(subset3.melt,resid=lr.fit2$residuals)
head(subset3.melt2)
ggplot(subset3.melt2,aes(x=value,y=resid))+geom_point()+geom_smooth(method="loess")+facet_wrap(~variable,scales="free")

#AUC or ROC
library(ROCR)
lr.fit<-glm()
lr.pred<-prediction(predictions=predict(lr.fit,subset3),labels=subset3$SeriousDlqin2yrs)
performance(lr.pred,"auc")@y.values[[1]]
lr.ROC<-performance(lr.pred,"tpr","fpr")
plot(lr.ROC)

X.lda <- lda(formula=SeriousDlqin2yrs~., data=subset3)
out.lda <- predict(X.lda,newdata=subset3)  
scores.lda <- out.lda$posterior[,2]
result.lda <- HMeasure(subset3$SeriousDlqin2yrs, scores.lda)
summary.hmeasure(result.lda)
plotROC(result.lda)


#confident interval and bootstrapped
confint(lr.fit2)
bs<-function(data,i){
  d<-data[i,]
  fit<-glm(SeriousDlqin2yrs~.-RevolvingUtilizationOfUnsecuredLines,data=d,family=binomial)
  return(coef(fit))
}
bootResults<-boot(data=subset3,statistic=bs,R=1000)
boot.ci (bootResults)
########part3#######
#hierarchical cluster
loanp.s<-scale(loanp, center = TRUE, scale = FALSE)
df<-loanp.s[1:100,]
head(loanp.s)
df.dist <- dist(df,method = "euclidean")
df.hclust <- hclust(df.dist, method="single")
plot(df.hclust)

df.dist <- dist(df,method = "euclidean")
df.hclust <- hclust(df.dist, method="complete")
plot(df.hclust)

#k-mean cluster
df2<-loanp
wss <- (nrow(df2)-1)*sum(apply(df2,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(df2, 
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

dftemp <- scale(df2); fit <- kmeans(df2, 2) 
library(cluster); 
clusplot(dftemp, fit$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)

split
s1<-subset(subset3, SeriousDlqin2yrs==1)
s1.1<-subset(subset33, SeriousDlqin2yrs==1)
s2<-subset(subset3, SeriousDlqin2yrs==0)
s2.1<-subset(subset33, SeriousDlqin2yrs==0)

lr.fit3.1<-glm(SeriousDlqin2yrs~.-RevolvingUtilizationOfUnsecuredLines,data=s1,family=binomial)
summary(lda.fit3.1)
cv.glm(data=s1.1,glmfit=lr.fit3.1)$delta
lr.fit3.2<-glm(SeriousDlqin2yrs~.-RevolvingUtilizationOfUnsecuredLines,data=s2,family=binomial)
summary(lr.fit3.2)
cv.glm(data=s2.1,glmfit=lr.fit3.2)$delta
