x11()
1+1
ggplot(data= blood, aes(x=sysBP, y=diaBP, col=blood$currentSmoker)) + geom_point()

# 1. 寃곗륫移? ?쁽?솴?쓣 ?떆媛곹솕
# 2. 寃곗륫移? 泥섎━(?떎以묐?泥?)
# 3. ?씠?긽移? ?솗?씤 (boxplot)
# 4. ?씠?긽移? 泥섎━
# 5. 蹂?닔 媛? ?뜲?씠?꽣 ?떆媛곹솕
# 6. ?쑀?쓽誘명븳 insight ?룄異?


## ?븰?젰?뿉 ?뵲瑜? ?삁?븬
table(blood[order(blood$sysBP, decreasing=F),][1:1000,3])
table(blood$education)

ggplot(data=blood, aes(x=sysBP, y=diaBP, col=education)) + geom_point()
blood3 <- blood
blood3$edulev <- as.factor(ifelse(blood3$education==1|blood3$education==2,1,3))
ggplot(data=blood3, aes(x=sysBP, y=diaBP, col=edulev)) + geom_point(position = 'jitter')

## ?븰?젰蹂? 怨좏삁?븬 鍮꾩쑉
edu1 <- length(blood[blood$education==1 & blood$highBP == 1,1]) / length(blood[blood$education==1,1])
edu2 <- length(blood[blood$education==2 & blood$highBP == 1,1]) / length(blood[blood$education==2,1])
edu3 <- length(blood[blood$education==3 & blood$highBP == 1,1]) / length(blood[blood$education==3,1])
edu4 <- length(blood[blood$education==4 & blood$highBP == 1,1]) / length(blood[blood$education==4,1])

## ?쟾泥? ?룊洹? 怨좏삁?븬 鍮꾩쑉
edu_tot <- length(blood[blood$highBP==1,1])/nrow(blood)

# ?븰?젰蹂? 怨좏삁?븬 鍮꾩쑉 ?떆媛곹솕
edu.df <- data.frame(edu_lev=c('1','2','3','4'),
                     percent=c(edu1,edu2,edu3,edu4))

g2 <- ggplot(edu.df, aes(x=edu_lev, y=percent)) + geom_bar(stat='identity')
g2 + geom_hline(yintercept=edu_tot)

### ?씠?긽移?(洹밸떒移?) 李얘린
library(ggplot2)
x11()
# cigsPerDay
ggplot(data=blood, aes(x=0, y=cigsPerDay)) + geom_boxplot()

# 0?씤 鍮꾪씉?뿰?옄?뒗 ?룷?븿?븯硫? ?븞?릺?땲 鍮쇨퀬 ?떎?떆 洹몃┝
blood2 <- blood[blood$currentSmoker==1,]
ggplot(data=blood2, aes(x=0, y=cigsPerDay)) + geom_boxplot()

# totchol
ggplot(data=blood, aes(x=0,y=totChol)) + geom_boxplot()  ## 洹밸떒移? ?몢媛? ?솗?씤
summary(blood)

# upper ?씠?긽移? ?솗?씤
cholup <- 263+ IQR(blood$totChol)*1.5
sum(blood$totChol>=cholup)  ## 53媛?

# lower ?씠?긽移? ?솗?씤
choldown <- 206-IQR(blood$totChol)*1.5 
sum(blood$totChol<=choldown) ## 3媛?

# 洹밸떒移? ?뜲?씠?꽣?솗?씤  
blood[order(blood$totChol,decreasing=T),][1:5,]  # 3161,1112 

# BMI
ggplot(data=blood, aes(x=0,y=BMI)) + geom_boxplot()

# heartrate
ggplot(data=blood, aes(x=0,y=heartRate)) + geom_boxplot()

# glucose
ggplot(data=blood, aes(x=0,y=glucose)) + geom_boxplot()
summary(blood)

# sysBP
ggplot(data=blood, aes(x=0,y=sysBP)) + geom_boxplot()



# diaBP
ggplot(data=blood, aes(x=0,y=diaBP)) + geom_boxplot()

ggplot(blood, aes(x=glucose, y=sysBP)) + geom_point()
ggplot(blood, aes(x=glucose, y=diaBP)) + geom_point()

## 蹂?닔媛꾩쓽 愿怨? ?뙆?븙

# age, sysBP
age_sysBP <- aggregate(blood$sysBP,list(blood$age),mean)
names(age_sysBP) <- c("age","sysBP")       
ggplot(data=age_sysBP,aes(x=age,y=sysBP))+geom_point()

# age, diaBP
age_diaBP <- aggregate(blood$diaBP,list(blood$age),mean)
names(age_diaBP) <- c("age","diaBP")
ggplot(data=age_diaBP,aes(x=age, y=diaBP)) + geom_point()
a
# cigsPerDay, sysBP
cigs_sysBP <- aggregate(blood2$sysBP,list(blood2$cigsPerDay),mean)
names(cigs_sysBP) <- c("cigsPerDay","sysBP")
ggplot(data=cigs_sysBP, aes(x=cigsPerDay, y=sysBP)) + geom_point()

# ?씉?뿰?웾?뿉 ?뵲瑜? 踰붿＜?솕
pack <- c()
for(i in 1:nrow(blood2)){
  if(blood2$cigsPerDay[i]<=10){
    pack <- c(pack,'half')
  }
  if(blood2$cigsPerDay[i]>10 & blood2$cigsPerDay[i]<=20){
    pack <- c(pack,'one')
  }
  if(blood2$cigsPerDay[i]>20 & blood2$cigsPerDay[i]<=30){
    pack <- c(pack,'onehalf')
  }
  if(blood2$cigsPerDay[i]>30){
    pack <- c(pack,'overonehalf')
  }
}
pack
blood2$pack <- pack

cigs_sysBP <- aggregate(blood2$sysBP,list(blood2$pack),mean)
names(cigs_sysBP) <- c("cigsPerDay","sysBP")
ggplot(data=cigs_sysBP, aes(x=cigsPerDay, y=sysBP)) + geom_bar(stat='identity')

# currentSmoker, sysBP

smoke_sysBP <- aggregate(blood$sysBP, list(blood$currentSmoker), mean)
names(smoke_sysBP) <- c("smoker","sysBP")
ggplot(data=smoke_sysBP, aes(x=smoker, y=sysBP)) + geom_bar(stat='identity') + 
  coord_cartesian(ylim = c(0,140))

table(blood$currentSmoker,blood$highBP)
# BMI, sysBP
x11()
ggplot(data=blood, aes(x=BMI, y=sysBP)) + geom_point()

# male, sysBP
male_sysBP <- aggregate(blood$sysBP, list(blood$male), mean)
names(male_sysBP) <- c("male","sysBP")
ggplot(data=male_sysBP, aes(x=male, y=sysBP)) + geom_bar(stat='identity') 

# education, sysBP
edu_sysBP <- aggregate(blood$sysBP, list(blood$education), mean)
names(edu_sysBP) <- c("education","sysBP")
ggplot(data=edu_sysBP, aes(x=education, y=sysBP))+ geom_bar(stat='identity')

# education, diaBP
edu_diaBP <- aggregate(blood$diaBP, list(blood$education), mean)
names(edu_diaBP) <- c("education","diaBP")

library(ggplot2)

# ?굹?씠 BMI?뿉 ?뵲瑜? 怨좏삁?븬 ?뿬遺 ?궛?젏?룄
ggplot(data=blood, aes(x=age, y= BMI, col=highBP)) + geom_point(position = 'jitter')

# ?굹?씠 BMI?뿉 ?뵲瑜? 怨좏삁?븬 ?닔以 ?궛?젏?룄
ggplot(data=blood, aes(x=age, y=BMI, col=hyplev)) + geom_point()

names(blood)

# 踰붿＜?삎 蹂?닔?뱾?뿉 ?뵲瑜? ?삁?븬 ?궛?젏?룄
ggplot(blood, aes(x=sysBP, y=diaBP, col=education)) + geom_point()
ggplot(blood, aes(x=sysBP, y=diaBP, col=male)) + geom_point()
ggplot(blood, aes(x=sysBP, y=diaBP, col=currentSmoker)) + geom_point()
ggplot(blood, aes(x=sysBP, y=diaBP, col=BPMeds)) + geom_point(position='jitter')

# 怨좏삁?븬?씤 ?궗?엺?뱾?쓽 ?븰?젰蹂? ?삁?븬 ?궛?젏?룄
blood3 <- blood[blood$highBP==1,]
ggplot(blood3, aes(x=sysBP, y=diaBP, col=education)) + geom_point()

# facet grid
ggplot(blood, aes(x=sysBP, y=diaBP)) + geom_point() + facet_wrap( .~ education, ncol=2)
# ?씉?뿰蹂?
ggplot(blood, aes(x=sysBP, y=diaBP)) + geom_point() + facet_wrap( .~ currentSmoker)

# ?뇤議몄쨷 ?뿬遺蹂?
table(blood$prevalentStroke)
ggplot(blood, aes(x=sysBP, y=diaBP, col=prevalentStroke)) + geom_point()

# sysBP, BMI , education
ggplot(blood, aes(x=BMI, y=sysBP, col=education)) + geom_point()

# 2?룄怨좏삁?븬 以묒뿉?꽌 蹂닿린
blood4 <- blood[blood$hyplev==4,]

# glucose
ggplot(blood, aes(x=glucose, y=sysBP)) + geom_point()

# prevalentHyp
g1<- ggplot(blood, aes(x=sysBP, y=diaBP, col=prevalentHyp)) + geom_point()
g1 + geom_hline(yintercept=90) + geom_vline(xintercept=140)

# BPMeds
ggplot(blood, aes(x=sysBP, y=diaBP, col=BPMeds)) + geom_point(position='jitter') 

# 怨좏삁?븬?씠怨? ?삁?븬?빟 癒밸뒗 ?궗?엺
table(blood$BPMeds,blood$highBP)

# 怨좏삁?븬?씤?뜲 ?삁?븬?빟 ?븞癒밸뒗?궗?엺
blooda <- blood[blood$BPMeds==0 & blood$highBP==1,]
dim(blooda)
ggplot(blooda , aes(x=BMI, y=sysBP)) + geom_point()

# ?삁?븬?빟 癒밸뒗?뜲?룄 怨좏삁?븬?씤?궗?엺
blood_hh <- blood[blood$BPMeds==1 & blood$highBP==1,]
dim(blood_hh)
ggplot(blood_hh , aes(x=BMI, y=sysBP)) + geom_point()

## 湲猷⑥퐫?뒪?닔移섏? ?떦?눊蹂묒뿬遺
ggplot(blood, aes(x=glucose, y=sysBP, col=diabetes)) + geom_point()
ggplot(blood, aes(x=glucose, y=diaBP, col=diabetes)) + geom_point()
ggplot(blood, aes(x=sysBP, y=diaBP, col=diabetes)) + geom_point()

# 怨쇨굅?뿏 ?젙?긽?삁?븬?씠??吏留? ?쁽?옱 怨좏삁?븬?씤 ?궗?엺
danger <- blood[blood$prevalentHyp==0 & blood$highBP==1,]
summary(danger)
summary(blood)


#### ?뜲?씠?꽣 遺꾪븷 ####
set.seed(32150897)
train.row <- sample(1:nrow(blood),nrow(blood)*0.5)
valid.row <- sample(setdiff(1:nrow(blood),train.row),nrow(blood)*0.3) 
test.row <- setdiff(1:nrow(blood),c(train.row,valid.row))

train.df <- blood[train.row,]
valid.df <- blood[valid.row,]
test.df <- blood[test.row,]

#### regression model ####
library(forecast)

names(train.df)
sel_var.1 <- c(1,2,4,5,6,7,10,11,12,8)
sel_var.2 <- c(1,2,4,5,6,7,10,11,12,9)
train.df.reg.sys <- train.df[,sel_var.1] 
valid.df.reg.sys <- valid.df[,sel_var.1]
train.df.reg.dia <- train.df[,sel_var.2]
valid.df.reg.dia <- valid.df[,sel_var.2]

sys.lm <- lm(sysBP ~ ., data=train.df.reg.sys)
dia.lm <- lm(diaBP ~ ., data=train.df.reg.dia)
summary(sys.lm)

## sysBP ?삁痢≪꽦?뒫 援ы븯湲?
sys.lm.pred <- predict(sys.lm, valid.df.reg.sys)  

accuracy(sys.lm.pred, valid.df.reg.sys$sysBP)

## sysBP ?삁痢? ?뼢?긽李⑦듃 洹몃━湲?
library(gains)

gain1 <- gains(valid.df.reg.sys$sysBP, sys.lm.pred)
sys1 <- valid.df.reg.sys$sysBP
plot(c(0,gain1$cume.pct.of.total*sum(sys1))~c(0,gain1$cume.obs),
     xlab="# cases", ylab="Cumulative MEDV", main="full_var Lift Chart", type="l")

lines(c(0,sum(sys1))~c(0,dim(valid.df.reg.sys)[1]), col="gray", lty=2)

barplot(gain1$mean.resp/mean(sys1), names.arg = gain1$depth, ylim=c(0,2),
        xlab = "Percentile", ylab = "Mean Response", main = "full_var Decile-wise lift chart")

## diaBP ?삁痢≪꽦?뒫 援ы븯湲?
dia.lm.pred <- predict(dia.lm, valid.df.reg.dia)  

accuracy(dia.lm.pred, valid.df.reg.dia$diaBP)

## diaBP ?삁痢? ?뼢?긽李⑦듃 洹몃━湲?

gain2 <- gains(valid.df.reg.dia$diaBP, dia.lm.pred)
dia1 <- valid.df.reg.dia$diaBP
plot(c(0,gain2$cume.pct.of.total*sum(dia1))~c(0,gain2$cume.obs),
     xlab="# cases", ylab="Cumulative MEDV", main="full_var Lift Chart", type="l")

lines(c(0,sum(dia1))~c(0,dim(valid.df.reg.dia)[1]), col="gray", lty=2)

barplot(gain2$mean.resp/mean(dia1), names.arg = gain2$depth, ylim=c(0,2),
        xlab = "Percentile", ylab = "Mean Response", main = "full_var Decile-wise lift chart")

# sysBP ?젙洹쒖꽦 寃?넗 

sys.lm.pred <- predict(sys.lm, valid.df.reg.sys)
all.residuals1 <- valid.df.reg.sys$sysBP - sys.lm.pred
hist(all.residuals1, breaks = 25, xlab = "sys_Residuals", main = "")

# diaBP ?젙洹쒖꽦 寃?넗 

dia.lm.pred <- predict(dia.lm, valid.df.reg.dia)
all.residuals2 <- valid.df.reg.dia$diaBP - dia.lm.pred
hist(all.residuals2, breaks = 25, xlab = "dia_Residuals", main = "")



# 媛?뒫?븳 紐⑤뱺 ?쉶洹紐⑦삎 ?깘?깋踰? 

# use regsubsets() in package leaps to run an exhaustive search.
# unlike with lm, categorical predictors must be turned into dummies manually.
install.packages('leaps')
library(leaps)
# create dummies for fuel type
education <- as.data.frame(model.matrix(~ 0 + education, data=train.df.reg.sys))
# replace Fuel_Type column with 2 dummies

train.df.reg.sys1 <- cbind(train.df.reg.sys[,-1], education[,1:3]) 
head(train.df.reg.sys1)
search.sys <- regsubsets(sysBP ~ ., data = train.df.reg.sys1, nbest = 1, nvmax = dim(train.df.reg.sys1)[2],
                     method = "exhaustive")
# nbest?뒗 蹂?닔?쓽 媛??닔蹂꾨줈 紐? 媛쒖쓽 紐⑦삎?쓣 ?젣?떆?븷吏 ?젣?뼱?븿 
sum.sys <- summary(search.sys)
names(sum.sys)
# show models
sum.sys$which
# show metrics
sum.sys$rsq
sum.sys$adjr2
sum.sys$cp
sum
result.sys=with(sum.sys,round(cbind(which,rsq,adjr2,cp,bic),3)) # 紐⑦삎?꽦?뒫?룊媛吏?몴?뿉 ?뵲瑜? 寃곌낵 
result.sys  ## 7踰덉㎏ , ()

# regsubsets()寃곌낵濡쒕??꽣 媛? 紐⑦삎?뿉 ???븳 ?쟻?빀寃곌낵瑜? ?솗?씤?븯湲? ?쐞?빐?꽌?뒗 "coef()"?븿?닔瑜? ?궗?슜
k=which.min(sum.sys$bic) # BIC湲곗??쑝濡? 蹂?닔?꽑?깮?쓣 ?븷 寃쎌슦 媛?옣 ?옉?? 媛믪뿉 ?빐?떦?븯?뒗 紐⑦삎?쓣 ?꽑?깮?븿 
coef(search.sys, k) # ?꽑?삎?쉶洹紐⑦삎?쓽 ?쟻?빀寃곌낵瑜? ?븣?젮以?

###### 蹂?닔?꽑?깮 ?썑 sysBP ?삁痢≪꽦?뒫 (BPMeds, prevalentHyp, totChol, BMI, heartRate, glucose, education)
names(train.df.reg.sys)
train.df.reg.sys.sel <- train.df.reg.sys[,c(1,3,5,6,7,8,9,10)]
valid.df.reg.sys.sel <- valid.df.reg.sys[,c(1,3,5,6,7,8,9,10)]

sys.lm.sel <- lm(sysBP ~ . , data=train.df.reg.sys.sel)
sys.lm.sel.pred <- predict(sys.lm.sel ,valid.df.reg.sys.sel)
pred.error.sys <- accuracy(sys.lm.sel.pred , valid.df.reg.sys$sysBP)
pred.error.sys

## 蹂?닔?꽑?깮 ?썑?쓽 sysBP ?삁痢? ?뼢?긽李⑦듃 洹몃━湲?

gain3 <- gains(valid.df.reg.sys.sel$sysBP, sys.lm.sel.pred)
sys1 <- valid.df.reg.sys.sel$sysBP
plot(c(0,gain3$cume.pct.of.total*sum(sys1))~c(0,gain3$cume.obs),
     xlab="# cases", ylab="Cumulative MEDV", main="selected_var Lift Chart", type="l")

lines(c(0,sum(sys1))~c(0,dim(valid.df.reg.sys.sel)[1]), col="gray", lty=2)

barplot(gain3$mean.resp/mean(sys1), names.arg = gain3$depth, ylim=c(0,2),
        xlab = "Percentile", ylab = "Mean Response", main = "selected_var Decile-wise lift chart")


## diaBP 蹂?닔?꽑?깮  
education <- as.data.frame(model.matrix(~ 0 + education, data=train.df.reg.dia))


train.df.reg.dia <- cbind(train.df.reg.dia[,-1], education[,1:3]) 
head(train.df.reg.dia)
search.dia <- regsubsets(diaBP ~ ., data = train.df.reg.dia, nbest = 1, nvmax = dim(train.df.reg.dia)[2],
                         method = "exhaustive")
# nbest?뒗 蹂?닔?쓽 媛??닔蹂꾨줈 紐? 媛쒖쓽 紐⑦삎?쓣 ?젣?떆?븷吏 ?젣?뼱?븿 
sum.dia <- summary(search.dia)
names(sum.dia)
# show models
sum.dia$which
# show metrics
sum.dia$rsq
sum.dia$adjr2
sum.dia$cp

result.dia=with(sum.dia,round(cbind(which,rsq,adjr2,cp,bic),3)) # 紐⑦삎?꽦?뒫?룊媛吏?몴?뿉 ?뵲瑜? 寃곌낵 
result.dia  ## 4踰덀뀎 , ()

# regsubsets()寃곌낵濡쒕??꽣 媛? 紐⑦삎?뿉 ???븳 ?쟻?빀寃곌낵瑜? ?솗?씤?븯湲? ?쐞?빐?꽌?뒗 "coef()"?븿?닔瑜? ?궗?슜
k2=which.min(sum.dia$bic) # BIC湲곗??쑝濡? 蹂?닔?꽑?깮?쓣 ?븷 寃쎌슦 媛?옣 ?옉?? 媛믪뿉 ?빐?떦?븯?뒗 紐⑦삎?쓣 ?꽑?깮?븿 
coef(search.dia, k2) # ?꽑?삎?쉶洹紐⑦삎?쓽 ?쟻?빀寃곌낵瑜? ?븣?젮以?

## 蹂?닔?꽑?깮 ?썑 diaBP ?삁痢≪꽦?뒫 (prevalentHyp, totChol, BMI, heartRate)

names(train.df.reg.dia)
train.df.reg.dia.sel <- train.df.reg.dia[,c(5,6,7,8,10)]
valid.df.reg.dia.sel <- valid.df.reg.dia[,c(5,6,7,8,10)]

dia.lm.sel <- lm(diaBP ~ . , data=train.df.reg.dia.sel)
dia.lm.sel.pred <- predict(dia.lm.sel ,valid.df.reg.dia.sel)
pred.error.dia <- accuracy(dia.lm.sel.pred , valid.df.reg.dia$diaBP)
pred.error.dia

## 蹂?닔?꽑?깮 ?썑 diaBP ?뼢?긽李⑦듃 洹몃━湲?
gain4 <- gains(valid.df.reg.dia.sel$diaBP, dia.lm.sel.pred)
dia1 <- valid.df.reg.dia.sel$diaBP
plot(c(0,gain4$cume.pct.of.total*sum(dia1))~c(0,gain4$cume.obs),
     xlab="# cases", ylab="Cumulative MEDV", main="selected_var Lift Chart", type="l")

lines(c(0,sum(dia1))~c(0,dim(valid.df.reg.dia.sel)[1]), col="gray", lty=2)

barplot(gain4$mean.resp/mean(dia1), names.arg = gain4$depth, ylim=c(0,2),
        xlab = "Percentile", ylab = "Mean Response", main = "selected_var Decile-wise lift chart")


######################### k-nn ########################

library(caret)
library(FNN)

?knn.reg
train.df.sys.nn <- train.df[,c(1,2,4,5,6,7,10,11,12,8)]
train.df.dia.nn <- train.df[,c(1,2,4,5,6,7,10,11,12,9)]
valid.df.sys.nn <- valid.df[,c(1,2,4,5,6,7,10,11,12,8)]
valid.df.dia.nn <- valid.df[,c(1,2,4,5,6,7,10,11,12,9)]


#### sysBP knn ####
##### ?젙洹쒗솕 ?썑 怨꾩궛
train.norm.df.sys <- train.df.sys.nn
valid.norm.df.sys <- valid.df.sys.nn

names(train.df.sys.nn)
norm.values <- preProcess(train.df.sys.nn[,c(6,7,8,9)], method=c("center", "scale"))
norm.values$mean
train.norm.df.sys[,6:9] <- predict(norm.values, train.df.sys.nn[,6:9])
valid.norm.df.sys[,6:9] <- predict(norm.values, valid.df.sys.nn[,6:9])
train.norm.df.sys$education <- as.numeric(train.norm.df.sys$education)
valid.norm.df.sys$education <- as.numeric(valid.norm.df.sys$education)
train.norm.df.sys$currentSmoker <- as.numeric(train.norm.df.sys$currentSmoker)
valid.norm.df.sys$currentSmoker <- as.numeric(valid.norm.df.sys$currentSmoker)
train.norm.df.sys$BPMeds <- as.numeric(train.norm.df.sys$BPMeds)
valid.norm.df.sys$BPMeds <- as.numeric(valid.norm.df.sys$BPMeds)
train.norm.df.sys$prevalentStroke <- as.numeric(train.norm.df.sys$prevalentStroke)
valid.norm.df.sys$prevalentStroke <- as.numeric(valid.norm.df.sys$prevalentStroke)
train.norm.df.sys$prevalentHyp <- as.numeric(train.norm.df.sys$prevalentHyp)
valid.norm.df.sys$prevalentHyp <- as.numeric(valid.norm.df.sys$prevalentHyp)

names(train.norm.df.sys)
nn.sys <- knn.reg(train = train.norm.df.sys[,6:9], test = valid.norm.df.sys[,6:9],
                  train.norm.df.sys[,10], k = 6)

accuracy.df.sys <- data.frame(k = seq(1, 100, 1), accuracy = rep(0, 100))
# compute knn for different k on validation.
for(i in 1:100) {
  knn.pred.sys <- knn.reg(train.norm.df.sys[, 1:9], valid.norm.df.sys[,1:9],
                          train.norm.df.sys[,10], k = i)
  accuracy.df.sys[i, 2] <- sqrt(sum((knn.pred.sys[[4]]-valid.norm.df.sys[,10])^2)/length(valid.norm.df.sys[,10]))
}
accuracy.df.sys  ## k=16 rmse 15.8
?accuracy 
# ?쟾泥댄룊洹좎뿉?꽌?쓽 RMSE  ## 21.6
sqrt(sum((rep(132,length(valid.df.sys.nn$sysBP))-valid.norm.df.sys$sysBP)^2)/length(valid.df.sys.nn$sysBP))

## k=16?뿉?꽌 ?떎?떆 ?삁痢?
knn.pred.sys.f <- knn.reg(train.norm.df.sys[,1:9],valid.norm.df.sys[,1:9],
                          train.norm.df.sys[,10], k=16)

## ?삁痢↔컪留? ?븷?떦
knn.pred.sys.f <- knn.pred.sys.f[[4]]
## ?뼢?긽李⑦듃 ##

gain.nn1 <- gains(valid.df.sys.nn$sysBP, knn.pred.sys.f)
sys.nn1 <- valid.df.sys.nn$sysBP
plot(c(0,gain.nn1$cume.pct.of.total*sum(sys.nn1))~c(0,gain.nn1$cume.obs),
     xlab="# cases", ylab="Cumulative sysBP", main="k=16 Lift Chart", type="l")

lines(c(0,sum(sys.nn1))~c(0,dim(valid.df.sys.nn)[1]), col="gray", lty=2)
barplot(gain.nn1$mean.resp/mean(sys.nn1), names.arg = gain.nn1$depth, ylim=c(0,2),
        xlab = "Percentile", ylab = "Mean Response", main = "k=16 Decile-wise lift chart")

############# diaBP knn #####################
##### ?젙洹쒗솕 ?썑 怨꾩궛
train.norm.df.dia <- train.df.dia.nn
valid.norm.df.dia <- valid.df.dia.nn

names(train.df.dia.nn)
norm.values.dia <- preProcess(train.df.dia.nn[,c(6,7,8,9)], method=c("center", "scale"))
norm.values.dia$mean
train.norm.df.dia[,6:9] <- predict(norm.values.dia, train.df.dia.nn[,6:9])
valid.norm.df.dia[,6:9] <- predict(norm.values.dia, valid.df.dia.nn[,6:9])
train.norm.df.dia$education <- as.numeric(train.norm.df.dia$education)
valid.norm.df.dia$education <- as.numeric(valid.norm.df.dia$education)
train.norm.df.dia$currentSmoker <- as.numeric(train.norm.df.dia$currentSmoker)
valid.norm.df.dia$currentSmoker <- as.numeric(valid.norm.df.dia$currentSmoker)
train.norm.df.dia$BPMeds <- as.numeric(train.norm.df.dia$BPMeds)
valid.norm.df.dia$BPMeds <- as.numeric(valid.norm.df.dia$BPMeds)
train.norm.df.dia$prevalentStroke <- as.numeric(train.norm.df.dia$prevalentStroke)
valid.norm.df.dia$prevalentStroke <- as.numeric(valid.norm.df.dia$prevalentStroke)
train.norm.df.dia$prevalentHyp <- as.numeric(train.norm.df.dia$prevalentHyp)
valid.norm.df.dia$prevalentHyp <- as.numeric(valid.norm.df.dia$prevalentHyp)

names(train.norm.df.dia)

accuracy.df.dia <- data.frame(k = seq(1, 100, 1), accuracy = rep(0, 100))
# compute knn for different k on validation.
for(i in 1:100) {
  knn.pred.dia <- knn.reg(train.norm.df.dia[, 1:9], valid.norm.df.dia[,1:9],
                          train.norm.df.dia[,10], k = i)
  accuracy.df.dia[i, 2] <- sqrt(sum((knn.pred.dia[[4]]-valid.norm.df.dia[,10])^2)/length(valid.norm.df.dia[,10]))
}
accuracy.df.dia  ## k=20 rmse 9.19
?accuracy
# ?쟾泥댄룊洹좎뿉?꽌?쓽 RMSE  ## 11.8
sqrt(sum((rep(82.6,length(valid.df.dia.nn$diaBP))-valid.norm.df.dia$diaBP)^2)/length(valid.df.dia.nn$diaBP))

## k=20?뿉?꽌 ?떎?떆 ?삁痢?
knn.pred.dia.f <- knn.reg(train.norm.df.dia[,1:9],valid.norm.df.dia[,1:9],
                          train.norm.df.dia[,10], k=20)

## ?삁痢↔컪留? ?븷?떦
knn.pred.dia.f <- knn.pred.dia.f[[4]]

## ?뼢?긽李⑦듃 ##

gain.nn2 <- gains(valid.df.dia.nn$diaBP, knn.pred.dia.f)
dia.nn1 <- valid.df.dia.nn$diaBP
plot(c(0,gain.nn2$cume.pct.of.total*sum(dia.nn1))~c(0,gain.nn2$cume.obs),
     xlab="# cases", ylab="Cumulative diaBP", main="k=20 Lift Chart", type="l")

lines(c(0,sum(dia.nn1))~c(0,dim(valid.df.dia.nn)[1]), col="gray", lty=2)
barplot(gain.nn2$mean.resp/mean(dia.nn1), names.arg = gain.nn2$depth, ylim=c(0,2),
        xlab = "Percentile", ylab = "Mean Response", main = "k=20 Decile-wise lift chart")


############################### decision tree ###############################
library(rpart)
library(rpart.plot)
names(train.df)
train.df.tree.sys <- train.df[,c(1,2,4,5,6,7,10,11,12,8)]
valid.df.tree.sys <- valid.df[,c(1,2,4,5,6,7,10,11,12,8)]
train.df.tree.dia <- train.df[,c(1,2,4,5,6,7,10,11,12,9)]
valid.df.tree.dia <- valid.df[,c(1,2,4,5,6,7,10,11,12,9)]

############################## regression tree ##############################

##### sysBP #####

# ?셿?쟾 ?꽦?옣?븳 ?굹臾? 
set.seed(32150897)
full.rt.sys <- rpart(sysBP ~ . , data=train.df.tree.sys, method='anova',
                     cp=0)
prp(full.rt.sys, type=1, extra=1, under=T, split.font=1, varlen=-10)
pred.full.sys <- predict(full.rt.sys, newdata=valid.df.tree.sys)
accuracy(pred.full.sys, valid.df.tree.sys$sysBP)

# 媛吏移섍린
# 援먯감寃利앹쓣 ?넻?빐 鍮꾩슜蹂듭옟?룄?쓽 alpha(=>cp) 異붿젙
cv.rt.sys <- rpart(sysBP ~ ., data = train.df.tree.sys, method = "anova",
               cp = 0.00001, minsplit = 5, xval = 5)
# use printcp() to print the table.
printcp(cv.rt.sys)[1:20,]
plotcp(cv.rt.sys)

# xerror 理쒖냼媛? 李얘린
min(cv.rt.sys$cptable[,'xerror']) ## 0.512

# xerror < min(xerror) + xstd 瑜? 留뚯”?븯?뒗 CP 以? ?옂?끂?뱶媛 媛?옣 ?옉?? ?굹臾대え?삎(?옂2媛?)?? cp=0.01718 珥덇낵?븯?뒗寃쎌슦?씠?떎.
pruned.rt.sys1 <- prune(cv.rt.sys, cp=0.01720)
prp(pruned.rt.sys1)
pred.pruned1.sys<- predict(pruned.rt.sys1, newdata=valid.df.tree.sys)
accuracy(pred.pruned1.sys, valid.df.tree.sys$sysBP)

# xerror 媛?옣?궙??嫄몃줈 cp=0.01557珥덇낵 (?옂 3媛?)
pruned.rt.sys2 <- prune(cv.rt.sys, cp=0.016)
prp(pruned.rt.sys2)
pred.pruned2.sys <- predict(pruned.rt.sys2, newdata=valid.df.tree.sys)
accuracy(pred.pruned2.sys , valid.df.tree.sys$sysBP)  # ?꽦?뒫 ?뜑 醫뗭쓬  

## ?뼢?긽李⑦듃 洹몃━湲? ##

gain.rt1 <- gains(valid.df.tree.sys$sysBP, pred.pruned2.sys)
sys.rt1 <- valid.df.tree.sys$sysBP
plot(c(0,gain.rt1$cume.pct.of.total*sum(sys.rt1))~c(0,gain.rt1$cume.obs),
     xlab="# cases", ylab="Cumulative sysBP", main="regression tree Lift Chart", type="l")

lines(c(0,sum(sys.rt1))~c(0,dim(valid.df.tree.sys)[1]), col="gray", lty=2)
barplot(gain.rt1$mean.resp/mean(sys.rt1), names.arg = gain.rt1$depth, ylim=c(0,2),
        xlab = "Percentile", ylab = "Mean Response", main = "regression tree Decile-wise lift chart")

##### diaBP #####

# ?셿?쟾 ?꽦?옣?븳 ?굹臾? 
set.seed(32150897)
full.rt.dia <- rpart(diaBP ~ . , data=train.df.tree.dia, method='anova',
                     cp=0)
prp(full.rt.dia, type=1, extra=1, under=T, split.font=1, varlen=-10)
pred.full.dia <- predict(full.rt.dia, newdata=valid.df.tree.dia)
accuracy(pred.full.dia, valid.df.tree.dia$diaBP)

# 媛吏移섍린
# 援먯감寃利앹쓣 ?넻?빐 鍮꾩슜蹂듭옟?룄?쓽 alpha(=>cp) 異붿젙
cv.rt.dia <- rpart(diaBP ~ ., data = train.df.tree.dia, method = "anova",
                   cp = 0.00001, minsplit = 5, xval = 5)
# use printcp() to print the table.
printcp(cv.rt.dia)[1:20,]
plotcp(cv.rt.dia)

# xerror 理쒖냼媛? 李얘린
min(cv.rt.dia$cptable[,'xerror']) ## 0.601

# xerror < min(xerror) + xstd 瑜? 留뚯”?븯?뒗 CP 以? ?옂?끂?뱶媛 媛?옣 ?옉?? ?굹臾대え?삎(?옂2媛?)?? cp=0.02913 珥덇낵?븯?뒗寃쎌슦?씠?떎.
pruned.rt.dia1 <- prune(cv.rt.dia, cp=0.03)
prp(pruned.rt.dia1)
pred.pruned1.dia<- predict(pruned.rt.dia1, newdata=valid.df.tree.dia)
accuracy(pred.pruned1.dia, valid.df.tree.dia$diaBP)

# xerror 媛?옣?궙??嫄몃줈 cp=0.01029珥덇낵 (?옂 3媛?)
pruned.rt.dia2 <- prune(cv.rt.dia, cp=0.011)
prp(pruned.rt.dia2)
pred.pruned2.dia <- predict(pruned.rt.dia2, newdata=valid.df.tree.dia)
accuracy(pred.pruned2.dia , valid.df.tree.dia$diaBP) # ?꽦?뒫 ?뜑 醫뗭쓬  

## ?뼢?긽李⑦듃
gain.rt2 <- gains(valid.df.tree.dia$diaBP, pred.pruned2.dia)
dia.rt1 <- valid.df.tree.dia$diaBP
plot(c(0,gain.rt2$cume.pct.of.total*sum(dia.rt1))~c(0,gain.rt2$cume.obs),
     xlab="# cases", ylab="Cumulative diaBP", main="regression tree diaBP Lift Chart", type="l")

lines(c(0,sum(dia.rt1))~c(0,dim(valid.df.tree.dia)[1]), col="gray", lty=2)
barplot(gain.rt2$mean.resp/mean(dia.rt1), names.arg = gain.rt2$depth, ylim=c(0,2),
        xlab = "Percentile", ylab = "Mean Response", main = "regression tree diaBP Decile-wise lift chart")



############################ neural net ##############################

## ?뜲?씠?꽣 0怨? 1?궗?씠濡? ##
nnblood <- blood[,c(1,2,4,5,6,7,10,11,12,8,9)]
summary(nnblood)
nnblood$BMI <- (nnblood$BMI-15.5)/(56.8-15.5)
nnblood$totChol <- (nnblood$totChol-107)/(696-107)
nnblood$heartRate <- (nnblood$heartRate-44)/(143-44)
nnblood$glucose <- (nnblood$glucose-40)/(394-40)
nnblood$sysBP <- (nnblood$sysBP-83.5)/(295-83.5)
nnblood$diaBP <- (nnblood$diaBP-48)/(142.5-48)
nnblood$currentSmoker <- as.numeric(nnblood$currentSmoker)
nnblood$BPMeds <- as.numeric(nnblood$BPMeds)
nnblood$prevalentStroke <- as.numeric(nnblood$prevalentStroke)
nnblood$prevalentHyp <- as.numeric(nnblood$prevalentHyp)
nnblood$education <- as.numeric(nnblood$education)
for(i in 1:nrow(nnblood)){
  if(nnblood$education[i]==1){
    nnblood$education[i] <- 0
  }
  if(nnblood$education[i]==2){
    nnblood$education[i] <- 0.33
  }
  if(nnblood$education[i]==3){
    nnblood$education[i] <- 0.66
  }
  if(nnblood$education[i]==4){
    nnblood$education[i] <- 1
  }
}

summary(nnblood)

## ?뜲?씠?꽣 遺꾪븷
train.df.nrn.sys <- nnblood[train.row,-11]
valid.df.nrn.sys <- nnblood[valid.row,-11]
train.df.nrn.dia <- nnblood[train.row,-10]
valid.df.nrn.dia <- nnblood[valid.row,-10]

##### ?떊寃쎈쭩?쟻?빀 ####
# sysBP
library(neuralnet)
library(caret)
set.seed(32150897)
names(train.df.nrn.sys)
nrn.sys <- neuralnet(sysBP ~ education + currentSmoker + prevalentStroke + prevalentHyp +
                       totChol + BMI + heartRate + glucose , data=train.df.nrn.sys, hidden=3)
nrn.sys2 <- neuralnet(sysBP ~ ., data=train.df.nrn.sys,hidden=2) ## 紐⑤뱺蹂?닔 + ???땳?끂?뱶2媛? 理쒓퀬?꽦?뒫  


plot(nrn.sys2, rep='best')
pred.nrn.sys1 <- compute(nrn.sys2, train.df.nrn.sys[,-10])
pred.nrn.sys2 <- compute(nrn.sys2, valid.df.nrn.sys[,-10])

## ?썝?옒?떒?쐞濡? ?룎?젮二쇨린 sysBPmax 295 sysBPmin83.5    ##
org.pred.nrn.sys1 <- as.vector(pred.nrn.sys1$net.result)*(295-83.5)+83.5
org.pred.nrn.sys2 <- as.vector(pred.nrn.sys2$net.result)*(295-83.5)+83.5
## trainset ?삁痢? RMSE
names(train.df)

sqrt(sum((org.pred.nrn.sys1-train.df[,8])^2)/length(train.df[,8]))

## validation set ?삁痢? RMSE 
sqrt(sum((org.pred.nrn.sys2-valid.df[,8])^2)/length(valid.df[,8]))


### diaBP
names(train.df.nrn.dia)

## BEST ##  BPMeds + prevalentHyp + heartRate + totChol + BMI + glucose
nrn.dia2 <- neuralnet(diaBP ~ BPMeds + prevalentHyp + heartRate +
                        totChol + BMI + glucose, data=train.df.nrn.dia,hidden=4)


plot(nrn.dia2, rep='best')
pred.nrn.dia1 <- compute(nrn.dia2, train.df.nrn.dia[,-10])
pred.nrn.dia2 <- compute(nrn.dia2, valid.df.nrn.dia[,-10])
## ?썝?옒?떒?쐞濡? ?룎?젮二쇨린 diaBPmax 142.5 diaBPmin48    ##

org.pred.nrn.dia1 <- as.vector(pred.nrn.dia1$net.result)*(142.5-48)+48
org.pred.nrn.dia2 <- as.vector(pred.nrn.dia2$net.result)*(142.5-48)+48
## trainset ?삁痢? RMSE
names(train.df)
sqrt(sum((org.pred.nrn.dia1-train.df[,9])^2)/length(train.df[,9]))

## validation set ?삁痢? RMSE
sqrt(sum((org.pred.nrn.dia2-valid.df[,9])^2)/length(valid.df[,9]))


## neuralnet sysBP, diaBP ?뼢?긽李⑦듃
par(mfcol=c(2,2))
gain.nrn1 <- gains(valid.df.nrn.sys$sysBP, org.pred.nrn.sys2)
sys.nrn1 <- valid.df.nrn.sys$sysBP
plot(c(0,gain.nrn1$cume.pct.of.total*sum(sys.nrn1))~c(0,gain.nrn1$cume.obs),
     xlab="# cases", ylab="Cumulative sysBP", main="neuralnet sysBP Lift Chart", type="l")

lines(c(0,sum(sys.nrn1))~c(0,dim(valid.df.nrn.sys)[1]), col="gray", lty=2)
barplot(gain.nrn1$mean.resp/mean(sys.nrn1), names.arg = gain.nrn1$depth, ylim=c(0,2),
        xlab = "Percentile", ylab = "Mean Response", main = "neuralnet sysBP Decile-wise lift chart")

## diaBP
gain.nrn2 <- gains(valid.df.nrn.dia$diaBP, org.pred.nrn.dia2)
dia.nrn1 <- valid.df.nrn.dia$diaBP
plot(c(0,gain.nrn2$cume.pct.of.total*sum(dia.nrn1))~c(0,gain.nrn2$cume.obs),
     xlab="# cases", ylab="Cumulative diaBP", main="neuralnet diaBP Lift Chart", type="l")

lines(c(0,sum(dia.nrn1))~c(0,dim(valid.df.nrn.dia)[1]), col="gray", lty=2)
barplot(gain.nrn2$mean.resp/mean(dia.nrn1), names.arg = gain.nrn2$depth, ylim=c(0,2),
        xlab = "Percentile", ylab = "Mean Response", main = "neuralnet diaBP Decile-wise lift chart")


######### 蹂?닔異붽? ##########

addblood <- read.csv("framingham.csv")
blood$male <- addblood$male
blood$age <- addblood$age
blood$male <- as.factor(blood$male)
summary(blood)

## age sysBP, diaBP ?떆媛곹솕 ##
x11()
age_sysBP <- aggregate(blood$sysBP, list(blood$age), mean)
names(age_sysBP) <- c('age','sysBP')
age_diaBP <- aggregate(blood$diaBP, list(blood$age), mean)
names(age_diaBP) <- c('age','diaBP')
ggplot(age_sysBP, aes(x=age, y=sysBP)) + geom_point(col='blue') + ggtitle('age_sysBP')
ggplot(age_diaBP, aes(x=age, y=diaBP)) + geom_point(col='blue') + ggtitle('age_diaBP')

## male_sysBP, male_diaBP ?떆媛곹솕 ##
ggplot(blood, aes(x=sysBP, y=diaBP, col=male)) + geom_point()

# ?꽦蹂꾨퀎 怨좏삁?븬 鍮꾩쑉 
male0 <- length(blood[blood$male==0 & blood$highBP == 1,1]) / length(blood[blood$male==0,1])
male1 <- length(blood[blood$male==1 & blood$highBP == 1,1]) / length(blood[blood$male==1,1])

##################### ?꽦蹂꾨??닔 ?깉?씫!!

## 蹂?닔異붽? ?썑 ?뜲?씠?꽣遺꾪븷
names(blood)
blood <- blood[,-15]
train.df.f <- blood[train.row,]
valid.df.f <- blood[valid.row,]
test.df.f <- blood[test.row,]
summary(train.df.f)

#### ?떊寃쎈쭩 紐⑦삎 ?떎?떆 ####


## ?뜲?씠?꽣 0怨? 1?궗?씠濡? ##

nnblood2 <- blood[,c(1,2,4,5,6,7,10,11,12,15,8,9)]

summary(nnblood2)
nnblood2$BMI <- (nnblood2$BMI-15.5)/(56.8-15.5)
nnblood2$totChol <- (nnblood2$totChol-107)/(696-107)
nnblood2$heartRate <- (nnblood2$heartRate-44)/(143-44)
nnblood2$glucose <- (nnblood2$glucose-40)/(394-40)
nnblood2$age <- (nnblood2$age-32)/(70-32)
nnblood2$sysBP <- (nnblood2$sysBP-83.5)/(295-83.5)
nnblood2$diaBP <- (nnblood2$diaBP-48)/(142.5-48)
nnblood2$currentSmoker <- as.numeric(nnblood2$currentSmoker)
nnblood2$BPMeds <- as.numeric(nnblood2$BPMeds)
nnblood2$prevalentStroke <- as.numeric(nnblood2$prevalentStroke)
nnblood2$prevalentHyp <- as.numeric(nnblood2$prevalentHyp)
nnblood2$education <- as.numeric(nnblood2$education)
for(i in 1:nrow(nnblood2)){
  if(nnblood2$education[i]==1){
    nnblood2$education[i] <- 0
  }
  if(nnblood2$education[i]==2){
    nnblood2$education[i] <- 0.33
  }
  if(nnblood2$education[i]==3){
    nnblood2$education[i] <- 0.66
  }
  if(nnblood2$education[i]==4){
    nnblood2$education[i] <- 1
  }
}

summary(nnblood2)
## ?뜲?씠?꽣 遺꾪븷
train.df.nrn2.sys <- nnblood2[train.row,-12]
valid.df.nrn2.sys <- nnblood2[valid.row,-12]
train.df.nrn2.dia <- nnblood2[train.row,-11]
valid.df.nrn2.dia <- nnblood2[valid.row,-11]
test.df.nrn2.sys <- nnblood2[test.row,-12] 
test.df.nrn2.dia <- nnblood2[test.row,-11]

##### ?떊寃쎈쭩?쟻?빀 ####
# sysBP
library(neuralnet)
library(caret)
set.seed(32150897)
names(train.df.nrn2.sys)

nrn.sys2.f <- neuralnet(sysBP ~ education + currentSmoker + BPMeds + prevalentHyp + 
                          totChol + BMI + heartRate + 
                          glucose + age, data=train.df.nrn2.sys,hidden=2) ## 紐⑤뱺蹂?닔 + ???땳?끂?뱶2媛? 理쒓퀬?꽦?뒫  

plot(nrn.sys2.f, rep='best')
pred.nrn.sys1.f <- compute(nrn.sys2.f, train.df.nrn2.sys[,-11])
pred.nrn.sys2.f <- compute(nrn.sys2.f, valid.df.nrn2.sys[,-11])

## ?썝?옒?떒?쐞濡? ?룎?젮二쇨린 sysBPmax 295 sysBPmin83.5    ##
org.pred.nrn.sys1.f <- as.vector(pred.nrn.sys1.f$net.result)*(295-83.5)+83.5
org.pred.nrn.sys2.f <- as.vector(pred.nrn.sys2.f$net.result)*(295-83.5)+83.5
## trainset ?삁痢? RMSE
names(train.df)

sqrt(sum((org.pred.nrn.sys1.f-train.df[,8])^2)/length(train.df[,8]))

## validation set ?삁痢? RMSE 
sqrt(sum((org.pred.nrn.sys2.f-valid.df[,8])^2)/length(valid.df[,8]))


### diaBP
names(train.df.nrn2.dia)

## BEST ##  BPMeds + prevalentHyp + heartRate + totChol + BMI + glucose + age , hidden=3
nrn.dia2.f <- neuralnet(diaBP ~ BPMeds + prevalentHyp + heartRate + totChol + BMI + glucose +
                          age, data=train.df.nrn2.dia,hidden=3)


plot(nrn.dia2.f, rep='best')
pred.nrn.dia1.f <- compute(nrn.dia2.f, train.df.nrn2.dia[,-11])
pred.nrn.dia2.f <- compute(nrn.dia2.f, valid.df.nrn2.dia[,-11])
## ?썝?옒?떒?쐞濡? ?룎?젮二쇨린 diaBPmax 142.5 diaBPmin48    ##

org.pred.nrn.dia1.f <- as.vector(pred.nrn.dia1.f$net.result)*(142.5-48)+48
org.pred.nrn.dia2.f <- as.vector(pred.nrn.dia2.f$net.result)*(142.5-48)+48
## trainset ?삁痢? RMSE
names(train.df)
sqrt(sum((org.pred.nrn.dia1.f-train.df[,9])^2)/length(train.df[,9]))

## validation set ?삁痢? RMSE
sqrt(sum((org.pred.nrn.dia2.f-valid.df[,9])^2)/length(valid.df[,9]))


## neuralnet sysBP, diaBP ?뼢?긽李⑦듃
x11()
par(mfcol=c(1,2))
gain.nrn1.f <- gains(valid.df.nrn2.sys$sysBP, org.pred.nrn.sys2.f)
sys.nrn1.f <- valid.df.nrn2.sys$sysBP
plot(c(0,gain.nrn1.f$cume.pct.of.total*sum(sys.nrn1.f))~c(0,gain.nrn1.f$cume.obs),
     xlab="# cases", ylab="Cumulative sysBP", main="neuralnet sysBP Lift Chart", type="l")

lines(c(0,sum(sys.nrn1.f))~c(0,dim(valid.df.nrn2.sys)[1]), col="gray", lty=2)
barplot(gain.nrn1.f$mean.resp/mean(sys.nrn1.f), names.arg = gain.nrn1.f$depth, ylim=c(0,2),
        xlab = "Percentile", ylab = "Mean Response", main = "neuralnet sysBP Decile-wise lift chart")

## diaBP
gain.nrn2.f <- gains(valid.df.nrn2.dia$diaBP, org.pred.nrn.dia2.f)
dia.nrn1.f <- valid.df.nrn2.dia$diaBP
plot(c(0,gain.nrn2.f$cume.pct.of.total*sum(dia.nrn1.f))~c(0,gain.nrn2.f$cume.obs),
     xlab="# cases", ylab="Cumulative diaBP", main="neuralnet diaBP Lift Chart", type="l")

lines(c(0,sum(dia.nrn1.f))~c(0,dim(valid.df.nrn2.dia)[1]), col="gray", lty=2)
barplot(gain.nrn2.f$mean.resp/mean(dia.nrn1.f), names.arg = gain.nrn2.f$depth, ylim=c(0,2),
        xlab = "Percentile", ylab = "Mean Response", main = "neuralnet diaBP Decile-wise lift chart")

###################### 理쒖쥌紐⑦삎?룊媛 ###############################

names(test.df.nrn2.sys)

pred.nrn.sys.final <- compute(nrn.sys2.f, test.df.nrn2.sys[,-11])

## ?썝?옒?떒?쐞濡? ?룎?젮二쇨린 sysBPmax 295 sysBPmin83.5    ##
org.pred.nrn.sys.final <- as.vector(pred.nrn.sys.final$net.result)*(295-83.5)+83.5
## test set ?삁痢≪꽦?뒫
accuracy(org.pred.nrn.sys.final , test.df[,8])

### diaBP
names(test.df.nrn2.dia)

pred.nrn.dia.final <- compute(nrn.dia2.f, test.df.nrn2.dia[,-11])

## ?썝?옒?떒?쐞濡? ?룎?젮二쇨린 diaBPmax 142.5 diaBPmin48    ##
org.pred.nrn.dia.final <- as.vector(pred.nrn.dia.final$net.result)*(142.5-48)+48

## testset ?삁痢≪꽦?뒫
accuracy(org.pred.nrn.dia.final , test.df[,9])


## fianl neuralnet sysBP, diaBP ?뼢?긽李⑦듃
x11()
gain.nrn1.final <- gains(test.df.nrn2.sys$sysBP, org.pred.nrn.sys.final)
sys.nrn.final <- test.df.nrn2.sys$sysBP
plot(c(0,gain.nrn1.final$cume.pct.of.total*sum(sys.nrn.final))~c(0,gain.nrn1.final$cume.obs),
     xlab="# cases", ylab="Cumulative sysBP", main="Final neuralnet sysBP Lift Chart", type="l")

lines(c(0,sum(sys.nrn.final))~c(0,dim(test.df.nrn2.sys)[1]), col="gray", lty=2)
barplot(gain.nrn1.final$mean.resp/mean(sys.nrn.final), names.arg = gain.nrn1.final$depth, ylim=c(0,2),
        xlab = "Percentile", ylab = "Mean Response", main = "Final neuralnet sysBP Decile-wise lift chart")

## diaBP
gain.nrn2.final <- gains(test.df.nrn2.dia$diaBP, org.pred.nrn.dia.final)
dia.nrn.final <- test.df.nrn2.dia$diaBP
plot(c(0,gain.nrn2.final$cume.pct.of.total*sum(dia.nrn.final))~c(0,gain.nrn2.final$cume.obs),
     xlab="# cases", ylab="Cumulative diaBP", main="neuralnet diaBP Lift Chart", type="l")

lines(c(0,sum(dia.nrn.final))~c(0,dim(test.df.nrn2.dia)[1]), col="gray", lty=2)
barplot(gain.nrn2.final$mean.resp/mean(dia.nrn.final), names.arg = gain.nrn2.final$depth, ylim=c(0,2),
        xlab = "Percentile", ylab = "Mean Response", main = "Final neuralnet diaBP Decile-wise lift chart")

### 怨좏삁?븬 遺꾨쪟 ?젙?솗?룄 痢≪젙 ###
test.df10 <- test.df[,c(8,9,13,14)]
test.df10$pred_sysBP <- org.pred.nrn.sys.final
test.df10$pred_diaBP <- org.pred.nrn.dia.final

pred_highBP <- ifelse(test.df10$pred_sysBP>=140 | test.df10$pred_diaBP>=90, 1, 0)
test.df10$pred_highBP <- pred_highBP

table(test.df10$highBP,test.df10$pred_highBP)
## 怨좏삁?븬 ?젙遺꾨쪟?쑉 ##
(521+224)/848  ## 0.879


### ?솚?옄援? 蹂? ?쟻?빀 ###
## 50?꽭?씠?븯 ## 
# testset?뿉?꽌 50?꽭?씠?븯 ?뻾踰덊샇 李얘린
under50row<- which(test.df.f$age<=50)
test.df.sys.50 <- test.df.nrn2.sys[under50row,]
pred.nrn.sys.50 <- compute(nrn.sys2.f, test.df.sys.50[,-11])

## ?썝?옒?떒?쐞濡? ?룎?젮二쇨린 sysBPmax 295 sysBPmin83.5    ##
org.pred.nrn.sys.50 <- as.vector(pred.nrn.sys.50$net.result)*(295-83.5)+83.5
## test set ?삁痢≪꽦?뒫
accuracy(org.pred.nrn.sys.50 , test.df[under50row,8])

### diaBP

test.df.dia.50 <- test.df.nrn2.dia[under50row,]
pred.nrn.dia.50 <- compute(nrn.dia2.f, test.df.dia.50[,-11])

## ?썝?옒?떒?쐞濡? ?룎?젮二쇨린 diaBPmax 142.5 diaBPmin48    ##
org.pred.nrn.dia.50 <- as.vector(pred.nrn.dia.50$net.result)*(142.5-48)+48

## testset ?삁痢≪꽦?뒫
accuracy(org.pred.nrn.dia.50 , test.df[under50row,9])


#### 50?꽭?씠?븯 怨좏삁?븬 ?젙遺꾨쪟?쑉
test.df50 <- test.df[under50row,c(8,9,13,14)]
test.df50$pred_sysBP <- org.pred.nrn.sys.50
test.df50$pred_diaBP <- org.pred.nrn.dia.50

pred_highBP50 <- ifelse(test.df50$pred_sysBP>=140 | test.df50$pred_diaBP>=90, 1, 0)
test.df50$pred_highBP <- pred_highBP50

table(test.df50$highBP,test.df50$pred_highBP)

## 怨좏삁?븬 ?젙遺꾨쪟?쑉 ##
(325+79)/(325+7+39+79)  ## 0.898
