library(tidyr)
names(data)
a <- gather(data,key,value,ALSFRS0:ALSFRS18.12)
gather(data,key,value)
a <- gather(data,key,value,c("ALSFRS0":"ALSFRS0.12"):c("ALSFRS18":"ALSFRS18.12"))
b <- separate(a,key,c("valuable","type"),6)
b
c <- spread(b,variable,value)
filter(c,ID)
filter(a,ID)
names(a)
filter(a,"ID")
data %>% group_by(ID)
names(data)
data %>% group_by(ID)
filter(data,ID==348)
 names(data)
# data IDë³„ë¡œ ALSFRS ë¶„ë¥˜?•˜ê¸?
data %>% gather(key,value,ALSFRS0:ALSFRS18.12) %>% 
  separate(key,c("variable","type"),6) %>% 
  spread(variable,value)
# character
n=3
m="3"
# n?˜ ë¬¸ìž?—¬ë¶€ ?™•?¸ ë°? ë¬¸ìžë¡? ë³€ê²½í•˜
is.character(n)
class(n)
as.character(n)
# ë¬¸ìž?•©ì¹˜ê¸°
hi <- paste("hi","jack")
hi
paste("1",1:10,sep="-")
paste("ALS",1:18,sep="")
paste("a0","b","c")
noquote(paste("a0","b","c"))
mtcars
rownames(mtcars)
nchar(cars)
nchar(mtcars)
cars <- rownames(mtcars)
# mtcars?˜ ?–‰ ë³€?ˆ˜ ì¤? ê¸€?ž?ˆ˜êµ¬í•˜ê¸?
nchar(cars)
# ?–‰ ë³€?ˆ˜?˜ ê¸¸ì´ ì¤? ê°€?ž¥ ê¸? ë³€?ˆ˜ê°€ ?žˆ?Š” ?œ„ì¹? ì°¾ê¸°
which(nchar(cars)==max(nchar(cars)))
# ?–‰ë³€?ˆ˜?˜ ê¸¸ì´ê°€ ê°€?ž¥ ê¸? ë³€?ˆ˜?˜ ?´ë¥?
cars[which(nchar(cars)==max(nchar(cars)))]
# ?´ë¦„ì— zê°€ ?“¤?–´?žˆ?Š” ë³€?ˆ˜ ì°¾ê¸°
cars[grep("z",cars)]
cars[grep("v",cars)]
# cars?—?„œ ë¬¸ìž?“¤ ? „ë¶€?‹¤ ?†Œë¬¸ìž ?˜?Š” ??€ë¬¸ìžë¡? ë°”ê¾¸ê¸?
tolower(cars)
toupper(cars)
# cars?—?„œ toyotaê°€ ?“¤?–´?žˆ?Š” ê²ƒì˜ ?•­ëª? ë³´ì—¬ì£¼ê¸° 
grep("toyota",tolower(cars),value=TRUE)
grep("toyota",tolower(cars),value=T)
# stringr ?Œ¨?‚¤ì§€ ?„¤ì¹?
library(stringr)
# cars?—?„œ tê°€ ?“¤?–´?žˆ?Š” ê°ê°ë³€?ˆ˜?“¤?‚´?—?„œ?˜ ê°??ˆ˜ ë³´ì—¬ì¤?
str_count(cars,"t")
# car?—?„œ tê°€ ?“¤?–´?žˆ?Š” ì´? ê°??ˆ˜ 
sum(str_count(tolower(cars),"toyota"))
# 40ë¶€?„° 120ê¹Œì?€ë¥? 300ê°œê?€ ?‚˜?˜¤ê²? ê· ì¼?•œ ê°„ê²©?œ¼ë¡? ?ˆ«?ž ìª¼ê°œê¸?
x <- seq(40,120,length=300)
x
# xë¥? ?‰ê· ì´ 80?´ê³? ?‘œì¤€?Ž¸ì°¨ê?€ 10?´ ?˜ê²? ? •ê·œë¶„?¬ë¡? 
y <- dnorm(x,mean=80,sd=10)
y
# 40ë¶€?„° 120ê¹Œì?€ ? •ê·œë¶„?¬ë¥? ê·¸ëž˜?”„ë¡? ê·¸ë¦¬ê¸?
plot(x,y)
# ê·¸ëž˜?”„ë¥? ?„ ?œ¼ë¡? ?‚˜?˜¤ê²? ?•˜ê³? ?ƒ‰?„ ë¹¨ê°„?ƒ‰?œ¼ë¡?
plot(x,y,type="l",col="red")
# ?¼?¸ ì¶”ê?€
lines(x,dnorm(x,mean=80,sd=20),col="blue")
# 65ë¶€?„° 75ê¹Œì?€?˜ ?™•ë¥ êµ¬?•˜ê¸?
x2 <- seq(65,75,length=200)
y2 <- dnorm(x2,mean=80,sd=10)
polygon(c(65,x2,75),c(0,y2,0),col="grey")
# ?‰ê· ì´ 80?´ê³? ?‘œì¤€?Ž¸ì°¨ê?€ 10?¸ ? •ê·œë¶„?¬?˜ 65?—?„œ 75?‚¬?´?˜ ?™•ë¥ êµ¬?•˜ê¸?
pnorm(75,mean=80,sd=10)-pnorm(65,mean=80,sd=10)
# ?‰ê· ë³´?‹¤ ?°ê°’ì—?„œ ?˜¤ë¥¸ìª½ ?ê¹Œì?€?˜ ?™•ë¥?
pnorm(92,mean=80,sd=10,lower.tail=F)
1-pnorm(92,mean=80,sd=10) 
pnorm(68,mean=80,sd=10)
# 30%ì§€? ì°¾ê¸°
qnorm(0.3,mean=80,sd=10)
qnorm(0.8,mean=80,sd=10)
# ì¤‘ê°„ 60%ì°¾ê¸°
qnorm(0.2,mean=80,sd=10)
qnorm(0.8,mean=80,sd=10)
# ë¬¸ìž ìª¼ê°œê¸?
a <- strsplit("how are you?",split="")
# ë¬¸ìž ?‹¤?‹œ ?•©ì¹˜ê¸° 
paste(a[[1]],collapse="")
# ë¬¸ìž ?ˆœ?„œ ?’¤ë°”ê¾¸ê¸?
reversed <- a[[1]][12:1]
reversed
# ë¬¸ìž ?ˆœ?„œ ?•œê¸€?ž?”© ?’¤ë°”ê¾¸?Š” ?•¨?ˆ˜ ë§Œë“¤ê¸?
reverse_myf <- function (string){
  a <- strsplit(string,split="")
  reversed <- a[[1]][nchar(string):1]
  paste(reversed,collapse="")
}
reverse_myf("love of my life")
# ?–´? ˆ?‹¨?œ„ë¡? ?’¤ì§‘ê¸°
rev_word <- function(string){
  a <- strsplit(string,split=" ")
  str_length <- length(a[[1]])
  reversed <- a[[1]][str_length:1]
  paste(reversed,collapse=" ")  
}
rev_word("how is she?")
# ì¹´ì´? œê³±ê?€? •, ?–‰?˜ ?•©??€ rowSums(), ?—´?˜ ?•©??€ colSums(), outer()?Š” ë§¤íŠ¸ë¦??Š¤?‚´?˜ ?–‰ê³? ?—´?„ ê°ê° ë§¤ì¹˜?‹œì¼œì„œ ê³±ì„ êµ¬í•¨
# ?ž?œ ?„?Š” (2-1)*(2-1)ë¡? êµ¬í•¨(df), ?“¤?–´ê°€?žˆ?Š” ?ˆ«?žê°€ ?ž‘?•„?„œ Yates correction?„ ?–ˆ?Š”?° ?´?Ÿ°ê²½ìš° correct=Fë¥? ?„£?–´ì¤?. 
data <- matrix(c(1,2,3,4),nrow=2,byrow=T)
data
chisq <- function(obs){
  expected <- outer(rowSums(obs),colSums(obs))/sum(obs)
  sum((obs-expected)^2/expected)
}
data
chisq(data)
1-pchisq(0.07936508,1)
chisq.test(data)
chisq.test(data,correct=F)
# dot chart, cex?Š” ê¸€?ž?¬ê¸?, colorë¥? ë³€?ˆ˜ë³„ë¡œ êµ¬ë¶„?•´?„œ ì§€? •?•˜ê¸°ìœ„?•´ factorë¡? ë³€ê²?, cylë³„ë¡œ ê·¸ë£¹?„ ?‚˜?ˆ ?„œ ?‚˜?˜¤ê²? ?•¨. 
plot(mtcars$mpg)
dotchart(mtcars$mpg,labels=row.names(mtcars),cex=0.6)
carmpg <- mtcars[order(mtcars$mpg),]
carmpg
carmpg$cyl <- factor(carmpg$cyl)
carmpg$color[carmpg$cyl==4] <- "blue"
carmpg$color[carmpg$cyl==6] <- "green"
carmpg$color[carmpg$cyl==8] <- "red"
dotchart(carmpg$mpg,labels=row.names(carmpg),cex=.7,col=carmpg$color,groups=carmpg$cyl,main="milage",xlab="miles per gallon")
# 
mtcars  
newdata <- mtcars[,1:2]
newdata[which(newdata$cyl==4),]
newdata[newdata$cyl==4,]
# binomial (0.5ÀÇ È®·üÀÎµ¥ 10°³»Ì¾ÒÀ»¶§ 6°³°¡ ³ª¿Ã È®·ü),hypergeometric distribution (ÀüÃ¼ Áß ³²ÀÚ°¡ 8, ¿©ÀÚ°¡ 10¸íÀÏ¶§ 10¸íÀ» ¼±¹ßÇØ¼­ ±× Áß¿¡¼­ ³²ÀÚ°¡ 5¸íÀÏ È®·ü)
dbinom(6,10,0.5)
dhyper(5,8,10,10)
?dbinom
dhyper(3,24,36,10)
# hypergeometric distributionÀ» 100¹ø ½ÇÇàÇÏ°ÔÇÏ´Â ¸í·É¹®
a=100
approx <- numeric(length=a)
for (i in 1:a){
  approx[i]=dhyper(3,4*i,6*i,10)
}
approx
plot(approx[2:100])
# hypergeometric distributionÀÌ binomial distribution¿¡ ±ÙÁ¢ÇÑ´Ù´Â °ÍÀ» º¸ÀÌ±â À§ÇÑ ¼±Ãß°¡
abline(h=dbinom(3,10,0.4),col="red")
approx-dbinom(3,10,0.4)
# Aids2 Åë°è·®À» °¡Áö°í ºÐ¼®, aggregate(±Ã±ÝÇÑºÎºÐ, ±âÁØ, ¾Ë°í½ÍÀº °ª) 
install.packages("MASS")
library(MASS)
data(Aids2)
str(Aids2)
?Aids2
head(Aids2)
summary(Aids2)
which(Aids2$age==0)
Aids2[Aids2$age==0,]
Alive <- Aids2[which(Aids2$status=="A"),]
Alive
Dead <- Aids2[Aids2$status=="D",]
Dead
aggregate(Alive$age,by=list(Alive$sex),mean)
aggregate(Dead$age,by=list(Dead$sex),mean)
aggregate(Aids2$age,by=list(Aids2$sex,Aids2$status),median)
# line plot, par(mfrow=c(n1,n2))·Î ¿·¿¡ÀÖ´Â ±×·¡ÇÁÃ¢ ºÐÇÒ°¡´É, ±×·¡ÇÁ Å¸ÀÔÀÌ³ª Á¡ÀÇ ¸ð¾ç º¯°æ  
x <- c(1:10)
y <- x^2-x+10
par(mfrow=c(2,4))
for(i in 1:8){
  plot(x,y, type="p",col="blue",pch=i)
}
x <- c(1:10)
y <- x^2-x+10
par(mfrow=c(2,4))
types=c("p","l","o","b","c","s","S","h")
for(i in 1:8){
  plot(x,y, type=types[i],col="blue",pch=i)
}
# if,else¹®, ¹®ÀåÀÌ ³ª¿À°Ô ÇÏ·Á¸é return, ¼ýÀÚ¿Í ¹®ÀåÀÌ È¥ÀçµÉ¶§´Â paste, ¼Ò¼öÁ¡ 2ÀÚ¸®·Î ÇÏ·Á¸é round(~,2) 
mean_by_cyl <- function(x){
  if(x==4){a <- round(mean(mtcars[which(mtcars$cyl==4),][,1]),2)
    return(paste("the avg mile per gallon of",x,"cylinder car is",a))
  }
  else if(x==6){b <- round(mean(mtcars[which(mtcars$cyl==6),][,1]),2)
  return(paste("the avg mile per gallon of",x,"cylinder car is",b))
  }else if(x==8){c <- round(mean(mtcars[which(mtcars$cyl==8),][,1]),2)
  return(paste("the avg mile per gallon of",x,"cylinder car is",c))
  }else{print("Wrong number")
  }
}
mean_by_cyl <- function(x){
  mean(mtcars[which(mtcars$cyl==x),][,1])
}
mean_by_cyl(6)  
# mySQL
install.packages("RMySQL")
library(RMySQL)
mydb <- dbConnect(MySQL(),user="root",password="5553",dbname="sampdb")
# R¸¶Å©´Ù¿î 
update.packages()
# matrix, Çà·Ä°öÀº %*%·Î, t(b)ÇÏ¸é Çà·ÄÇüÅÂ ¹Ù²ñ 
a=matrix(c(1,2,3,6,7,8),nrow=2,byrow=F)
a
b=array(1:3,c(2,3))
b
class(b)
a*b
a%*%b
t(b)
a%*%t(b)
a==b
# matrix ÆÇº°½Ä(det), ¿ªÇà·Ä (solve) °¢°¢ ³»ÀåÇÔ¼ö°¡ ÀÖÀ½. 
def_f <- function(a){
  d <- a[1,1]*a[2,2]-a[1,2]*a[2,1]
  return(d)
}
c <- matrix(c(1,4,2,7),nrow=2)
def_f(b)
det(b)
array(1:3,c(4,5))
inv_f <- function(x){
  b <- matrix(nrow=2,ncol=2)
  b[1,1] <- x[2,2]
  b[2,2] <- x[1,1]
  b[1,2] <- -x[1,2]
  b[2,1] <- -x[2,1]
  return(b)
}
inv_f(c)
solve(c)
c%*%solve(c)
round(c%*%solve(c),2)
# eigen vector(°öÇßÀ»¶§ ÀÚ±âÀÚ½ÅÀÇ »ó¼ö¹è·Î ¸¸µå´Â Çà·Ä),eigen value
a <- matrix(c(3,2,7,4,2,1,4,5,7),nrow=3)
a
ev <- eigen(a)$values
evec <- eigen(a)$vectors
evec%*%diag(ev)%*%solve(evec)
ev
trans <- function (x){
  b <- matrix(nrow=nrow(x),ncol=ncol(x))
  for (i in 1:nrow(x)){
    for (j in 1:ncol(x)){
      b[j,i] <- a[i,j]
    }
  }
  return(b)
}
trans(b)
# ggplot, stat_smooth´Â °üÅëÇÏ´Â ¼±À» ¸¸µé¾îÁÜ. levelÀº ½Å·Úµµ 
age <- c(21,42,53,67,45,23,29,68,74,89)
hr <- c(102,105,123,53,74,98,45,21,57,90)
df <- data.frame(age,hr)
df
lm_result <- lm(hr~age,data=df)
lm_result
hr <- -0.4323*age+98.8882
print(paste(-0.4323,"*age","+",98.8882))
library(ggplot2)
ggplot(df,aes(age,hr))+geom_point()+xlab("AGE")+ylab("Heart Rate")+ggtitle("age and heart rate")+stat_smooth(method=lm,level=0.99)
# ¼±Çü¸ðµ¨, ÀÜÂ÷¸¦ ÃÖ¼Ò·Î ÇÏ´Â ¼±ÀÌ ±×¾îÁü , summary·Î ¼öÄ¡µé ÇÑ¹ø¿¡ º¼ ¼ö ÀÖÀ½. 
fit <- lm(hr~age)
fit
plot(age,hr,xlim=c(0,100))
abline(fit,col="red")
fit$coefficients[[1]]
fit$coefficients[[2]]
fit$residuals
summary(fit)
# imputation된 결과들 ALSFRS_R-imputated뒤에 일련번호 1부터 10까지 붙여서 저자
for (i in 1:10)
{
  write.csv(complete(imp, i), file=paste("ALSFRS_R-imputated", i, ".csv", sep=""))
}장
# imputation된 CSV파일 불러들여서 NA값 갯수 확인 
for (i in 1:10){
  data_i <- read.csv(paste("ALSFRS_R-imputated", i, ".csv", sep=""))
  print(table(is.na(data_i)))
}
data_1 <- read.csv("ALSFRS_R-imputated1.csv")
table(is.na(data_1))
sum(is.na(data_1))
colSums(is.na(data_1))
# 1차 imputation된 결과들 2차 imputation
for (i in 1:10){
  imp <- mice(data_i,10)
  fit <- with(imp, glm(ALS.x~.,data=data_i))
  pooled <- pool(fit)
  summary(fit)
}
