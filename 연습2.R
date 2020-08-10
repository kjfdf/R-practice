# 파일읽어오기
A <- read.csv("C:/Users/BRMH/Desktop/유일한/홍윤호교수님 연구/ALSFRS_R-edit1.csv",header=T,sep=",")
A
# power analysis
install.packages("pwr")
library(pwr)
mean1 <- mean(score[group==1])
mean2 <- mean(score[group==2])
sd1 <- sd(score[group==1])
sd2 <- sd(score[group==2])
effectsize <- abs(mean1-mean2)/(sqrt((sd1^2+sd2^2)/2))
pwr.t.test(d=effectsize,power=.8,sig.level=.05,type="two.sample",alternative="two.sided")
# 결과값이 연속변수가 아니거나 정규분포가 아니면 Mann-Whitney U test or Wilcoxon's Rank sum test, 정규분포이고 등분산이 아니면 Welch's T test, 정규분포이고 등분산이면 Student T-test
library(moonBook)
data(acs)
acs
str(acs)
summary(acs)
table(is.na(acs))
shapiro.test(acs$age[acs$sex=="Male"])
shapiro.test(acs$age[acs$sex=="Female"])
var.test(data=acs,age~sex) #p<.05면 등분산성이 아닌것. p>.05이어야 등분산성을 가정할 수 있어서 Student's t-test사용 가능 
t.test(data=acs,age~sex,var.equal=T) #Student's t test
t.test(data=acs,age~sex,var.equal=F) #Welch's test
# power analysis
install.packages("pwr")
library(pwr)
mean1 <- mean(score[group==1])
mean2 <- mean(score[group==2])
sd1 <- sd(score[group==1])
sd2 <- sd(score[group==2])
effectsize <- abs(mean1-mean2)/(sqrt((sd1^2+sd2^2)/2))
pwr.t.test(d=effectsize,power=.8,sig.level=.05,type="two.sample",alternative="two.sided")
library(moonBook)
data(acs)
acs
str(acs)
summary(acs)
table(is.na(acs))
shapiro.test(acs$age[acs$sex=="Male"])
shapiro.test(acs$age[acs$sex=="Female"])
var.test(data=acs,age~sex) #p<.05면 등분산성이 아닌것. p>.05이어야 등분산성을 가정할 수 있어서 Student's t-test사용 가능 
t.test(data=acs,age~sex,var.equal=T) #Student's t test
t.test(data=acs,age~sex,var.equal=F) #Welch's test
# 결과값이 연속변수가 아니거나 정규분포가 아니면 Mann-Whitney U test or Wilcoxon's Rank sum test, 정규분포이고 등분산이 아니면 Welch's T test, 정규분포이고 등분산이면 Student T-test
# 3그룹 이상 사후검정
install.packages("moonBook")
library(moonBook)
#kruskal walis test에서의 사후검정
install.packages("userfriendlyscience") #kruskal walis test에서의 사후검정
library(userfriendlyscience)
posthocTGH(Dx,y=LDLC,data=acs,method="games=howell")
# Welch's ANOVA의 사후검정
install.packages("nparcomp")
library(nparcomp)
result <- mctp(LDLC~Dx,data=acs)
summary(result)
# odds ratio
data("mtcars")
attach(mtcars)
table(cyl,am)
library(moonBook)
mtcars$tm <- ifelse(am==0,"automatic","manual")
table(mtcars$cyl,mtcars$tm)
mtcars$sm <- factor(mtcars$am,labels=c("automatic","manual"))
result <- table(cyl,mtcars$sm)
result
addmargins(result) #table끝에 합계추가
chisq.test(result)
fisher.test(result)
#Cochrane armitage test
acs$smoking=factor(acs$smoking,levels=c("Never","Ex-smoker","Smoker")) #never, ex-smoker, smoker 순으로 서열을 부여하기 위해 순서 변경 
