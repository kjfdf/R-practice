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
prop.trend.test()
#mosaic plot
mosaicplot(result)
demo("colors")
colors()
mosaicplot(result,color=c("tan1","firebrick2"),xlab="Smoking",ylab="Hypertension")
t(result)
mytable(smoking~age,data=acs)
# bar plot
mtcars
result = table(mtcars$cyl,mtcars$sm)
barplot(result, ylim=c(0,20),legend=rownames(result)) #figure legend넣기 
mylegend=paste(rownames(result),"cyl") #legend 항목 뒤에 단위 "cyl"붙여주기
barplot(result, ylim=c(0,20),legend=mylegend)
barplot(result, ylim=c(0,20),legend=mylegend, beside=T) #막대들을 옆으로 쌓아서 보여주기 
barplot(result, ylim=c(0,20),legend=mylegend, beside=T,horiz=T) #옆으로 눕혀서 보여주기 
# 2그룹에서 짝을 이룬 데이터 분석 정규분포고 연속변수일때는 paired t-test, 정규분포가 아니거나 연속변수가 아닐때는 Wilcoxon signed rank test
# 3그룹이상에서 짝을 이룬 데이터 분석은 중 결과가 정규분포이고 연속변수일때는 one-way repeated measures ANOVA, 정규분포가 아니거나 연속변수가 아닐때는 Friedman test, 세그룹이상인데 A약과 B약을 쓴 그룹간의 시간대별 비교는 two-way repeated measured ANOVA   
