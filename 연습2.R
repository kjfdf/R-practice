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
library(tidyr)
setwd("C:/Users/s")
data <- read.csv("C:/Users/s/desktop/ALSFRS_R-2020.7.15 데이터 편집-YIH(1달내 여러 차례 반복시 최소값만 남김).csv",header=T)
# paired t-test
data2 <- gather(data,key= "group", value= "result",-c(SubjectID,MONTH))
data2
head(data2)
shapiro.test(pd2$result[pd2$group=="before"])
shapiro.test(pd2$result[pd2$group=="after"])
d <- pd2$result[pd2$group=="before"]-pd2$result[pd2$group=="after"]
shapiro.test(d)
install.packages("PairedData")
library(PairedData)
before <- subset(data, group=="month0",result, drop=T)
after <-  subset(data, group=="month1",result, drop=T)
data3 <- paired(before,after)
plot(data3,type="profile")+theme_bw()
t.test(result~group,data=data2,paired=T)
# Wilcoxon signed rank test
data(sleep)
head(sleep)
shapiro.test(sleep$extra[sleep$group==2]-sleep$extra[sleep$group==1])
with(sleep,
     shapiro.test(extra[group==2]-extra[group==1]))
attach(sleep)
shapiro.test(extra[group==2]-extra[group==1])
before <- subset(sleep,group==2,extra,drop=T)
after <- subset(sleep,group==1,extra,drop=T)
d <- paired(before,after)
plot(d,type="profile")+theme_bw()
wilcox.test(extra[group==2]-extra[group==1],exact=F)
# one way repeated measures ANOVA
install.packages("gplots")
library(gplots)
install.packages("car")
library(car)
means <- c(mean(OW$score0),mean(ow$score1),mean(ow$score3),mean(ow$score6))
plotCI(x=means,uiw=se, type='l',ylab='score',xlab='month',main='one way test') #error bar는uiw=se로 추가
multmodel <- lm(cbind(ow$score0,ow$score1,ow$score3,ow$score6)~1)
trials <- factor(c("score0","score1","score3","score6"),ordered=F)
model1 <- anova(multmodel,idata=data.frame(trials),idesign=~trials,type="III")
summary(model1,multivariate=F)
owlong <- gather(ow,key="ID",value="score")
out=aov(score~ID,data=owlong)
shapiro.test(resid(out))
TukeyHSD(out)
se <- sd(owlong$score)/sqrt(length(owlong$score))  #error bar넣는법
# Friedman test
?friedman.test
RoundingTimes <-
  matrix(c(5.40, 5.50, 5.55,
           5.85, 5.70, 5.75,
           5.20, 5.60, 5.50,
           5.55, 5.50, 5.40,
           5.90, 5.85, 5.70,
           5.45, 5.55, 5.60,
           5.40, 5.40, 5.35,
           5.45, 5.50, 5.35,
           5.25, 5.15, 5.00,
           5.85, 5.80, 5.70,
           5.25, 5.20, 5.10,
           5.65, 5.55, 5.45,
           5.60, 5.35, 5.45,
           5.05, 5.00, 4.95,
           5.50, 5.50, 5.40,
           5.45, 5.55, 5.50,
           5.55, 5.55, 5.35,
           5.45, 5.50, 5.55,
           5.50, 5.45, 5.25,
           5.65, 5.60, 5.40,
           5.70, 5.65, 5.55,
           6.30, 6.30, 6.25),
         nrow = 22,
         byrow = TRUE,
         dimnames = list(1 : 22,
                         c("Round Out", "Narrow Angle", "Wide Angle")))
install.packages("reshape")
library(reshape)
RT2 <- melt(RoundingTimes)
out <- aov(value~X2,data=RT2)
shapiro.test(resid(out))
boxplot(value~X2,data=RT2)
friedman.test(RoundingTimes)
install.packages("coin")#friedman test의 사후검정 
library(coin)
friedman.test.with.post.hoc <- function(formu, data, to.print.friedman = T, to.post.hoc.if.signif = T,  to.plot.parallel = T, to.plot.boxplot = T, signif.P = .05, color.blocks.in.cor.plot = T, jitter.Y.in.cor.plot =F)
{
  # Loading needed packages
  if(!require(coin))
  {
    print("You are missing the package 'coin', we will now try to install it...")
    install.packages("coin")
    library(coin)
  }
  
  if(!require(multcomp))
  {
    print("You are missing the package 'multcomp', we will now try to install it...")
    install.packages("multcomp")
    library(multcomp)
  }
  
  if(!require(colorspace))
  {
    print("You are missing the package 'colorspace', we will now try to install it...")
    install.packages("colorspace")
    library(colorspace)
  }
  
  
  # get the names out of the formula
  formu.names <- all.vars(formu)
  Y.name <- formu.names[1]
  X.name <- formu.names[2]
  block.name <- formu.names[3]
  
  if(dim(data)[2] >3) data <- data[,c(Y.name,X.name,block.name)]	# In case we have a "data" data frame with more then the three columns we need. This code will clean it from them...
  
  # Note: the function doesn't handle NA's. In case of NA in one of the block T outcomes, that entire block should be removed.
  
  # stopping in case there is NA in the Y vector
  if(sum(is.na(data[,Y.name])) > 0) stop("Function stopped: This function doesn't handle NA's. In case of NA in Y in one of the blocks, then that entire block should be removed.")
  
  # make sure that the number of factors goes with the actual values present in the data:
  data[,X.name ] <- factor(data[,X.name ])
  data[,block.name ] <- factor(data[,block.name ])
  number.of.X.levels <- length(levels(data[,X.name ]))
  if(number.of.X.levels == 2) { warning(paste("'",X.name,"'", "has only two levels. Consider using paired wilcox.test instead of friedman test"))}
  
  # making the object that will hold the friedman test and the other.
  the.sym.test <- symmetry_test(formu, data = data,	### all pairwise comparisons
                                teststat = "max",
                                xtrafo = function(Y.data) { trafo( Y.data, factor_trafo = function(x) { model.matrix(~ x - 1) %*% t(contrMat(table(x), "Tukey")) } ) },
                                ytrafo = function(Y.data){ trafo(Y.data, numeric_trafo = rank, block = data[,block.name] ) }
  )
  # if(to.print.friedman) { print(the.sym.test) }
  
  
  if(to.post.hoc.if.signif)
  {
    if(pvalue(the.sym.test) < signif.P)
    {
      # the post hoc test
      The.post.hoc.P.values <- pvalue(the.sym.test, method = "single-step")	# this is the post hoc of the friedman test
      
      
      # plotting
      if(to.plot.parallel & to.plot.boxplot)	par(mfrow = c(1,2)) # if we are plotting two plots, let's make sure we'll be able to see both
      
      if(to.plot.parallel)
      {
        X.names <- levels(data[, X.name])
        X.for.plot <- seq_along(X.names)
        plot.xlim <- c(.7 , length(X.for.plot)+.3)	# adding some spacing from both sides of the plot
        
        if(color.blocks.in.cor.plot)
        {
          blocks.col <- rainbow_hcl(length(levels(data[,block.name])))
        } else {
          blocks.col <- 1 # black
        }
        
        data2 <- data
        if(jitter.Y.in.cor.plot) {
          data2[,Y.name] <- jitter(data2[,Y.name])
          par.cor.plot.text <- "Parallel coordinates plot (with Jitter)"
        } else {
          par.cor.plot.text <- "Parallel coordinates plot"
        }
        
        # adding a Parallel coordinates plot
        matplot(as.matrix(reshape(data2,  idvar=X.name, timevar=block.name,
                                  direction="wide")[,-1])  ,
                type = "l",  lty = 1, axes = FALSE, ylab = Y.name,
                xlim = plot.xlim,
                col = blocks.col,
                main = par.cor.plot.text)
        axis(1, at = X.for.plot , labels = X.names) # plot X axis
        axis(2) # plot Y axis
        points(tapply(data[,Y.name], data[,X.name], median) ~ X.for.plot, col = "red",pch = 4, cex = 2, lwd = 5)
      }
      
      if(to.plot.boxplot)
      {
        # first we create a function to create a new Y, by substracting different combinations of X levels from each other.
        subtract.a.from.b <- function(a.b , the.data)
        {
          the.data[,a.b[2]] - the.data[,a.b[1]]
        }
        
        temp.wide <- reshape(data,  idvar=X.name, timevar=block.name,
                             direction="wide") 	#[,-1]
        wide.data <- as.matrix(t(temp.wide[,-1]))
        colnames(wide.data) <- temp.wide[,1]
        
        Y.b.minus.a.combos <- apply(with(data,combn(levels(data[,X.name]), 2)), 2, subtract.a.from.b, the.data =wide.data)
        names.b.minus.a.combos <- apply(with(data,combn(levels(data[,X.name]), 2)), 2, function(a.b) {paste(a.b[2],a.b[1],sep=" - ")})
        
        the.ylim <- range(Y.b.minus.a.combos)
        the.ylim[2] <- the.ylim[2] + max(sd(Y.b.minus.a.combos))	# adding some space for the labels
        is.signif.color <- ifelse(The.post.hoc.P.values < .05 , "green", "grey")
        
        boxplot(Y.b.minus.a.combos,
                names = names.b.minus.a.combos ,
                col = is.signif.color,
                main = "Boxplots (of the differences)",
                ylim = the.ylim
        )
        legend("topright", legend = paste(names.b.minus.a.combos, rep(" ; PostHoc P.value:", number.of.X.levels),round(The.post.hoc.P.values,5)) , fill =  is.signif.color )
        abline(h = 0, col = "blue")
        
      }
      
      list.to.return <- list(Friedman.Test = the.sym.test, PostHoc.Test = The.post.hoc.P.values)
      if(to.print.friedman) {print(list.to.return)}
      return(list.to.return)
      
    }	else {
      print("The results where not significant, There is no need for a post hoc test")
      return(the.sym.test)
    }
  }
  
  # Original credit (for linking online, to the package that performs the post hoc test) goes to "David Winsemius", see:
  # http://tolstoy.newcastle.edu.au/R/e8/help/09/10/1416.html
}
friedman.test.with.post.hoc(value~X2|X1,RT2)
0.05/3 #post hoc analysis결과에서 0.016보다 p value가 작은 경우가 통계적으로 의미있는 차이가 있는것임. 
# two way repeated measures ANOVA
acne <- read.csv("10_rmanova.csv",header=T)
acl <- reshape(acne,direction="long",varying=3:6,sep="")
acl$group <- factor(acl$group)
acl$id <- factor(acl$id)
acl$time <- factor(acl$time)
str(acl)
interaction.plot(acl$time,acl$group,acl$month)
acd <- aov(month~group*time+Error(id),data=acl)
summary(acd)
# 상관분석 number class끼리 상관분석가능, logistic regression을 해서 요약결과를 abline으로 그린뒤 cor plot에 삽입가능 
library(moonBook)
data(acs)
colnames(acs)
str(acs)
cor.test(TC,height)
plot(TC~height)
out <- lm(TC~height)
summary(out)
abline(out,col="red")
# jitter를 이용해서 겹친데이터 plot으로 그리기 
plot(jitter(TC,10)~jitter(height,10))
install.packages("SwissAir")
library(SwissAir)
data(AirQual)
str(AirQual)
class(AirQual)
library(dplyr)
ox <- AirQual %>% select(ad.O3,lu.O3,sz.O3,ad.NO,lu.NO,sz.NO)
names(ox) <- c("ad","lu","sz")
plot(lu~sz,data=ox)
install.packages("hexbin")
library(hexbin)
bin <- hexbin(ox$lu,ox$sz,xbins=50)
plot(bin)
smoothScatter(ox$lu,ox$sz)
install.packages("IDPmisc")
library(IDPmisc)
iplot(ox$lu,ox$sz)
# logistic regression
par(mfrow=c(2,2))
plot(fit)
par(mfrow=c(1,1))
fit2 <- lm(weight~height+I(height^2), data=women) #다항회귀(polynomial regression)
# 다중회귀분석
state.x77
states <- as.data.frame(state.x77[,c("Murder","Population","Illiteracy","Income","Frost")])
fit <- lm(Murder~.,data=states)
summary(fit)
par(mfrow=c(2,2))
plot(fit)
install.packages("fmsb")
library(fmsb)
VIF(fit) #다중공선성 확인. VIF>5 면 다중공선성 존재, 10보다 크면 심한 다중공선성 
sqrt(VIF(fit))
influencePlot(fit,id.method="identify")#이상관측치 확인 plot
# 회귀모형의 교정
powerTransform(states$Murder)
boxTidwell(Murder~Population+Illiteracy,data=states)
ncvTest(fit)
spreadLevelPlot(fit)
# 회귀모형 변수의 선택 
summary(fit)
AIC(fit)# AIC(Akaike's an Information Criterion)
reduced.fit <- step(fit, direction="backward")# stepwise regression (Backward or forward stepwise regression)
reduced.fit1 <- step(fit, direction="forward", scope=(Murder~Population+Illiteracy+Income+Frost),trace=0)
summary(reduced.fit1)
# all subset regression
library(leaps)
leaps <- regsubsets(Murder~Population+Illiteracy+Income+Frost,data=states,nbest=4)
plot(leaps,scale="adjr2")
# logistic regression: 생존or사망 all or none분석 
require(survival)
str(colon)
table(is.na(colon))
colon1 <- na.omit(colon)
str(colon1)
result <- glm(status~rx+sex+age+obstruct+perfor+adhere+nodes+differ+extent+surg,family=binomial,data=colon1)
par(mfrow=c(2,2))
plot(result)
summary(result)
reduced.model <- step(result)
library(moonBook)
extractOR(reduced.model)
fit <- glm(formula=status~rx+obstruct+adhere+nodes+extent+surg,family=binomial,data=colon1)# 과산포(overdispersion)가 있으면 family=quasibinomial
fit.od <- glm(formula=status~rx+obstruct+adhere+nodes+extent+surg,family=quasibinomial,data=colon1)
pchisq(summary(fit.od)$dispersion*fit$df.residual,fit$df.residual,lower=F) #값이 >.05이면 과산포가없다고 확신할 수 있음 
# ORplot
ORplot(fit)
?ORplot
ORplot(fit,main="plot for odds ratios",type=2,show.OR = F,pch=15,lwd=2,show.CI = T,col=c("green","purple"))
# 포아송 회귀분석 : 발생율(사망율, 질병발생율)에 관한 분석
data(breslow.dat,package="robust")
library(robust)
summary(breslow.dat)
install.packages("qcc")
library(qcc)
qcc.overdispersion.test(breslow.dat$sumY,type="poisson") #종속변수를 가지고 분석, p-value가 과산포가 있다고 생각해야함
fit <- glm(sumY~Base+Age+Trt,family=quasipoisson,data=breslow.dat)
summary(fit)
extractOR(fit,digits=3)
ORplot(fit,type=2,show.CI = T)
# 생존분석
require(survival)
data(colon)
colon1 <- na.omit(colon)
colon1$ts <- Surv(colon1$time,colon1$status==1) #1이면 사망 혹은 재발을 의미, time은 관찰기간을 의미 
fit <- survfit(ts~rx,data=colon1)
plot(fit,col=1:3,lty=1:3,fun="cumhaz",mark.time=F)
legend("topleft",legend=levels(colon$rx),col=1:3,lty=1:3)
# log-rank test
survdiff(Surv(time,status==1)~rx,data=colon1)
#  Cox regression
out <- coxph(ts~rx,data=colon1)
summary(out)
# Hazard Ratio
library(moonBook)
out <- mycph(ts~.-id-study-time-status-etype,data=colon1)
out
out2 <- coxph(ts~.-id-study-time-status-etype,data=colon1)
final <- step(out2,direction="backward")
HRplot(final,type=2,show.CI = T)
# big data 정리
na.omit(data)
output <- lm(he_wt~sex,data=~)# 정규성확인
shapiro.test(resid(output))
var.test(he_wt~sex,data=~)
t.test(he_wt~sex,data=~,var.equal=F)
library(moonBook)
densityplot(he_wt~sex,data=~)
