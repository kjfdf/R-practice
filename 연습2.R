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
