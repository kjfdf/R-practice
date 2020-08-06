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
# data ID별로 ALSFRS 분류하기
data %>% gather(key,value,ALSFRS0:ALSFRS18.12) %>% 
  separate(key,c("variable","type"),6) %>% 
  spread(variable,value)
# character
n=3
m="3"
# n의 문자여부 확인 및 문자로 변경하
is.character(n)
class(n)
as.character(n)
# 문자합치기
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
# mtcars의 행 변수 중 글자수구하기
nchar(cars)
# 행 변수의 길이 중 가장 긴 변수가 있는 위치 찾기
which(nchar(cars)==max(nchar(cars)))
# 행변수의 길이가 가장 긴 변수의 이르
cars[which(nchar(cars)==max(nchar(cars)))]
# 이름에 z가 들어있는 변수 찾기
cars[grep("z",cars)]
cars[grep("v",cars)]
# cars에서 문자들 전부다 소문자 또는 대문자로 바꾸기
tolower(cars)
toupper(cars)
# cars에서 toyota가 들어있는 것의 항목 보여주기 
grep("toyota",tolower(cars),value=TRUE)
grep("toyota",tolower(cars),value=T)
# stringr 패키지 설치
library(stringr)
# cars에서 t가 들어있는 각각변수들내에서의 갯수 보여줌
str_count(cars,"t")
# car에서 t가 들어있는 총 갯수 
sum(str_count(tolower(cars),"toyota"))
# 40부터 120까지를 300개가 나오게 균일한 간격으로 숫자 쪼개기
x <- seq(40,120,length=300)
x
# x를 평균이 80이고 표준편차가 10이 되게 정규분포로 
y <- dnorm(x,mean=80,sd=10)
y
# 40부터 120까지 정규분포를 그래프로 그리기
plot(x,y)
# 그래프를 선으로 나오게 하고 색을 빨간색으로
plot(x,y,type="l",col="red")
# 라인 추가
lines(x,dnorm(x,mean=80,sd=20),col="blue")
# 65부터 75까지의 확률구하기
x2 <- seq(65,75,length=200)
y2 <- dnorm(x2,mean=80,sd=10)
polygon(c(65,x2,75),c(0,y2,0),col="grey")
# 평균이 80이고 표준편차가 10인 정규분포의 65에서 75사이의 확률구하기
pnorm(75,mean=80,sd=10)-pnorm(65,mean=80,sd=10)
# 평균보다 큰값에서 오른쪽 끝까지의 확률
pnorm(92,mean=80,sd=10,lower.tail=F)
1-pnorm(92,mean=80,sd=10) 
pnorm(68,mean=80,sd=10)
# 30%지점찾기
qnorm(0.3,mean=80,sd=10)
qnorm(0.8,mean=80,sd=10)
# 중간 60%찾기
qnorm(0.2,mean=80,sd=10)
qnorm(0.8,mean=80,sd=10)
# 문자 쪼개기
a <- strsplit("how are you?",split="")
# 문자 다시 합치기 
paste(a[[1]],collapse="")
# 문자 순서 뒤바꾸기
reversed <- a[[1]][12:1]
reversed
# 문자 순서 한글자씩 뒤바꾸는 함수 만들기
reverse_myf <- function (string){
  a <- strsplit(string,split="")
  reversed <- a[[1]][nchar(string):1]
  paste(reversed,collapse="")
}
reverse_myf("love of my life")
# 어절단위로 뒤집기
rev_word <- function(string){
  a <- strsplit(string,split=" ")
  str_length <- length(a[[1]])
  reversed <- a[[1]][str_length:1]
  paste(reversed,collapse=" ")  
}
rev_word("how is she?")
# 카이제곱검정, 행의 합은 rowSums(), 열의 합은 colSums(), outer()는 매트릭스내의 행과 열을 각각 매치시켜서 곱을 구함
# 자유도는 (2-1)*(2-1)로 구함(df), 들어가있는 숫자가 작아서 Yates correction을 했는데 이런경우 correct=F를 넣어줌. 
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
# dot chart, cex는 글자크기, color를 변수별로 구분해서 지정하기위해 factor로 변경, cyl별로 그룹을 나눠서 나오게 함. 
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
