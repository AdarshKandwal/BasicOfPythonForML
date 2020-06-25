library(rafalib)
data(father.son,package="UsingR") ##available from CRAN
x <- father.son$fheight

round(sample(x,20),1)

hist(x,breaks =seq(floor(min(x)),ceiling(max(x))),main ="heights",xlab = "heights in inches")


#empirical cumulative density function
mycdf <-ecdf(x)

xs<-seq(floor(min(x)),ceiling(max(x)),0.1)

plot(xs,mycdf((xs)),type="l",xlab="Height",ylab ="F(X)")

#qq plot 
#1 manual way
x <- father.son$fheight
mean(x<69)
pnorm(69,mean(x),sd(x))
#2plotting
ps <- seq(0.01,0.99,0.01)
qs <- quantile(x,ps)
normalqs <- qnorm(ps,mean(x),popsd(x))
plot(normalqs,qs,xlab="Normal percentiles",ylab="Height percentiles")
abline(0,1) ##identity line

load("C:/Users/Dell/Downloads/skew.RData")
dim(dat)
par(mfrow = c(3,3))
for (i in 1:9) {
  # **put your qqnorm call here**
  x <- dat[,i]
  qqnorm(x,  main=paste0("Q-Q plot for column V.",i,sep=""))
  qqline(x)
}

#making a histogram for skewed ones that is 4 and 9

yy <- dat[,4]
hist(dat[,4])

xx <-dat[,9]
hist(xx)


##box plot
head(InsectSprays)
boxplot(split(InsectSprays[,count], factor))
boxplot( split(InsectSprays$count, InsectSprays$spray), main="split(values, factor version) version")

library(dplyr)
data(nym.2002, package="UsingR")
data

num2 <-as.data.frame(nym.2002)
boxplot(time~gender, num2)