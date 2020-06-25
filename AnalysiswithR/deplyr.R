install.packages("dplyr")
library("dplyr")
data=read.csv("femaleMiceWeights.csv")
head(data)
control=filter(data,diet=="chow")
head(control)
control=select(control,Bodyweight)
head(control)
unlist(control)

control2=filter(data,diet=="chow")%>% select(Bodyweight)%>%unlist
control2

install.packages("downloader")
library(downloader)
url="https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/msleep_ggplot2.csv"
filename <- basename(url)
download(url,filename)

datax=read.csv("msleep_ggplot2.csv")
head(datax)
class(datax)

filterx=filter(datax,order=="Primates")
head(filterx)
nrow(filterx)
class(filterx)

filterx=select(datax,sleep_total)
head(filterx)

mean(unlist(filterx))

filter(datax, order=="Primates") %>% summarise( mean( sleep_total) )




library(downloader) 
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/mice_pheno.csv"
filename <- basename(url)
download(url, destfile=filename)
dat <- read.csv(filename) 

dat <- na.omit( dat )

colnames(dat)
control=filter(dat,Diet=="chow"& Sex=="M")%>% select(Bodyweight)%>%summarise(mean(Bodyweight))
control

library(rafalib)
?popsd
popsd(control)

set.seed(1)
X <- sample(control,25)
mean(X)

x <- filter(dat, Sex=="M" & Diet=="chow") %>% select(Bodyweight) %>% unlist
mean(x)

set.seed(1)
X <- sample(x,25)
mean(X)

y <- filter(dat, Sex=="M" & Diet=="hf") %>% select(Bodyweight) %>% unlist
mean(y)
popsd(y)

set.seed(1)
Y <- sample(y,25)
mean(Y)

abs( ( mean(y) - mean(x) ) - ( mean(Y) - mean(X) ) )


x <- filter(dat, Sex=="F" & Diet=="chow") %>% select(Bodyweight) %>% unlist
set.seed(2)
X <- sample(x,25)
y <- filter(dat, Sex=="F" & Diet=="hf") %>% select(Bodyweight) %>% unlist
set.seed(2)
Y <- sample(y,25)
abs( ( mean(y) - mean(x) ) - ( mean(Y) - mean(X) ) )
