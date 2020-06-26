url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/babies.txt"
filename <- basename(url)
download(url, destfile=filename)
babies <- read.table("babies.txt", header=TRUE)

N=5
set.seed(1)
rejects <- replicate(10000,{
  dat.ns <- sample(bwt.nonsmoke , N)
  dat.s <- sample(bwt.smoke , N)
  t.test(dat.s, dat.ns)$p.value < 0.05
})

mean(rejects)


Ns=c(10,60,90,120)
res <- sapply(Ns, function(N){
  set.seed(1)
  rejects <- replicate(10000,{
    dat.ns <- sample(bwt.nonsmoke , N)
    dat.s <- sample(bwt.smoke , N)
    t.test(dat.s, dat.ns)$p.value < 0.05
  })
  mean(rejects)
})
Ns[ which.min( abs( res - .8) ) ] 

Ns=c(10,60,90,120)
res <- sapply(Ns, function(N){
  set.seed(1)
  rejects <- replicate(10000,{
    dat.ns <- sample(bwt.nonsmoke , N)
    dat.s <- sample(bwt.smoke , N)
    t.test(dat.s, dat.ns)$p.value < 0.01
  })
  mean(rejects)
})
Ns[ which.min( abs( res - .8) ) ]

#week3 montecarlo excersice

set.seed(1)
X<-rnorm(5)
t<-(sqrt(5)*mean(X))/sd(X)
t

#permutation

url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/babies.txt"
filename <- basename(url)
download(url, destfile=filename)
babies <- read.table("babies.txt", header=TRUE)
bwt.nonsmoke <- filter(babies, smoke==0) %>% select(bwt) %>% unlist 
bwt.smoke <- filter(babies, smoke==1) %>% select(bwt) %>% unlist

N=10
set.seed(1)
nonsmokers <- sample(bwt.nonsmoke , N)
smokers <- sample(bwt.smoke , N)
obs <- mean(smokers) - mean(nonsmokers)

set.seed(1)
null <- replicate(1000, {
  shuffle <- sample( dat )
  smokersstar <- shuffle[1:N]
  nonsmokersstar <- shuffle[(N+1):(2*N)]
  mean(smokersstar)-mean(nonsmokersstar)
})
( sum( abs(null) >= abs(obs)) +1 ) / ( length(null)+1 ) 
##we add the 1s to avoid p-values=0 but we also accept:
( sum( abs(null) >= abs(obs)) ) / ( length(null) )


N=10
set.seed(1)
nonsmokers <- sample(bwt.nonsmoke , N)
smokers <- sample(bwt.smoke , N)
obs <- median(smokers) - median(nonsmokers)

set.seed(1)
null <- replicate(1000, {
  shuffle <- sample( dat )
  smokersstar <- shuffle[1:N]
  nonsmokersstar <- shuffle[(N+1):(2*N)]
  median(smokersstar)-median(nonsmokersstar)
})
( sum( abs(null) >= abs(obs)) +1 ) / ( length(null)+1 ) 
##we add the 1s to avoid p-values=0 but we also accept:
( sum( abs(null) >= abs(obs)) ) / ( length(null) )

d = read.csv("assoctest.csv")
head(d)

