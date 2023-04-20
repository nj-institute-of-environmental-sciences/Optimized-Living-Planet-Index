getwd()
setwd("C:/Users/wxj95/Desktop/Index")
read.csv("da2.csv",header=T)
a <- read.csv("da2.csv",header=T)
a
wilcox.test(a, alternative="two.sided", paired=FALSE, exact=TRUE, correct=FALSE)
wilcox.test("A.X2011","A.X2012" alternative="two.sided", paired=FALSE, exact=TRUE, correct=FALSE)