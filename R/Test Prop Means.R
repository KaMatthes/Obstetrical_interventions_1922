res <- prop.test(x = c(727, 423), n = c(1407, 1145))
res

https://stackoverflow.com/questions/63056742/independent-sample-t-test-in-r-using-test-statistics-mean-std-dev-count



means <- c(2.5, 3.7)
stddev <- c(1.5, 1.8)
n <- c(25,35)

df <- sum(n)-2
stddev_pooled <- sqrt(sum((n-1)*stddev^2)/df)
tstat <- (means[1]-means[2])/(stddev_pooled*sqrt(sum(1/n)))

2*pt(abs(tstat),df=df,lower.tail=FALSE)

install.packages("DescTools")

TTestA(mx=39.8, my=40.1, sx=2.5, sy=2.7, nx=1194, ny=1141, var.equal=TRUE)

