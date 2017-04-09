add2 <- function(x,y) {
  x+y
  
}


above10 <- function(x){
  use <- x > 10
  x[use] 
  
}

above <- function(x,n = 10) { 
  use <- x > n
  x[use]
  
}

columnmean <- function(y, removeNA = TRUE){
  nc <- ncol(y)
  means <- numeric(nc)
  
  for (i in 1:nc){
    
    means[i] <- mean(y[,i], na.rm = removeNA)
  }
  means
}

function(data, fixed = c(FALSE,FALSE)){
  params <- fixed
  function(p){
    params[!fixed] <- p
    mu <- params[1]
    sigma <- params[2]
    a <- -0.5*length(data)*log(2*pi*sigma^2)
    b <- -0.5*sum((data-mu)^2/(sigma^2))
    -(a+b)
  }
}

library(manipulate)
library(ggplot2)
myHist <- function(mu){
  g <- ggplot(galton, aes(x = child))
  g <- g + geom_histogram(fill = "salmon", binwidth = 1, aes(y=..density..), colour = "black")
  g <- g + geom_density(size = 2)
  g <- g + geom_vline(xintercept = mu, size = 2)
  mse <- round(mean((galton$child - mu)^2),3)
  g <- g + labs(title = paste('mu = ', mu, 'MSE = ', mse))
  g
}
manipulate(myHist(mu), mu = slider(62, 74, step = 0.5))

nosim <- 1000
n <- 10
sd(apply(matrix(rnorm(nosim*n), nosim),1, mean))


nosim <- 1000
n <- 10
sd(apply(matrix(sample(0:1, nosim*n, replace = TRUE)),1, mean))

round(x(var(x),var(x)/n, sd(x), sd(x)/sqrt(n)),2)

choose(8,7)*.5^8 + choose(8,8)*.5^8

pbinom(6,size=8,prob=.5,lower.tail=FALSE) # fenbu hanshu haishi survival function

pnorm(1160, mean=1020, sd=50, lower.tail=FALSE)
pnorm(2.8, lower.tail=FALSE)
qnorm(0.75, mean=1020, sd=50)


# the number of people that show up at a bus stop is Poisson with a mean of 2.5 per hour
# if watching the bus stop for 4 hours, what is the probablity that 3 or fewer people show up for the whole time
ppois(3, lambda = 2.5 * 4)
# flip a coin with success probability 0.01 five hundred times
# what is the probability of 2 or fewer successes
pbinom(2, size=500, prob=0.01)
ppois(2, lambda = 500 * 0.01)

# Laws of large numbers
n <- 1000
means <- cumsum(rnorm(n))/(1:n)
means <- cumsum(sample(0:1, n ,replace = TRUE))/(1:n)

library(UsingR)
data(father.son)
x <- father.son$sheight
(mean(x)+c(-1,1)*qnorm(0.975)*sd(x)/sqrt(length(x)))/12

0.56 + c(-1,1)*qnorm(0.975)*sqrt(0.56*0.44/100)
binom.test(56,100)$conf.int


n <- 20 # n<- 100
pvals <- seq(0.1, 0.9, by = 0.05)
nosim <- 1000
coverage <- sapply(pvals, function(p) {
  phats <- rbinom(nosim, prob = p, size=n)/n  # phats <- rbinom(nosim, prob = p, size=n + 2)/(n+4)
  ll <- phats - qnorm(0.975)*sqrt(phats*(phats)/n)
  ul <- phats + qnorm(0.975)*sqrt(phats*(1-phats)/n)
  mean(ll<p & ul>p)
})

# myCoverage <- function(m){
#   g <- ggplot(coverage, aes(x = child))
#   g
# }
# manipulate(myCoverage(m), m = slider(62, 74, step = 0.5))


x <- 5
t<- 94.32
lambda <- x/t
round(lambda + c(-1,1)*qnorm(0.975)*sqrt(lambda/t),3)

poisson.test(x, T=94.32)$conf

#simulating the posisson converage

lambdavals <- seq(0.005, 0.1, by = 0.01)
nosim <- 1000
t <- 100
coverage <- sapply(lambdavals, function(lambda){
  lhats <- rpois(nosim, lambda = lambda*t)/t
  ll <- lhats - qnorm(0.975)*sqrt(lhats/t)
  ul <- lhats + qnorm(0.975)*sqrt(lhats/t)
  mean(ll < lambda & ul > lambda)
})

# myCoverage <- function(m){
# p <- ggplot(lambdavals, aes(x=lambdavals, y=coverage))
# p + geom_point() + geom_smooth()
# p
# }
# manipulate(myCoverage(m), m = slider(62, 74, step = 0.5))

data(sleep)
head(sleep)
g1 <- sleep$extra[1:10]
g2 <- sleep$extra[11:20]
difference <- g2-g1
mn <- mean(difference)
s <- sd(difference)
n <- 10

mn + c(-1,1)*qt(.975, n-1)*s/sqrt(n)
t.test(difference)
t.test(g2,g1,paired = TRUE)
t.test(extra ~ I(relevel(group,2)), paired = TRUE, data=sleep)

#comparing SBP for 8 oral contraceptive users versus 21 controls 
#Xoc = 132.86 mmHg with soc = 15.34 mmHg
#Xc = 127.44 mmHg with sc =18.23 mmHg
# Pooled variance estimate

sp <- sqrt((7*15.34^2 + 20 * 18.23^2)/(8 + 21 - 2))
132.86 - 127.44 + c(-1,1)*qt(.975, 27)*sp*(1/8+1/21)^.5

n1 <- length(g1); n2 <- length(g2)
sp <- sqrt(((n1-1)*sd(x1)^2 + (n2-1)*sd(x2)^2)/(n1+n2-2))
md <- mean(g2) - mean(g1)
semd <- sp*sqrt(1/n1 + 1/n2)
rbind(
  md + c(-1,1)*qt(.975, n1+n2-2)*semd, 
  t.test(g2, g1, paired=FALSE, var.equal = TRUE)$conf,
  t.test(g2, g1, paired=FALSE)$conf
  
)

# ChickWeight data in R
library(datasets)
data(ChickWeight)
library(reshape2)
wideCW <- dcast(ChickWeight, Diet + Chick ~ Time, value.var = "weight")
names(wideCW)[-(1:2)] <- paste("time", names(wideCW)[-(1:2)], sep = "")
library(dplyr)
wideCW <- mutate(wideCW, 
        gain = time21 - time0
        )

wideCW14 <- subset(wideCW, Diet %in% c(1,4))
rbind(
  t.test(gain ~ Diet, paired = FALSE, var.equal = TRUE, data = wideCW14)$conf,
  t.test(gain ~ Diet, paired = FALSE, var.equal = FALSE, data = wideCW14)$conf
)


librayr(UsingR)
data("father.son")
t.test(father.son$sheight - father.son$fheight)

library(manipulate)
mu0=30
mua=32
sigma=4
n=16
z <- qnorm(1-alpha)
pnorm(mu0 + z * sigma/sqrt(n), mean=mua, sd=sigma/sqrt(n), lower.tail = FALSE)
pnorm(mu0 + z * sigma/sqrt(n), mean=mu0, sd=sigma/sqrt(n), lower.tail = FALSE)
myplot <- function(sigma, mua, n, alpha){
  g = ggplot(data.frame(mu = c(27,36)), aes(x=mu))
  g = g + stat_function(fun=dnorm, geom = "line",
                        args = list(mean = mu0, sd = sigma/sqrt(n)),
                        size = 2, col = "red")
  g = g + stat_function(fun=dnorm, geom="line",
                        args = list(mean = mua, sd = sigma/sqrt(n)),
                        size = 2, col = "blue")
  xitc = mu0 + qnorm(1 - alpha) * sigma/sqrt(n)
  g = g + geom_vline(xintercept = xitc, size = 3)
  g
}
manipulate(myplot(sigma, mua, n, alpha),
           sigma = slider(1,10,step=1, initial = 4),
           mua = slider(30,35,step = 1, initial = 32),
           n = slider(1,50,step=1, initial = 16),
           alpha = slider(0.01, 0.1, step = 0.01, initial = 0.05)
           )

power.t.test(n = 16, delta = 2/4, sd = 1, type = "one.sample", alt = "one.sided")$power

# delta/sd = constant, the result will be the same
power.t.test(power = 0.8, delta = 2/4, sd = 1, type = "one.sample", alt = "one.sided")$n

set.seed(1010093)
pValues <- rep(NA, 1000)
for(i in 1:1000){
  y <- rnorm(20)
  x <- rnorm(20)
  pValues[i] <- summary(lm(y~x)$coefficients[2,4])
}
# Controls false positive rate
sum(pValues < 0.05)
# Controls FWER
sum(p.adjust(pValues, method = "bonferroni") < 0.05)
# Controls FDR Benjamini-Hochberg Correction
sum(p.adjust(pValuesm, method = "BH")<0.05)

set.seed(1010093)
pValues <- rep(NA, 1000)
for(i in 1:1000){
  x <- rnorm(20)
  # First 500 beta=0, last 500 beta=2
  if(i<=500){y <- rnorm(20)}else{y <- rnorm(20, mean=2*x)}
  pValues[i] <- summary(lm(y~x))$coeff[2,4]
  }
  trueStatus <- rep(c("zero", "not zero"), each=500)
  table(pValues < 0.05, trueStatus)

#Controls FWER
table(p.adjust(pValues, method="bonferroni")<0.05, trueStatus)

#P-values versus adjusted P-values
par(mfrow=c(1,2))
plot(pValues, p.adjust(pValues, method = "bonferroni"), pch=19)
plot(pValues, p.adjust(pValues, method = "BH"), pch=19)
    
library(UsingR)
data(father.son)
x <- father.son$sheight
n <- length(x)
B <- 10000
resamples <- matrix(sample(x, n*B, replace = TRUE), B, n)
resampledMedians <- apply(resamples, 1, median)
sd(resampledMedians)
# quantile(resampledMedians, c(0.025, 0.975))
myplot <- function(resampledMedians){
g = ggplot(data.frame(medians=resampledMedians), aes(x=resampledMedians))
g = g + geom_histogram(color = "black", fill = "lightblue", binwidth = 0.05)
g
}
manipulate(myplot(resampledMedians)

)


subdata <- InsectSprays[InsectSprays$spray %in% c("B", "C"),]
y <- subdata$count
group <- as.character(subdata$spray)
testStat <- function(w,g) mean(w[g == "B"]) - mean(w[g == "C"])
observedStat <- testStat(y, group)
permutations <- sapply(1:10000, function(i) testStat(y, sample(group)))
observedStat
mean(permutations > observedStat)
myplot <- function(permutations){
  g = ggplot(data.frame(y=y),aes(x=data.frame(x=permutations), y=y))
  g = g + geom_histogram(color = "black", fill = "lightblue", binwidth = 0.05)
  g
}
hist(permutations)

ggplot(galton, aes(x=parent, y=child)) + geom_point()

library(dplyr)
# constructs table for different combination of parent-child height
freqData <- as.data.frame(table(galton$child, galton$parent))
names(freqData) <- c("child (in)", "parent (in)", "freq")
# convert to numeric values
freqData$child <- as.numeric(as.character(freqData$child))
freqData$parent <- as.numeric(as.character(freqData$parent))
# filter to only meaningful combinations
myPlot <- function(beta){
g <- ggplot(filter(freqData, freq > 0), aes(x = parent, y = child))
g <- g + scale_size(range = c(2, 20), guide = "none" )
# plot grey circles slightly larger than data as base (achieve an outline effect)
g <- g + geom_point(colour="grey50", aes(size = freq+10, show_guide = FALSE))
# plot the accurate data points
g <- g + geom_point(aes(colour=freq, size = freq))
# change the color gradient from default to lightblue -> $white
g <- g + scale_colour_gradient(low = "lightblue", high="white")
g <- g + geom_abline(intercept = 0, slope = beta, size = 3)
mse <- mean((y-beta*x)^2)
g <- g + ggtitle(paste("beta = ", beta, "mse = ", round(mse,3)))
g
}
manipulate(myPlot(beta), beta = slider(0.6, 1.2, step = 0.02))

lm(I(child - mean(child))~I(parent - mean(parent)) -1, data=galton)



y<- galton$child
x <- galton$parent
beta1 <- cor(y,x)* sd(y)/sd(x)
beta0 <- mean(y) - beta1* mean(x)
rbind(c(beta0, beta1), coef(lm(y~x)))


yc <- y - mean(y)
xc <- x - mean(x)
beta1 <- sum(yc*xc)/sum(xc^2)
c(beta1, coef(lm(y~x))[2])

lm(yc ~ xc -1)

yn <- (y - mean(y))/sd(y)
xn <- (x - mean(x))/sd(x)
c(cor(y,x), cor(yn,xn), coef(lm(yn~xn))[2])


g <- ggplot(filter(freqData, freq > 0), aes(x = parent, y = child))
g <- g + scale_size(range = c(2, 20), guide = "none" )
# plot grey circles slightly larger than data as base (achieve an outline effect)
g <- g + geom_point(colour="grey50", aes(size = freq+10, show_guide = FALSE))
# plot the accurate data points
g <- g + geom_point(aes(colour=freq, size = freq))
# change the color gradient from default to lightblue -> $white
g <- g + scale_colour_gradient(low = "lightblue", high="white")
g <- g + geom_smooth(method = "lm", formula = y~x)
g


library(UsingR)
data(father.son)
y <- (father.son$sheight - mean(father.son$sheight))/sd(father.son$sheight)
x <- (father.son$fheight - mean(father.son$fheight))/sd(father.son$fheight)
rho <- cor(x,y)
library(ggplot2)
g = ggplot(data.frame(x=x,y=y), aes(x=x,y=y))
g = g + geom_point(size=6, colour = "black", alpha=0.2)
g = g + geom_point(size=4, colour = "salmon", alpha=0.2)
g = g + xlim(-4,4) + ylim(-4,4)
g = g + geom_abline(intercept = 0, slope = 1)
g = g + geom_vline(xintercept = 0)
g = g + geom_hline(yintercept = 0)
g = g + geom_abline(intercept = 0, slope = rho, size = 2)
g = g + geom_abline(intercept = 0, slope = 1/rho, size = 2)
g


library(UsingR)
data("diamond")
library(ggplot2)
g = ggplot(diamond, aes(x = carat, y = price))
g = g + xlab("Mass (carats)")
g = g + ylab("Price (SIN $)")
g = g + geom_point(size=6, colour = "black", alpha = 0.2)
g = g + geom_point(size=5, colour = "blue", alpha = 0.2)
g = g + geom_smooth(method = "lm", colour = "black")
g
fit <- lm(price ~ carat, data = diamond)
coef(fit)
summary(fit)
fit2 <- lm(price ~ I(carat - mean(carat)), data = diamond)
coef(fit2)
fit3 <- lm(price~I(carat*10), data = diamond)
coef(fit3)
newx <- c(0.16,0.27,0.34)
coef(fit)[1] + coef(fit)[2]*newx
predict(fit, newdata = data.frame(carat = newx))



fit4 <- lm(sheight ~ fheight, data = father.son)
coef(fit4)
jeffrey <- c(178,177,176)
coef(fit4)[1] + coef(fit4)[2]*jeffrey

y <- diamond$price; x <- diamond$carat; n <- length(y)
fit <- lm(y~x)
e <- resid(fit)
yhat <- predict(fit)
max(abs(e - (y-yhat)))
max(abs(e - (y - coef(fit)[1] - coef(fit)[2]*x)))
plot(diamond$carat, diamond$price,
     xlab = "Mass (carats)",
     ylab = "Price SIN $",
     bg = "lightblue",
     col = "black", cex=1.1, pch=21, frame=FALSE
)
abline(fit, lwd=2)
points(diamond$carat, predict(fit), pch = 19, col = "red")
lines(c(0.16, 0.16, 0.12), c(200, coef(fit)[1] + coef(fit)[2] * 0.16,
                             coef(fit)[1] + coef(fit)[2] * 0.16))
text(newx+c(0.03, 0, 0), rep(250, 3), labels = newx, pos = 2)
for(i in 1:n)
  lines(c(x[i], x[i]), c(y[i], yhat[i], col = "red", lwd = 2))


plot(x,e,
     xlab = "Mass (carats)",
     ylab = "Residual SIN $",
     bg = "lightblue",
     col = "black", cex=1.1, pch=21, frame=FALSE
)
abline( h =0, lwd=2)
for(i in 1:n)
  lines(c(x[i], x[i]), c(e[i], 0), col = "red", lwd = 2)

x = runif(100,-3,3); y=x+sin(x)+rnorm(100, sd=.2);
library(ggplot2)
g = ggplot(data.frame(x=x,y=y), aes(x=x,y=y))
g = g + geom_smooth(method = "lm", colour = 'black')
g = g + geom_point(size = 7, colour = "black", alpha = 0.4)
g = g + geom_point(size = 5, colour = "red", alpha = 0.4)
g

#Residual plot
g = ggplot(data.frame(x=x,y=y), aes(x=x,y=y))
g = g + geom_hline(yintercept = 0, size=2);
g = g + geom_point(size = 7, colour = "black", alpha = 0.4)
g = g + geom_point(size = 5, colour = "red", alpha = 0.4)
g = g + xlab("X") + ylab("Residual")
g

x = runif(100,0,6); y = x+rnorm(100, mean=0, sd=.001*x);
library(ggplot2)
g = ggplot(data.frame(x=x,y=y), aes(x=x,y=y))
g = g + geom_smooth(method = "lm", colour = 'black')
g = g + geom_point(size = 7, colour = "black", alpha = 0.4)
g = g + geom_point(size = 5, colour = "red", alpha = 0.4)
g

g = ggplot(data.frame(x=x, y=resid(lm(y~x))),
           aes(x=x,y=y))
g = g + geom_hline(yintercept = 0, size=2)
g = g + geom_point(size = 7, colour = "black", alpha = 0.4)
g = g + geom_point(size = 5, colour = "red", alpha = 0.4)
g = g + xlab("X") + ylab("Residual")
g

diamond$e <- resid(lm(price~carat, data = diamond))
g = ggplot(diamond, aes(x=carat, y=e))
g = g + xlab("Mass (carats)")
g = g + ylab("Residual price (SIN $)")
g = g + geom_hline(yintercept = 0, size=2)
g = g + geom_point(size = 7, colour = "black", alpha = 0.5)
g = g + geom_point(size = 5, colour = "blue", alpha = 0.2)
g

e = c(resid(lm(price~1, data=diamond)), #variation in diamond prices around the average diamond price
      resid(lm(price~carat, data=diamond)))#variation around the regression line Mind
fit = factor(c(rep("Itc", nrow(diamond)),
               rep("Itc, slope", nrow(diamond))))

g = ggplot(data.frame(e=e, fit=fit), aes(y=e, x=fit, fill=fit))
g = g + geom_dotplot(binaxis = "y", dotsize = 80, stackdir = "center", binwidth = 0.3)
g = g + xlab("Fitting approach")
g = g + ylab("Residual price (SIN $)")
g


y <- diamond$price; x <- diamond$carat; n <- length(y)
fit <- lm(y~x)
summary(fit)$sigma
sqrt(sum(resid(fit)^2)/(n-2))

library(UsingR)
data("diamond")
y <- diamond$price; x <- diamond$carat; n <- length(y)
beta1 <- cor(y,x)*sd(y)/sd(x)
beta0 <- mean(y) - beta1*mean(x)
e <- y - beta0 - beta1*x
sigma <- sqrt(sum(e^2)/(n-2))
ssx <- sum((x-mean(x))^2)
seBeta0 <- (1/n + mean(x)^2/ssx)^.5*sigma
seBeta1 <- sigma/sqrt(ssx)
tBeta0 <- beta0/seBeta0
tBeta1 <- beta1/seBeta1
pBeta0 <- 2*pt(abs(tBeta0), df=n-2, lower.tail = FALSE)
pBeta1 <- 2*pt(abs(tBeta1), df=n-2, lower.tail = FALSE)
coefTable <- rbind(c(beta0, seBeta0, tBeta0, pBeta0), c(beta1, seBeta1, tBeta1, pBeta1))
colnames(coefTable) <- c("Estimate","Std. Error", "t value", "Pr(>|t|)")
rownames(coefTable) <- c("(Intercept)", "x")
coefTable
fit <- lm(y~x)
sumCoef <- summary(fit)$coefficients
sumCoef[1,1] + c(-1,1)*qt(.975, df=fit$df)*sumCoef[1,2]
sumCoef[2,1] + c(-1,1)*qt(.975, df=fit$df)*sumCoef[2,2]



library(ggplot2)
newx = data.frame(x=seq(min(x), max(x), length=100))
p1 = data.frame(predict(fit, newdata = newx, interval = ("confidence")))
p2 = data.frame(predict(fit, newdata = newx, interval = ("prediction")))
p1$interval = "confidence"
p2$interval = "prediction"
p1$x = newx$x
p2$x = newx$x
dat = rbind(p1,p2)
names(dat)[1]="y"

g=ggplot(dat, aes(x=x,y=y))
g=g+geom_ribbon(aes(ymin=lwr, ymax=upr, fill=interval), alpha=0.2)
g=g+geom_line()
g=g+geom_point(data=data.frame(x=x,y=y),aes(x=x,y=y),size=2)
g

n=100;x=rnorm(n);x2=rnorm(n);x3=rnorm(n)
y = 1 + x + x2 + x3 + rnorm(n, sd=.1)
ey = resid(lm(y ~ x2 + x3))
ex = resid(lm(x~ x3 + x3))
sum(ex*ey)/sum(ex^2)
coef(lm(ey~ex-1))
coef(lm(y~x + x2 + x3))


# load dataset
require(datasets); data(swiss); require(GGally)
# produce pairwise plot using ggplot2
g = ggpairs(swiss, lower = list(continuous = wrap("smooth", method = "lm")))

summary(lm(Fertility~ . , data = swiss))$coefficients
summary(lm(Fertility~Agriculture , data = swiss))$coefficients


n < -100; x2 <- 1:n; x1 <- .01*x2 + runif(n, -.1, .1); y = -x1 + x2 + rnorm(n, sd = .01)
summary(lm(y~x1))$coef
summary(lm(y~x1+x2))$coef

dat = data.frame(y=y,x1=x1,x2=x2,ey=resid(lm(y~x2)), ex1=resid(lm(x1~x2)))
library(ggplot2)
g <- ggplot(dat, aes(y = y, x = x1, colour = x2)) +
  geom_point(colour="grey50", size = 2) +
  geom_smooth(method = lm, se = FALSE, colour = "black") + geom_point(size = 1.5) +
  ggtitle("unadjusted = y vs x1")


#redisual plot
g2 <- ggplot(dat, aes(y = ey, x = ex1, colour = x2)) +
  geom_point(colour="grey50", size = 2) +
  geom_smooth(method = lm, se = FALSE, colour = "black") + geom_point(size = 1.5) +
  ggtitle("adjusted = y, x1 residuals with x2 removed") + labs(x = "resid(x1~x2)",
                                                               y = "resid(y~x2)")
# combine plots
multiplot(g, g2, cols = 2)

z <- swiss$Agriculture + swiss$Education
lm(Fertility~.+z, data = swiss)

data(InsectSprays)
ggplot(data = InsectSprays, aes(y = count, x = spray, fill = spray)) +
  geom_violin(colour = "black", size = 2) + xlab("Type of spray") +
  ylab("Insect count")

summary(lm(count ~ spray, data = InsectSprays))$coefficients
# hard coding dummy variables
lm(count ~ I(1 * (spray == 'B')) + I(1 * (spray == 'C')) +
     I(1 * (spray == 'D')) + I(1 * (spray == 'E')) +
     I(1 * (spray == 'F')), data = InsectSprays)$coefficients
# linear model with omitted intercept
summary(lm(count ~ spray - 1, data = InsectSprays))$coefficients
spray2 <- relevel(InsectSprays$spray, "C")
# rerun linear regression with releveled factor
summary(lm(count ~ spray2, data = InsectSprays))$coef

spray2 <- relevel(InsectSprays$spray, "C")
library(dplyr)
swiss = mutate(swiss, CatholicBin = 1 * (Catholic > 50))
g = ggplot(swiss, aes(x = Agriculture, y = Fertility, colour = factor(CatholicBin)))
g = g + geom_point(size = 6, colour = "black") + geom_point(size = 4)
g = g + xlab("% in Agriculture") + ylab("Fertility")
g
  
fit=lm(Fertility~Agriculture, data=swiss)
g1=g
g1=g1+geom_abline(intercept = coef(fit)[1], slope = coef(fit)[2],size=2)
g1

fit = lm(Fertility~Agriculture+factor(CatholicBin), data=swiss)
summary(fit)$coef
g1=g
g1=g1+geom_abline(intercept = coef(fit)[1], slope = coef(fit)[2],size=2)
g1=g1+geom_abline(intercept = coef(fit)[1]+coef(fit)[3], slope = coef(fit)[2],size=2)
g1

fit = lm(Fertility~Agriculture*factor(CatholicBin), data=swiss)
g1=g
g1=g1+geom_abline(intercept = coef(fit)[1], slope = coef(fit)[2],size=2)
g1=g1+geom_abline(intercept = coef(fit)[1]+coef(fit)[3], slope = coef(fit)[2]+coef(fit)[4],size=2)
g1


n <- 100; t <- rep(c(0,1), c(n/2, n/2)); x <- c(runif(n/2),runif(n/2))
beta <- 0; beta1 <-2; tau<-1; sigma<- .2
y <- beta0 + x*beta1 + t*tau + rnorm(n, sd=sigma)
plot(x,y, type="n", frame=FALSE)
abline(lm(y~x),lwd=2)
abline(h = mean(y[1:(n/2)]), lwd=3)
abline(h = mean(y[(n/2 + 1):n]), lwd=3)
fit <- lm(y~x+t)
abline(coef(fit)[1], coef(fit)[2], lwd=3)
abline(coef(fit)[1]+coef(fit)[3], coef(fit)[2], lwd=3)
points(x[1:(n/2)], y[1:(n/2)], pch=21, col="black",bg="lightblue",cex=1)
points(x[(n/2+1):n], y[(n/2+1):n], pch=21, col="black",bg="salmon",cex=1)
# between the horizontal line shows the marginal effects
# y = beta0 + beta1T + beta2X + e 
# beta1 represnt the change in intercepts between the groups
# beta2 represents the common slope that exists across the two groups


library(rgl)
plot3d(x1,x2,y)

data("swiss"); par(mfrow=c(2,2))
fit <- lm(Fertility~., data=swiss)
plot(fit)

n <- 100; x <- c(10, rnorm(n)); y <- c(10, rnorm(n))
plot(x, y,frame=FALSE,cex=2,pch=21,bg="lightblue", col="black")
abline(lm(y~x))
fit <- lm(y~x)
round(dfbetas(fit)[1:10,2],3)
round(hatvalues(fit)[1:10],3)


dat <- read.table('http://www4.stat.ncsu.edu/~stefanski/NSF_Supported/Hidden_Images/orly_owl_files/orly_owl_Lin_4p_5_flat.txt')
pairs(dat)
summary(lm(V1~.-1, data=dat))$coef
fit<- lm(V1~.-1, data=dat)
plot(predict(fit),resid(fit), pch='.')











