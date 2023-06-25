setwd("~/GitHub/BAYES_R_MARKDOWN")

pbinom(0,100,0.00720,lower.tail=FALSE)

integrate (function(p) dbeta(p,14,27),0,0.5)

integrate (function(x) dbeta(x,5,1195),0.005,1)

integrate(function(x) dnorm(x,mean=20.6,sd=1.62),10,18)

xs <- seq(0.005,0.01,by=0.00001)
png("PDF_plot.png") 
plot(xs,dbeta(xs,300,40000-300),type="l",lwd=3,
     ylab="density",
     xlab="probability of subscripton",
     main="PDF Beta(300,39700)")
dev.off()

pbeta(0.0065,300,39700)

pbeta(1,300,39700) - pbeta(0.0085,300,39700)

pbinom(3,5,0.5)

qbeta(0.999,300,39700)

n.trials <- 100000
prior.alpha <- 3
prior.beta <- 7
a.samples <- rbeta(n.trials,36+prior.alpha,114+prior.beta)
b.samples <- rbeta(n.trials,50+prior.alpha,100+prior.beta)
p.b_superior <- sum(b.samples > a.samples)/n.trials

p.b_superior  

pbinom(24,100,0.5)

dx <- 0.01
hypothesis <- seq(0,1,by=dx)
bayes.factor <- function (h_top,h_bottom) {
  ((h_top)^24*(1-h_top)^76)/((h_bottom)^24*(1-h_bottom)^76)
}

bfs <- bayes.factor(hypothesis,0.5)
png("all_possible_hypotheses_plot.png")
plot(hypothesis,bfs,type="l")
dev.off()
max(bfs)
hypothesis[which.max(bfs)]

priors <- ifelse(hypothesis >=0.2 & hypothesis <=0.3, 1/1000,1)
plot(hypothesis,priors,type="l")

posteriors <-priors*bfs
plot(hypothesis,posteriors,type="l")

sum(posteriors)
p.posteriors <- posteriors/sum(posteriors)
sum(p.posteriors)
plot(hypothesis,p.posteriors,type="l")

sum(p.posteriors[which(hypothesis < 0.5)])
sum(p.posteriors*hypothesis)
hypothesis[which.max(p.posteriors)]