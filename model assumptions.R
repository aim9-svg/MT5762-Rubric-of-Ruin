library(gvlma) 
library(car)

finalmodel <- lm(baby_wt ~ gestation + m_race + m_ht + m_wt + d_wt + 
                  smoke + previous_preg + gestation:m_race + gestation:m_wt + 
                  gestation:previous_preg + m_wt:d_wt, 
                data = lmData) 
par(mfrow = c(2, 2))
plot(finalmodel)

par(mfrow = c(1, 1))

# Model Assumptions
# Normality of residuals
qqPlot(finalmodel,labels=row.names(babies_data),id.method="identify",simulate=TRUE,main="Q-Q Plot")  

# Distribution of Studnetized Residual
tresid <-rstudent(finalmodel)  
hist(tresid,breaks = 10,freq = FALSE, xlab="Studnetized Residual", main="Distribution of Studnetized Residual") 
rug(jitter(tresid),col="brown")  
curve(dnorm(x,mean=mean(tresid),sd=sd(tresid)), add=TRUE,col="blue",lwd=2)  
lines(density(tresid)$x,density(tresid)$y, col="red",lwd=2,lty=2)  
legend("topright",legend=c("Normal Curve","Kernel Density Curve"), lty=1:2,col=c("blue","red"),cex=0.7)

# Independence of residuals error terms
durbinWatsonTest(finalmodel)


# Linearity of the data
plot(finalmodel, 1)

# Homogeneity of residuals variance
plot(finalmodel, 3)

