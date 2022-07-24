
## Worcester's eight synergistic log-linear sub-models

## Fitting Worcester's saturated model and examine residuals

freq <- c(14, 7, 12, 25, 2, 22, 8, 84)

# options(contrasts=c("contr.treatment", "contr.poly")) #for effect coding
# options(contrasts=c("contr.sum", "contr.poly")) #for effect coding

factorsTSU <- data.frame( A = gl(2,4),  B = gl(2,1,8), C = gl(2,2,8))
factorsTSU<-within(factorsTSU, A<-relevel(A, ref = "2"))
factorsTSU<-within(factorsTSU, B<-relevel(B, ref = "2"))
factorsTSU<-within(factorsTSU, C<-relevel(C, ref = "2"))
factorsTSU<-cbind(factorsTSU, freq)

X3way <- model.matrix( ~ A*B*C, data = factorsTSU)
X3way   # Design matrix for saturated model

X_worc<- X3way[,-1]
X_worc  # model matrix

# Saturated model

fit.X_worc <- glm(freq~X_worc, family = poisson, data = factorsTSU)
cbind(freq,fitted = fitted( fit.X_worc ) , resid = 
        residuals (fit.X_worc , type = "pearson"))

## Fitting all 8 models and reproduce chi-square values

M1<-X3way[,2:8]           # full model
M2<-X3way[,c(2:4,5,6,7)]  #ABC=0
M3<-X3way[,c(2:4,6,7)]    #ABC=AB=0
M4<-X3way[,c(2:4,5,7)]    #ABC=AC=0
M5<-X3way[,c(2:4,5,6)]    #ABC=BC=0
M6<-X3way[,c(2:4,5)]      #ABC=AC=BC=0
M7<-X3way[,c(2:4,5,8)]    #AC=BC=0
M8<-X3way[,c(2:4,8)]      #AB=AC=BC=0


fit.M1<- glm(freq~M1, family = poisson, data = factorsTSU)
fit.M2<- glm(freq~M2, family = poisson, data = factorsTSU)
fit.M3<- glm(freq~M3, family = poisson, data = factorsTSU)
fit.M4<- glm(freq~M4, family = poisson, data = factorsTSU)
fit.M5<- glm(freq~M5, family = poisson, data = factorsTSU)
fit.M6<- glm(freq~M6, family = poisson, data = factorsTSU)
fit.M7<- glm(freq~M7, family = poisson, data = factorsTSU)
fit.M8<- glm(freq~M8, family = poisson, data = factorsTSU)

resi<-cbind( Model1=residuals(fit.M1, type = "pearson"),
             Model2=residuals(fit.M2, type = "pearson"),
             Model3=residuals(fit.M3, type = "pearson"),
             Model4=residuals(fit.M4, type = "pearson"),
             Model5=residuals(fit.M5, type = "pearson"),
             Model6=residuals(fit.M6, type = "pearson"),
             Model7=residuals(fit.M7, type = "pearson"),
             Model8=residuals(fit.M8, type = "pearson"))

chisq.worc<-round(colSums(resi^2),2)


Gsquare<-cbind( Model1=fit.M1$deviance,
                Model2=fit.M2$deviance,
                Model3=fit.M3$deviance,
                Model4=fit.M4$deviance,
                Model5=fit.M5$deviance,
                Model6=fit.M6$deviance,
                Model7=fit.M7$deviance,
                Model8=fit.M8$deviance)
Gsquare<-round(Gsquare,4)             

value_fitted<-cbind( Model1=fitted(fit.M1),
                     Model2=fitted(fit.M2),
                     Model3=fitted(fit.M3),
                     Model4=fitted(fit.M4),
                     Model5=fitted(fit.M5),
                     Model6=fitted(fit.M6),
                     Model7=fitted(fit.M7),
                     Model8=fitted(fit.M8))

good.fit<-rbind(chisq.worc, Gsquare)
row.names(good.fit)<-c("Chi_square", "G_square")

good.fit # Goodness of fit: Chi Square and G Square values for 8 models

rnames<-c("m_hat_111","m_hat_121", "m_hat_112","m_hat_122","m_hat_211","m_hat_221","m_hat_212","m_hat_222")
row.names(value_fitted)<-rnames
round(value_fitted,2) #Fitted values for all 8 models

# p-value for X^2
pchisq(27.9800, 2, lower.tail = F) 


# p-value for G^2
anova( fit.M8, fit.M1, test = "LRT") #LRT and Chisq give the same result

#Alternative way 
#install.packages("lmtest")
#library(lmtest)
#lrtest(fit.M1, fit.M8)
