#Let’s assume that you know from past experience that reaction time has a standard
#deviation of 1.25 seconds. Also suppose that a 1-second difference in reaction time is
#considered an important difference. You’d therefore like to conduct a study in which
#you’re  able  to  detect  an  effect  size  of  d  =  1/1.25  =  0.8  or  larger.  
#Additionally,  you want to be 90 percent sure to detect such a difference if it exists,
#and 95 percent sure that you won’t declare a difference to be significant when 
#it’s actually due to random variability. 
#How many participants will you need in your study?

library(pwr)
pwr.t.test(d=.8,sig.level=.05,power=.9,type="two.sample",alternative="two.sided")

#Assume that in comparing the two conditions you want to be able to detect a 0.5 
#standard deviation difference in population means. You want to #limit the chances
#of falsely declaring the population means to be different to 1 out of 100. 
#Additionally, you can only afford to include 40 participants in the study. What’s
#the probability that you’ll be able to detect a difference between the population means
#that’s this large, given the constraints outlined? 

pwr.t.test(n=20,d=.5,sig.level=.01,type="two.sample",alternative="two.sided")

#The previous examples assumed that there are equal sample sizes in the two groups.
#If the sample sizes for the two groups are unequal, the function 
#pwr.t2n.test(n1=, n2=, d=, sig.level=, power=, alternative=)

pwr.t2n.test(n1=20,n2=10,d=.5,sig.level=.01,alternative="two.sided")

# ANOVA
#----------------------------------------------------------------------------------------------
#For a one-way ANOVAcomparing five groups, calculate the sample size needed in 
#each group to obtain a power of 0.80, when the effect size is 0.25 and
#a significance level of 0.05 is employed. The code looks like this:

pwr.anova.test(k=5,f=.25,sig.level=.05,power=.8)

#CORRELATION
#----------------------------------------------------------------------------------------------
#For example, let’s assume that you’re studying the relationship between depression
#and loneliness. Your null and research hypotheses are
#H0: ρ        ≤0.25 versus H1: ρ> 0.25
#where ρ is  the  population  correlation  between  these  two  psychological  variables.
#You’ve set your significance level to 0.05 and you want to be 90 percent confident that
#you’ll reject H0 if it’s false. How many observations will you need?

pwr.r.test(r=.25,sig.level=.05,power=.9,alternative="greater")

#LINEAR MODEL
#----------------------------------------------------------------------------------------------
#Let’s  say  you’re  interested  in  whether  a  boss’s  leadership  style  impacts  workers’
#satisfaction above and beyond the salary and perks associated with the job. Leadership
#style  is  assessed  by  four  variables,  and  salary  and  perks  are  associated  with  three
#variables.  Past  experience  suggests  that  salary  and  perks  account  for  roughly  30
#percent of the variance in worker satisfaction. From a practical standpoint, it would
#be interesting if leadership style accounted for at least 5 percent above this figure.
#Assuming a significance level of 0.05, how many subjects would be needed to identify
#such a contribution with 90 percent confidence?
#Here, sig.level=0.05, power=0.90, u=3  (total  number  of  predictors  minus  the   
#number of predictors in set B), and the effect size is f2= (.35-.30)/(1-.35) = 0.0769    

pwr.f2.test(u=3,f2=0.0769,sig.level=.05,power=.90)
#In multiple regression, the denominator degrees of freedom equals N-k-1, where N is
#the number of observations and k is the number of predictors. In this case, N-7-1=185,
#which means the required sample size is N = 185 + 7 + 1 = 193.

#TEST OF PROPRORTIONS
#--------------------------------------------------------------------------------------------
#Let’s say that you suspect that a popular medication relieves symptoms in 60 percent
#of  users.  A  new  (and  more  expensive)  medication  will  be  marketed  if  it  improves
#symptoms in 65 percent of users. How many participants will you need to include in
#a study comparing these two medications if you want to detect a difference this large? 
#Assume that you want to be 90 percent confident in a conclusion that the new drug
#is better and 95 percent confident that you won’t reach this conclusion erroneously.
#You’ll use a one-tailed test because you’re only interested in assessing whether the new
#drug is better than the standard. 
#For unequal ns the desired function is 
#pwr.2p2n.test(h =, n1 =, n2 =, sig.level=, power=).

pwr.2p.test(h=ES.h(.65,.6),sig.level=.05,power=.90,alternative="greater")

#CHI SQUARE TEST
#--------------------------------------------------------------------------------------------
#  let’s  assume  that  you’re  looking  the  relationship  between
#ethnicity  and  promotion. You  anticipate  that  70  percent  of  your  sample  will  be
#Caucasian,  10  percent  will  be  African  American,  and  20  percent  will  be  Hispanic.
#Further, you believe that 60 percent of Caucasians tend to be promoted, compared
#with 30 percent for African Americans, and 50 percent for Hispanics
#For example, you expect that 42 percent of the population will be promoted Caucasians 
#(.42 = .70 ×.60) and 7 percent of the population will be nonpromoted African
#Americans  (.07  =  .10 × .70).  Let’s  assume  a  significance  level  of  0.05 
#and  the  desired power level is 0.90. The degrees of freedom in a two-way 
#contingency table are (r-1)*(c-1), where r is the number of rows and c is the
#number of columns. You can calculate the hypothesized effect size with 
#the following code:
prob<- matrix(c(.42,.28,.03,.07,.10,.10),byrow=TRUE,nrow=3)
ES.w2(prob)
#Using this information, you can calculate the necessary sample size like this:
pwr.chisq.test(w=.1853, df=2, sig.level=.05, power=.9)

#Sample sizes for detecting significant effects in a one-way ANOVA
#----------------------------------------------------------------------------------------------
library(pwr)
es <- seq(.1, .5, .01) 
nes <- length(es)
samsize <- NULL 
for (i in 1:nes){ 
        result <- pwr.anova.test(k=5, f=es[i], sig.level=.05, power=.9) 
        samsize[i] <- ceiling(result$n) 
} 
plot(samsize,es, type="l", lwd=2, col="red", 
     ylab="Effect Size", 
     xlab="Sample Size (per cell)", 
     main="One Way ANOVA with Power=.90 and Alpha=.05") 

#Sample size curves for detecting correlations of various sizes
#-------------------------------------------------------------------------------------
library(pwr)
r <- seq(.1,.5,.01)#correlation coefficient
nr <- length(r)
p <- seq(.4,.9,.1) #power
np <- length(p) 
samsize <- array(numeric(nr*np), dim=c(nr,np))
for (i in 1:np){
        for (j in 1:nr){
                result <- pwr.r.test(n = NULL, r = r[j],
                                     sig.level = .05, power = p[i],
                                     alternative = "two.sided")
                samsize[j,i] <- ceiling(result$n)
        }
}
xrange <- range(r)
yrange <- round(range(samsize))
colors <- rainbow(length(p))
plot(xrange, yrange, type="n",
     xlab="Correlation Coefficient (r)",
     ylab="Sample Size (n)" )
for (i in 1:np){
                lines(r, samsize[,i], type="l", lwd=2, col=colors[i])
}
abline(v=0, h=seq(0,yrange[2],50), lty=2, col="grey89")
abline(h=0, v=seq(xrange[1],xrange[2],.02), lty=2,
       col="gray89")
title("Sample Size Estimation for Correlation Studies\n Sig=0.05 (Two-tailed)")
legend("topright", title="Power", as.character(p),fill=colors)

#The pifacepackage provides a Java GUIfor sample-size methods
#that interfaces with R. The GUIallows the user to vary study parameters interactively
#and see their impact on other parameters
install.packages("piface", repos="http://R-Forge.R-project.org")
library(piface)
piface()