---
  title: "ES207_HW4"
author: "Anshika Kandhway"
date: "2/17/2020"
output: html_document
---
  
#Helsel and Hirsh Textbook
  
  #Q3.1 Compute both nonparametric and parametric 95% interval estimates for the median of the granodiorite data of exercise 2.3. Which is more appropriate for these data? Why?
  
  #a) Nonparametric intervals
  
#Chloride concentration, in mg/L


#Granodiorite
gran <- c(6.0, 0.5, 0.4, 0.7, 0.8, 6.0, 5.0, 0.6, 1.2, 
          0.3, 0.2, 0.5, 0.5, 10, 0.2, 0.2, 1.7, 3.0)

x <- sort(gran)
x



#From Helshel and Hirsh textbook, when sample size (n) is less that 20 then non-parametric intervals are calculated using following:
#Lower interval, lo = x`+1 and 
#upper interval, hi= n-x`
#From Appendix table B5 (of H&H textbook) and Quantile, q= 50% and CI=95% Table A.1 (of Performance Evaluation Of Computer And Communication Systems by Jean-Yves Le Boudec), we have lower and upper values at 5th and 14th position respectively
#Here n=18
lo_np= x[c(5)]
hi_np= x[c(14)]
print(paste("Nonparametric lower interval is", lo_np))
print(paste("Nonparametric upper interval is", hi_np))



#b) Parametric intervals


y <- log(gran)
y



mean_y <- mean(y)
gm <- exp(mean_y)
gm



lo_par = exp(mean_y - (qt(0.975, df=18-1)* sqrt(var(y)/18)))
lo_par
hi_par = exp(mean_y + (qt(0.975, df=18-1)* sqrt(var(y)/18)))
hi_par



print(paste("Parametric upper interval is", lo_par))
print(paste("Parametric upper interval is", hi_par))


hist(gran)


#Nonparmetric is preferable because data is small and non-normal (histogram). Also, for this the interval is wider than parametric interval which means there are more chances that median will be present in that interval.


#Q3.4 Construct the most appropriate 95 percent interval estimates for the mean and median annual streamflows for the Conecuh River at Brantley, Alabama (data in Appendix C2).


library(tidyverse)
#load Conecuh River data
crdata <- read_csv("Conecuh_River_apxc2.csv", col_names =TRUE, cols(
  Year = col_double(),
  `Flow (cfs)` = col_double()
))
crdata



hist(crdata$`Flow (cfs)`)


#Since, dataset is small and non-normal, nonparametric interval will be calculated.


#Interval esrimates for mean

lo_flow <- mean(crdata$`Flow (cfs)`) - 2*(sd(crdata$`Flow (cfs)`))
up_flow <- mean(crdata$`Flow (cfs)`) + 2*(sd(crdata$`Flow (cfs)`))




CI <-  predict(lm(crdata$`Flow (cfs)`~1),
               interval = "confidence",
               level = 0.95)
CI[1,]

print(paste("Interval for mean is 682.75 +/- 126.137"))


#Interval esrimates for median

#From Appendix table B5 (of H&H textbook) and Quantile, q= 50% and CI=95% Table A.1 (of Performance Evaluation Of Computer And Communication Systems by Jean-Yves Le Boudec)
#we have lower and upper values at 6th and 15th position respectively
sflow <- sort(crdata$`Flow (cfs)`)
med_sflow <- median(sflow)
lo_flowmd= sflow[c(5)]
hi_flowmd= sflow[c(14)]
print(paste("Median is", med_sflow))
print(paste("Nonparametric lower interval is", lo_flowmd))
print(paste("Nonparametric upper interval is", hi_flowmd))

