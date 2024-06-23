Data_HR=read.csv("D:\\MSc(ASA)\\Sem 2\\DDA\\HR-Employee-Attrition.csv")
View(Data_HR)
summary(Data_HR)
install.packages("vtable")
vtable::st(Data_HR)

vtable::sumtable(Data_HR)
vtable::vtable(Data_HR ,out='return')

#I can easily \input this into my LaTeX doc:
vtable::vt(Data_HR,out='latex',file='mytable1.tex')



scaled_data=Data_HR
par(mfrow=c(2,2))
hist(Data_HR$YearsSinceLastPromotion,main = paste("Histogram of Years Since Last Promotion"), xlab ="Years Since Last Promotion")
hist(Data_HR$YearsAtCompany,main = paste("Histogram of Years At Company"), xlab ="Years At Company")
hist(Data_HR$YearsInCurrentRole,main = paste("Histogram of Years In Current Role"), xlab ="Years In Current Role")
hist(Data_HR$YearsWithCurrManager,main = paste("Histogram of Years With Current Manager"), xlab ="Years With Current Manager")


#fitting the parametric Distributions 
gof=function(x){
  exp=fitdist(x,"exp")
  weibull=fitdist(x,"weibull",start = list(shape = 2, scale = 10),method = "mge")
  gamma=fitdist(x,"gamma",method = "mge")
  log_normal=fitdist(x,"lnorm",start = list(meanlog = 4, sdlog = 13),method = "mge")
  return(gofstat(list(exp,weibull,gamma,log_normal)))
}

#applying the function on the Time random variable
setwd("C:\\Users\\VEDSD\\Desktop\\Report Sem 2 Project")
sink('Goodness of Fit.txt')
lapply(Data_HR[,28:31], gof)

sink()

#Now K-M graph and Estimates
library(survival)
library(survminer)
library(survMisc)

KM_est=function(x){
  Dist=surv_fit(Surv(x,Attrition)~1,data=scaled_data)
  x=ggsurvplot(Dist,data=scaled_data)
  y=Dist$surv
  list1=list(x,y)
  return(list1)
  
}

lapply(Data_HR[,28:31], KM_est)




#For Years at company
Surv_obj=Surv(scaled_data$YearsAtCompany,as.numeric(scaled_data$Attrition))
Fit_dist=surv_fit(Surv_obj~1,data = scaled_data)
surv=data.frame("time"=Fit_dist$time,
                "survival function" = Fit_dist$surv,
                "hazard"=-log(Fit_dist$surv))

ggsurvplot(Fit_dist,data = scaled_data, main="KM survival Graph")
ggsur
View(surv)
install.packages("knitr")
knitr::kable(surv,"latex")


Reg_YearsAtCompany=coxph(Surv_obj~.,data=scaled_data[,-c(2,14,17:20,23:37,44,43)])
sink("Cox_Regression.txt")
summary(Reg_YearsAtCompany)
sink()

sink("Step_cox_Regression.txt")
step_Reg_cox=step(Reg_YearsAtCompany, direction = "both")
sink()

Cox=function(x){
  Dist=Surv(x,scaled_data$Attrition)
  Regression=coxph(Dist~.,data = scaled_data[,-c(2,14,17:20,23:37,44,43)])
  return(Regression)

}

lapply(Data_HR[,28:31], Cox)



sink("Reduced Model.txt")
summary(step_Reg_cox)
sink()
#For YearsInCurrentRole
scaled_data$Cen_YearsInCurrentRole=1
Surv_obj1=Surv(scaled_data$YearsInCurrentRole,scaled_data$Cen_YearsInCurrentRole)
Fit_dist1=surv_fit(Surv_obj1~1,data = scaled_data)
surv1=data.frame("time"=Fit_dist1$time,
                "survival function" = Fit_dist1$surv,
                "hazard"=-log(Fit_dist1$surv))

ggsurvplot(Fit_dist1,data = scaled_data)
summary(Fit_dist1)

Reg_YearsInCurrentRole=coxph(Surv_obj1~.,data=scaled_data[,-c(2,14,17:20,23:37)])
summary(Reg_YearsInCurrentRole)

step_Reg_cox1=step(Reg_YearsInCurrentRole,direction = "both")
AIC(Reg_YearsInCurrentRole)
AIC(step_Reg_cox1)


#For YearsWithCurrManager

Surv_obj2=Surv(scaled_data$YearsWithCurrManager,scaled_data$Cen_YearsWithCurrManager)
Fit_dist2=surv_fit(Surv_obj2~1,data = scaled_data)
surv2=data.frame("time"=Fit_dist2$time,
                 "survival function" = Fit_dist2$surv,
                 "hazard"=-log(Fit_dist2$surv))

ggsurvplot(Fit_dist2,data = scaled_data)
summary(Fit_dist1)

Reg_YearsWithCurrManager=coxph(Surv_obj2~.,data=scaled_data[,-c(2,14,17:20,23:37)])
step_Reg_cox2=step(Reg_YearsWithCurrManager,direction = "both")
AIC(Reg_YearsWithCurrManager)
AIC(step_Reg_cox2)


#for TotalWorkingYears

scaled_data$Cen_TotalWorkingYears=1
Surv_obj3=Surv(scaled_data$TotalWorkingYears,scaled_data$Cen_TotalWorkingYears)
Fit_dist3=surv_fit(Surv_obj3~1,data = scaled_data)
surv3=data.frame("time"=Fit_dist3$time,
                 "survival function" = Fit_dist3$surv,
                 "hazard"=-log(Fit_dist3$surv))

ggsurvplot(Fit_dist3,data = scaled_data)
summary(Fit_dist3)

Reg_TotalWorkingYears=coxph(Surv_obj3~.,data=scaled_data[,-c(2,14,17:20,23:37)])
step_Reg_cox3=step(Reg_TotalWorkingYears,direction = "both")
AIC(Reg_TotalWorkingYears)
AIC(step_Reg_cox3)

ggsurvplot(Fit_dist3)
summary(Reg_TotalWorkingYears)
