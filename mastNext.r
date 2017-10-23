library(lme4)
library(dplyr)
library(lubridate)
library(sandwich)

load('cpPaper.RData')

#### what's the next section each student works?
secOrder <- data%>%filter(is.finite(status) & is.finite(timestamp) )%>%group_by(field_id,section,unit,Curriculum,overall,year,Year,Yr,state,classid2,schoolid2)%>%summarize(date=max(date,na.rm=TRUE),time=max(timestamp),status=max(status))%>%arrange(time)%>%group_by(field_id,year,Year,Yr,state,classid2,schoolid2)%>%mutate(status=as.character(status),prevSec=c(NA,section[-n()]),prevStatus=c(NA,status[-n()]),prevUnit=c(NA,unit[-n()]),nextUnit=c(unit[-1],NA))


secOrder$cp <- secOrder$status=='changed placement'
secOrder$mast <- secOrder$status=='graduated'
secOrder$month <- months(secOrder$date)

secOrder$prevCP <- secOrder$prevStatus=='changed placement'

cpdStuds <- unique(data$field_id[!is.na(data$status) & data$status=='changed placement'])

mnDat <- filter(secOrder,prevStatus!='final_or_incomplete' &  field_id%in%cpdStuds)

mnDat$prevStatus <- factor(mnDat$prevStatus,levels=c('changed placement','promoted','graduated'))

### try with lm:
mnDat$secUnit <- paste(mnDat$section,mnDat$unit)

mnDat25 <- subset(mnDat,mnDat$secUnit%in%names(table(mnDat$secUnit))[table(mnDat$secUnit)>25])
mnDat50 <- subset(mnDat,mnDat$secUnit%in%names(table(mnDat$secUnit))[table(mnDat$secUnit)>50])
mnDat100 <- subset(mnDat,mnDat$secUnit%in%names(table(mnDat$secUnit))[table(mnDat$secUnit)>100])

mn1ols25 <- lm(mast~prevCP+as.factor(field_id)+as.factor(secUnit),data=mnDat25,subset=year==1)
mn1ols50 <- lm(mast~prevCP+as.factor(field_id)+as.factor(secUnit),data=mnDat50,subset=year==1)
mn1ols100 <- lm(mast~prevCP+as.factor(field_id)+as.factor(secUnit),data=mnDat100,subset=year==1)

print(head(coef(mn1ols25)))
print(head(coef(mn1ols50)))
print(head(coef(mn1ols100)))

mn1ols25.1 <- update(mn1ols25,.~.+month)

mn2ols25 <- lm(mast~prevCP+as.factor(field_id)+as.factor(secUnit),data=mnDat25,subset=year==2)
mn2ols25.1 <- update(mn2ols25,.~.+month)

m1ols25Split <- lm(mast~as.factor(prevStatus)+as.factor(field_id)+as.factor(secUnit),data=mnDat25,subset=year==1)
m2ols25Split <- lm(mast~as.factor(prevStatus)+as.factor(field_id)+as.factor(secUnit),data=mnDat25,subset=year==2)
m1ols25Split.1 <- update(m1ols25Split,.~.+month)
m2ols25Split.1 <- update(m2ols25Split,.~.+month)

library(sandwich)
mn1vcov <- vcovHC(mn1ols25,'HC')
mn2vcov <- vcovHC(mn2ols25,'HC')
mn1vcov.1 <- vcovHC(mn1ols25.1,'HC')
mn2vcov.1 <- vcovHC(mn2ols25.1,'HC')
m1Splitvcov <- vcovHC(m1ols25Split,'HC')
m2Splitvcov <- vcovHC(m2ols25Split,'HC')
m1Splitvcov.1 <- vcovHC(m1ols25Split.1,'HC')
m2Splitvcov.1 <- vcovHC(m2ols25Split.1,'HC')




save(list=ls(),
     file='mastNextols.RData')
