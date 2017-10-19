library(lme4)
library(dplyr)

load('cpPaper.RData')

#### what's the next section each student works?
secOrder <- data%>%filter(is.finite(status) & is.finite(timestamp) )%>%group_by(field_id,section,unit,Curriculum,overall,year,Year,Yr,state,classid2,schoolid2)%>%summarize(date=max(date,na.rm=TRUE),time=max(timestamp),status=max(status))%>%arrange(time)%>%group_by(field_id,year,Year,Yr,state,classid2,schoolid2)%>%mutate(status=as.character(status),prevSec=c(NA,section[-n()]),prevStatus=c(NA,status[-n()]),prevUnit=c(NA,unit[-n()]),nextUnit=c(unit[-1],NA))


secOrder$cp <- secOrder$status=='changed placement'
secOrder$mast <- secOrder$status=='graduated'

cpdStuds <- unique(data$field_id[!is.na(data$status) & data$status=='changed placement'])

mnDat <- filter(secOrder,prevStatus!='final_or_incomplete' &  field_id%in%cpdStuds)

mnDat$prevStatus <- factor(mnDat$prevStatus,levels=c('changed placement','promoted','graduated'))

### try with lm:
mnDat$secUnit <- paste(mnDat$section,mnDat$unit)

mnDat25 <- subset(mnDat,mnDat$secUnit%in%names(table(mnDat$secUnit))[table(mnDat$secUnit)>25])
mnDat50 <- subset(mnDat,mnDat$secUnit%in%names(table(mnDat$secUnit))[table(mnDat$secUnit)>50])
mnDat100 <- subset(mnDat,mnDat$secUnit%in%names(table(mnDat$secUnit))[table(mnDat$secUnit)>100])

mn1ols25 <- lm(mast~prevStatus+as.factor(field_id)+as.factor(secUnit),data=mnDat25,subset=year==1)
mn1ols50 <- lm(mast~prevStatus+as.factor(field_id)+as.factor(secUnit),data=mnDat50,subset=year==1)
mn1ols100 <- lm(mast~prevStatus+as.factor(field_id)+as.factor(secUnit),data=mnDat100,subset=year==1)

print(head(coef(mn1ols25)))
print(head(coef(mn1ols50)))
print(head(coef(mn1ols100)))

mn2ols25 <- lm(mast~prevStatus+as.factor(field_id)+as.factor(secUnit),data=mnDat25,subset=year==2)


library(sandwich)
mn1vcov <- vcovHC(mn1ols25,'HC')
mn2vcov <- vcovHC(mn2ols25,'HC')


### what if remove cp?
mn1olsNoCP <- update(mn1ols25,subset=status!='changed placement')
mn2olsNoCP <- update(mn2ols25,subset=status!='changed placement')

mn1vcovNoCP <- vcovHC(mn1olsNoCP,'HC')
mn2vcovNoCP <- vcovHC(mn2olsNoCP,'HC')

save(mn1ols25,mn2ols25,mn1vcov,mn2vcov,mn1olsNoCP,mn2olsNoCP,mn1vcovNoCP,mn2vocvNoCP,file='mastNextOLS.RData')

mn1ols <- lm(mast~prevStatus+as.factor(field_id)+as.factor(secUnit),data=mnDat,subset=year==1)
mn2ols <- lm(mast~prevStatus+as.factor(field_id)+as.factor(secUnit),data=mnDat,subset=year==2)



mn1 <- glm(mast~prevStatus+as.factor(field_id)+as.factor(secUnit),data=mnDat,family=binomial,subset=year==1)




mn2 <- glm(mast~prevStatus+as.factor(field_id)+as.factor(secUnit),data=mnDat,family=binomial,subset=year==2)

mastNext <- glmer(mast~(1|field_id/classid2)+(1|section/unit)+as.factor(prevStatus),data=mnDat,family=binomial)

mastNext2 <- update(mastNext,data=subset(mnDat,status!='changed placement' & status!='final_or_incomplete'))

mnDat$month <- months(mnDat$date)

mastNext3 <- update(mastNext,.~.+Year+month+schoolid2)
