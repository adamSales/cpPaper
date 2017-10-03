library(dplyr)
library(lme4)
library(splines)
load('cpPaper.RData')



cDat <- data%>%filter(unit%in%unique(data$unit[data$curriculum=='Algebra I']))%>%group_by(field_id,unit,section,classid2,state)%>%summarize(status=max(status,na.rm=TRUE),startDate=min(date,na.rm=TRUE))
cpDat <- merge(cpDat,stud,all.x=TRUE,all.y=FALSE)

cpDat$month <- months(cpDat$date,TRUE)

### take out classrooms, units, with no CPs
cpDat <- subset(cpDat,unit %in% unique(cpDat$unit[cpDat$status=='changed placement']))
cpDat <- subset(cpDat, classid2 %in% unique(cpDat$classid2[cpDat$status=='changed placement']))
cpDat <- subset(cpDat,unit %in% unique(cpDat$unit[cpDat$status=='changed placement']))
cpDat <- subset(cpDat, classid2 %in% unique(cpDat$classid2[cpDat$status=='changed placement']))

cpDat$late <- cpDat$month%in%c('Jan','Feb','March','April')

cpModDat$cp <- cpModDat$status=='changed placement'

cpMod <- glmer(cp~Year*(ns(xirt,5)+late)+spec_gifted+spec_speced+spec_esl+state+(ns(xirt,5)+late|classid2)+(1|field_id)+(late|unit),data=cpDat,family=binomial)


### does failing to master one section increase the odds of failing to master the next?
