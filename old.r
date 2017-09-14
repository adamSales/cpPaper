sectionLevelDat <- data%>% group_by(field_id,unit,section) %>% summarize(state=state[1],schoolidn=schoolidn[1],grdlvl=grdlvl[1],grade=grade[1],teachid=teachid[1],classid=classid[1],customized=mean(overall=='Customized',na.rm=TRUE),dateBegin=min(date,na.rm=TRUE),dateEnd=max(date,na.rm=TRUE),time=sum(total_t1,na.rm=TRUE),status=status[1],ndays=n_distinct(date,na.rm=TRUE),year=year[1],nprob=n_distinct(Prob1,na.rm=TRUE))

studLevel <- sectionLevelDat%>%group_by(field_id)%>%summarize(state=state[1],schoolidn=schoolidn[1],grdlvl=grdlvl[1],grade=grade[1],teachid=teachid[1],classid=classid[1],customized=mean(customized,na.rm=TRUE),dateBegin=min(dateBegin,na.rm=TRUE),dateEnd=max(dateEnd,na.rm=TRUE),time=sum(time,na.rm=TRUE),nsec=n_distinct(section,na.rm=TRUE),grad=sum(status=='graduated',na.rm=TRUE),cp=sum(status=='changed placement',na.rm=TRUE),prom=sum(status=='promoted',na.rm=TRUE),ndays=sum(ndays,na.rm=TRUE),year=year[1],nprob=sum(nprob,na.rm=TRUE))

studLevel <- within(studLevel,{
                        Year <- ifelse(year==1,'Yr 1','Yr 2')
                        nprob[nprob==0] <- NA
                          time[time==0] <- NA
                          ndays[ndays==0] <- NA
                      })

sectionLevelDat <- within(sectionLevelDat,{
                          nprob[nprob==0] <- NA
                          time[time==0] <- NA
                          ndays[ndays==0] <- NA
                      })
sectionLevelDat$unit[grep('unit-conversions',sectionLevelDat$unit)] <- 'unit-conversions'
unitLevel <- sectionLevelDat%>%group_by(field_id,unit)%>%summarize(state=state[1],schoolidn=schoolidn[1],grdlvl=grdlvl[1],grade=grade[1],teachid=teachid[1],classid=classid[1],customized=mean(customized,na.rm=TRUE),dateBegin=min(dateBegin,na.rm=TRUE),dateEnd=max(dateEnd,na.rm=TRUE),time=sum(time,na.rm=TRUE),cp=mean(status=='changed placement',na.rm=TRUE),grad=mean(status=='graduated',na.rm=TRUE),prom=mean(status=='promoted',na.rm=TRUE),finc=mean(status=='final_or_incomplete',na.rm=TRUE),ndays=sum(ndays,na.rm=TRUE),year=year[1],nprob=sum(nprob,na.rm=TRUE))


workedUnits <- table(unitLevel$unit,unitLevel$year)[units,]
workedUnits <- data.frame(numWorked=c(workedUnits[,1],workedUnits[,2]),year=c(rep('Year 1',length(units)),rep('Year 2',length(units))),
                          Unit=factor(rep(units,2),units))
workedUnits$meanWorked <- workedUnits$numWorked/ifelse(workedUnits$year=='Year 1',
                                                       n_distinct(unitLevel$field_id[unitLevel$year==1 & !is.na(unitLevel$unit)]),
                                                       n_distinct(unitLevel$field_id[unitLevel$year==2 & !is.na(unitLevel$unit)]))



## masteredUnits <- with(subset(unitLevel,grad==1),table(unit,year))[units,]
## masteredUnits <- data.frame(numMastered=c(masteredUnits[,1],masteredUnits[,2]),year=c(rep('Year 1',length(units)),rep('Year 2',length(units))),
##                           Unit=factor(rep(units,2),units))
## masteredUnits$meanMastered <- masteredUnits$numMastered/ifelse(masteredUnits$year=='Year 1',
##                                                        n_distinct(unitLevel$field_id[unitLevel$year==1 & !is.na(unitLevel$unit)]),
##                                                        n_distinct(unitLevel$field_id[unitLevel$year==2 & !is.na(unitLevel$unit)]))

## ggplot(masteredUnits,aes(x=Unit,y=meanMastered,color=year,group=year))+geom_point()+geom_line()+theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5))

### just to try it in a different way...
## use1 <- rbind(x[x$field_id%in%unique(data$field_id[data$year==1]),c('field_id','unit')],
##               advance[advance$field_id%in%unique(data$field_id[data$year==1]),c('field_id','unit')])
## use1 <- use1[!duplicated(use1),]
## use1$year <- 1

## use1S <- split(use1,use1$field_id)
## means1 <- sapply(units,function(unit) mean(sapply(use1S,function(sss) is.element(unit,sss$unit))))

## use2 <- rbind(x[x$field_id%in%unique(data$field_id[data$year==2]),c('field_id','unit')],
##               advance[advance$field_id%in%unique(data$field_id[data$year==2]),c('field_id','unit')])
## use2 <- use2[!duplicated(use2),]
## use2$year <- 2

## use2S <- split(use2,use2$field_id)
## means2 <- sapply(units,function(unit) mean(sapply(use2S,function(sss) is.element(unit,sss$unit))))

## workedUnits$mean2 <- c(means1,means2)
#### OK it works (except for "unit conversions" which I messed around with)
