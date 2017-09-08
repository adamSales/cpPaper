library(dplyr)
library(ggplot2)

data <- read.csv("totFINAL2.csv")
###NOTES: 'overall' field is either Standard or Customized
data$overall=as.factor(data$overall)

dataA <- data
####################################### START HERE #######################
####################################### FIGURE 1 #########################

### boxplots of number of sections in customized and reg curriculua by state/year
## secLevel <- aggregate(data,by=list(state=data$state,curriculum=data$curriculum,field_id=data$field_id,year=data$year,section=data$section,overall=data$overall),FUN=length)
## secByStud <- aggregate(subset(secLevel,select=c(state,year,section,field_id,overall,)),
##  by=list(secLevel$field_id),FUN=length)
source('dataMerge.r')

secByStud <- summarize(group_by(data,state,year,field_id,year),numSec=n_distinct(section,na.rm=TRUE))
secByStud$numSec[secByStud$numSec==0] <- NA

secByStud$yearFac <- c('Yr 1','Yr 2')[secByStud$year]


secStateYear <- ggplot(secByStud,aes(yearFac,numSec))+geom_boxplot()+facet_grid(~state)+ylim(0,250)+labs(x='',y='Number of Sections Per Student')
ggsave('fig1.jpg',width=6,height=3)

#secStateYearCurr <- ggplot(secByStud,aes(yearFac,numSec,fill=Curriculum))+geom_boxplot()+facet_grid(~state)+ylim(0,250)+labs(x='',y='Number of Sections Per Student')
#ggsave('fig1a.jpg',width=6,height=3)


cc <- data %>% group_by(state,year,field_id,section) %>% summarize(curr=mean(overall=='Customized',na.rm=TRUE))
perCCdat <- cc %>% filter(state%in%c('KY','TX','MI')) %>% group_by(state,year)%>% summarize(perCC=mean(curr,na.rm=TRUE))

#perCCdat <- data%>% filter(state%in%c('KY','TX','MI')) %>% group_by(state,year,section,overall) %>% mutate(perCC= mean(overall=='Customized'))

perCCdat$yearFac <- ifelse(perCCdat$year==1,'Yr 1','Yr 2')
percCC <- ggplot(perCCdat,aes(yearFac,perCC))+geom_col()+facet_grid(~state)+labs(x='',y='% Sections from Customized Curriculua')
ggsave('percentCC.jpg',width=6,height=3)

sectionLevelDat <- data%>% group_by(field_id,unit,section) %>% summarize(state=state[1],schoolidn=schoolidn[1],grdlvl=grdlvl[1],grade=grade[1],teachid=teachid[1],classid=classid[1],customized=mean(overall=='Customized',na.rm=TRUE),dateBegin=min(date,na.rm=TRUE),dateEnd=max(date,na.rm=TRUE),time=sum(total_t1,na.rm=TRUE),status=status[1],ndays=n_distinct(date,na.rm=TRUE),year=year[1],nprob=n_distinct(Prob1,na.rm=TRUE))

studLevel <- sectionLevelDat%>%group_by(field_id)%>%summarize(state=state[1],schoolidn=schoolidn[1],grdlvl=grdlvl[1],grade=grade[1],teachid=teachid[1],classid=classid[1],customized=mean(customized,na.rm=TRUE),dateBegin=min(dateBegin,na.rm=TRUE),dateEnd=max(dateEnd,na.rm=TRUE),time=sum(time,na.rm=TRUE),nsec=n_distinct(section,na.rm=TRUE),grad=sum(status=='graduated',na.rm=TRUE),cp=sum(status=='changed placement',na.rm=TRUE),prom=sum(status=='promoted',na.rm=TRUE),ndays=sum(ndays,na.rm=TRUE),year=year[1],nprob=sum(nprob,na.rm=TRUE))

sectionLevelDat <- within(sectionLevelDat,{
                          nprob[nprob==0] <- NA
                          time[time==0] <- NA
                          ndays[ndays==0] <- NA
                      })

### mastered sections by state/year
mastSec <- sectionLevelDat%>%group_by(state,year,field_id,year)%>%summarize(nGrad=mean(status=='graduated',na.rm=TRUE)*n())
mastSec$yearFac <- c('Yr 1','Yr 2')[mastSec$year]

mastStateYear <- ggplot(mastSec,aes(yearFac,nGrad))+geom_boxplot()+facet_grid(~state)+labs(x='',y='# Sections Mastered Per Student')+ylim(0,200)
ggsave('fig3.jpg',width=6,height=3)

### which units were worked, by year
sectionLevelDat$unit[grep('unit-conversions',sectionLevelDat$unit)] <- 'unit-conversions'
unitLevel <- sectionLevelDat%>%group_by(field_id,unit)%>%summarize(state=state[1],schoolidn=schoolidn[1],grdlvl=grdlvl[1],grade=grade[1],teachid=teachid[1],classid=classid[1],customized=mean(customized,na.rm=TRUE),dateBegin=min(dateBegin,na.rm=TRUE),dateEnd=max(dateEnd,na.rm=TRUE),time=sum(time,na.rm=TRUE),cp=mean(status=='changed placement',na.rm=TRUE),grad=mean(status=='graduated',na.rm=TRUE),prom=mean(status=='promoted',na.rm=TRUE),finc=mean(status=='final_or_incomplete',na.rm=TRUE),ndays=sum(ndays,na.rm=TRUE),year=year[1],nprob=sum(nprob,na.rm=TRUE))

curricula <- read.csv('~/Box Sync/CT/data/sectionLevelUsageData/RAND_study_curricula.csv',stringsAsFactors=FALSE)
curricula <- subset(curricula,curriculum_name=='algebra i')
curricula$unit <- tolower(curricula$unit)
curricula <- subset(curricula,unit%in%intersect(curricula$unit[curricula$ct=='2007'],curricula$unit[curricula$ct=='2008r1']))
units <- curricula$unit[curricula$ct=='2007']



workedUnits <- table(unitLevel$unit,unitLevel$year)[units,]
workedUnits <- data.frame(numWorked=c(workedUnits[,1],workedUnits[,2]),year=c(rep('Year 1',length(units)),rep('Year 2',length(units))),
                          Unit=factor(rep(units,2),units))
workedUnits$meanWorked <- workedUnits$numWorked/ifelse(workedUnits$year=='Year 1',
                                                       n_distinct(unitLevel$field_id[unitLevel$year==1 & !is.na(unitLevel$unit)]),
                                                       n_distinct(unitLevel$field_id[unitLevel$year==2 & !is.na(unitLevel$unit)]))


unitsWorked <- ggplot(workedUnits,aes(x=Unit,y=meanWorked,color=year,group=year))+geom_point()+geom_line()+theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5))

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


### problems per unit by year
n1 <- n_distinct(data$field_id[data$year==1 & !is.na(data$Prob1)])
n2 <- n_distinct(data$field_id[data$year==2 & !is.na(data$Prob1)])

data$unit[grep('unit-conversions-',data$unit)] <- 'unit-conversions'
probsByUnit <- data%>% filter(!is.na(Prob1)) %>% group_by(unit,year)%>%summarize(nprobU=n())
probsByUnit <- mutate(probsByUnit, meanProbU=nprobU/ifelse(year==1,n1,n2))

probsByUnit$year <- factor(probsByUnit$year)
probsByUnit2 <- probsByUnit%>%filter(unit%in%units)%>%mutate(Unit=factor(unit,levels=units))

ggplot(probsByUnit2,aes(x=Unit,y=meanProbU,group=year,color=year))+geom_point()+geom_line()+theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5))

ggplot(probsByUnit,aes(x=unit,y=meanProbU,group=year,color=year))+geom_point()+geom_line()+theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5))

units1 <- unique(probsByUnit$unit[probsByUnit$meanProbU>1])

probsByUnit3 <- probsByUnit%>%filter(unit%in%units1)#%>%mutate(Unit=factor(unit,levels=c(unit[year==1][order(meanProbU[year==1],decreasing=TRUE)],setdiff(unit[year==2],unit[year==1]))))
probsByUnit3$Unit <- factor(probsByUnit3$unit,levels=with(probsByUnit3,c(unit[year==1][order(meanProbU[year==1],decreasing=TRUE)],setdiff(unit[year==2],unit[year==1]))))
ggplot(probsByUnit3,aes(x=Unit,y=meanProbU,group=year,color=year))+geom_point()+geom_line()+theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5))

probsByUnit4 <- probsByUnit3
probsByUnit4 <- within(probsByUnit4,Unit[Unit%in%c("unit-conversions-one-step","unit-conversions-mult-step")] <- 'unit-conversions')

ggplot(probsByUnit4,aes(x=Unit,y=meanProbU,group=year,color=year))+geom_point()+geom_line()+theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5))


posttest <- read.csv('~/Box Sync/CT/data/RANDstudyData/posttest_item_scores_by_fieldid.csv')
posttest[is.na(posttest)] <- 0
posttest <- posttest[order(posttest$YEAR),]
posttest <- posttest[!duplicated(posttest$field_id),]
data <- left_join(data,posttest)

nprob2 <- data%>%filter(!is.na(Prob1))%>%group_by(field_id)%>%summarize(year=year[1],nprob2=sum(unit%in%c("quadratics-factoring_es", "quadeqnswithfactoring", "polynomial-multiplying-factoring", "quadratics-solving_es")),prob2=posttest_a2[1],xirt=xirt[1])



### days per section
daysTimePerSec <- data%>%
    filter(!(state=='MI'&overall=='Customized'))%>%
    group_by(field_id,year,state,section,overall)%>%
        summarize(days=max(date,na.rm=TRUE)-min(date,na.rm=TRUE),
                  ndays=n_distinct(date,na.rm=TRUE),
                  time=sum(total_t1,na.rm=TRUE))%>%
            mutate(Year=c('Yr 1','Yr 2')[year],
                   Curriculum=factor(overall,levels=c('Standard','Customized')),
                   time=time/60000)


daysTimePerSec <- within(daysTimePerSec,{
                             time[time==0]=NA
                             days[days==0]=NA
                             ndays[ndays==0]=NA
                             time[!is.finite(time)]=NA})
            ## group_by(field_id,year,state,overall)%>%
            ##     summarize(ndays=mean(ndays,na.rm=TRUE),
            ##               days=mean(days,na.rm=TRUE),
            ##               time=mean(time,na.rm=TRUE))





print(daysSec <- ggplot(daysTimePerSec,aes(x=Year,y=days,fill=Curriculum))+geom_boxplot(outlier.shape=NA)+facet_grid(~state)+ylim(0,30))

print(ndaySec <- ggplot(daysTimePerSec,aes(x=Year,y=ndays,fill=Curriculum))+geom_boxplot(outlier.shape=NA)+facet_grid(~state)+ylim(0,7))

print(timeSec <- ggplot(daysTimePerSec,aes(x=Year,y=time,fill=Curriculum))+geom_boxplot(outlier.shape=NA)+facet_grid(~state)+ylim(0,200)+labs(ylab='Minutes Per Section',xlab=NULL))


