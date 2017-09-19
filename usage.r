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
    filter(!(state=='MI'&overall=='Customized'&year==1))%>%
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

timePerSecGrad <- data%>%
    filter(!(state=='MI'&overall=='Customized'&year==1) & status=='graduated')%>%
    group_by(field_id,year,state,section,overall)%>%
        summarize(time=sum(total_t1,na.rm=TRUE))%>%
            mutate(Year=c('Yr 1','Yr 2')[year],
                   Curriculum=factor(overall,levels=c('Standard','Customized')),
                   time=time/60000)

timePerSecGrad <- within(timePerSecGrad,{
                             time[time==0]=NA
                             time[!is.finite(time)]=NA})



print(daysSec <- ggplot(daysTimePerSec,aes(x=Year,y=days,fill=Curriculum))+geom_boxplot(outlier.shape=NA)+facet_grid(~state)+ylim(0,30))

print(ndaySec <- ggplot(daysTimePerSec,aes(x=Year,y=ndays,fill=Curriculum))+geom_boxplot(outlier.shape=NA)+facet_grid(~state)+ylim(0,7))

print(timeSec <- ggplot(daysTimePerSec,aes(x=Year,y=time,fill=Curriculum))+geom_boxplot(outlier.shape=NA)+facet_grid(~state)+ylim(0,200)+labs(y='Minutes Per Section',xlab=NULL))

ggsave('timePerSec.pdf')

print(timeSec <- ggplot(daysTimePerSec,aes(x=Year,y=time))+geom_boxplot(outlier.shape=NA)+facet_grid(~state)+ylim(0,200)+labs(y='Minutes Per Section',x=NULL))

print(timeSecGrad <- ggplot(timePerSecGrad,aes(x=Year,y=time,fill=Curriculum))+geom_boxplot(outlier.shape=NA)+facet_grid(~state)+ylim(0,200)+labs(ylab='Minutes Per Section',xlab=NULL))
ggsave('timePerSecGrad.pdf')

