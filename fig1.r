library(scales)
library(reshape2)
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

## either:
source('dataMerge.r')

## or:
load('cpPaper.RData')


data$state <- data$State

#### how much problem-level missingness is there (compared to section-level dataset)?
smallDiffID <- unique(stud$field_id[stud$schoolid2%in%smallDiff])
probSecs <- prob%>%filter(field_id%in%smallDiffID & !field_id%in%twice)%>%distinct(field_id,unit,section)

advSecs <- adv%>%filter(field_id%in%smallDiffID & !field_id%in%twice)%>%distinct(field_id,unit,section)

AS <- apply(advSecs,1,paste,collapse='')
PS <- apply(probSecs,1,paste,collapse='')

nboth <- length(intersect(AS,PS))
nadv <- length(setdiff(AS,PS))
npr <- length(setdiff(PS,AS))

mean(AS%in%PS)
mean(PS%in%AS)

idsSecUn <- data%>%filter(field_id%in%setdiff(smallDiffID,twice))%>%group_by(field_id,section,unit)%>%summarize(probNA=all(is.na(Prob1)),advNA=all(is.na(status)))

idsSecUn%>%ungroup()%>%summarize(both=mean(!probNA & !advNA),neither=mean(probNA & advNA),advOnly=mean(probNA &!advNA),probOnly=mean(advNA & !probNA))

########################################
### # sections by state/year
#####################################


secByStud <- summarize(group_by(data,state,Yr,field_id),numSec=n_distinct(section,na.rm=TRUE),time=sum(total_t1,na.rm=TRUE),mastered=n_distinct(section[status=='graduated'],na.rm=TRUE))

secByStud <- within(secByStud,{
                        numSec[numSec==0] <- NA
                        time[time==0] <- NA
                        mastered[mastered==0] <- NA
                    })

print(secStateYear <- ggplot(secByStud,aes(Yr,numSec))+geom_boxplot()+facet_grid(~state)+coord_cartesian(ylim=c(0,250))+labs(x='',y='Number of Sections Per Student'))
ggsave('numSecStateYear.jpg',width=6,height=3)

secByStudCurr <- summarize(group_by(data,state,Yr,field_id,curriculum),numSec=n_distinct(section,na.rm=TRUE),time=sum(total_t1,na.rm=TRUE),mastered=n_distinct(section[status=='graduated'],na.rm=TRUE))

secByStudCurr <- within(secByStudCurr,{
                        numSec[numSec==0] <- NA
                        time[time==0] <- NA
                        mastered[mastered==0] <- NA
                    })

print(currStateYear <- ggplot(filter(secByStudCurr,!is.na(curriculum)),aes(Yr,numSec,fill=curriculum))+geom_boxplot())#+facet_grid(~state))

currPer <- secByStudCurr %>%filter(!is.na(curriculum))%>%group_by(Yr,curriculum,state)%>%summarize(numSec=sum(numSec,na.rm=TRUE))
currPer$perSec <- currPer$numSec/sapply(1:nrow(currPer),function(i) sum(currPer$numSec[currPer$state==currPer$state[i] & currPer$Yr==currPer$Yr[i]],na.rm=TRUE))
levels(currPer$curriculum) <- list(`>Algebra I`=c('Algebra II','Geometry'),`Algebra I`='Algebra I',`Bridge-to-Algebra`='Bridge-to-Algebra',
                                   Customized='Customized')

print(currPerPlot <- ggplot(currPer,aes(Yr,perSec,fill=curriculum))+geom_col()+facet_grid(~state)+labs(y='% of Sections Worked',x='',fill='Curriculum'))
ggsave('curricula.jpg',width=6,height=3)


### time on system by state/year
print(timeStateYear <- ggplot(secByStud,aes(Yr,time/3600000))+geom_boxplot(outlier.shape=NA)+facet_grid(~state)+coord_cartesian(ylim=c(0,100))+labs(x='',y='Time on CTAI Software per Student (Hours)'))
ggsave('timeStatYear.jpg',width=6,height=3)

### mastered sections by state/year
print(mastStateYear <- ggplot(secByStud,aes(Yr,mastered))+geom_boxplot()+facet_grid(~state)+labs(x='',y='# Sections Mastered Per Student')+coord_cartesian(ylim=c(0,250)))
ggsave('mastSecStateYear.jpg',width=6,height=3)


##################################3
### % of worked sections from customized vs standard curriculum
##################################

cc <- data %>% group_by(state,Year,field_id,section,Curriculum) %>% summarize(curr=mean(Curriculum=='Customized',na.rm=TRUE))%>% group_by(state,Year)%>% summarize(perCC=mean(curr,na.rm=TRUE))%>%filter(perCC>0.00000)

#perCCdat <- data%>% filter(state%in%c('KY','TX','MI')) %>% group_by(state,year,section,overall) %>% mutate(perCC= mean(overall=='Customized'))

#perCCdat$yearFac <- ifelse(perCCdat$year==1,'Yr 1','Yr 2')
print(percCC <- ggplot(cc,aes(Year,perCC))+geom_col()+facet_grid(~state)+labs(x='',y='% Sections from Customized Curriculua')+ylim(0,1))
ggsave('percentCC.jpg',width=6,height=3)



#############################################
### which units were worked, by year
#############################################3

curricula <- read.csv('~/Box Sync/CT/data/sectionLevelUsageData/RAND_study_curricula.csv',stringsAsFactors=FALSE)
curricula <- subset(curricula,curriculum_name=='algebra i')
curricula$unit <- tolower(curricula$unit)
curricula <- subset(curricula,unit%in%intersect(curricula$unit[curricula$ct=='2007'],curricula$unit[curricula$ct=='2008r1']))
units <- curricula$unit[curricula$ct=='2007']
sectionStats <- read.csv('~/Box Sync/CT/data/sectionLevelUsageData/section_stats_withAbb.csv',stringsAsFactors=FALSE)
UnitName <- sectionStats$unit_name_abb[match(units,sectionStats$unit_id)]
UnitName[units=='inequality-systems-solving'] <- 'Systems of Lin. Ineq.'
UnitName[units=='intro-pythag-theorem'] <- 'Pythagorean Theorem'
UnitName[units=='linear-inequality-graphing'] <- 'Graphs of Lin. Ineq.'
UnitName[units=='linear-systems-solving'] <- 'Systems of Lin. Eq.'
UnitName[units=='probability'] <- 'Probability'
UnitName[units=='unit-conversions'] <- 'Unit Conversions'

nstud <- data%>%filter(!is.na(unit))%>%group_by(Year)%>%summarize(nstud=n_distinct(field_id))
data$Unit <- data$unit
data$Unit[grep('unit-conversions',data$Unit)] <- 'unit-conversions'

unitLevel <- data%>%filter(Unit%in%units)%>%group_by(Unit,Year,state)%>%summarize(numWorked= n_distinct(field_id,na.rm=TRUE),numCP=sum(status=='changed placement',na.rm=TRUE),meanCP=mean(status=='changed placement',na.rm=TRUE))

unitLevel$perWorked <- unitLevel$numWorked/nstud$nstud[match(unitLevel$Year,nstud$Year)]

unitLevel$Unit <- factor(unitLevel$Unit,levels=units)
levels(unitLevel$Unit) <- UnitName

print(unitsWorked <- ggplot(unitLevel,aes(x=Unit,y=perWorked,color=Year,group=Year))+geom_point()+geom_line()+theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5))+labs(x='',y='% Worked')+theme(legend.position='top'))

print(unitCPs <- ggplot(filter(unitLevel,numWorked>100),aes(x=Unit,y=meanCP,color=Year,group=Year))+geom_point()+geom_line()+theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5))+labs(x='',y='% Reassigned')+theme(legend.position='top'))


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



##### CPs
statusPerSec <- data%>%
    filter(!is.na(Curriculum) &!(state=='MI'&Curriculum=='Customized'&year==1))%>%
    group_by(field_id,Yr,state,section,Curriculum,status)%>%
        summarize(cp=any(status=='changed placement'))%>%group_by(state,Curriculum,Yr,field_id)%>%summarise(pcp=mean(cp,na.rm=TRUE),ncp=sum(cp,na.rm=TRUE))

print(cpByStud <- ggplot(statusPerSec,aes(x=Yr,y=pcp,fill=Curriculum))+geom_boxplot()+facet_grid(~state))
ggsave('cpByStud.jpg',width=6,height=3)

### how many classmates graduated, promoted, CP'd same section before i?
## section-level data:

data$status <- ordered(data$status,c('final_or_incomplete','changed placement','promoted','graduated'))
secLev <- data%>%group_by(field_id,unit,section,Year,Yr,curriculum,classid2,schoolid2)%>%
    summarize(startDate=min(date,na.rm=TRUE),endDate=max(date,na.rm=TRUE),state=state[1],status=max(status,na.rm=TRUE))
secLev$status[!is.finite(secLev$status)] <- NA
secLev$endDate[!is.finite(secLev$endDate)] <- NA

countFun <- function(id,ed,cdat){
#    id <- cdat$field_id[i]
#    ed <- cdat$endDate[i]
    cdat <- subset(cdat,field_id!=id & endDate<ed)
    c(graduatedBefore=sum(cdat$status=='graduated',na.rm=TRUE),
      promotedBefore=sum(cdat$status=='promoted',na.rm=TRUE),
      cpBefore=sum(cdat$status=='changed placement',na.rm=TRUE))
}

secLevSplit <- split(secLev,with(secLev,list(unit,section,classid2)))
secLevCP <- subset(secLev,status=='changed placement')
cpOrd <- do.call('rbind',lapply(1:nrow(secLevCP),function(i) with(secLevCP,countFun(id=field_id[i],ed=endDate[i],cdat=secLevSplit[[paste(unit[i],section[i],classid2[i],sep='.')]]))))
save(cpOrd,file='cpOrd.RData')

cdatFun <- function(cdat){
    cdat$total <- nrow(cdat)
    if(all(!is.finite(cdat$endDate))){
        cdat$graduatedBefore <- NA
        cdat$promotedBefore <- NA
        cdat$cpBefore <- NA
        return(cdat)
    }
    cdat <- cbind(cdat,do.call('rbind',lapply(1:nrow(cdat),countFun,cdat=cdat)))
    cdat
}

cpOrd <- secLev%>%group_by(unit,section,Year,classid2)%>%do(cdatFun(.))
save(cpOrd,file='cpOrd.RData')

cpOrd$perc_graduatedBefore=cpOrd$graduatedBefore/(cpOrd$total-1)
cpOrd$perc_promotedBefore=cpOrd$promotedBefore/(cpOrd$total-1)
cpOrd$perc_changedplacementBefore=cpOrd$cpBefore/(cpOrd$total-1)
cpOrd$perc_totalBefore=cpOrd$perc_changedplacementBefore+cpOrd$perc_promotedBefore+cpOrd$perc_graduatedBefore

mean(cpOrd$perc_totalBefore,na.rm=TRUE)

#### overall by year
ordOverall <- cpOrd%>%group_by(Year)%>%summarize(Mastered=mean(perc_graduatedBefore,na.rm=TRUE),Promoted=mean(perc_promotedBefore,na.rm=TRUE),Reassigned=mean(perc_changedplacementBefore,na.rm=TRUE))%>%melt()

ggplot(ordOverall,aes(x=Year,y=value,fill=variable))+geom_bar(stat="identity")+scale_y_continuous(labels=percent)+labs(x='',y='% of Classmates',fill='Classmates\' \nSection Status')+coord_flip()+theme(legend.position='top')

cpOrd <- cpOrd%>%filter(unit %in% units)
cpOrd$unit <- factor(cpOrd$unit,levels=units)
levels(cpOrd$unit) <- UnitName

cpOrd1 <- subset(cpOrd,status=='changed placement' & Year=='Year 1')
cpOrd2 <- subset(cpOrd,status=='changed placement' & Year=='Year 2')

cpOrdUnit1 <- aggregate(cpOrd1[,c('perc_graduatedBefore','perc_promotedBefore','perc_changedplacementBefore')],by=list(unit=cpOrd1$unit),FUN=mean,na.rm=TRUE)
cpOrdUnit2 <- aggregate(cpOrd2[,c('perc_graduatedBefore','perc_promotedBefore','perc_changedplacementBefore')],by=list(unit=cpOrd2$unit),FUN=mean,na.rm=TRUE)

cpOrdUnit1 <- reshape2::melt(cpOrdUnit1)
cpOrdUnit2 <- reshape2::melt(cpOrdUnit2)

cpOrdUnit1$Year <- 'Year 1'
cpOrdUnit2$Year <- 'Year 2'
cpOrdUnit <- rbind(cpOrdUnit1,cpOrdUnit2)

levels(cpOrdUnit$variable) <- c('Mastered','Promoted','Reassigned')

ggplot(data=cpOrdUnit, aes(x=unit, y=value, fill=variable)) +
  geom_bar(stat="identity") +   theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5)) +
  labs(x='',y='% of Classmates',fill='Classmates\' \nSection Status')+facet_grid(Year~.)
ggsave('beforeCP.pdf',width=6.5,height=5)



#### cp % by class total
cpOrd1$ClassSize <- cpOrd1$total
cpOrd1$ClassSize[cpOrd1$ClassSize>=25] <- 25
cpOrd2$ClassSize <- cpOrd2$total
cpOrd2$ClassSize[cpOrd2$ClassSize>=25] <- 25

cpOrdTotal1 <- aggregate(cpOrd1[,c('perc_graduatedBefore','perc_promotedBefore','perc_changedplacementBefore')],by=list(total=cpOrd1$ClassSize),FUN=mean,na.rm=TRUE)
cpOrdTotal2 <- aggregate(cpOrd2[,c('perc_graduatedBefore','perc_promotedBefore','perc_changedplacementBefore')],by=list(total=cpOrd2$ClassSize),FUN=mean,na.rm=TRUE)

cpOrdTotal1 <- reshape2::melt(cpOrdTotal1,id='total')
cpOrdTotal2 <- reshape2::melt(cpOrdTotal2,id='total')

cpOrdTotal1$Year <- 'Year 1'
cpOrdTotal2$Year <- 'Year 2'
cpOrdTotal <- rbind(cpOrdTotal1,cpOrdTotal2)

levels(cpOrdTotal$variable) <- c('Mastered','Promoted','Reassigned')

ggplot(data=cpOrdTotal, aes(x=total, y=value, fill=variable)) +
  geom_bar(stat="identity") +   theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5)) +
  labs(x='Class Size (in Dataset)',y='% of Classmates',fill='Classmates\' Section Status:')+facet_grid(Year~.)+theme(legend.position='top')+
  scale_x_continuous(breaks=seq(0,25,5),labels=c(seq(0,20,5),'25+'))

ggsave('beforeCPsize.pdf',width=6.5,height=5)



### usage over time
nsecByDate <- secLev%>%filter(!is.na(section) & !is.na(endDate) & !(Yr=='Yr 1' & endDate>as.Date('06/30/2008',format='%m/%d/%Y')) & !(Yr=='Yr 2' & endDate<as.Date('07/01/2008',format='%m/%d/%Y')))%>%mutate(month=months(endDate,abb=TRUE))%>%group_by(field_id,Year,month)%>%summarize(nsec=n())%>%group_by(Year,month)%>%summarize(nsec=median(nsec,na.rm=TRUE))

cpPerByDate <- secLev%>%filter(!is.na(section) & !is.na(endDate) & !(Yr=='Yr 1' & endDate>as.Date('06/30/2008',format='%m/%d/%Y')) & !(Yr=='Yr 2' & endDate<as.Date('07/01/2008',format='%m/%d/%Y')))%>%mutate(month=months(endDate,abb=TRUE))%>%group_by(Year,month)%>%summarize(cpPer=mean(status=='changed placement',na.rm=TRUE))

nsecByDate$month <- factor(nsecByDate$month,levels=months(seq.Date(as.Date('2007/07/01'),as.Date('2008/06/01'),by='month'),TRUE))
cpPerByDate$month <- factor(cpPerByDate$month,levels=months(seq.Date(as.Date('2007/07/01'),as.Date('2008/06/01'),by='month'),TRUE))


ggplot(filter(nsecByDate,!month%in%c('Jul','Aug')),aes(x=month,y=nsec,color=Year,group=Year))+geom_point()+geom_line()+labs(x='',color='',y='Median # Sections Worked per Student')+theme(legend.position='top')
ggsave('numSecByMonth.pdf')
library(scales)
ggplot(filter(nsecByDate,!month%in%c('Jul','Aug')),aes(x=month,y=cpPer,color=Year,group=Year))+geom_point()+geom_line()+scale_y_continuous(labels = percent)+labs(x='',color='',y='% of Worked Sections Ending in Reassignment')+theme(legend.position='top')
ggsave('cpByMonth.pdf')




