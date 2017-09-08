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



load('~/Box Sync/CT/data/problemLevelUsageData/probLevelData.RData')
load('~/Box Sync/CT/data/sectionLevelUsageData/advanceDataWfinalinc.RData')


usageIDs <- unique(c(unique(x$field_id),unique(advance$field_id)))

hs1 <- read.csv('~/Box Sync/CT/data/RANDstudyData/H1_algebra_rcal_20121119_fieldid.csv')
hs2 <- read.csv('~/Box Sync/CT/data/RANDstudyData/H2_algebra_rcal_20121119_fieldid.csv')

stopifnot(all.equal(names(hs1)[1:100],names(hs2)[1:100]))

stud <- rbind(hs1[,1:100],hs2[!hs2$field_id%in%hs1$field_id,1:100])
stud$field_id <- c(hs1$field_id,hs2$field_id[!hs2$field_id%in%hs1$field_id])
stud <- stud[stud$treatment==1,]

stud$obsUsage <- stud$field_id%in%usageIDs

schoolMiss <- NULL
for(scl in unique(stud$schoolid2)){
 schoolMiss <- rbind(schoolMiss,with(stud,
  c(mean(obsUsage[year==1 & schoolid2==scl]),
    mean(obsUsage[year==2 & schoolid2==scl]))))}
rownames(schoolMiss) <- unique(stud$schoolid2)
schoolMiss <- cbind(schoolMiss,diff=schoolMiss[,1]-schoolMiss[,2])
smallDiff <- rownames(schoolMiss)[!is.na(schoolMiss[,'diff']) & abs(schoolMiss[,'diff'])<0.4 & schoolMiss[,1]>0& schoolMiss[,2]>0]

twice <- intersect(hs1$field_id,hs2$field_id)

totalTreatedStudents <- n_distinct(stud$field_id)

prob <- x[,-grep(2,names(x))]
prob <- prob[prob$field_id%in%stud$field_id,]
for(vv in names(prob)){
 if(is.factor(prob[[vv]])) prob[[vv]] <- as.character(prob[[vv]])
 if(is.character(prob[[vv]]))
  prob[[vv]] <- factor(tolower(prob[[vv]]))
}
prob$curriculum <- as.character(prob$curriculum)
prob$curriculum[grep('cc',prob$curriculum)] <- 'CC'
prob$curriculum <- sub('del_','',prob$curriculum)
prob$curriculum <- factor(prob$curriculum)

adv <- advance[advance$field_id%in%stud$field_id,]
for(vv in names(adv)) if(is.factor(adv[[vv]])) levels(adv[[vv]]) <- tolower(levels(adv[[vv]]))
adv$curriculum <- as.character(adv$curriculum)
adv$curriculum[grep('cc',adv$curriculum)] <- 'CC'
adv$curriculum <- sub('del_','',adv$curriculum)
adv$curriculum <- factor(adv$curriculum)

data <- full_join(prob,adv)

### take out year 2 data for students who were in the study both years
probDate <- sapply(strsplit(as.character(data$ts1),' '),`[`,1)
data$date <- as.Date(probDate,'%m/%d/%y')

remove <- with(data,ifelse(field_id%in%twice,
                         ifelse(is.na(date),
                                ifelse(is.na(year),
                                       study.year==2,year==2),
                                date>as.Date('2008-08-01')),FALSE))

data <- data[!remove,]

data$year <- data$study.year <- NULL

data <- full_join(data,stud)

data <- data[!grepl('test',data$section,ignore.case=TRUE),]

data$overall <- ifelse(data$curriculum=='CC',
                       'Customized','Standard')


##########################################################
### should we remove schools where usage missingness was very
### different between years 1 & 2??
data <- data[data$schoolid2%in%smallDiff,]
#############################################

levels(data$state) <- names(sort(table(stud$state[stud$schoolid2%in%smallDiff])))






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
