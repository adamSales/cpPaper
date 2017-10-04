library(lubridate)
library(scales)
library(reshape2)
library(dplyr)
library(ggplot2)

## either:
source('dataMerge.r')

## or:
load('cpPaper.RData')


data$state <- data$State


##########################################################################
### missingness
##########################################################################
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
### # sections/time on system by state/year
#####################################


secByStud <- summarize(group_by(data,state,Yr,field_id),numSec=n_distinct(section,na.rm=TRUE),time=sum(total_t1,na.rm=TRUE),mastered=n_distinct(section[status=='graduated'],na.rm=TRUE))

secByStud <- within(secByStud,{
                        numSec[numSec==0] <- NA
                        time[time==0] <- NA
                        mastered[mastered==0] <- NA
                    })

print(secStateYear <- ggplot(secByStud,aes(Yr,numSec))+geom_boxplot()+facet_grid(~state)+coord_cartesian(ylim=c(0,250))+labs(x='',y='Number of Sections Per Student'))
ggsave('numSecStateYear.jpg',width=6,height=3)

### time on system by state/year
print(timeStateYear <- ggplot(secByStud,aes(Yr,time/3600000))+geom_boxplot(outlier.shape=NA)+facet_grid(~state)+coord_cartesian(ylim=c(0,100))+labs(x='',y='Time on CTAI Software per Student (Hours)'))
ggsave('timeStatYear.jpg',width=6,height=3)

### mastered sections by state/year
print(mastStateYear <- ggplot(secByStud,aes(Yr,mastered))+geom_boxplot()+facet_grid(~state)+labs(x='',y='# Sections Mastered Per Student')+coord_cartesian(ylim=c(0,250)))
ggsave('mastSecStateYear.jpg',width=6,height=3)

###### accounting for curriculum
secByStudCurr <- summarize(group_by(data,state,Yr,field_id,curriculum),numSec=n_distinct(section,na.rm=TRUE),time=sum(total_t1,na.rm=TRUE),mastered=n_distinct(section[status=='graduated'],na.rm=TRUE))

secByStudCurr <- within(secByStudCurr,{
                        numSec[numSec==0] <- NA
                        time[time==0] <- NA
                        mastered[mastered==0] <- NA
                    })

print(currStateYear <- ggplot(filter(secByStudCurr,!is.na(curriculum)),aes(Yr,numSec,fill=curriculum))+geom_boxplot()+facet_grid(~state))


#### what proportion of worked sections from each curriculum?
 currWorked <- data%>%distinct(field_id,section,unit,Curriculum,Year,State,overall)

currWorkedYr <- currWorked%>%filter(!is.na(Curriculum))%>%group_by(Curriculum,Year,overall)%>%summarize(n=n())
nByYr <- currWorkedYr%>%group_by(Year)%>%summarize(n=sum(n))
currWorkedYr <- within(currWorkedYr,p <- n/ifelse(Year=='Year 1',nByYr$n[nByYr$Year=='Year 1'],
                                                  nByYr$n[nByYr$Year=='Year 2']))

ggplot(currWorkedYr,aes(Year,p,fill=Curriculum,shape=overall))+geom_col()


cwy <- currWorkedYr%>%group_by(Curriculum,overall)%>%summarize(`Year 1`=p[Year=='Year 1'],`Year 2`=p[Year=='Year 2'])%>%arrange(Curriculum,overall)
COLS <- c('#1b9e77','#d95f02','#7570b3')

omar <- par()$oma

pdf('curriculumByYear.pdf',width=6.4,height=3)
par(mar=c(3,3,2,1))
par(oma=omar+c(0,0,0,10))
barplot(as.matrix(cwy[,3:4]),col=COLS[as.numeric(cwy$Curriculum)],angle=ifelse(cwy$overall=='Customized',45,NA),density=ifelse(cwy$overall=='Customized',50,NA),yaxt='n')
axis(2,seq(0,1,.1),labels=paste0(seq(0,100,10),'%'))
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
legend(1.7,1, c("Bridge-to-\n Algebra",'Algebra I','>Algebra I','','Standard','Customized'),
       fill=c(COLS,NA,'#010101','#010101'),border=rep('white',6),
       angle=c(NA,NA,NA,NA,NA,45),density=c(NA,NA,NA,NA,NA,50),xpd = TRUE, bty = "n",inset=c(0,0))
dev.off()

### by state
currWorkedSt <- currWorked%>%filter(!is.na(Curriculum))%>%group_by(Curriculum,Year,State,overall)%>%summarize(n=n())
nBySt <- currWorkedSt%>%group_by(Year,State)%>%summarize(n=sum(n))
currWorkedSt$p <- NA
for(i in 1:nrow(currWorkedSt)){
    currWorkedSt$p[i] <- currWorkedSt$n[i]/nBySt$n[nBySt$State==currWorkedSt$State[i] & nBySt$Year==currWorkedSt$Year[i]]
    }


pdf('curriculumByState.pdf',width=6.4,height=3)
par(oma=c(0,2,0,0),mar=c( 3,.5,2,0))
par(mfrow=c(1,8))
for(st in levels(data$State)){
    cwy <- currWorkedSt%>%filter(State==st)%>%group_by(Curriculum,overall)%>%summarize(`Yr 1`=p[Year=='Year 1'],`Yr 2`=p[Year=='Year 2'])
    cwy <- cwy[nrow(cwy):1,]
    barplot(as.matrix(cwy[,3:4]),col=COLS[as.numeric(cwy$Curriculum)],angle=ifelse(cwy$overall=='Customized',45,NA),density=ifelse(cwy$overall=='Customized',50,NA),main=st,yaxt='n')
    if(st==levels(data$State)[1]) axis(2,at=seq(0,1,0.1),labels=paste0(seq(0,100,10),'%'))
    #box()
}
plot(1:10,type='n',xaxt='n',yaxt='n',bty='n')
legend(-1.5,7.5, c("Bridge-to-\n Algebra",'Algebra I','>Algebra I','','Standard','Customized'),
       fill=c(COLS,NA,'#010101','#010101'),border=rep('white',6),
       angle=c(NA,NA,NA,NA,NA,45),density=c(NA,NA,NA,NA,NA,50),xpd = TRUE, bty = "n",inset=c(0,0))
dev.off()




    currPer <- secByStudCurr %>%filter(!is.na(curriculum))%>%group_by(Yr,curriculum,state)%>%summarize(numSec=sum(numSec,na.rm=TRUE))
currPer$perSec <- currPer$numSec/sapply(1:nrow(currPer),function(i) sum(currPer$numSec[currPer$state==currPer$state[i] & currPer$Yr==currPer$Yr[i]],na.rm=TRUE))
levels(currPer$curriculum) <- list(`>Algebra I`=c('Algebra II','Geometry'),`Algebra I`='Algebra I',`Bridge-to-Algebra`='Bridge-to-Algebra',
                                   Customized='Customized')

### what proportions of worked sections came from what curriculum?
### apportion customized curricula to others:
print(currPerPlot <- ggplot(currPer,aes(Yr,perSec,fill=curriculum))+geom_col()+facet_grid(~state)+labs(y='% of Sections Worked',x='',fill='Curriculum'))
ggsave('curricula.jpg',width=6,height=3)



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
UnitName[units=='linear-systems-solving'] <- 'Systems of Lin. Eq. Solving'
UnitName[units=='probability'] <- 'Probability'
UnitName[units=='unit-conversions'] <- 'Unit Conversions'

nstud <- data%>%filter(!is.na(unit))%>%group_by(Year)%>%summarize(nstud=n_distinct(field_id))
data$Unit <- data$unit
data$Unit[grep('unit-conversions',data$Unit)] <- 'unit-conversions'

unitLevel <- data%>%filter(Unit%in%units)%>%group_by(Unit,Year)%>%summarize(numWorked= n_distinct(field_id,na.rm=TRUE),numCP=sum(status=='changed placement',na.rm=TRUE),meanCP=mean(status=='changed placement',na.rm=TRUE))

unitLevel$perWorked <- unitLevel$numWorked/nstud$nstud[match(unitLevel$Year,nstud$Year)]

unitLevel$Unit <- factor(unitLevel$Unit,levels=units)
levels(unitLevel$Unit) <- UnitName

unitLevel$year <- factor(ifelse(unitLevel$Year=='Year 1',1,2))
print(unitsWorked <- ggplot(unitLevel,aes(x=Unit,y=perWorked,color=year,group=year))+geom_point()+geom_line()+theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5,size=10),legend.text=element_text(size=10),legend.position=c(.96,.82))+labs(x='',y='% Worked',color='Year')+scale_y_continuous(labels=percent))
ggsave('unitsWorked.jpg',height=4,width=6)


unitCPs <- ggplot(filter(unitLevel,numWorked>100),aes(x=Unit,y=meanCP,color=year,group=year))+
          geom_point()+geom_line()+
          theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5,size=10))+
          theme(legend.position=c(.96,.82),legend.text=element_text(size=10))+
          labs(x='',y='% Reassigned',color='Year')+
          scale_y_continuous(labels=percent)
ggsave('CPbyUnit.jpg',unitCPs,height=4,width=6)


#######################################################################################
##### CPs
####################################################################################
statusPerSec <- data%>%
    filter(!is.na(Curriculum) &!(state=='MI'&Curriculum=='Customized'&year==1))%>%
    group_by(field_id,Yr,state,section,Curriculum,status)%>%
        summarize(cp=any(status=='changed placement'))%>%group_by(state,Curriculum,Yr,field_id)%>%summarise(pcp=mean(cp,na.rm=TRUE),ncp=sum(cp,na.rm=TRUE))

print(cpByStud <- ggplot(statusPerSec,aes(x=Yr,y=pcp,fill=Curriculum))+geom_boxplot()+facet_grid(~state))
ggsave('cpByStud.jpg',width=6,height=3)






### how many classmates graduated, promoted, CP'd same section before i?
## section-level data:

data$status <- ordered(data$status,c('final_or_incomplete','changed placement','promoted','graduated'))
secLev <- data%>%filter(is.finite(status) & is.finite(date))%>%
    group_by(field_id,unit,section,Year,Yr,classid2,schoolid2)%>%
    summarize(startDate=min(date),endDate=max(date),state=state[1],
              status=max(status))%>%
    arrange(endDate)

### cp over timep
secLev <- within(secLev,endMonth <- factor(month(secLev$endDate,TRUE,TRUE),levels=c('Sep','Oct','Nov','Dec','Jan','Feb','Mar','Apr','May','Jun','Jul','Aug')))
secLevMonth <- secLev%>%group_by(endMonth,Year)%>%summarize(nsec=n(),CPper=mean(status=='changed placement',na.rm=TRUE))
secLev$cp <- as.numeric(secLev$status=='changed placement')

ggplot(filter(secLevMonth,ncp>100),aes(endMonth,CPper,group=Year,color=Year,size=ncp))+geom_point()+geom_line(size=1)+
    ## geom_smooth(aes(as.numeric(endMonth)+day(endDate)/31-0.5,cp,group=Year,
    ##                 color=Year,size=1),
    ##             method = "glm", formula = y ~ splines::bs(x, 4),data=secLev,method.args=list(family='binomial'),
                                        #            show.legend=FALSE)+
scale_y_continuous(breaks=seq(0,0.05,.01),labels=percent)+
    coord_cartesian(ylim=c(0,0.05))+ labs(x='Month',y='% of Sections Ending in Reassignment')
ggsave('cpByMonth.jpg',width=6.5,height=3)

secLev$unitSectionClass <- paste(secLev$unit,secLev$section,secLev$classid2)
secLev$nCPbf <- secLev$nGrad <- secLev$nProm <- secLev$nFoI <- secLev$total <- secLev$nCPsame <- NA
cpDat <- subset(secLev,status=='changed placement')
cpClasses <- unique(cpDat$unitSectionClass)
secLevCP <- filter(secLev,unitSectionClass%in%cpClasses)

secLevCPsplit <- split(secLevCP,secLevCP$unitSectionClass)


ncp <- nrow(cpDat)
for(i in 1:ncp){
    cls <- secLevCPsplit[[cpDat$unitSectionClass[i]]]
    date <- cpDat$endDate[i]
    total <- nrow(cls)
    if(total==1) cpDat[i, c('nCPbf','nGrad','nProm','nFoI','nCPsame')] <- 0
    else{
        cls <- cls[cls$field_id!=cpDat$field_id[i] & cls$endDate<=date,]
        bf <- nrow(cls)
        if(bf==0) cpDat[i, c('nCPbf','nGrad','nProm','nFoI','nCPsame')] <- 0
        else{
            cpDat[i, c('nFoI','nCPbf','nProm','nGrad')] <-
                xtabs(~status,data=cls)
            cpDat$nCPsame[i] <- sum(cls$endDate==date & cls$status=='changed placement')
            cpDat$nCPbf[i] <- cpDat$nCPbf[i]-cpDat$nCPsame[i]
        }
    }
    cpDat$total[i] <- total
    if(i%%100==0) cat(round(i/ncp*100,2),'% ')
}
cat('\n')

classSize <- data%>%filter(!is.na(section))%>%group_by(classid2)%>%summarize(nstud=n_distinct(field_id))
cpDat$totClassmates <- classSize$nstud[match(cpDat$classid2,classSize$classid2)]

cpDat$nBefore <- rowSums(cpDat[,c('nCPbf','nGrad','nProm','nFoI')])
cpDat$nLater <- cpDat$total-cpDat$nBefore-cpDat$nCPsame-1
cpDat$nNW <- cpDat$totClassmates-cpDat$total
for(vv in c('CPbf','Grad','Prom','FoI','Before','CPsame','Later','NW'))
    cpDat[[paste0('p',vv)]] <- cpDat[[paste0('n',vv)]]/(cpDat$totClassmates-1)




#### overall by year
ordOverall <- cpDat%>%filter(total>1 & is.finite(pLater) & is.finite(pNW))%>%group_by(Year)%>%summarize(Mastered=mean(pGrad,na.rm=TRUE),Promoted=mean(pProm,na.rm=TRUE),ReassignedBF=mean(pCPbf,na.rm=TRUE),ReassignedSame=mean(pCPsame,na.rm=TRUE),Final=mean(pFoI),Later=mean(pLater,na.rm=TRUE),NW=mean(pNW,na.rm=TRUE))%>%melt()
ordOverall$Year <- factor(ordOverall$Year,levels=c('Year 2','Year 1'))

ggplot(ordOverall,aes(x=Year,y=value,fill=variable))+geom_bar(stat="identity")+scale_y_continuous(labels=percent,limits=c(0,1))+labs(x='',y='% of Classmates',fill='Classmates\' \nSection Status')+coord_flip()+theme(legend.position='top')
ggsave('classmatesByYear.jpg')

ggplot(cpDat,aes(pBefore))+geom_histogram()+facet_grid(Year~.)

### by state
stateBefore <- ggplot(cpDat,aes(Year,pBefore))+geom_boxplot()+facet_grid(~state)

stateCP <- cpDat%>%filter(total>1)%>%ungroup%>%select(Year,state,pCPbf,pCPsame,pGrad,pProm)%>%melt(id.vars=c('Year','state'))
stateTot <- ggplot(stateCP,aes(variable,value))+geom_boxplot()+facet_grid(Year~state)
### by unit
cpDatUnit <- cpDat%>%filter(unit %in% units)%>%group_by(unit,Year)%>%summarize(Mastered=mean(pGrad,na.rm=TRUE),Promoted=mean(pProm,na.rm=TRUE),Reassigned=mean(pCP,na.rm=TRUE),Final=mean(pFoI),totalCP=n())#%>%melt()
cpDatUnit$unit <- factor(cpDatUnit$unit,levels=units)
levels(cpDatUnit$unit) <- UnitName

ggplot(data=cpDatUnit, aes(x=unit, y=value, fill=variable)) +
  geom_bar(stat="identity") +   theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5)) +
  labs(x='',y='% of Classmates',fill='Classmates\' \nSection Status')+facet_grid(Year~.)
ggsave('beforeCP.pdf',width=6.5,height=5)

#### cpBefore by # of CP/unit



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

### cpBefore by time
cpDat <- within(cpDat,endMonth <- factor(month(cpDat$endDate,TRUE,TRUE),levels=c('Sep','Oct','Nov','Dec','Jan','Feb','Mar','Apr','May','Jun','Jul','Aug')))
cpDatMonth <- cpDat%>%group_by(endMonth,Year)%>%summarize(ncp=n(),avgPBefore=mean(pBefore,na.rm=TRUE))

ggplot(cpDatMonth,aes(endMonth,avgPBefore,group=Year,color=Year,size=ncp))+geom_point()+geom_smooth(aes(as.numeric(endMonth)+day(endDate)/31-0.5,pBefore,group=Year,color=Year,size=1,weight=total),method = "glm", formula = y ~ splines::bs(x, 4),data=cpDat,method.args=list(family='binomial'),show.legend=FALSE)+scale_y_continuous(breaks=seq(0,1,.2),labels=percent)+labs(x='Month',y='% of Classmates Finish Together or Earlier')
ggsave('cpBeforeByDate.jpg')


### next unit
secLevSeq <- filter(secLev,unit%in%units)
secLevSeq$unit <- factor(secLevSeq$unit,levels=units)
secLevSeq <- secLevSeq%>%arrange(endDate)
secLevSeq <- subset(secLevSeq,field_id%in%cpDat$field_id)
unitSplit <- split(secLevSeq$unit,secLevSeq$field_id)
dist <- lapply(unitSplit,function(x) c(as.numeric(x[-1]),NA)-as.numeric(x))#,secLevSeq$field_id)
secLevSeq$dist <- unsplit(dist,secLevSeq$field_id)


secLevSeq <- secLevSe


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




#### what's the next section each student works?
secOrder <- data%>%filter(is.finite(status) & is.finite(timestamp))%>%group_by(field_id,section,unit,Curriculum,overall,year,Year,Yr,state,classid2,schoolid2)%>%summarize(time=max(timestamp),status=max(status))%>%arrange(time)%>%group_by(field_id,year,Year,Yr,state,classid2,schoolid2)%>%mutate(prevSec=c(NA,section[-n()]),prevStatus=c(NA,status[-n()]),prevUnit=c(NA,unit[-n()]),nextUnit=c(unit[-1],NA))

secOrder$cp <- secOrder$status=='changed placement'
secOrder$mast <- secOrder$status=='graduated'
library(lme4)
mastNext <- glmer(mast~(1|field_id)+(1|classid2)+section+as.factor(prevStatus),data=secOrder,family=binomial)

### transition plot
library(Gmisc)
secOrderCP <- subset(secOrder,status=='changed placement')


secOrderCP$Unit <- secOrderCP$unit
secOrderCP$Unit[grep('unit-conversions',secOrderCP$Unit)] <- 'unit-conversions'

secOrderCP$unitName <- factor(secOrderCP$Unit,levels=units)
levels(secOrderCP$unitName) <- UnitName

secOrderCP$nextUnit[grep('unit-conversions',secOrderCP$nextUnit)] <- 'unit-conversions'

secOrderCP$nextUnitName <- factor(secOrderCP$nextUnit,levels=units)
levels(secOrderCP$nextUnitName) <- UnitName

trans <- with(secOrderCP,table(unitName,nextUnitName))

trans <- trans[rowSums(trans)>50,rowSums(trans)>50]
trans <- trans[rowSums(trans)>50,rowSums(trans)>50]

transitionPlot(trans)

nsec <- nrow(trans)

plot(1,1,xlim=c(0,6),ylim=c(0,nsec),cex=0)
text(rep(1,nsec),nsec:1,rownames(trans))
text(rep(5,nsec),nsec:1,rownames(trans))
for(i in 1:nsec){
    arrows(rep(2.5,nsec),rep(nsec-i+1,nsec),rep(3.5,nsec),nsec:1,lwd=5*trans[i,]/rowSums(trans)[i])#,col=rgb(0,0,0,trans[i,]/max(trans)))
}
