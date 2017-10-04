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

##########################################################################
### curricula
##########################################################################
data$overall <- factor(ifelse(data$curriculum=='Customized','Customized','Standard'),levels=c('Standard','Customized'))
data$Curriculum=data$curriculum
levels(data$Curriculum) <- list(`>Algebra I`=c('Algebra II','Geometry'),`Algebra I`='Algebra I',
                                `Bridge-to-Algebra`='Bridge-to-Algebra',Customized='Customized')

### apportion customized sections to other curricula:
custSec <- unique(data$section[data$overall=='Customized'])
alg1Sec <- unique(data$section[data$Curriculum=='Algebra I'])
advSec <- unique(data$section[data$Curriculum=='>Algebra'])
BtASec <- unique(data$section[data$Curriculum=='Bridge-to-Algebra'])

currFunc <- function(sec){

    if(sec%in%alg1Sec) return('Algebra I')
    if(sec%in%BtASec) return('Bridge-to-Algebra')
    if(sec%in%advSec) return('>Algebra I')
    if(grepl('triangle',sec)) return('>Algebra I')
    if(grepl('geo-',sec)) return('>Algebra I')
    if(grepl('exponents',sec)) return('Algebra I')
    if(grepl('cta1',sec)) return('Algebra I')
    if(grepl('graph',sec)) return('Algebra I')
    if(grepl('linear',sec)) return('Algebra I')
    if(grepl('quad',sec)) return('Algebra I')
    return('na')
}

standCurr <- vapply(custSec, currFunc,'a')
standCurr[standCurr=='na'] <- NA
data$Curriculum[!is.na(data$Curriculum) & data$overall=='Customized' & !is.na(data$section)] <-
    standCurr[match(data$section[!is.na(data$section) & data$overall=='Customized' & !is.na(data$Curriculum)],custSec)]
data$Curriculum[grep('algebrai',data$curriculumOrig)] <- 'Algebra I'
data$Curriculum[grep('algebra1',data$curriculumOrig)] <- 'Algebra I'
data$Curriculum[grep('geom',data$curriculumOrig)] <- '>Algebra I'


#### what proportion of worked sections from each curriculum?
currWorked <- data%>%distinct(field_id,section,unit,Curriculum,Year,State,overall)

currWorkedYr <- currWorked%>%filter(!is.na(Curriculum))%>%group_by(Curriculum,Year,overall)%>%summarize(n=n())
nByYr <- currWorkedYr%>%group_by(Year)%>%summarize(n=sum(n))
currWorkedYr <- within(currWorkedYr,p <- n/ifelse(Year=='Year 1',nByYr$n[nByYr$Year=='Year 1'],
                                                  nByYr$n[nByYr$Year=='Year 2']))

ggplot(currWorkedYr,aes(Year,p,fill=Curriculum,shape=overall))+geom_col()


cwy <- currWorkedYr%>%group_by(Curriculum,overall)%>%summarize(`Year 1`=p[Year=='Year 1'],`Year 2`=p[Year=='Year 2'])%>%arrange(Curriculum,overall)
COLS <- c('#1b9e77','#d95f02','#7570b3')

cwy <- data%>%filter(!is.na(Curriculum))%>%group_by(Curriculum,overall)%>%
    summarize(`Year 1`=sum(Year=='Year 1'),`Year 2`=sum(Year=='Year 2'))
for(y in c(1,2)) cwy[,paste('Year',y)] <- cwy[,paste('Year',y)]/sum(cwy[,paste('Year',y)])


omar <- par()$oma

pdf('curriculumByYear.pdf',width=6.4,height=3)
par(mar=c(3,3,2,1))
par(oma=omar+c(0,0,0,10))
barplot(as.matrix(cwy[,3:4]),col=COLS[as.numeric(cwy$Curriculum)],angle=ifelse(cwy$overall=='Customized',45,NA),density=ifelse(cwy$overall=='Customized',50,NA),yaxt='n')
axis(2,c(0,.25,.5,.75,1),labels=paste0(c(0,25,50,75,100),'%'))
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
#    cwy <- currWorkedSt%>%filter(State==st)%>%group_by(Curriculum,overall)%>%summarize(`Yr 1`=p[Year=='Year 1'],`Yr 2`=p[Year=='Year 2'])
    cwy <- data%>%filter(!is.na(Curriculum) & State==st)%>%group_by(Curriculum,overall)%>%
    summarize(`Yr 1`=sum(Year=='Year 1'),`Yr 2`=sum(Year=='Year 2'))
    for(y in c(1,2)) cwy[,paste('Yr',y)] <- cwy[,paste('Yr',y)]/sum(cwy[,paste('Yr',y)])
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

########################################
### learn section order?
##########################################
curTab <- table(data$curriculumOrig)
curr <- names(curTab)[curTab>5000]

secOrder <- data%>%filter(status%in%c('graduated','promoted')&curriculumOrig%in%curr&is.finite(timestamp))%>%
    select(field_id,version,unit,timestamp,curriculumOrig)%>% group_by(field_id,unit,version,curriculumOrig)%>%summarize(time=max(timestamp))%>%arrange(time)
secOrder <- droplevels(secOrder)




Mode <- function(x){
    tab <- table(x)
    names(tab)[which.max(tab)]
}

orders <- list()
questionable <- list()
for(vers in levels(secOrder$version)){
for(cur in curr){
    cat(cur,'\n')
    first <- secOrder%>%filter(curriculumOrig==cur)%>%group_by(field_id)%>%summarize(fst=unit[1])
    if(nrow(first)>0){
        print(table(first$fst))
        print(ord <- Mode(first$fst))
    }}
ord <- 'cta1_01'
        qqq <- NULL
        so <- secOrder%>%filter(version==vers)%>%group_by(field_id)%>%mutate(nextSU=c(unit[-1],NA))%>%ungroup()
        nSU <- length(unique(so$unit))
        lo <- 1
        while(lo<nSU){
            tab <- table(so$nextSU[so$unit==ord[lo]])
            if(length(tab)>1) qqq <- c(qqq,ord[lo])
            ord <- c(ord,names(tab)[which.max(tab)])
            lo <- length(ord)
        }
        orders[[vers]] <- ord
        questionable[[vers]] <- qqq
    }

save(orders,questionable,file='learnedOrders.RData')

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
##### overall status
####################################################################################
statusPerSec <- data%>%
    filter(!is.na(Curriculum) &!(state=='MI'&Curriculum=='Customized'&year==1))%>%
    group_by(field_id,Yr,state,section,Curriculum,status)%>%
        summarize(cp=any(status=='changed placement'))%>%group_by(state,Curriculum,Yr,field_id)%>%summarise(pcp=mean(cp,na.rm=TRUE),ncp=sum(cp,na.rm=TRUE))

statusOverall <-  data%>%filter(!is.na(status))%>%
    group_by(field_id,Yr,state,unit,section,Curriculum,overall)%>%summarize(status=max(status))
statusStateYr <- statusOverall %>% group_by(state,Yr)%>%
    summarize(pgrad=mean(status=='graduated'),pfoi=mean(status=='final_or_incomplete'),
              pcp=mean(status=='changed placement'),pprom=mean(status=='promoted'))%>%
    melt(measure.vars=c('pgrad','pfoi','pcp','pprom'))

levels(statusStateYr$variable) <- list(Final='pfoi',Reassigned='pcp',Promoted='pprom',Graduated='pgrad')

print(statusStateYrPlot <- ggplot(statusStateYr,aes(Yr,value,fill=variable))+geom_col()+facet_grid(~state)+labs(y='% of Sections Worked',x='',fill='Exit Status')+scale_y_continuous(labels=percent))

ggsave('statusStateYr.jpg',width=6,height=3)

statusCurr <- statusOverall%>%filter(!is.na(Curriculum))%>% group_by(Curriculum,overall)%>%
    summarize(pgrad=mean(status=='graduated'),pfoi=mean(status=='final_or_incomplete'),
              pcp=mean(status=='changed placement'),pprom=mean(status=='promoted'))%>%
    melt(measure.vars=c('pgrad','pfoi','pcp','pprom'))
levels(statusCurr$variable) <- list(Final='pfoi',Reassigned='pcp',Promoted='pprom',Graduated='pgrad')
statusCurr$Curriculum <- factor(statusCurr$Curriculum,levels=rev(levels(statusCurr$Curriculum)))
print(ggplot(statusCurr,aes(overall,value,fill=variable))+geom_col()+facet_grid(~Curriculum)+labs(y='% of Sections Worked',x='',fill='Exit Status')+scale_y_continuous(labels=percent))

ggsave('statusCurr.jpg',width=6,height=3)

#######################################################################################
##### CPs
####################################################################################


secLev <- data%>%filter(is.finite(status) & is.finite(timestamp) &is.finite(date))%>%
    group_by(field_id,unit,section,Year,Yr,classid2,schoolid2)%>%
    summarize(startDate=min(date),endDate=max(date),startTime=min(timestamp),endTime=max(timestamp),state=state[1],
              status=max(status),Curriculum=Curriculum[1],overall=overall[1],version=version[1])%>%
    arrange(endDate)

<<<<<<< HEAD
### cp over time
secLev <- within(secLev,endMonth <- factor(month(secLev$endDate,TRUE,TRUE),levels=c('Aug','Sep','Oct','Nov','Dec','Jan','Feb','Mar','Apr','May','Jun','Jul')))
secLevMonth <- secLev%>%group_by(endMonth,Year)%>%summarize(ncp=n(),CPper=mean(status=='changed placement',na.rm=TRUE))
secLev$cp <- as.numeric(secLev$status=='changed placement')

ggplot(filter(secLevMonth,endMonth!='Jul'),aes(endMonth,CPper,group=Year,color=Year,size=ncp))+geom_point()+
    ## geom_smooth(aes(as.numeric(endMonth)+day(endDate)/31-0.5,cp,group=Year,
    ##                 color=Year,size=1),
    ##             method = "glm", formula = y ~ splines::bs(x, 3),data=secLev,method.args=list(family='binomial'),
    ##             show.legend=FALSE)
    geom_line(size=1)+
scale_y_continuous(breaks=seq(0,0.05,.01),labels=percent)+
    coord_cartesian(ylim=c(0,0.05))+ labs(x='Month',y='% Sections Ending in Reassignment',size='Worked \n Sections',color='')
=======
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


### with state?
secLevMonthSt <- secLev%>%filter(state%in%c('TX','KY','MI') & endMonth!='Jul')%>%group_by(endMonth,state,Year)%>%summarize(nsec=n(),CPper=mean(status=='changed placement',na.rm=TRUE))
ggplot(secLevMonthSt,aes(endMonth,CPper,group=state,color=state,size=nsec))+geom_point()+facet_grid(Year~.)+
    geom_line(size=1)+
    ## geom_smooth(aes(as.numeric(endMonth)+day(endDate)/31-1.5,cp,group=state,
    ##                 color=state,size=1),
    ##             method = "glm",
    ##             formula = y ~ splines::ns(x, knots=seq(1,10,2)),
    ##             data=filter(secLev,state%in%c('TX','CT','NJ')& endMonth!='Jul'),
    ##             method.args=list(family='binomial'),
    ##             show.legend=FALSE)+
    scale_y_continuous(labels=percent)+
    labs(x='Month',y='% Sections Ending in Reassignment',size='Worked \n Sections',color='')
ggsave('cpByMonthSt.jpg',width=6.5,height=6)



### how many classmates graduated, promoted, CP'd same section before i?
## section-level data:

secLev$unitSectionClass <- paste(secLev$unit,secLev$section,secLev$classid2)
cpDat <- subset(secLev,status=='changed placement' & Curriculum=='Algebra I')
cpClasses <- unique(cpDat$unitSectionClass)
secLevCP <- filter(secLev,unitSectionClass%in%cpClasses)
levels(secLevCP$status) <- list(promGrad=c('promoted','graduated','final_or_incomplete'),cp='changed placement')
secLevCPsplit <- split(secLevCP[,c('field_id','endDate','status')],secLevCP$unitSectionClass)


ncp <- nrow(cpDat)
seqDat <- matrix(NA,nrow=ncp,ncol=6)
for(i in 1:ncp){
    if(i%%100==0) cat(round(i/ncp*100,2),'% ',sep='')
    cls <- secLevCPsplit[[cpDat$unitSectionClass[i]]]
    if(nrow(cls)==1){
        seqDat[i,] <- 0
        next
    }
    cls <- filter(cls,field_id!=cpDat$field_id[i])
    date <- cpDat$endDate[i]
    ord <- factor(ifelse(cls$endDate<date,'bf',
                  ifelse(cls$endDate==date,'same','after')),levels=c('bf','same','after'))
    clsSplit <- split(cls,list(ord,cls$status))
    seqDat[i,] <- vapply(clsSplit,nrow,1)
}
cat('\n')
colnames(seqDat) <- names(clsSplit)

classSize <- data%>%filter(!is.na(section))%>%group_by(classid2)%>%summarize(nstud=n_distinct(field_id))
cpDat$totClassmates <- classSize$nstud[match(cpDat$classid2,classSize$classid2)]

for(nn in names(clsSplit)) cpDat[[nn]] <- seqDat[,nn]/cpDat$totClassmates
cpDat$nw <- (cpDat$totClassmates-rowSums(seqDat))/cpDat$totClassmates
cpDat$total <- rowSums(seqDat)
cpDat$bf.promGrad <- cpDat$bf.promGrad+cpDat$same.promGrad

### plot them all

mmm <- with(subset(cpDat,totClassmates>15),max(table(state,Year)))

cpDat2 <- cpDat%>%filter(totClassmates>15)%>%arrange(bf.promGrad,bf.cp,same.cp,after.cp,after.promGrad)%>%group_by(Year,state)%>%mutate(cpid=seq(floor((mmm-n())/2)+1,floor((mmm-n())/2)+n()))%>%ungroup()

aaa <- melt(cpDat2,measure.vars=c('bf.promGrad','bf.cp','same.cp','after.cp','after.promGrad','nw'))
levels(aaa$variable) <- list(
    `Never Worked\n Section`='nw',
    `Mastered/Promoted\n Later`='after.promGrad',
    `Reassigned\n Later`='after.cp',
    `Reassigned\n Same Day`='same.cp',
    `Reassigned\n Before`='bf.cp',
    `Mastered/Promoted\n Before`='bf.promGrad')





#aaa$variable <- factor(aaa$variable,levels=rev(c('pGrad','pProm','pCPbf','pCPsame','pLater','pNW')))
ggplot(filter(aaa,state%in%c('TX','KY','MI')),aes(as.factor(cpid),value,fill=variable))+geom_col()+facet_grid(Year~state)+theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) + scale_y_continuous(label=percent)+labs(fill='% of Classmates')+
    scale_fill_manual(values=c('#1b9e77','#d95f02','#7570b3','#e7298a','#66a61e','#e6ab02'))
ggsave('cpClassmatesDist.jpg',width=6,height=4)

#### overall by year
ordOverall <- cpDat%>%filter(total>1 & is.finite(pLater) & is.finite(pNW))%>%group_by(Year)%>%summarize(Mastered=mean(pGrad,na.rm=TRUE),Promoted=mean(pProm,na.rm=TRUE),ReassignedBF=mean(pCPbf,na.rm=TRUE),ReassignedSame=mean(pCPsame,na.rm=TRUE),Final=mean(pFoI),Later=mean(pLater,na.rm=TRUE),NW=mean(pNW,na.rm=TRUE))%>%melt()
ordOverall$Year <- factor(ordOverall$Year,levels=c('Year 2','Year 1'))

ggplot(ordOverall,aes(x=Year,y=value,fill=variable))+geom_bar(stat="identity")+scale_y_continuous(labels=percent,limits=c(0,1))+labs(x='',y='% of Classmates',fill='Classmates\' \nSection Status')+coord_flip()+theme(legend.position='top')
ggsave('classmatesByYear.jpg')


ordOverall2 <- cpDat%>%filter(total>1 & is.finite(pLater) & is.finite(pNW))%>%group_by(Year)%>%summarize(`Finished \n Before`=mean(pGrad+pProm,na.rm=TRUE),`Reassigned \n Before`=mean(pCPbf,na.rm=TRUE),`Reassigned \n Same Day`=mean(pCPsame,na.rm=TRUE),`Exited \n Later`=mean(pLater,na.rm=TRUE),`Never Worked \n Section`=mean(pNW,na.rm=TRUE))%>%melt()
ordOverall2$Year <- factor(ordOverall2$Year,levels=c('Year 2','Year 1'))
ggplot(ordOverall2,aes(x=Year,y=value,fill=variable))+geom_bar(stat="identity")+scale_y_continuous(labels=percent,limits=c(0,1))+labs(x='',y='% of Classmates',fill='Classmates\' \nSection Status')+coord_flip()+theme(legend.position='top')

monthOrder <- function(x){
    x[x<8] <- x[x<8]+12
    x-8
}
cpDat <- mutate(cpDat,finishedBefore=pGrad+pProm,finishedBefore2=(nGrad+nProm+nCPbf)/(nGrad+nProm+nCPbf+nLater),
                date=monthOrder(month(endDate))-day(endDate)/31)

ggplot(cpDat,aes(date,finishedBefore))+geom_jitter(width=0.03,height=0)+facet_grid(Year~.)+geom_smooth()+xlim(0,10)+scale_y_continuous(label=percent,limits=c(0,1))+scale_x_continuous(limits=c(0,10),breaks=0:10+0.5,labels=c(month.abb[8:12],month.abb[1:6]))


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


### next unit #1

secLevSeq <- filter(secLev,unit%in%units)
#secLevSeq$unit <- factor(secLevSeq$unit,levels=units)
secLev <- secLev%>%arrange(endTime)
secLev <- secLev%>%group_by(field_id)%>%mutate(nextUnit= c(unit[-1],NA))%>%ungroup()
secLevCP <- subset(secLev,status=='changed placement')
ttt <- table(secLevCP$unit)
tmat <- with(secLevCP,table(unit,nextUnit))[names(ttt)[ttt>100],names(ttt)[ttt>100]]
uuu <- units[units%in%rownames(tmat)]
tmat[uuu,uuu]
Gmisc::transitionPlot(tmat[uuu,uuu])


nextUnit <- lapply(unique(secLevCP$unit),function(un) table(secLevCP$nextUnit[secLevCP$unit==un]))
names(nextUnit) <- unique(secLevCP$unit)
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


## classroom
adv$year <- NULL
adv <- cbind(adv,stud[match(adv$field_id,stud$field_id),c('year','state','schoolid2','classid2')])
classMeans <- adv%>%group_by(year,classid2)%>%summarize(size=n(),cp=mean(status=='changed placement'))

qplot(size,cp,data=classMeans)+facet_grid(year~.)

ggplot(classMeans,aes(cp))+geom_histogram()+facet_grid(year~.)

### model

adv <- cbind(adv,stud[match(adv$field_id,stud$field_id),c('xirt','spec_speced','spec_gifted','grade')])


data$status <- ordered(data$status,c('final_or_incomplete','changed placement','promoted','graduated'))

secLev <- data%>%filter(is.finite(status) & is.finite(date))%>%
    group_by(field_id,unit,section,Year,Yr,classid2,schoolid2,state,xirt,spec_speced,spec_gifted,grade)%>%
    summarize(startDate=min(date),endDate=max(date),
              status=max(status))%>%
    arrange(endDate)

secLev$unitSectionClass <- paste(secLev$unit,secLev$section,secLev$classid2)
secLevCPsplit <- split(secLev,secLev$unitSectionClass)

secLev$nCPbf <- secLev$nGrad <- secLev$nProm <- secLev$nFoI <- secLev$total <- secLev$nCPsame <- NA

for(i in 1:nrow(secLev)){
    cls <- secLevCPsplit[[secLev$unitSectionClass[i]]]
    date <- secLev$endDate[i]
    total <- nrow(cls)
    if(total==1) secLev[i, c('nCPbf','nGrad','nProm','nFoI','nCPsame')] <- 0
    else{
        cls <- cls[cls$field_id!=secLev$field_id[i] & cls$endDate<=date,]
        bf <- nrow(cls)
        if(bf==0) secLev[i, c('nCPbf','nGrad','nProm','nFoI','nCPsame')] <- 0
        else{
            secLev[i, c('nFoI','nCPbf','nProm','nGrad')] <-
                xtabs(~status,data=cls)
            secLev$nCPsame[i] <- sum(cls$endDate==date & cls$status=='changed placement')
            secLev$nCPbf[i] <- secLev$nCPbf[i]-secLev$nCPsame[i]
        }
    }
    secLev$total[i] <- total
    if(i%%100==0) cat(round(i/ncp*100,2),'% ')
}
cat('\n')

save(secLev,file='secLev.RData')

classSize <- data%>%filter(!is.na(section))%>%group_by(classid2)%>%summarize(nstud=n_distinct(field_id))
secLev$totClassmates <- classSize$nstud[match(secLev$classid2,classSize$classid2)]

secLev$nBefore <- rowSums(secLev[,c('nCPbf','nGrad','nProm','nFoI')])
secLev$nLater <- secLev$total-secLev$nBefore-secLev$nCPsame-1
secLev$nNW <- secLev$totClassmates-secLev$total
for(vv in c('CPbf','Grad','Prom','FoI','Before','CPsame','Later','NW'))
    secLev[[paste0('p',vv)]] <- secLev[[paste0('n',vv)]]/(secLev$totClassmates-1)

secLev$cp <- secLev$status=='changed placement'
library(lme4)
cpMod <- glmer(cp~Year*(xirt+grade+spec_speced+spec_gifted+pBefore+totClassmates+endDate+state)+(Year*(xirt+grade+spec_speced+spec_gifted+pBefore+totClassmates+endDate+state)|unit)+(xirt+grade+spec_speced+spec_gifted+pBefore+totClassmates+endDate+state|classid2)+(1|schoolid2),data=secLev)
save(cpMod,file='cpMod.RData')
=======
#### what's the next section each student works?
secOrder <- data%>%filter(is.finite(status) & is.finite(timestamp))%>%group_by(field_id,section,unit,Curriculum,overall,year,Year,Yr,state,classid2,schoolid2)%>%summarize(time=max(timestamp),status=max(status))%>%arrange(time)%>%group_by(field_id,year,Year,Yr,state,classid2,schoolid2)%>%mutate(prevSec=c(NA,section[-n()]),prevStatus=c(NA,status[-n()]),prevUnit=c(NA,unit[-n()]))


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
