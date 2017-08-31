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



pload('~/Box Sync/CT/data/problemLevelUsageData/probLevelData.RData')
hs1 <- read.csv('~/Box Sync/CT/data/RANDstudyData/H1_algebra_rcal_20121119_fieldid.csv')
hs2 <- read.csv('~/Box Sync/CT/data/RANDstudyData/H2_algebra_rcal_20121119_fieldid.csv')



stopifnot(all.equal(names(hs1)[1:100],names(hs2)[1:100]))

stud <- rbind(hs1[,1:100],hs2[!hs2$field_id%in%hs1$field_id,1:100])
stud$field_id <- c(hs1$field_id,hs2$field_id[!hs2$field_id%in%hs1$field_id])
stud <- stud[stud$treatment==1,]

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

load('~/Box Sync/CT/data/sectionLevelUsageData/advanceData.RData')
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

secByStud <- summarize(group_by(data,state,year,field_id,year),numSec=n_distinct(section,na.rm=TRUE))
secByStud$numSec[secByStud$numSec==0] <- NA

secByStud$yearFac <- c('Yr 1','Yr 2')[secByStud$year]
secByStud$Curriculum <- secByStud$overall

secStateYear <- ggplot(secByStud,aes(yearFac,numSec))+geom_boxplot()+facet_grid(~state)+ylim(0,250)+labs(x='',y='Number of Sections Per Student')
ggsave('fig1.jpg',width=6,height=3)

#secStateYearCurr <- ggplot(secByStud,aes(yearFac,numSec,fill=Curriculum))+geom_boxplot()+facet_grid(~state)+ylim(0,250)+labs(x='',y='Number of Sections Per Student')
#ggsave('fig1a.jpg',width=6,height=3)


cc <- data %>% group_by(state,year,field_id,section

perCCdat <- data%>% filter(state%in%c('KY','TX','MI')) %>% group_by(state,year,section,overall) %>% mutate(perCC= mean(overall=='Customized'))

percCC <- ggplot(perCCdat,aes(yearFac,perCC))+geom_col()+facet_grid(~state)+labs(x='',y='% from Customized Curriculua')
ggsave('percentCC.jpg',width=6,height=3)

sectionLevelDat <- data%>% select(field_id,year,curriculum,unit,section,version,state,schoolidn,grdlvl,grade,teachid,classid,overall,date,total_t1,status) %>%
 group_by(field_id,year,curriculum,unit,section,version) %>%
 summarize(state=state[1],schoolidn=schoolidn[1],grdlvl=grdlvl[1],grade=grade[1],teachid=teachid[1],classid=classid[1],overall=overall[1],dateBegin=min(date),dateEnd=max(date),time=sum(total_t1),status=status[1])

for(nn in c("curriculum",  "unit" ,       "section", "version"))
 sectionLevelDat[[nn]] <- factor(tolower(as.character(sectionLevelDat[[nn]])))

sectionLevelDat <- merge(sectionLevelDat,advance,all.x=TRUE,all.y=FALSE)

### mastered sections by state/year
mastSec <- sectionLevelDat%>%group_by(state,year,field_id,year,overall)%>%summarize(nGrad=mean(status=='graduated',na.rm=TRUE)*n())
mastSec$yearFac <- c('Yr 1','Yr 2')[mastSec$year]

mastStateYear <- ggplot(mastSec,aes(yearFac,nGrad))+geom_boxplot()+facet_grid(~state)+labs(x='',y='# Sections Mastered Per Student')+ylim(0,250)
ggsave('fig1.jpg',width=6,height=3)


secStateYear <- ggplot(subset(secByStud,status=='graduated'),aes(yearFac,numSec))+geom_boxplot()+facet_grid(~state)+ylim(0,200)+labs(x='',y='Number of Sections Per Student')
ggsave('fig3.jpg',width=6,height=3)


