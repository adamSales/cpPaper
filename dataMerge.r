library(dplyr)

load('~/Box Sync/CT/data/problemLevelUsageData/probLevelData.RData')
load('~/Box Sync/CT/data/sectionLevelUsageData/advanceDataWfinalinc.RData')


usageIDs <- unique(c(unique(x$field_id),unique(advance$field_id)))

hs1 <- read.csv('~/Box Sync/CT/data/RANDstudyData/H1_algebra_rcal_20121119_fieldid.csv')
hs2 <- read.csv('~/Box Sync/CT/data/RANDstudyData/H2_algebra_rcal_20121119_fieldid.csv')

hs2 <- hs2[!hs2$field_id%in%hs1$field_id,]
stopifnot(all.equal(names(hs1)[c(2:100,174:194)],names(hs2)[c(2:100,176:196)]))
stud <- rbind(hs1[,c(2:100,174:194)],hs2[,c(2:100,176:196)])

stud <- stud[stud$treatment==1,]

stud$obsUsage <- stud$field_id%in%usageIDs

stud$pretest <- rowMeans(stud[,grep('Exirt2_',names(stud))],na.rm=TRUE)


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
prob$curriculumOrigProb <- prob$curriculum
prob$curriculum <- as.character(prob$curriculum)
prob$curriculum[grep('cc',prob$curriculum)] <- 'Customized'
prob$curriculum <- sub('del_','',prob$curriculum)
prob$curriculum <- sub(' bonus','',prob$curriculum)
prob$curriculum <- factor(prob$curriculum)

adv <- advance[advance$field_id%in%stud$field_id,]
for(vv in names(adv)) if(is.factor(adv[[vv]])) levels(adv[[vv]]) <- tolower(levels(adv[[vv]]))
adv$curriculumOrigAdv <- adv$curriculum
adv$curriculum <- as.character(adv$curriculum)
adv$curriculum[grep('cc',adv$curriculum)] <- 'Customized'
adv$curriculum <- sub(' bonus','',adv$curriculum)
adv$curriculum <- sub('del_','',adv$curriculum)
adv$curriculum <- factor(adv$curriculum)

data <- full_join(prob,adv)

data$curriculumOrig <- gsub('del_','',as.character(data$curriculumOrigAdv))
data$curriculumOrig <- gsub(' bonus','',data$curriculumOrig)

### take out year 2 data for students who were in the study both years
probDate <- sapply(strsplit(as.character(data$ts1),' '),`[`,1)
data$date <- as.Date(probDate,'%m/%d/%y')

## remove <- with(data,ifelse(field_id%in%twice,
##                          ifelse(is.na(date),
##                                 ifelse(is.na(year),
##                                        study.year==2,year==2),
##                                 date>as.Date('2008-08-01')),FALSE))

## data <- data[!remove,]

data$year <- data$study.year <- NULL
data$year <- stud$year[match(data$field_id,stud$field_id)]

data <- data[!grepl('test',data$section,ignore.case=TRUE),]

data <- filter(data,!(year==2 & date<as.Date('2008-06-01')))
data <- filter(data,!(year==1 & date>as.Date('2008-08-01')))

data$Curriculum <- factor(ifelse(data$curriculum=='Customized',
                                 'Customized','Standard'),
                          levels=c('Standard','Customized'))

data <- full_join(data,stud)


##########################################################
### should we remove schools where usage missingness was very
### different between years 1 & 2??
data <- data[data$schoolid2%in%smallDiff,]
#############################################
studPerState <- group_by(data,state)%>%summarize(nstud=n_distinct(field_id))
data$State <- factor(data$state,levels=levels(data$state)[order(studPerState$nstud,decreasing=TRUE)])

data$Yr <- c('Yr 1','Yr 2')[data$year]
data$Year <- c('Year 1','Year 2')[data$year]

levels(data$curriculum) <- list(`Bridge-to-Algebra`='bridge-to-algebra',`Algebra I`='algebra i',`Algebra II`='algebra ii',Geometry='geometry',Customized='Customized')

data$status <- ordered(data$status,c('final_or_incomplete','changed placement','promoted','graduated'))
data$state <- data$State

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


save(data,prob,adv,stud,file='cpPaper.RData')
