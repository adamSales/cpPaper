library(dplyr)
library(lme4)

### we have more usage data in year 1 than year 2
print(load('~/Box Sync/CT/data/sectionLevelUsageData/advanceData.RData'))

advance%>%group_by(year)%>%summarize(n_distinct(field_id))

print(load('~/Box Sync/CT/data/problemLevelUsageData/probLevelData.RData'))


x%>%group_by(study.year)%>%summarize(n_distinct(field_id))


#### what about as percentage of students in RAND dataset?

hs1 <- read.csv('~/Box Sync/CT/data/RANDstudyData/H1_algebra_rcal_20121119_fieldid.csv')
hs2 <- read.csv('~/Box Sync/CT/data/RANDstudyData/H2_algebra_rcal_20121119_fieldid.csv')

### treated students in RAND dataset:
ids <- unique(c(hs1$field_id[hs1$treatment==1],hs2$field_id[hs2$treatment==1]))
year <- ifelse(ids%in%hs1$field_id,1,2)

### "advance" dataset:
mean(ids[year==1]%in%advance$field_id)
mean(ids[year==2]%in%advance$field_id)

### "problem level" dataset:
mean(ids[year==1]%in%x$field_id)
mean(ids[year==2]%in%x$field_id)

### both:
usageIDs <- unique(c(unique(x$field_id),unique(advance$field_id)))
mean(ids[year==1]%in%usageIDs)
mean(ids[year==2]%in%usageIDs)

### (is this just because I counted "both-year" students as yr-1?)
length(intersect(hs1$field_id,hs2$field_id))
(sum(ids[year==2]%in%usageIDs)+193)/(length(ids[year==2])+193)

### what predicts missingness?
stopifnot(all.equal(names(hs1)[1:100],names(hs2)[1:100]))

stud <- rbind(hs1[,1:100],hs2[!hs2$field_id%in%hs1$field_id,1:100])
stud$field_id <- c(hs1$field_id,hs2$field_id[!hs2$field_id%in%hs1$field_id])
stud <- stud[stud$treatment==1,]

stud$obsUsage <- stud$field_id%in%usageIDs

Mode <- function(x) levels(x)[which.max(table(x))]

## median/mode imputation for predictors, with NA flags:
studImp1 <- within(subset(stud,year==1),{
                   gradeNA <- is.na(grade)
                   grade[is.na(grade)] <- median(grade,na.rm=TRUE)
                   raceNA <- is.na(race)
                   race[is.na(race)] <- Mode(race)
                   sexNA <- is.na(sex)
                   sex[is.na(sex)] <- 'F'
                   specedNA <- is.na(spec_speced)
                   spec_speced[is.na(spec_speced)] <- 0
                   giftedNA <- is.na(spec_gifted)
                   spec_gifted[is.na(spec_gifted)] <- 0
                   eslNA <- is.na(spec_esl)
                   spec_esl[is.na(spec_esl)] <- 0
                   frlNA <- is.na(frl)
                   frl[is.na(frl)] <- 1
                   xirtNA <- is.na(xirt)
                   xirt[is.na(xirt)] <- median(xirt,na.rm=TRUE)})
studImp2 <- within(subset(stud,year==2),{
                   gradeNA <- is.na(grade)
                   grade[is.na(grade)] <- median(grade,na.rm=TRUE)
                   raceNA <- is.na(race)
                   race[is.na(race)] <- Mode(race)
                   sexNA <- is.na(sex)
                   sex[is.na(sex)] <- 'F'
                   specedNA <- is.na(spec_speced)
                   spec_speced[is.na(spec_speced)] <- 0
                   giftedNA <- is.na(spec_gifted)
                   spec_gifted[is.na(spec_gifted)] <- 0
                   eslNA <- is.na(spec_esl)
                   spec_esl[is.na(spec_esl)] <- 0
                   frlNA <- is.na(frl)
                   frl[is.na(frl)] <- 1
                   xirtNA <- is.na(xirt)
                   xirt[is.na(xirt)] <- median(xirt,na.rm=TRUE)})

studImp <- rbind(studImp1,studImp2)

## missingness due to year?
summary(glmer(obsUsage~year+(1+year|schoolid2),family=binomial,data=stud))

## due to changes in composition?
summary(mainEffMod <- glmer(obsUsage~state+grade+gradeNA+race+raceNA+sex+sexNA+spec_speced+specedNA+spec_gifted+spec_esl+eslNA+frl+frlNA+xirt+xirtNA+year+(1|schoolid2),data=studImp,family=binomial))

summary(missMod <- glmer(obsUsage~(state+grade+gradeNA+race+raceNA+sex+sexNA+spec_speced+specedNA+spec_gifted+spec_esl+eslNA+frl+frlNA+xirt+xirtNA)*year+(1|schoolid2),data=studImp,family=binomial))

schoolMiss <- NULL
for(scl in unique(stud$schoolid2)){
 schoolMiss <- rbind(schoolMiss,with(stud,
  c(mean(obsUsage[year==1 & schoolid2==scl]),
    mean(obsUsage[year==2 & schoolid2==scl]))))}
rownames(schoolMiss) <- unique(stud$schoolid2)
schoolMiss <- cbind(schoolMiss,diff=schoolMiss[,1]-schoolMiss[,2])
smallDiff <- rownames(schoolMiss)[!is.na(schoolMiss[,'diff']) & abs(schoolMiss[,'diff'])<0.4 & schoolMiss[,1]>0& schoolMiss[,2]>0]



