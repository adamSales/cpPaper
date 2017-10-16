library(dplyr)
library(lme4)
library(splines)
load('cpPaper.RData')



cpDat <- data%>%filter(unit%in%unique(data$unit[data$curriculum=='Algebra I']))%>%group_by(field_id,unit,section,classid2,state)%>%summarize(status=max(status,na.rm=TRUE),startDate=min(date,na.rm=TRUE))
cpDat <- merge(cpDat,stud,all.x=TRUE,all.y=FALSE)

cpDat$month <- months(cpDat$startDate,TRUE)

cpDat$cp <- cpDat$status=='changed placement'
cpDat$mast <- cpDat$status=='graduated'



### take out classrooms, units, with no CPs
cpDatFull <- cpDat
cpDat <- subset(cpDat,unit %in% unique(cpDat$unit[cpDat$status=='changed placement']))
cpDat <- subset(cpDat, classid2 %in% unique(cpDat$classid2[cpDat$status=='changed placement']))
cpDat <- subset(cpDat,unit %in% unique(cpDat$unit[cpDat$status=='changed placement']))
cpDat <- subset(cpDat, classid2 %in% unique(cpDat$classid2[cpDat$status=='changed placement']))

cpDat$late <- cpDat$month%in%c('Jan','Feb','March','April')


cpMod <- glmer(cp~year*(xirt+late)+spec_gifted+spec_speced+spec_esl+state+(xirt+late|classid2)+(1|field_id)+(late|unit),data=cpDat,family=binomial)

cpModSimp <- glmer(cp~ year*ns(xirt,5)+(1|field_id),data=cpDat,family=binomial)

cpModSt <- list()
for(st in c('TX','KY','MI','LA','CT')){
    cpModSt[[st]] <- glmer(cp~ year*ns(xirt,5)+(1|field_id),data=subset(cpDat,state==st),family=binomial)
}
save(cpModSt,cpModSimp,file='cpXirtMods.RData')

plot(cpDat$xirt,predict(cpModSimp,cpDat,allow.new.levels=TRUE,re.form=~0,type='response'),col=cpDat$year)
legend('topleft',legend=c('year 1','year 2'),col=1:2,pch=1)

### mastMod
mastMod <- glmer(mast ~ year * (ns(xirt, 5) + late) + spec_gifted + spec_speced +
    spec_esl + state + (ns(xirt, 5) + late | classid2) + (1 |
    field_id) + (late | unit),data=cpDat,family=binomial)

mastModSimp <- glmer(mast~ year*ns(xirt,5)+(1|field_id),data=cpDat,family=binomial)

plot(cpDat$xirt,predict(mastModSimp,cpDat,allow.new.levels=TRUE,re.form=~0,type='response'),col=cpDat$year)
legend('topleft',legend=c('year 1','year 2'),col=1:2,pch=1)


### variance components
varCompMod <- glmer(cp~(1|field_id)+(1|classid2)+(1|schoolid2)+state,family=binomial,data=cpDatFull)

vcModYr <- list(yr1=glmer(cp~(1|field_id)+(1|classid2)+(1|schoolid2)+(1|state)+(1|unit),family=binomial,data=subset(cpDatFull,year==1)),yr2=glmer(cp~(1|field_id)+(1|classid2)+(1|schoolid2)+(1|state)+(1|unit),family=binomial,data=subset(cpDatFull,year==2)))

vcMods <- list()
for(st in c('TX','KY','MI'))
 for(yr in c(1,2))
     vcMods[[paste0(st,'_',yr)]] <- glmer(cp~(1|field_id)+(1|classid2)+(1|schoolid2)+(1|unit),family=binomial,data=subset(cpDatFull,state==st & year==yr))

save(vcMods,vcModYr,file='vcMods.RData')


### post-test scores
### do this one in jags
library(R2jags)

cpDat <- cpDat[order(cpDat$field_id),]
cpDat <- cpDat[!is.na(cpDat$section) & !is.na(cpDat$cp),]

cpDat <- droplevels(cpDat)

jagsDat <- list()
jagsDat$cp <- cpDat$cp
jagsDat$stud <- as.numeric(as.factor(cpDat$field_id))
jagsDat$section <- as.numeric(as.factor(cpDat$section))


jagsDat$nWS <- nrow(cpDat)


### want imputed xirt
h1 <- read.csv('~/Box Sync/CT/data/RANDstudyData/H1_algebra_rcal_20121119_fieldid.csv')
h2 <- read.csv('~/Box Sync/CT/data/RANDstudyData/H2_algebra_rcal_20121119_fieldid.csv')

h2 <- h2[!h2$field_id%in%h1$field_id,]

stud <- rbind(h1[,c(2:56,174:194)],h2[,c(2:56,176:196)])
stud <- stud[stud$field_id%in%cpDat$field_id,]
stud <- stud[order(stud$field_id),]
stud$pretest <- rowMeans(stud[,grep('Exirt2_',names(stud))],na.rm=TRUE)
## mode imputation:
for(vv in c('race','sex','grade','spec_speced','spec_gifted','spec_esl','frl')){
    stud[[vv]][is.na(stud[[vv]])] <- names(which.max(table(stud[[vv]])))
}
stud <- droplevels(stud)


jagsDat$nWS <- nrow(cpDat)
jagsDat$X <- model.matrix(~year+ns(pretest,5)+race+sex+grade+spec_speced+spec_gifted+spec_esl+frl,data=stud)
jagsDat$year <- cpDat$year
jagsDat$class <- as.numeric(as.factor(stud$classid2))
jagsDat$school <- as.numeric(as.factor(stud$schoolid2))
jagsDat$state <- as.numeric(as.factor(stud$state))
jagsDat$nstud <- nrow(stud)
jagsDat$ncls <- max(jagsDat$class)
jagsDat$nscl <- max(jagsDat$school)
jagsDat$nst <- max(jagsDat$state)
jagsDat$nsec <- max(jagsDat$section)
with(jagsDat,stopifnot( ncls==length(unique(class)) & nscl==length(unique(school)) & nst==length(unique(state)) & nsec==length(unique(section))))
jagsDat$ncov <- ncol(jagsDat$X)


## make sure student id's match up right
for(i in 1:jagsDat$nstud)
    if(cpDat$field_id[jagsDat$stud==i][1]!=stud$field_id[i]) cat(i,' ')

mod <- jags(jagsDat,parameters=c('alpha0','ability','difficulty','classEffAb','schoolEffAb','stateEffAb','beta0','beta','beta.cp','classEffY','schoolEffY','stateEffY','sigAb','sigStd','sigClsAb','sigClsY','sigSclAb1','sigSclAb2','sigStAb1','sigStAb2','sigSclY1','sigSclY2','sigStY1','sigStY2'),model.file='cpMod.jags',n.chains=4,n.iter=10000,n.thin=10)
save(mod,file='posttestMod.RData')

### does failing to master one section increase the odds of failing to master the next?
cpNext <-
