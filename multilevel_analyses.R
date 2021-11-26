# Rosa Cheesman
# R Code for ADHD*school interaction analyses in MoBa

library(lme4)
library(reshape2)
library(ggplot2)
library(plyr)

# Phenotypic model fitting
# |==================================================|
# NB data are in long format, with up to 3 rows per child (IID) for grades 5, 8, 9 ('time';coded as -4, -3, 0)
# poeng= points in Norwegian

# inattention
base_int    <- lmer(poeng ~ int+  time+    (1|IID),REML=FALSE,data=data)
sch_int_int <- lmer(poeng ~ int+time+ (1|IID)+(1|school),REML=FALSE,data=data)
sch_slp_int <- lmer(poeng ~ int+time+ (1|IID)+(1+int|school),REML=FALSE,data=data)
anova(base_int,sch_int_int,sch_slp_int)
# hyperactivity
base_hyp    <- lmer(poeng ~ hyp+   time+   (1|IID),REML=FALSE,data=data)
sch_int_hyp <- lmer(poeng ~ hyp+time+ (1|IID)+(1|school),REML=FALSE,data=data)
sch_slp_hyp <- lmer(poeng ~ hyp+time+ (1|IID)+(1+hyp|school),REML=FALSE,data=data)
anova(base_hyp,sch_int_hyp,sch_slp_hyp)

# GxE  model fitting
# |==================================================|
base_g    <- lmer(poeng ~ ADHD_ch_s+ ADHD_par_s+mPC1+mPC2+mPC3+mPC4+mPC5+pPC1+pPC2+pPC3+pPC4+pPC5+ time+(1|IID),REML=FALSE,data=data_2)
sch_int_g <- lmer(poeng ~ ADHD_ch_s+ ADHD_par_s+mPC1+mPC2+mPC3+mPC4+mPC5+pPC1+pPC2+pPC3+pPC4+pPC5+ time+(1|IID)+(1|school),REML=FALSE,data=data_2)
sch_slp_g <- lmer(poeng ~ ADHD_ch_s+ ADHD_par_s+mPC1+mPC2+mPC3+mPC4+mPC5+pPC1+pPC2+pPC3+pPC4+pPC5+ time+(1|IID)+(1+ADHD_ch_s|school),REML=FALSE,data=data_2)
anova(base_g  ,sch_int_g,sch_slp_g)


# test models including parent and school level SES
# |==================================================|
merged_register<-merge(data,grkrets_school_ses,by='w19_0634_lnr',all.x=T )

baseint    <- lmer(poeng ~ int+time+ (1|IID)+(1+int|school),REML=FALSE,data=merged_register)
par_ses_int <- lmer(poeng ~ int+par_earned_income_2014_2017+par_EduYears11_2018+time+ (1|IID)+(1+int|school),REML=FALSE,data=merged_register)
sch_covsint <- lmer(poeng ~ int+par_earned_income_2014_2017+par_EduYears11_2018+sch_avg_pedu10+sch_avg_prank6_9+sch_avg_nonwest+orgnr5ginipeduc10+orgnr5giniprank6_9+time+ (1|IID)+(1+int|school),REML=FALSE,data=merged_register)
sch_covs_slpint<- lmer(poeng ~ int+par_earned_income_2014_2017+par_EduYears11_2018+sch_avg_pedu10+sch_avg_prank6_9+sch_avg_nonwest+orgnr5ginipeduc10+orgnr5giniprank6_9+int*sch_avg_pedu10+int*sch_avg_prank6_9+int*sch_avg_nonwest+int*orgnr5ginipeduc10+int*orgnr5giniprank6_9+time+(1|IID)+(1+int|school),REML=FALSE,data=merged_register)

basehyp    <- lmer(poeng ~ hyp+time+ (1|IID)+(1+hyp|school),REML=FALSE,data=merged_register)
par_ses_hyp <- lmer(poeng ~ hyp+par_earned_income_2014_2017+par_EduYears11_2018+time+ (1|IID)+(1+hyp|school),REML=FALSE,data=merged_register)
sch_covshyp <- lmer(poeng ~ hyp+par_earned_income_2014_2017+par_EduYears11_2018+sch_avg_pedu10+sch_avg_prank6_9+sch_avg_nonwest+orgnr5ginipeduc10+orgnr5giniprank6_9+time+ (1|IID)+(1+hyp|school),REML=FALSE,data=merged_register)
sch_covs_slphyp<- lmer(poeng ~ hyp+par_earned_income_2014_2017+par_EduYears11_2018+sch_avg_pedu10+sch_avg_prank6_9+sch_avg_nonwest+orgnr5ginipeduc10+orgnr5giniprank6_9+hyp*sch_avg_pedu10+hyp*sch_avg_prank6_9+hyp*sch_avg_nonwest+hyp*orgnr5ginipeduc10+hyp*orgnr5giniprank6_9+time+(1|IID)+(1+hyp|school),REML=FALSE,data=merged_register)

baseADHD_ch_s    <- lmer(poeng ~ ADHD_ch_s+ADHD_par_s+mPC1+mPC2+mPC3+mPC4+mPC5+pPC1+pPC2+pPC3+pPC4+pPC5+time+ (1|IID)+(1+ADHD_ch_s|school),REML=FALSE,data=merged_register)
par_ses_ADHD_ch_s <- lmer(poeng ~ ADHD_ch_s+ADHD_par_s+mPC1+mPC2+mPC3+mPC4+mPC5+pPC1+pPC2+pPC3+pPC4+pPC5+par_earned_income_2014_2017+par_EduYears11_2018+time+ (1|IID)+(1+ADHD_ch_s|school),REML=FALSE,data=merged_register)
sch_covsADHD_ch_s <- lmer(poeng ~ ADHD_ch_s+ADHD_par_s+mPC1+mPC2+mPC3+mPC4+mPC5+pPC1+pPC2+pPC3+pPC4+pPC5+par_earned_income_2014_2017+par_EduYears11_2018+sch_avg_pedu10+sch_avg_prank6_9+sch_avg_nonwest+orgnr5ginipeduc10+orgnr5giniprank6_9+time+ (1|IID)+(1+ADHD_ch_s|school),REML=FALSE,data=merged_register)
sch_covs_slpADHD_ch_s<- lmer(poeng ~ ADHD_ch_s+ADHD_par_s+mPC1+mPC2+mPC3+mPC4+mPC5+pPC1+pPC2+pPC3+pPC4+pPC5+par_earned_income_2014_2017+par_EduYears11_2018+sch_avg_pedu10+sch_avg_prank6_9+sch_avg_nonwest+orgnr5ginipeduc10+orgnr5giniprank6_9+ADHD_ch_s*sch_avg_pedu10+ADHD_ch_s*sch_avg_prank6_9+ADHD_ch_s*sch_avg_nonwest+ADHD_ch_s*orgnr5ginipeduc10+ADHD_ch_s*orgnr5giniprank6_9+time+(1|IID)+(1+ADHD_ch_s|school),REML=FALSE,data=merged_register)




# estimate between-school clustering of ADHD
# |==================================================|

clust1<-lmer(int~(1|School), data = iccsss)
clust2<-lmer(hyp~(1|School), data = iccsss)
clust3<-lmer(ADHD_ch_s~(1|School), data = iccsss)
clust4<-lmer(ADHD_par_s~(1|School), data = iccsss)
iccsss$chresid<-resid(lm(ADHD_ch_s~ADHD_par_s, data=iccsss,na.action=na.exclude))
clust5<-lmer(chresid~(1|School), data = iccsss)


