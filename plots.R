
# plots
# |==================================================|

# distributions of slopes
# |==================================================|
# use the results from models controlling for parental SES 

dat1 <- read.table(text = "y x mean_slope sd_slope
ach ADHD-PGS -0.072 .043
ach Inattention -0.23 .082
ach Hyperactivity -0.09 .071", header = TRUE)

densities1 <- as.data.frame(apply(dat1[, c(-1:-2)], 1, function(x) rnorm(n = 2500, mean = x[1], sd = x[2])))

colnames(densities1) <- c('Within-family ADHD-PGS',"Inattention Symptoms","Hyperactivity Symptoms")

densities1$Subject<-'Achievement'

#Put into long format
densities.m <- melt(densities1)
densities.m$variable=factor(densities.m$variable)

df <- aggregate(value ~ variable, densities.m, mean)

names(df)[2] <- "Mean_eff"
df2<-merge(df, densities.m,by='variable')

df2$variable=factor(df2$variable)
df2$Mean_eff=as.numeric(df2$Mean_eff)
df2$value=as.numeric(df2$value)


theme_set(theme_bw()) 
ggplot(df2, aes(x = value, fill = variable)) +                       
  geom_density(position = "identity", alpha = 0.2)+
  ggtitle("Between-school variation in the effects of ADHD on achievement") +
  xlab("Effect on achievement outcome")+
  xlim(-.5, .25)+
  theme(plot.title = element_text(color="black", size=16, face="bold",hjust = 0.5),
        legend.title=element_text(size=16),
        legend.text=element_text(size=16),
        axis.text=element_text(size=10),
        axis.title=element_text(size=12),
        strip.text = element_text(size = 16))+
  scale_x_continuous(breaks=seq(-0.5,1,.1))+
  guides(fill=guide_legend(title="ADHD variable"))+
  geom_vline(data=df2, aes(xintercept=Mean_eff, color=variable),
             linetype="dashed", show.legend = F)+
  geom_vline(xintercept=0)

# plot variation in school effect by PGS
# |==================================================|
# use the par SES controlled versions for each predictor

# Returns the intercept variance for given value of x
# x = covariate value
# v_u0 = intercept variance at x = 0
# v_u1 = slope variance
# c_u0u1 = intercept-slope covariance at x = 0
v_u0_x = function(x, v_u0, v_u1, c_u0u1) {
  v_u0 + 2 * c_u0u1 * x + v_u1 * x^2 
}

# Returns the intercept-slope correlation for given value of x
# x = covariate value
# v_u0 = intercept variance at x = 0
# v_u1 = slope variance
# c_u0u1 = intercept-slope covariance at x = 0
r_u0u1_x = function(x, v_u0, v_u1, c_u0u1) {
  (c_u0u1 + x * v_u1) / sqrt(v_u0_x(x, v_u0, v_u1, c_u0u1) * v_u1)
}

# Returns the % variance explained by intercept for given value of x
# x = covariate value
# v_u0 = intercept variance at x = 0
# v_u1 = slope variance
# c_u0u1 = intercept-slope covariance at x = 0
# v_e = residual variance
# rc added: v_iid is inidividual variance at x=0
frac_u0_x = function(x, v_u0, v_u1, c_u0u1, v_iid,v_e) {
  var_u0 = v_u0_x(x, v_u0, v_u1, c_u0u1)
  var_u0 / (var_u0 + v_iid+v_e)
}


# get model output
# choose ones with par pgs and ses controls
fit0<-par_ses_int
fit01<-par_ses_hyp
fit02<-par_ses_ADHD_ch_s


VC_est = as.data.frame(VarCorr(fit0))
iid = VC_est$vcov[1]
c11 = VC_est$vcov[2]
c22 = VC_est$vcov[3]
c21 = VC_est$vcov[4]
eps = VC_est$vcov[5]

VC_est1 = as.data.frame(VarCorr(fit01))
iid1 = VC_est1$vcov[1]
c111 = VC_est1$vcov[2]
c221 = VC_est1$vcov[3]
c211 = VC_est1$vcov[4]
eps1 = VC_est1$vcov[5]

VC_est2 = as.data.frame(VarCorr(fit02))
iid2 = VC_est2$vcov[1]
c112 = VC_est2$vcov[2]
c222 = VC_est2$vcov[3]
c212 = VC_est2$vcov[4]
eps2 = VC_est2$vcov[5]



# school effect
curve(frac_u0_x(x, c112, c222, c212, iid2,eps2), 
      from = -4, to = 4,
      ylim=c(0,.1),
      xlab = "",
      ylab = "% Variance in achievement due to school",
      col='red')
curve(frac_u0_x(x, c11, c22, c21, iid,eps), 
      from = -4, to = 4,
      ylim=c(0,.1),
      col='green',
      add=TRUE)
curve(frac_u0_x(x, c111, c221, c211, iid1,eps1), 
      from = -4, to = 4,
      ylim=c(0,.1),
      col='light blue',
      add=TRUE)
legend(x="bottomright",legend=c("Within-family ADHD PGS","Inattention Symptoms", "Hyperactivity Symptoms"),
       col=c("red","green", "light blue"),lty =c(1, 2),box.lty=0)




