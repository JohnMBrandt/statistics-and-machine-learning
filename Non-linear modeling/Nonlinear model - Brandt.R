## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ---- eval=FALSE---------------------------------------------------------
## 
## lme(weight ~ chestGSc + lengthSc + headWSc + ageNA +
##       splinepoints_chest + splinepoints_length,
##             random = ~ 1 | name/age,
##             weights=varExp(form=~as.vector(chestGSc + splinepoints_chest)),
##             correlation=corCAR1(form = ~ age | name))

## ---- message=FALSE, warning=FALSE---------------------------------------
require(pacman)
p_load(ggplot2, nlme, lme4, reshape2, dplyr, car)

bears <- read.fwf("Bears.full.dat", skip=20,
      col.names=c("id", "age", "month", "sex", "headL", "headW",
                 "neckG", "length", "chestG", "weight", "obs", "name"),
      widths=c(9, 7, 8, 8, 8, 8, 8, 8, 8, 8, 8, 10), 
      strip.white=T, na.string = "")

## ------------------------------------------------------------------------
bears$name <- as.character(bears$name)
bears[87,12] <- "Unnamed1"
bears[88,12] <- "Unnamed2"
bears$name <- as.factor(bears$name)
bears$month <- as.factor(bears$month)
bears$sex <- as.factor(bears$sex)
attach(bears)

## ------------------------------------------------------------------------
cor(weight, bears[,5:9])

## ---- message=FALSE, warning=FALSE---------------------------------------
bearsplotdata <- bears[,0:12]
bearsplotdata <- bearsplotdata[,-c(1,4,11,12)]
bearsplotdata <- melt(bearsplotdata, id.vars=c("weight"))

bearsplot1 <- ggplot(data=bearsplotdata, aes(y=weight, x=value))+
  theme_bw()+
  geom_point()+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        strip.background = element_blank(),
        panel.grid.minor = element_blank()) + 
  facet_wrap(~variable, ncol=3, scales="free_x")
print(bearsplot1)

## ------------------------------------------------------------------------
bears$lengthtransf <- 1/bears$length
bears$chestGtransf <- bears$chestG^2
bears$neckGtransf <- bears$neckG^2


## ------------------------------------------------------------------------
bears$headLSc <- scale(bears$headL)
bears$neckGSctransf <- scale(bears$neckGtransf)
bears$neckGSc <- scale(bears$neckG)
bears$headWSc <- scale(bears$headW)
bears$lengthSc <- scale(bears$length)
bears$lengthSctransf <- scale(bears$lengthtransf)
bears$chestGSc <- scale(bears$chestG)
bears$chestGSctransf <- scale(bears$chestGtransf)

## ------------------------------------------------------------------------
bears$ageNA <- 0
bears$ageNA[is.na(bears$age)] <- 1

summary(lm(bears$age[bears$ageNA==0] ~ bears$headWSc[bears$ageNA==0]))
meanage <- mean(bears$age[bears$ageNA==0])
bearschange <- bears$headWSc[bears$ageNA==1]*21.532

bears$age[bears$ageNA==1] <- meanage + bearschange
bears$age <- jitter(bears$age, amount=0.0001)

mean(bears$age[bears$ageNA==1])
mean(bears$age[bears$ageNA==0])

cor(bears$chestG[bears$ageNA==0], bears$age[bears$ageNA==0])
cor(bears$chestG[bears$ageNA==1], bears$age[bears$ageNA==1])
bears$agescale <- scale(bears$age)

## ------------------------------------------------------------------------
lm1 <- lm(weight ~ chestGSctransf + sex + neckGSctransf + lengthSctransf + headLSc 
          + headWSc, data=bears)

summary(lm1)

AIC(lm1)


## ------------------------------------------------------------------------
vif(lm1)

## ------------------------------------------------------------------------
cor(bears$chestGSctransf, bears$neckGSctransf)

## ------------------------------------------------------------------------
cor(bears$chestGSctransf, bears$weight)
cor(bears$neckGSctransf, bears$weight)

## ------------------------------------------------------------------------
lme_init <- lme(weight ~ chestGSctransf + lengthSctransf + headWSc +  headLSc,
            random = ~ 1 | name/age, data=bears)

## ------------------------------------------------------------------------
lme.power <- lme(weight ~ chestGSctransf + lengthSctransf + headWSc,
            random = ~ 1 | name/age, 
            weights=varPower(form=~as.vector(lengthSctransf)), data=bears)

lme.exp <- lme(weight ~ chestGSctransf + lengthSctransf + headWSc,
            random = ~ 1 | name/age, 
            weights=varExp(form=~as.vector(lengthSctransf)), data=bears)

## ------------------------------------------------------------------------
AIC(lme.power, lme.exp)

## ------------------------------------------------------------------------
resid <- resid(lme.exp)
residplot <- ggplot(data=bears, aes(y=resid, x=weight))+
  geom_point()+
  geom_hline(yintercept=0, color="blue")+
  theme_bw()

print(residplot)

## ------------------------------------------------------------------------
bearsRM <- bears[-132,]
lme3 <- lme(weight ~ chestGSctransf + lengthSctransf + headWSc, 
            random = ~ 1 |name/age, weights=varExp(form=~as.vector(lengthSctransf)), 
            data=bearsRM)

## ------------------------------------------------------------------------
resid3 <- resid(lme3)
residplot3 <- ggplot(data=bearsRM, aes(y=resid3, x=weight))+
  geom_point(size=1.7)+
  geom_hline(yintercept=0, color="blue", size=1.25)+
  theme_bw()

print(residplot3)

## ------------------------------------------------------------------------
plot1 <- ggplot(data=bearsRM, aes(y=weight, x= chestGSc))+
  geom_point()+
  theme_bw()+
  geom_point(aes(y=lme3$fitted[,1], x=chestGSc), color="blue")

print(plot1)


## ------------------------------------------------------------------------
fitted3 <- fitted(lme3)
fittedvsactual <- ggplot(data=bearsRM, aes(y=fitted3, x=weight))+
  theme_bw()+
  geom_point(size=2.3)+
  geom_smooth(method="lm", se=FALSE)
print(fittedvsactual)

## ------------------------------------------------------------------------


gls1 <- gls(weight ~ chestGSctransf + lengthSctransf + headWSc,
            weights=varExp(form=~as.vector(lengthSctransf)), data=bearsRM)

gls2 <- gls(weight ~ chestGSctransf + lengthSctransf + headWSc + ageNA,
            weights=varExp(form=~as.vector(lengthSctransf)),
            correlation=corCAR1(form = ~ age |name), data=bearsRM)

gls3 <- gls(weight ~ chestGSctransf + lengthSctransf + headWSc,
            weights=varExp(form=~as.vector(lengthSctransf)),
            correlation=corCAR1(form = ~age | name), data=bearsRM)

## ------------------------------------------------------------------------
AIC(gls1, gls2, gls3)

## ------------------------------------------------------------------------
resid.gls <- resid(gls2)
residplot.gls <- ggplot(data=bearsRM, aes(y=resid.gls, x=weight))+
  geom_point(size=1.7)+
  geom_hline(yintercept=0, color="blue", size=1.25)+
  theme_bw()

print(residplot.gls)

## ------------------------------------------------------------------------
natural.spline.comp<-function(X,t,j) {
  k<-length(t)
  pmax(0,X-t[j])^3-pmax(0,X-t[k-1])^3*(t[k]-t[j])/(t[k]-t[k-1])+
    pmax(0,X-t[k])^3*(t[k-1]-t[j])/(t[k]-t[k-1])
}

## ---- message=FALSE, warning=FALSE---------------------------------------
attach(bearsRM)
knots <- quantile(chestGSc, probs=c(0.10,0.50,0.90), na.rm=TRUE) 
knots_2 <- quantile(chestGSc, probs=c(0.05,0.30,0.90), na.rm=TRUE) 
knots_3 <- quantile(chestGSc, probs=c(0.07,0.25,0.90), na.rm=TRUE) 
knots_4 <- quantile(chestGSc, probs=c(0.10,0.35,0.75), na.rm=TRUE) 

splinepoints <- natural.spline.comp(chestGSc, knots, 1)
splinepoints_2 <- natural.spline.comp(chestGSc, knots_2, 1)
splinepoints_3 <- natural.spline.comp(chestGSc, knots_3, 1)
splinepoints_4 <- natural.spline.comp(chestGSc, knots_4, 1)

## ------------------------------------------------------------------------
gls_spline1 <- gls(weight ~ chestGSc + lengthSctransf + headWSc + ageNA + splinepoints,
            weights=varExp(form=~as.vector(lengthSctransf)),
            correlation=corCAR1(form = ~ age |name), data=bearsRM)

gls_spline2 <- gls(weight ~ chestGSc + lengthSctransf + headWSc + ageNA + splinepoints_2,
            weights=varExp(form=~as.vector(lengthSctransf)),
            correlation=corCAR1(form = ~ age |name), data=bearsRM)

gls_spline3 <- gls(weight ~ chestGSc + lengthSctransf + headWSc + ageNA + splinepoints_3,
            weights=varExp(form=~as.vector(lengthSctransf)),
            correlation=corCAR1(form = ~ age |name), data=bearsRM)

gls_spline4 <- gls(weight ~ chestGSc + lengthSctransf + headWSc + ageNA + splinepoints_4,
            weights=varExp(form=~as.vector(lengthSctransf)),
            correlation=corCAR1(form = ~ age |name), data=bearsRM)

## ------------------------------------------------------------------------
AIC(gls_spline1 ,gls_spline2 ,gls_spline3 ,gls_spline4)

## ------------------------------------------------------------------------
knots_l_1 <- quantile(lengthSc, probs=c(0.10,0.50,0.90), na.rm=TRUE) 
knots_l_2 <- quantile(lengthSc, probs=c(0.05,0.30,0.90), na.rm=TRUE) 
knots_l_3 <- quantile(lengthSc, probs=c(0.07,0.25,0.90), na.rm=TRUE) 
knots_l_4 <- quantile(lengthSc, probs=c(0.10,0.35,0.75), na.rm=TRUE) 

splinepoints_l_1 <- natural.spline.comp(lengthSc, knots, 1)
splinepoints_l_2 <- natural.spline.comp(lengthSc, knots_2, 1)
splinepoints_l_3 <- natural.spline.comp(lengthSc, knots_3, 1)
splinepoints_l_4 <- natural.spline.comp(lengthSc, knots_4, 1)

## ------------------------------------------------------------------------
gls_spline_l_1 <- gls(weight ~ chestGSc + lengthSc + headWSc +
                     ageNA + splinepoints_3 + splinepoints_l_1,
            weights=varExp(form=~as.vector(lengthSctransf)),
            correlation=corCAR1(form = ~ age |name), data=bearsRM)

gls_spline_l_2 <- gls(weight ~ chestGSc + lengthSc + headWSc +
                     ageNA + splinepoints_3 + splinepoints_l_2,
            weights=varExp(form=~as.vector(lengthSctransf)),
            correlation=corCAR1(form = ~ age |name), data=bearsRM)

gls_spline_l_3 <- gls(weight ~ chestGSc + lengthSc + headWSc +
                     ageNA + splinepoints_3 + splinepoints_l_3,
            weights=varExp(form=~as.vector(lengthSctransf)),
            correlation=corCAR1(form = ~ age |name), data=bearsRM)

gls_spline_l_4 <- gls(weight ~ chestGSc + lengthSc + headWSc +
                     ageNA + splinepoints_3 + splinepoints_l_4,
            weights=varExp(form=~as.vector(lengthSctransf)),
            correlation=corCAR1(form = ~ age |name), data=bearsRM)

## ------------------------------------------------------------------------
AIC(gls_spline_l_1, gls_spline_l_2, gls_spline_l_3, gls_spline_l_4)

## ------------------------------------------------------------------------
gls_spline_opt1 <- gls(weight ~ chestGSc + lengthSc + headWSc +
                     ageNA + splinepoints_3 + splinepoints_l_4,
            weights=varExp(form=~as.vector(splinepoints_3)),
            correlation=corCAR1(form = ~ age | name), data=bearsRM)

gls_spline_opt2 <- gls(weight ~ chestGSc + lengthSc + headWSc +
                     ageNA + splinepoints_3 + splinepoints_l_4,
            weights=varExp(form=~as.vector(splinepoints_l_4)),
            correlation=corCAR1(form = ~ age | name), data=bearsRM)

gls_spline_opt3 <- gls(weight ~ chestGSc + lengthSc + headWSc +
                     ageNA + splinepoints_3 + splinepoints_l_4,
            weights=varExp(form=~as.vector(chestGSc + splinepoints_3)),
            correlation=corCAR1(form = ~ age | name), data=bearsRM)

gls_spline_opt4 <- gls(weight ~ chestGSc + lengthSc + headWSc +
                     ageNA + splinepoints_3 + splinepoints_l_3,
            weights=varExp(form=~as.vector(lengthSc + splinepoints_l_3)),
            correlation=corCAR1(form = ~ age | name), data=bearsRM)

AIC(gls_spline_opt1, gls_spline_opt2, gls_spline_opt3, gls_spline_opt4)

gls_spline <- gls_spline_opt3

## ---- message=FALSE, warning=FALSE---------------------------------------
lme_final <- lme(weight ~ chestGSc + lengthSc + headWSc +
                     ageNA + splinepoints_3 + splinepoints_l_4, 
            random = ~ 1 |name/age,
            weights=varExp(form=~as.vector(chestGSc + splinepoints_3)),
            correlation=corCAR1(form = ~ age | name), data=bearsRM)
            

## ------------------------------------------------------------------------
cat(" AIC LM         ", round(AIC(lm1),2),
    "\n", "AIC GLS        ", round(AIC(gls2),2), "\n",
    "AIC GLS spline ", round(AIC(gls_spline), 2), "\n",
    "AIC LME spline ", round(AIC(lme_final),2),
    "\n")

## ------------------------------------------------------------------------
resid_lm <- resid(lm1)
resid_gls_spline <- resid(gls_spline)
resid_gls <- resid(gls2)
resid_lme <- resid(lme_final)

residplot_lm <- ggplot(data=bears, aes(y=resid_lm, x=weight))+
  geom_point()+
  geom_hline(yintercept=0, color="blue")+
  theme_bw()+
  ggtitle("Linear model")

residplot_gls_spline <- ggplot(data=bearsRM, aes(y=resid_gls_spline, x=weight))+
  geom_point()+
  geom_hline(yintercept=0, color="blue")+
  theme_bw()+
  ggtitle("GLS - cubic splines")

residplot_gls <- ggplot(data=bearsRM, aes(y=resid_gls, x=weight))+
  geom_point()+
  geom_hline(yintercept=0, color="blue")+
  theme_bw()+
  ggtitle("GLS")

residplot_lme <- ggplot(data=bearsRM, aes(y=resid_lme, x=weight))+
  geom_point()+
  geom_hline(yintercept=0, color="blue")+
  theme_bw()+
  ggtitle("LME - spline")

multiplot(residplot_lm, residplot_gls_spline, residplot_gls, residplot_lme, cols=2)


## ------------------------------------------------------------------------
fitted_lm <- fitted(lm1)
fitted_gls_spline <- fitted(gls_spline)
fitted_gls <- fitted(gls2)
fitted_lme <- fitted(lme_final)

fitted_plot_lm <- ggplot(data=bears, aes(y=fitted_lm, x=weight))+
  theme_bw()+
  geom_point(size=1.7)+
  geom_smooth(method="lm", se=FALSE)+
  xlab("")+
  ylab("Fitted weight")+
  ggtitle("Linear model")+
  theme(panel.grid.minor=element_blank())

fitted_plot_gls_spline <- ggplot(data=bearsRM, aes(y=fitted_gls_spline, x=weight))+
  theme_bw()+
  geom_point(size=1.7)+
  geom_smooth(method="lm", se=FALSE)+
  ggtitle("GLS - spline")+
  xlab("Observed weight")+
  ylab("Fitted weight")+
  theme(panel.grid.minor=element_blank())

fitted_plot_gls <- ggplot(data=bearsRM, aes(y=fitted_gls, x=weight))+
  theme_bw()+
  geom_point(size=1.7)+
  geom_smooth(method="lm", se=FALSE)+
  ggtitle("GLS")+
  xlab("")+
  ylab("")+
  theme(panel.grid.minor=element_blank())

fitted_plot_lme <- ggplot(data=bearsRM, aes(y=fitted_lme, x=weight))+
  theme_bw()+
  geom_point(size=1.7)+
  geom_smooth(method="lm", se=FALSE)+
  ggtitle("LME - spline")+
  ylab("")+
  xlab("Observed weight")+
  theme(panel.grid.minor=element_blank())

multiplot(fitted_plot_lm, fitted_plot_gls_spline, fitted_plot_gls, fitted_plot_lme, cols=2)


## ------------------------------------------------------------------------
RSS_lm <- sum(resid(lm1)^2)
RSS_gls <- sum(resid(gls2)^2)
RSS_gls_spline <- sum(resid(gls_spline)^2)
RSS_lme_spline <- sum(resid(lme_final)^2)

cat(" RSS LM         ", round(RSS_lm,2),
    "\n", "RSS GLS        ", round(RSS_gls,2), "\n",
    "RSS GLS spline ", round(RSS_gls_spline, 2), "\n",
    "RSQ LME spline ", round(RSS_lme_spline,2),
    "\n")



## ---- eval=FALSE---------------------------------------------------------
## 
## lme(weight ~ chestGSc + lengthSc + headWSc + ageNA +
##       splinepoints_chest + splinepoints_length,
##             random = ~ 1 | name/age,
##             weights=varExp(form=~as.vector(chestGSc + splinepoints_chest)),
##             correlation=corCAR1(form = ~ age | name))

## ------------------------------------------------------------------------
AIC(lme_final)
(RSS_lme_spline <- sum(resid(lme_final)^2))

## ------------------------------------------------------------------------
fitted_plot_lme <- ggplot(data=bearsRM, aes(y=fitted_lme, x=weight))+
  theme_bw()+
  geom_point(size=2.3)+
  geom_smooth(method="lm", se=FALSE)+
  ggtitle("Fitted vs observed")

residplot_lme <- ggplot(data=bearsRM, aes(y=resid_lme, x=weight))+
  geom_point()+
  geom_hline(yintercept=0, color="blue")+
  theme_bw()+
  ggtitle("Residuals")

multiplot(residplot_lme, fitted_plot_lme)

## ------------------------------------------------------------------------
summary(lme_final)

