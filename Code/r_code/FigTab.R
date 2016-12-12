library(data.table)
library(reshape)
library(reshape2)
library(gdxrrw)
library(lattice)
library(splitstackshape)
library(plyr)
library(gdxrrw)
library("ggplot2")  
library(plyr)

library(Rmisc)
library(beeswarm)

library(gdxrrw)
library("ggplot2")  

igdx("C:/GAMS/win64/24.4")
setwd("H:/")

datanames <- c("ind1","scen","GCM","SSP","crop", "ind2", "reg", "year")


pivot0<- rgdx.param("H:/Water-SA/gdx/output_SSPs_CCAFS-0.gdx","pivot",names=datanames,compress=T)
pivot1<- rgdx.param("H:/Water-SA/gdx/output_SSPs_CCAFS-1.gdx","pivot",names=datanames,compress=T)
pivot2<- rgdx.param("H:/Water-SA/gdx/output_SSPs_CCAFS-2.gdx","pivot",names=datanames,compress=T)
pivot3<- rgdx.param("H:/Water-SA/gdx/output_SSPs_CCAFS-3.gdx","pivot",names=datanames,compress=T)
pivot4<- rgdx.param("H:/Water-SA/gdx/output_SSPs_CCAFS-4.gdx","pivot",names=datanames,compress=T)
pivot5<- rgdx.param("H:/Water-SA/gdx/output_SSPs_CCAFS-5.gdx","pivot",names=datanames,compress=T)
pivot6<- rgdx.param("H:/Water-SA/gdx/output_SSPs_CCAFS-6.gdx","pivot",names=datanames,compress=T)
pivot7<- rgdx.param("H:/Water-SA/gdx/output_SSPs_CCAFS-7.gdx","pivot",names=datanames,compress=T)
pivot8<- rgdx.param("H:/Water-SA/gdx/output_SSPs_CCAFS-8.gdx","pivot",names=datanames,compress=T)
pivot9<- rgdx.param("H:/Water-SA/gdx/output_SSPs_CCAFS-9.gdx","pivot",names=datanames,compress=T)




pivot<-merge(pivot0,pivot1  ,all.x = TRUE, all.y = TRUE)
pivot<-merge(pivot,	pivot2	,all.x = TRUE, all.y = TRUE)
pivot<-merge(pivot,	pivot3	,all.x = TRUE, all.y = TRUE)
pivot<-merge(pivot,	pivot4	,all.x = TRUE, all.y = TRUE)
pivot<-merge(pivot,	pivot5	,all.x = TRUE, all.y = TRUE)
pivot<-merge(pivot,	pivot6	,all.x = TRUE, all.y = TRUE)
pivot<-merge(pivot,	pivot7	,all.x = TRUE, all.y = TRUE)
pivot<-merge(pivot,	pivot8	,all.x = TRUE, all.y = TRUE)
pivot<-merge(pivot,	pivot9	,all.x = TRUE, all.y = TRUE)

#remove SSP
pivot<-pivot[,-4]

#remove GCM
pivot<-pivot[,-3]

write.csv(pivot,"R_data_figures/pivot_SA.csv", na="")


kcals<-subset(pivot, pivot$ind1=="pckal")

#remove "total" column
kcals<-kcals[,-3]

#remove "-" column
kcals<-kcals[,-3]

#remove "ind" column
kcals<-kcals[,-1]

kcal_val<-kcals$value
kcal_year<-kcals$year
kcal_reg<-kcals$reg
kcal_year_reg<-paste(kcal_year,kcal_reg)

ggplot(data=kcals,  aes(x=kcal_year_reg, kcal_val))+
  geom_point(data=kcals, mapping=aes(x=kcal_year_reg, kcal_val, color = scen)) 


kcals2050<-subset(kcals,kcals$year=="2050")

kcal_val2050<-kcals2050$value

kcal_reg2050<-kcals2050$reg


ggplot(data=kcals2050,  aes(x=scen, kcal_val2050))+
  geom_point(data=kcals2050, mapping=aes(x=scen, kcal_val2050, color = reg)) 

ggplot(data=kcals2050,  aes(x=kcal_reg2050, kcal_val2050))+
  geom_point(data=kcals2050, mapping=aes(x=kcal_reg2050, kcal_val2050, color = scen)) 

write.csv(kcals2050,"R_data_figures/SA_kcals.csv", na="")

kcal_mate_val<-kcals_mate$val
kcal_mate_reg<-kcals_mate$reg
kcal_mate_mate<-kcals_mate$mate

kcals_mate_display<-paste(kcal_mate_reg,kcal_mate_mate)

kcals_mateSSP<-subset(kcals_mate, kcals_mate$scen=="SSP1" |kcals_mate$scen=="SSP2"|kcals_mate$scen=="SSP3"|kcals_mate$scen=="SSP4"|kcals_mate$scen=="SSP5")
kcal_mate_valSSP<-kcals_mateSSP$val
kcal_mate_regSSP<-kcals_mateSSP$reg
kcal_mate_mateSSP<-kcals_mateSSP$mate
kcals_mate_displaySSP<-paste(kcal_mate_regSSP,kcal_mate_mateSSP)

kcals_mateCCAFS<-subset(kcals_mate, kcals_mate$scen=="Jugaad" |kcals_mate$scen=="NewUSA"|kcals_mate$scen=="PeoplePower"|kcals_mate$scen=="UnstFlourish"|kcals_mate$scen=="Precipice")
kcal_mate_valCCAFS<-kcals_mateCCAFS$val
kcal_mate_regCCAFS<-kcals_mateCCAFS$reg
kcal_mate_mateCCAFS<-kcals_mateCCAFS$mate

kcals_mate_displayCCAFS<-paste(kcal_mate_regCCAFS,kcal_mate_mateCCAFS)



ggplot(data=kcals_mate,  aes(x=kcals_mate_display, kcal_mate_val))+
#  geom_point(data=kcals_mate, mapping=aes(x=kcals_mate_display, kcal_mate_val, color = scen))+
  geom_point(data=kcals_mateSSP, mapping=aes(x=kcals_mate_displaySSP, kcal_mate_valSSP,  color = kcal_mate_mateSSP), size= 3.5)+
  geom_point(data=kcals_mateCCAFS, mapping=aes(x=kcals_mate_displayCCAFS, kcal_mate_valCCAFS,  shape = kcals_mateCCAFS$scen), size= 3.5)


ggsave("H:/R_data_figures/SA_CCAFS/kcals_SSPs.jpeg")

prod_allcrops<-subset(pivot, pivot$ind1=="Supply" & pivot$crop =="AllCrops")



##### water use

Water_reg_Compare0<- rgdx.param("H:/Water-SA/gdx/output_SSPs_CCAFS-0.gdx","Water_reg_Compare",names=datanames,compress=T)
Water_reg_Compare1<- rgdx.param("H:/Water-SA/gdx/output_SSPs_CCAFS-1.gdx","Water_reg_Compare",names=datanames,compress=T)
Water_reg_Compare2<- rgdx.param("H:/Water-SA/gdx/output_SSPs_CCAFS-2.gdx","Water_reg_Compare",names=datanames,compress=T)
Water_reg_Compare3<- rgdx.param("H:/Water-SA/gdx/output_SSPs_CCAFS-3.gdx","Water_reg_Compare",names=datanames,compress=T)
Water_reg_Compare4<- rgdx.param("H:/Water-SA/gdx/output_SSPs_CCAFS-4.gdx","Water_reg_Compare",names=datanames,compress=T)
Water_reg_Compare5<- rgdx.param("H:/Water-SA/gdx/output_SSPs_CCAFS-5.gdx","Water_reg_Compare",names=datanames,compress=T)
Water_reg_Compare6<- rgdx.param("H:/Water-SA/gdx/output_SSPs_CCAFS-6.gdx","Water_reg_Compare",names=datanames,compress=T)
Water_reg_Compare7<- rgdx.param("H:/Water-SA/gdx/output_SSPs_CCAFS-7.gdx","Water_reg_Compare",names=datanames,compress=T)
Water_reg_Compare8<- rgdx.param("H:/Water-SA/gdx/output_SSPs_CCAFS-8.gdx","Water_reg_Compare",names=datanames,compress=T)
Water_reg_Compare9<- rgdx.param("H:/Water-SA/gdx/output_SSPs_CCAFS-9.gdx","Water_reg_Compare",names=datanames,compress=T)


Water_reg_Compare<-merge(Water_reg_Compare0, Water_reg_Compare1  ,all.x = TRUE, all.y = TRUE)
Water_reg_Compare<-merge(Water_reg_Compare,	Water_reg_Compare2	,all.x = TRUE, all.y = TRUE)
Water_reg_Compare<-merge(Water_reg_Compare,	Water_reg_Compare3	,all.x = TRUE, all.y = TRUE)
Water_reg_Compare<-merge(Water_reg_Compare,	Water_reg_Compare4	,all.x = TRUE, all.y = TRUE)
Water_reg_Compare<-merge(Water_reg_Compare,	Water_reg_Compare5	,all.x = TRUE, all.y = TRUE)
Water_reg_Compare<-merge(Water_reg_Compare,	Water_reg_Compare6	,all.x = TRUE, all.y = TRUE)
Water_reg_Compare<-merge(Water_reg_Compare,	Water_reg_Compare7	,all.x = TRUE, all.y = TRUE)
Water_reg_Compare<-merge(Water_reg_Compare,	Water_reg_Compare8	,all.x = TRUE, all.y = TRUE)
Water_reg_Compare<-merge(Water_reg_Compare,	Water_reg_Compare9	,all.x = TRUE, all.y = TRUE)

write.csv(Water_reg_Compare,"R_data_figures/Water_reg_Compare_SA.csv", na="")

##### production


irracr_datanames<- c("reg","SSP","scen2", "scen" ,"year", "crop", "ind1" , "value")

IRRACR_reg_COMPARE0<- rgdx.param("H:/Water-SA/gdx/output_SSPs_CCAFS-0.gdx","IRRACR_reg_COMPARE",names=irracr_datanames,compress=T)
IRRACR_reg_COMPARE1<- rgdx.param("H:/Water-SA/gdx/output_SSPs_CCAFS-1.gdx","IRRACR_reg_COMPARE",names=irracr_datanames,compress=T)
IRRACR_reg_COMPARE2<- rgdx.param("H:/Water-SA/gdx/output_SSPs_CCAFS-2.gdx","IRRACR_reg_COMPARE",names=irracr_datanames,compress=T)
IRRACR_reg_COMPARE3<- rgdx.param("H:/Water-SA/gdx/output_SSPs_CCAFS-3.gdx","IRRACR_reg_COMPARE",names=irracr_datanames,compress=T)
IRRACR_reg_COMPARE4<- rgdx.param("H:/Water-SA/gdx/output_SSPs_CCAFS-4.gdx","IRRACR_reg_COMPARE",names=irracr_datanames,compress=T)
IRRACR_reg_COMPARE5<- rgdx.param("H:/Water-SA/gdx/output_SSPs_CCAFS-5.gdx","IRRACR_reg_COMPARE",names=irracr_datanames,compress=T)
IRRACR_reg_COMPARE6<- rgdx.param("H:/Water-SA/gdx/output_SSPs_CCAFS-6.gdx","IRRACR_reg_COMPARE",names=irracr_datanames,compress=T)
IRRACR_reg_COMPARE7<- rgdx.param("H:/Water-SA/gdx/output_SSPs_CCAFS-7.gdx","IRRACR_reg_COMPARE",names=irracr_datanames,compress=T)
IRRACR_reg_COMPARE8<- rgdx.param("H:/Water-SA/gdx/output_SSPs_CCAFS-8.gdx","IRRACR_reg_COMPARE",names=irracr_datanames,compress=T)
IRRACR_reg_COMPARE9<- rgdx.param("H:/Water-SA/gdx/output_SSPs_CCAFS-9.gdx","IRRACR_reg_COMPARE",names=irracr_datanames,compress=T)


IRRACR_reg_COMPARE<-merge(IRRACR_reg_COMPARE0,IRRACR_reg_COMPARE1 ,all.x = TRUE, all.y = TRUE)
IRRACR_reg_COMPARE<-merge(IRRACR_reg_COMPARE,	IRRACR_reg_COMPARE2	,all.x = TRUE, all.y = TRUE)
IRRACR_reg_COMPARE<-merge(IRRACR_reg_COMPARE,	IRRACR_reg_COMPARE3	,all.x = TRUE, all.y = TRUE)
IRRACR_reg_COMPARE<-merge(IRRACR_reg_COMPARE,	IRRACR_reg_COMPARE4	,all.x = TRUE, all.y = TRUE)
IRRACR_reg_COMPARE<-merge(IRRACR_reg_COMPARE,	IRRACR_reg_COMPARE5	,all.x = TRUE, all.y = TRUE)
IRRACR_reg_COMPARE<-merge(IRRACR_reg_COMPARE,	IRRACR_reg_COMPARE6	,all.x = TRUE, all.y = TRUE)
IRRACR_reg_COMPARE<-merge(IRRACR_reg_COMPARE,	IRRACR_reg_COMPARE7	,all.x = TRUE, all.y = TRUE)
IRRACR_reg_COMPARE<-merge(IRRACR_reg_COMPARE,	IRRACR_reg_COMPARE8	,all.x = TRUE, all.y = TRUE)
IRRACR_reg_COMPARE<-merge(IRRACR_reg_COMPARE,	IRRACR_reg_COMPARE9	,all.x = TRUE, all.y = TRUE)


write.csv(IRRACR_reg_COMPARE,"R_data_figures/IRRACR_reg_COMPARE_SA.csv", na="")

IrProduction<-subset(IRRACR_reg_COMPARE,IRRACR_reg_COMPARE$ind1=="IRR_Production")
RfProduction<-subset(IRRACR_reg_COMPARE,IRRACR_reg_COMPARE$ind1=="RNFD_Production")
Production<-merge(IrProduction,RfProduction ,all.x = TRUE, all.y = TRUE)

Production<-Production[,-3]


write.csv(Production,"R_data_figures/Production_COMPARE_SA.csv", na="")

prod_val<-prod$val
prod_scen<-prod$scen
prod_mate<-prod$mate



prod_SSP<-subset(prod, prod$scen=="SSP1" |prod$scen=="SSP2"|prod$scen=="SSP3"|prod$scen=="SSP4"|prod$scen=="SSP5")
prod_valSSP<-prod_SSP$val
prod_scenSSP<-prod_SSP$scen
prod_mateSSP<-prod_SSP$mate

prod_CCAFS<-subset(prod, prod$scen=="Jugaad" |prod$scen=="NewUSA"|prod$scen=="PeoplePower"|prod$scen=="UnstFlourish"|prod$scen=="Precipice")
prod_valCCAFS<-prod_CCAFS$val
prod_scenCCAFS<-prod_CCAFS$scen
prod_mateCCAFS<-prod_CCAFS$mate

ggplot(data=prod,  aes(x=prod_mate, prod_val))+
  geom_point(data=prod_SSP, mapping=aes(x=prod_mateSSP, prod_valSSP,  color = prod_mateSSP), size= 3.5)+
  geom_point(data=prod_CCAFS, mapping=aes(x=prod_mateCCAFS, prod_valCCAFS,  shape = prod_scenCCAFS), size= 3.5)


ggsave("H:/R_data_figures/SA_CCAFS/total_production_SSPs.jpeg")


ggplot(data=kcals_mate,  aes(x=kcals_mate_display, kcal_mate_val))+
  #  geom_point(data=kcals_mate, mapping=aes(x=kcals_mate_display, kcal_mate_val, color = scen))+
  geom_point(data=kcals_mateSSP, mapping=aes(x=kcals_mate_displaySSP, kcal_mate_valSSP,  color = kcal_mate_mateSSP), size= 3.5)+
  geom_point(data=kcals_mateCCAFS, mapping=aes(x=kcals_mate_displayCCAFS, kcal_mate_valCCAFS,  shape = kcals_mateCCAFS$scen), size= 3.5)


##### calorie production

yldcals_def<- c("reg","crop","scen2", "scen" ,"scen3", "year", "ind1" , "value")


yldcals_0<- rgdx.param("H:/Water-SA/gdx/output_SSPs_CCAFS_CC-0.gdx","yld_cals",names=yldcals_def,compress=T)
yldcals_1<- rgdx.param("H:/Water-SA/gdx/output_SSPs_CCAFS_CC-1.gdx","yld_cals",names=yldcals_def,compress=T)
yldcals_2<- rgdx.param("H:/Water-SA/gdx/output_SSPs_CCAFS_CC-2.gdx","yld_cals",names=yldcals_def,compress=T)
yldcals_3<- rgdx.param("H:/Water-SA/gdx/output_SSPs_CCAFS_CC-3.gdx","yld_cals",names=yldcals_def,compress=T)
yldcals_4<- rgdx.param("H:/Water-SA/gdx/output_SSPs_CCAFS_CC-4.gdx","yld_cals",names=yldcals_def,compress=T)
yldcals_5<- rgdx.param("H:/Water-SA/gdx/output_SSPs_CCAFS_CC-5.gdx","yld_cals",names=yldcals_def,compress=T)
yldcals_6<- rgdx.param("H:/Water-SA/gdx/output_SSPs_CCAFS_CC-6.gdx","yld_cals",names=yldcals_def,compress=T)
yldcals_7<- rgdx.param("H:/Water-SA/gdx/output_SSPs_CCAFS_CC-7.gdx","yld_cals",names=yldcals_def,compress=T)
yldcals_8<- rgdx.param("H:/Water-SA/gdx/output_SSPs_CCAFS_CC-8.gdx","yld_cals",names=yldcals_def,compress=T)
yldcals_9<- rgdx.param("H:/Water-SA/gdx/output_SSPs_CCAFS_CC-9.gdx","yld_cals",names=yldcals_def,compress=T)
yldcals_10<- rgdx.param("H:/Water-SA/gdx/output_SSPs_CCAFS_CC-10.gdx","yld_cals",names=yldcals_def,compress=T)
yldcals_11<- rgdx.param("H:/Water-SA/gdx/output_SSPs_CCAFS_CC-11.gdx","yld_cals",names=yldcals_def,compress=T)
yldcals_12<- rgdx.param("H:/Water-SA/gdx/output_SSPs_CCAFS_CC-12.gdx","yld_cals",names=yldcals_def,compress=T)
yldcals_13<- rgdx.param("H:/Water-SA/gdx/output_SSPs_CCAFS_CC-13.gdx","yld_cals",names=yldcals_def,compress=T)
yldcals_14<- rgdx.param("H:/Water-SA/gdx/output_SSPs_CCAFS_CC-14.gdx","yld_cals",names=yldcals_def,compress=T)
yldcals_15<- rgdx.param("H:/Water-SA/gdx/output_SSPs_CCAFS_CC-15.gdx","yld_cals",names=yldcals_def,compress=T)
yldcals_16<- rgdx.param("H:/Water-SA/gdx/output_SSPs_CCAFS_CC-16.gdx","yld_cals",names=yldcals_def,compress=T)
yldcals_17<- rgdx.param("H:/Water-SA/gdx/output_SSPs_CCAFS_CC-17.gdx","yld_cals",names=yldcals_def,compress=T)
yldcals_18<- rgdx.param("H:/Water-SA/gdx/output_SSPs_CCAFS_CC-18.gdx","yld_cals",names=yldcals_def,compress=T)
yldcals_19<- rgdx.param("H:/Water-SA/gdx/output_SSPs_CCAFS_CC-19.gdx","yld_cals",names=yldcals_def,compress=T)
yldcals_20<- rgdx.param("H:/Water-SA/gdx/output_SSPs_CCAFS_CC-20.gdx","yld_cals",names=yldcals_def,compress=T)
yldcals_21<- rgdx.param("H:/Water-SA/gdx/output_SSPs_CCAFS_CC-21.gdx","yld_cals",names=yldcals_def,compress=T)
yldcals_22<- rgdx.param("H:/Water-SA/gdx/output_SSPs_CCAFS_CC-22.gdx","yld_cals",names=yldcals_def,compress=T)
yldcals_23<- rgdx.param("H:/Water-SA/gdx/output_SSPs_CCAFS_CC-23.gdx","yld_cals",names=yldcals_def,compress=T)
yldcals_24<- rgdx.param("H:/Water-SA/gdx/output_SSPs_CCAFS_CC-24.gdx","yld_cals",names=yldcals_def,compress=T)
yldcals_25<- rgdx.param("H:/Water-SA/gdx/output_SSPs_CCAFS_CC-25.gdx","yld_cals",names=yldcals_def,compress=T)
yldcals_26<- rgdx.param("H:/Water-SA/gdx/output_SSPs_CCAFS_CC-26.gdx","yld_cals",names=yldcals_def,compress=T)
yldcals_27<- rgdx.param("H:/Water-SA/gdx/output_SSPs_CCAFS_CC-27.gdx","yld_cals",names=yldcals_def,compress=T)
yldcals_28<- rgdx.param("H:/Water-SA/gdx/output_SSPs_CCAFS_CC-28.gdx","yld_cals",names=yldcals_def,compress=T)
yldcals_29<- rgdx.param("H:/Water-SA/gdx/output_SSPs_CCAFS_CC-29.gdx","yld_cals",names=yldcals_def,compress=T)
yldcals_30<- rgdx.param("H:/Water-SA/gdx/output_SSPs_CCAFS_CC-30.gdx","yld_cals",names=yldcals_def,compress=T)
yldcals_31<- rgdx.param("H:/Water-SA/gdx/output_SSPs_CCAFS_CC-31.gdx","yld_cals",names=yldcals_def,compress=T)
yldcals_32<- rgdx.param("H:/Water-SA/gdx/output_SSPs_CCAFS_CC-32.gdx","yld_cals",names=yldcals_def,compress=T)
yldcals_33<- rgdx.param("H:/Water-SA/gdx/output_SSPs_CCAFS_CC-33.gdx","yld_cals",names=yldcals_def,compress=T)


yld_cals<-merge(yldcals_0,yldcals_1 ,all.x = TRUE, all.y = TRUE)
yld_cals<-merge(yld_cals,	yldcals_2	,all.x = TRUE, all.y = TRUE)
yld_cals<-merge(yld_cals,	yldcals_3	,all.x = TRUE, all.y = TRUE)
yld_cals<-merge(yld_cals,	yldcals_4	,all.x = TRUE, all.y = TRUE)
yld_cals<-merge(yld_cals,	yldcals_5	,all.x = TRUE, all.y = TRUE)
yld_cals<-merge(yld_cals,	yldcals_6	,all.x = TRUE, all.y = TRUE)
yld_cals<-merge(yld_cals,	yldcals_7	,all.x = TRUE, all.y = TRUE)
yld_cals<-merge(yld_cals,	yldcals_8	,all.x = TRUE, all.y = TRUE)
yld_cals<-merge(yld_cals,	yldcals_9	,all.x = TRUE, all.y = TRUE)
yld_cals<-merge(yld_cals, yldcals_10 	,all.x = TRUE, all.y = TRUE)
yld_cals<-merge(yld_cals, yldcals_11 	,all.x = TRUE, all.y = TRUE)
yld_cals<-merge(yld_cals,	yldcals_12	,all.x = TRUE, all.y = TRUE)
yld_cals<-merge(yld_cals,	yldcals_13	,all.x = TRUE, all.y = TRUE)
yld_cals<-merge(yld_cals,	yldcals_14	,all.x = TRUE, all.y = TRUE)
yld_cals<-merge(yld_cals,	yldcals_15	,all.x = TRUE, all.y = TRUE)
yld_cals<-merge(yld_cals,	yldcals_16	,all.x = TRUE, all.y = TRUE)
yld_cals<-merge(yld_cals,	yldcals_17	,all.x = TRUE, all.y = TRUE)
yld_cals<-merge(yld_cals,	yldcals_18	,all.x = TRUE, all.y = TRUE)
yld_cals<-merge(yld_cals,	yldcals_19	,all.x = TRUE, all.y = TRUE)
yld_cals<-merge(yld_cals, yldcals_20 	,all.x = TRUE, all.y = TRUE)
yld_cals<-merge(yld_cals, yldcals_21 	,all.x = TRUE, all.y = TRUE)
yld_cals<-merge(yld_cals,	yldcals_22	,all.x = TRUE, all.y = TRUE)
yld_cals<-merge(yld_cals,	yldcals_23	,all.x = TRUE, all.y = TRUE)
yld_cals<-merge(yld_cals,	yldcals_24	,all.x = TRUE, all.y = TRUE)
yld_cals<-merge(yld_cals,	yldcals_25	,all.x = TRUE, all.y = TRUE)
yld_cals<-merge(yld_cals,	yldcals_26	,all.x = TRUE, all.y = TRUE)
yld_cals<-merge(yld_cals,	yldcals_27	,all.x = TRUE, all.y = TRUE)
yld_cals<-merge(yld_cals,	yldcals_28	,all.x = TRUE, all.y = TRUE)
yld_cals<-merge(yld_cals,	yldcals_29	,all.x = TRUE, all.y = TRUE)
yld_cals<-merge(yld_cals, yldcals_30 	,all.x = TRUE, all.y = TRUE)
yld_cals<-merge(yld_cals, yldcals_31 	,all.x = TRUE, all.y = TRUE)
yld_cals<-merge(yld_cals,	yldcals_32	,all.x = TRUE, all.y = TRUE)
yld_cals<-merge(yld_cals,	yldcals_33	,all.x = TRUE, all.y = TRUE)


write.csv(yld_cals,"R_data_figures/yld_cals.csv", na="")


yld_cals_actual<-subset(yld_cals, ind1 == "Yieldcals")

yld_cals_actual<-yld_cals_actual[,-3]
yld_cals_actual<-yld_cals_actual[,-4]
yld_cals_actual<-yld_cals_actual[,-5]


yld_cals_actual_SA<-subset(yld_cals_actual, reg=="SouthAsia")
yld_cals_actual_SA<-yld_cals_actual_SA[,-1]

yld_cals_actual_SA_All<-subset(yld_cals_actual_SA, crop =="ALL")
yld_cals_actual_SA_All<-yld_cals_actual_SA_All[,-1]


yld_cals_actual_SA_All$year <- as.numeric(as.character(yld_cals_actual_SA_All$year))

yld_cals_actual_SA_All<-subset(yld_cals_actual_SA_All, yld_cals_actual_SA_All$year >= 2010)

yld_cals_jugaad_CC<-subset(yld_cals_actual_SA_All, scen=="JugaadGCM1" | scen == "JugaadGCM2"  | scen =="JugaadGCM3"| scen =="JugaadGCM4")
yld_cals_yrs_jugaad<-yld_cals_jugaad_CC$year
yld_cals_value_jugaad<-yld_cals_jugaad_CC$value


yld_cals_newusa_CC<-subset(yld_cals_actual_SA_All, scen=="NewUSAGCM1" | scen == "NewUSAGCM2"  | scen =="NewUSAGCM3"| scen =="NewUSAGCM4")
yld_cals_uf_CC<-subset(yld_cals_actual_SA_All, scen=="UnstFlourishGCM1" | scen == "UnstFlourishGCM2"  | scen =="UnstFlourishGCM3"| scen =="UnstFlourishGCM4")
yld_cals_pp_CC<-subset(yld_cals_actual_SA_All, scen=="PeoplePowerGCM1" | scen == "PeoplePowerGCM2"  | scen =="PeoplePowerGCM3"| scen =="PeoplePowerGCM4")
yld_cals_precipice_CC<-subset(yld_cals_actual_SA_All, scen=="PrecipiceGCM1" | scen == "PrecipiceGCM2"  | scen =="PrecipiceGCM3"| scen =="PrecipiceGCM4")
yld_cals_ssp2_CC<-subset(yld_cals_actual_SA_All, scen=="scenSSP2GCM1" | scen == "scenSSP2GCM2"  | scen =="scenSSP2GCM3"| scen =="scenSSP2GCM4")

yld_cals_noCC<-subset(yld_cals_actual_SA_All, scen=="JugaadNoCC" | scen == "NewUSANoCC"  | scen =="UnstFlourishNoCC"| scen =="PeoplePowerNoCC" | scen=="PrecipiceNoCC"| scen=="scenSSP2")
yld_cals_yrs<-yld_cals_noCC$year
yld_cals_value<-yld_cals_noCC$value

#yld_cals_CC<-subset(yld_cals_actual_SA_All, scen != noccset) "JugaadNoCC" ) ! scen == "NewUSANoCC"  ! scen =="UnstFlourishNoCC"! scen =="PeoplePowerNoCC" ! scen=="PrecipiceNoCC"! scen=="scenSSP2")
#noccset<-c("JugaadNoCC","NewUSANoCC","UnstFlourishNoCC","PeoplePowerNoCC" ,"PrecipiceNoCC","scenSSP2")

yld_cals_CC<-merge(yld_cals_jugaad_CC,yld_cals_newusa_CC ,all.x = TRUE, all.y = TRUE)
yld_cals_CC<-merge(yld_cals_CC,	yld_cals_uf_CC	,all.x = TRUE, all.y = TRUE)
yld_cals_CC<-merge(yld_cals_CC,	yld_cals_pp_CC	,all.x = TRUE, all.y = TRUE)
yld_cals_CC<-merge(yld_cals_CC,	yld_cals_precipice_CC	,all.x = TRUE, all.y = TRUE)
yld_cals_CC<-merge(yld_cals_CC,	yld_cals_ssp2_CC	,all.x = TRUE, all.y = TRUE)

yld_cals_yrs_CC<-yld_cals_CC$year
yld_cals_value_CC<-yld_cals_CC$value

scen<-unique(yld_cals_actual_SA_All$scen)

ggplot(data=yld_cals_noCC,  aes(yld_cals_yrs, yld_cals_value, color=scen, group =scen)) +
  geom_point()+
  geom_line()+
  geom_boxplot(data=yld_cals_CC, aes(yld_cals_yrs_CC, yld_cals_value_CC, group = scen))


ggplot(data=yld_cals_CC, aes(yld_cals_yrs_CC, yld_cals_value_CC, color=scen))+
  geom_boxplot()+
  geom_line(data=yld_cals_noCC,  aes(yld_cals_yrs, yld_cals_value, color=scen, group =scen))+
  geom_point()


#new usa
yld_cals_newusa_CC<-subset(yld_cals_actual_SA_All, scen=="NewUSANoCC" |scen=="NewUSAGCM1" | scen == "NewUSAGCM2"  | scen =="NewUSAGCM3"| scen =="NewUSAGCM4")
yld_cals_newusa_noCC<-subset(yld_cals_actual_SA_All, scen=="NewUSANoCC")

yld_cals_newusa_yrs_CC<-yld_cals_newusa_CC$year
yld_cals_newusa_value_CC<-yld_cals_newusa_CC$value

yld_cals_newusa_yrs_noCC<-yld_cals_newusa_noCC$year
yld_cals_newusa_value_noCC<-yld_cals_newusa_noCC$value

yld_cals_SSP1<-subset(yld_cals_actual_SA_All, scen=="scenSSP1" )

yld_cals_SSP1_yrs<-yld_cals_SSP1$year
yld_cals_SSP1_value<-yld_cals_SSP1$value

years<-unique(yld_cals_newusa_CC$year)

p1<-ggplot(data=yld_cals_newusa_CC, aes(yld_cals_newusa_yrs_CC, yld_cals_newusa_value_CC, group = yld_cals_newusa_yrs_CC)) +
  geom_boxplot()+
  geom_line(data=yld_cals_newusa_noCC,  aes(yld_cals_newusa_yrs_noCC, yld_cals_newusa_value_noCC, color=scen, group =scen))+ 
  geom_point(data=yld_cals_newusa_noCC,  aes(yld_cals_newusa_yrs_noCC, yld_cals_newusa_value_noCC, color=scen, group =scen))+
  geom_line(data=yld_cals_SSP1,  aes(yld_cals_SSP1_yrs, yld_cals_SSP1_value, color=scen, group =scen))+
  ylab ("calories produced per ha") +
  xlab ("year") 
ggsave("H:/R_data_figures/SA_CCAFS/NewUSA.jpeg",width = 9, height = 5)
 

#jugaad
yld_cals_jugaad_CC<-subset(yld_cals_actual_SA_All, scen=="JugaadNoCC" |scen=="JugaadGCM1" | scen == "JugaadGCM2"  | scen =="JugaadGCM3"| scen =="JugaadGCM4")
yld_cals_jugaad_noCC<-subset(yld_cals_actual_SA_All, scen=="JugaadNoCC")

yld_cals_jugaad_yrs_CC<-yld_cals_jugaad_CC$year
yld_cals_jugaad_value_CC<-yld_cals_jugaad_CC$value

yld_cals_jugaad_yrs_noCC<-yld_cals_jugaad_noCC$year
yld_cals_jugaad_value_noCC<-yld_cals_jugaad_noCC$value

yld_cals_SSP3<-subset(yld_cals_actual_SA_All, scen=="scenSSP3" )

yld_cals_SSP3_yrs<-yld_cals_SSP3$year
yld_cals_SSP3_value<-yld_cals_SSP3$value



p2<-ggplot(data=yld_cals_jugaad_CC, aes(yld_cals_jugaad_yrs_CC, yld_cals_jugaad_value_CC, group =yld_cals_jugaad_yrs_CC)) +
  geom_boxplot()+
  geom_line(data=yld_cals_jugaad_noCC,  aes(yld_cals_jugaad_yrs_noCC, yld_cals_jugaad_value_noCC, color=scen, group =scen))+ 
  geom_point(data=yld_cals_jugaad_noCC,  aes(yld_cals_jugaad_yrs_noCC, yld_cals_jugaad_value_noCC, color=scen, group =scen))+
  geom_line(data=yld_cals_SSP3,  aes(yld_cals_SSP3_yrs, yld_cals_SSP3_value, color=scen, group =scen))+
  ylab ("calories produced per ha") +
  xlab ("year") 
ggsave("H:/R_data_figures/SA_CCAFS/jugaad.jpeg",width = 9, height = 5)

###

#unstable flourishing
yld_cals_uf_CC<-subset(yld_cals_actual_SA_All, scen=="UnstFlourishNoCC" |scen=="UnstFlourishGCM1" | scen == "UnstFlourishGCM2"  | scen =="UnstFlourishGCM3"| scen =="UnstFlourishGCM4")
yld_cals_uf_noCC<-subset(yld_cals_actual_SA_All, scen=="UnstFlourishNoCC")

yld_cals_uf_yrs_CC<-yld_cals_uf_CC$year
yld_cals_uf_value_CC<-yld_cals_uf_CC$value

yld_cals_uf_yrs_noCC<-yld_cals_uf_noCC$year
yld_cals_uf_value_noCC<-yld_cals_uf_noCC$value

yld_cals_SSP2<-subset(yld_cals_actual_SA_All, scen=="scenSSP2" )

yld_cals_SSP2_yrs<-yld_cals_SSP2$year
yld_cals_SSP2_value<-yld_cals_SSP2$value



p3<-ggplot(data=yld_cals_uf_CC, aes(yld_cals_uf_yrs_CC, yld_cals_uf_value_CC, group =yld_cals_uf_yrs_CC)) +
  geom_boxplot()+
  geom_line(data=yld_cals_uf_noCC,  aes(yld_cals_uf_yrs_noCC, yld_cals_uf_value_noCC, color=scen, group =scen))+ 
  geom_point(data=yld_cals_uf_noCC,  aes(yld_cals_uf_yrs_noCC, yld_cals_uf_value_noCC, color=scen, group =scen))+
  geom_line(data=yld_cals_SSP2,  aes(yld_cals_SSP2_yrs, yld_cals_SSP2_value, color=scen, group =scen))+
  ylab ("calories produced per ha") +
  xlab ("year") 
ggsave("H:/R_data_figures/SA_CCAFS/uf.jpeg",width = 9, height = 5)


###

#People power
yld_cals_pp_CC<-subset(yld_cals_actual_SA_All, scen=="PeoplePowerNoCC" |scen=="PeoplePowerGCM1" | scen == "PeoplePowerGCM2"  | scen =="PeoplePowerGCM3"| scen =="PeoplePowerGCM4")
yld_cals_pp_noCC<-subset(yld_cals_actual_SA_All, scen=="PeoplePowerNoCC")

yld_cals_pp_yrs_CC<-yld_cals_pp_CC$year
yld_cals_pp_value_CC<-yld_cals_pp_CC$value

yld_cals_pp_yrs_noCC<-yld_cals_pp_noCC$year
yld_cals_pp_value_noCC<-yld_cals_pp_noCC$value

yld_cals_SSP2<-subset(yld_cals_actual_SA_All, scen=="scenSSP2" )

yld_cals_SSP2_yrs<-yld_cals_SSP2$year
yld_cals_SSP2_value<-yld_cals_SSP2$value



p4<-ggplot(data=yld_cals_pp_CC, aes(yld_cals_pp_yrs_CC, yld_cals_pp_value_CC, group =yld_cals_pp_yrs_CC)) +
#  geom_boxplot()+
  geom_line(data=yld_cals_pp_noCC,  aes(yld_cals_pp_yrs_noCC, yld_cals_pp_value_noCC, color=scen, group =scen))+ 
  geom_point(data=yld_cals_pp_noCC,  aes(yld_cals_pp_yrs_noCC, yld_cals_pp_value_noCC, color=scen, group =scen))+
#  geom_boxplot(data=yld_cals_uf_CC, aes(yld_cals_uf_yrs_CC, yld_cals_uf_value_CC, color=scen, group =yld_cals_uf_yrs_CC))+
  geom_line(data=yld_cals_uf_noCC,  aes(yld_cals_uf_yrs_noCC, yld_cals_uf_value_noCC, color=scen, group =scen))+ 
  geom_point(data=yld_cals_uf_noCC,  aes(yld_cals_uf_yrs_noCC, yld_cals_uf_value_noCC, color=scen, group =scen))+
  geom_line(data=yld_cals_SSP2,  aes(yld_cals_SSP2_yrs, yld_cals_SSP2_value, color=scen, group =scen))+
  ylab ("calories produced per ha") +
  xlab ("year") 
ggsave("H:/R_data_figures/SA_CCAFS/pp.jpeg",width = 9, height = 5)

###

#Precipice
yld_cals_precipice_CC<-subset(yld_cals_actual_SA_All, scen=="PrecipiceNoCC" |scen=="PrecipiceGCM1" | scen == "PrecipiceGCM2"  | scen =="PrecipiceGCM3"| scen =="PrecipiceGCM4")
yld_cals_precipice_noCC<-subset(yld_cals_actual_SA_All, scen=="PrecipiceNoCC")

yld_cals_precipice_yrs_CC<-yld_cals_precipice_CC$year
yld_cals_precipice_value_CC<-yld_cals_precipice_CC$value

yld_cals_precipice_yrs_noCC<-yld_cals_precipice_noCC$year
yld_cals_precipice_value_noCC<-yld_cals_precipice_noCC$value

yld_cals_SSP5<-subset(yld_cals_actual_SA_All, scen=="scenSSP5" )

yld_cals_SSP5_yrs<-yld_cals_SSP5$year
yld_cals_SSP5_value<-yld_cals_SSP5$value



p5<-ggplot(data=yld_cals_precipice_CC, aes(yld_cals_precipice_yrs_CC, yld_cals_precipice_value_CC, group=yld_cals_precipice_yrs_CC )) +
  geom_boxplot()+
  geom_line(data=yld_cals_precipice_noCC,  aes(yld_cals_precipice_yrs_noCC, yld_cals_precipice_value_noCC, color=scen, group =scen))+ 
  geom_point(data=yld_cals_precipice_noCC,  aes(yld_cals_precipice_yrs_noCC, yld_cals_precipice_value_noCC, color=scen, group =scen))+
  geom_line(data=yld_cals_SSP5,  aes(yld_cals_SSP5_yrs, yld_cals_SSP5_value, color=scen, group =scen))+
  ylab ("calories produced per ha") +
  xlab ("year") 
ggsave("H:/R_data_figures/SA_CCAFS/precipice.jpeg",width = 9, height = 5)


multiplot(p1, p2, p3, p4, p5, cols=2)
ggsave("H:/R_data_figures/SA_CCAFS/calories_produced_perha_SA.jpeg",width = 9, height = 5)


yld_cals_actual_SA_All_2050<-subset(yld_cals_actual_SA_All, yld_cals_actual_SA_All$year == 2050)
write.csv(yld_cals_actual_SA_All_2050,"R_data_figures/yld_cals_2050_output.csv", na="")

yld_cals_2050_NoCC<-subset(yld_cals_2050, scen=="JugaadNoCC" | scen == "NewUSANoCC"  | scen =="UnstFlourishNoCC"| scen =="PeoplePowerNoCC" | scen=="PrecipiceNoCC")
yld_cals_2050_NoCC_group<-yld_cals_2050_NoCC$short
yld_cals_2050_NoCC_value<-yld_cals_2050_NoCC$value
short<-unique(yld_cals_2050_NoCC$short)

yld_cals_2050<-subset(yld_cals_2050, yld_cals_2050$short != "SSP")

yld_cals_2050_group<-yld_cals_2050$short
yld_cals_2050_value<-yld_cals_2050$value

yld_cals_2000<-subset(yld_cals_actual_SA_All, scen=="JugaadNoCC" | scen == "NewUSANoCC"  | scen =="UnstFlourishNoCC"| scen =="PeoplePowerNoCC" | scen=="PrecipiceNoCC")
yld_cals_2000<-subset(yld_cals_2000, yld_cals_2000$year == 2010)

yld_cals_2000_value<-yld_cals_2000$value
yld_cals_2000_group<-yld_cals_2000$short

ggplot(data=yld_cals_2050_NoCC, aes(yld_cals_2050_NoCC_group, yld_cals_2050_NoCC_value, color= short))+
  geom_point()+
  geom_boxplot(yld_cals_2050, aes(yld_cals_2050_group, yld_cals_2050_value, group = short))


ggplot(yld_cals_2050, aes(yld_cals_2050_group, yld_cals_2050_value, color = short))+
  geom_boxplot()+
   geom_point(data=yld_cals_2050_NoCC, aes(yld_cals_2050_NoCC_group, yld_cals_2050_NoCC_value, color= short), size = 3.5)+
  geom_hline(aes(yintercept=7.25))+
  annotate("text", label = "2010 values", x = "Precipice", y = 7.5, size = 4)+
  ylab ("calories produced per ha") +
  xlab ("Scenario") 
ggsave("H:/R_data_figures/SA_CCAFS/2050_calories_perHa.jpeg",width = 9, height = 5)


 
##### pivot table

pivot_def<- c("ind1" ,"scen", "GCM", "scen2", "crop", "type", "reg", "year",  "value")
pivot_0<- rgdx.param("H:/Water-SA/gdx/output_SSPs_CCAFS_CC-0.gdx","pivot",names=pivot_def,compress=T)
pivot_1<- rgdx.param("H:/Water-SA/gdx/output_SSPs_CCAFS_CC-1.gdx","pivot",names=pivot_def,compress=T)
pivot_2<- rgdx.param("H:/Water-SA/gdx/output_SSPs_CCAFS_CC-2.gdx","pivot",names=pivot_def,compress=T)
pivot_3<- rgdx.param("H:/Water-SA/gdx/output_SSPs_CCAFS_CC-3.gdx","pivot",names=pivot_def,compress=T)
pivot_4<- rgdx.param("H:/Water-SA/gdx/output_SSPs_CCAFS_CC-4.gdx","pivot",names=pivot_def,compress=T)
pivot_5<- rgdx.param("H:/Water-SA/gdx/output_SSPs_CCAFS_CC-5.gdx","pivot",names=pivot_def,compress=T)
pivot_6<- rgdx.param("H:/Water-SA/gdx/output_SSPs_CCAFS_CC-6.gdx","pivot",names=pivot_def,compress=T)
pivot_7<- rgdx.param("H:/Water-SA/gdx/output_SSPs_CCAFS_CC-7.gdx","pivot",names=pivot_def,compress=T)
pivot_8<- rgdx.param("H:/Water-SA/gdx/output_SSPs_CCAFS_CC-8.gdx","pivot",names=pivot_def,compress=T)
pivot_9<- rgdx.param("H:/Water-SA/gdx/output_SSPs_CCAFS_CC-9.gdx","pivot",names=pivot_def,compress=T)
pivot_10<- rgdx.param("H:/Water-SA/gdx/output_SSPs_CCAFS_CC-10.gdx","pivot",names=pivot_def,compress=T)
pivot_11<- rgdx.param("H:/Water-SA/gdx/output_SSPs_CCAFS_CC-11.gdx","pivot",names=pivot_def,compress=T)
pivot_12<- rgdx.param("H:/Water-SA/gdx/output_SSPs_CCAFS_CC-12.gdx","pivot",names=pivot_def,compress=T)
pivot_13<- rgdx.param("H:/Water-SA/gdx/output_SSPs_CCAFS_CC-13.gdx","pivot",names=pivot_def,compress=T)
pivot_14<- rgdx.param("H:/Water-SA/gdx/output_SSPs_CCAFS_CC-14.gdx","pivot",names=pivot_def,compress=T)
pivot_15<- rgdx.param("H:/Water-SA/gdx/output_SSPs_CCAFS_CC-15.gdx","pivot",names=pivot_def,compress=T)
pivot_16<- rgdx.param("H:/Water-SA/gdx/output_SSPs_CCAFS_CC-16.gdx","pivot",names=pivot_def,compress=T)
pivot_17<- rgdx.param("H:/Water-SA/gdx/output_SSPs_CCAFS_CC-17.gdx","pivot",names=pivot_def,compress=T)
pivot_18<- rgdx.param("H:/Water-SA/gdx/output_SSPs_CCAFS_CC-18.gdx","pivot",names=pivot_def,compress=T)
pivot_19<- rgdx.param("H:/Water-SA/gdx/output_SSPs_CCAFS_CC-19.gdx","pivot",names=pivot_def,compress=T)
pivot_20<- rgdx.param("H:/Water-SA/gdx/output_SSPs_CCAFS_CC-20.gdx","pivot",names=pivot_def,compress=T)
pivot_21<- rgdx.param("H:/Water-SA/gdx/output_SSPs_CCAFS_CC-21.gdx","pivot",names=pivot_def,compress=T)
pivot_22<- rgdx.param("H:/Water-SA/gdx/output_SSPs_CCAFS_CC-22.gdx","pivot",names=pivot_def,compress=T)
pivot_23<- rgdx.param("H:/Water-SA/gdx/output_SSPs_CCAFS_CC-23.gdx","pivot",names=pivot_def,compress=T)
pivot_24<- rgdx.param("H:/Water-SA/gdx/output_SSPs_CCAFS_CC-24.gdx","pivot",names=pivot_def,compress=T)
pivot_25<- rgdx.param("H:/Water-SA/gdx/output_SSPs_CCAFS_CC-25.gdx","pivot",names=pivot_def,compress=T)
pivot_26<- rgdx.param("H:/Water-SA/gdx/output_SSPs_CCAFS_CC-26.gdx","pivot",names=pivot_def,compress=T)
pivot_27<- rgdx.param("H:/Water-SA/gdx/output_SSPs_CCAFS_CC-27.gdx","pivot",names=pivot_def,compress=T)
pivot_28<- rgdx.param("H:/Water-SA/gdx/output_SSPs_CCAFS_CC-28.gdx","pivot",names=pivot_def,compress=T)
pivot_29<- rgdx.param("H:/Water-SA/gdx/output_SSPs_CCAFS_CC-29.gdx","pivot",names=pivot_def,compress=T)
pivot_30<- rgdx.param("H:/Water-SA/gdx/output_SSPs_CCAFS_CC-30.gdx","pivot",names=pivot_def,compress=T)
pivot_31<- rgdx.param("H:/Water-SA/gdx/output_SSPs_CCAFS_CC-31.gdx","pivot",names=pivot_def,compress=T)
pivot_32<- rgdx.param("H:/Water-SA/gdx/output_SSPs_CCAFS_CC-32.gdx","pivot",names=pivot_def,compress=T)
pivot_33<- rgdx.param("H:/Water-SA/gdx/output_SSPs_CCAFS_CC-33.gdx","pivot",names=pivot_def,compress=T)


pivot<-merge(pivot_0,pivot_1 ,all.x = TRUE, all.y = TRUE)
pivot<-merge(pivot,	pivot_2	,all.x = TRUE, all.y = TRUE)
pivot<-merge(pivot,	pivot_3	,all.x = TRUE, all.y = TRUE)
pivot<-merge(pivot,	pivot_4	,all.x = TRUE, all.y = TRUE)
pivot<-merge(pivot,	pivot_5	,all.x = TRUE, all.y = TRUE)
pivot<-merge(pivot,	pivot_6	,all.x = TRUE, all.y = TRUE)
pivot<-merge(pivot,	pivot_7	,all.x = TRUE, all.y = TRUE)
pivot<-merge(pivot,	pivot_8	,all.x = TRUE, all.y = TRUE)
pivot<-merge(pivot,	pivot_9	,all.x = TRUE, all.y = TRUE)
pivot<-merge(pivot, pivot_10 ,all.x = TRUE, all.y = TRUE)
pivot<-merge(pivot, pivot_11 ,all.x = TRUE, all.y = TRUE)
pivot<-merge(pivot,	pivot_12	,all.x = TRUE, all.y = TRUE)
pivot<-merge(pivot,	pivot_13	,all.x = TRUE, all.y = TRUE)
pivot<-merge(pivot,	pivot_14	,all.x = TRUE, all.y = TRUE)
pivot<-merge(pivot,	pivot_15	,all.x = TRUE, all.y = TRUE)
pivot<-merge(pivot,	pivot_16	,all.x = TRUE, all.y = TRUE)
pivot<-merge(pivot,	pivot_17	,all.x = TRUE, all.y = TRUE)
pivot<-merge(pivot,	pivot_18	,all.x = TRUE, all.y = TRUE)
pivot<-merge(pivot,	pivot_19	,all.x = TRUE, all.y = TRUE)
pivot<-merge(pivot, pivot_20 ,all.x = TRUE, all.y = TRUE)
pivot<-merge(pivot, pivot_21 ,all.x = TRUE, all.y = TRUE)
pivot<-merge(pivot,	pivot_22	,all.x = TRUE, all.y = TRUE)
pivot<-merge(pivot,	pivot_23	,all.x = TRUE, all.y = TRUE)
pivot<-merge(pivot,	pivot_24	,all.x = TRUE, all.y = TRUE)
pivot<-merge(pivot,	pivot_25	,all.x = TRUE, all.y = TRUE)
pivot<-merge(pivot,	pivot_26	,all.x = TRUE, all.y = TRUE)
pivot<-merge(pivot,	pivot_27	,all.x = TRUE, all.y = TRUE)
pivot<-merge(pivot,	pivot_28	,all.x = TRUE, all.y = TRUE)
pivot<-merge(pivot,	pivot_29	,all.x = TRUE, all.y = TRUE)
pivot<-merge(pivot, pivot_30 ,all.x = TRUE, all.y = TRUE)
pivot<-merge(pivot, pivot_31 ,all.x = TRUE, all.y = TRUE)
pivot<-merge(pivot,	pivot_32	,all.x = TRUE, all.y = TRUE)
pivot<-merge(pivot,	pivot_33	,all.x = TRUE, all.y = TRUE)


write.csv(pivot,"R_data_figures/pivot.csv", na="")



##### supply table


supply_def<- c("crop", "reg", "level" ,"scen2", "scen3", "scen", "type",  "year",  "value")


supply_0<- rgdx.param("H:/Water-SA/gdx/output_SSPs_CCAFS_CC-0.gdx","SupQuantity_Compare",names=supply_def,compress=T)
supply_1<- rgdx.param("H:/Water-SA/gdx/output_SSPs_CCAFS_CC-1.gdx","SupQuantity_Compare",names=supply_def,compress=T)
supply_2<- rgdx.param("H:/Water-SA/gdx/output_SSPs_CCAFS_CC-2.gdx","SupQuantity_Compare",names=supply_def,compress=T)
supply_3<- rgdx.param("H:/Water-SA/gdx/output_SSPs_CCAFS_CC-3.gdx","SupQuantity_Compare",names=supply_def,compress=T)
supply_4<- rgdx.param("H:/Water-SA/gdx/output_SSPs_CCAFS_CC-4.gdx","SupQuantity_Compare",names=supply_def,compress=T)
supply_5<- rgdx.param("H:/Water-SA/gdx/output_SSPs_CCAFS_CC-5.gdx","SupQuantity_Compare",names=supply_def,compress=T)
supply_6<- rgdx.param("H:/Water-SA/gdx/output_SSPs_CCAFS_CC-6.gdx","SupQuantity_Compare",names=supply_def,compress=T)
supply_7<- rgdx.param("H:/Water-SA/gdx/output_SSPs_CCAFS_CC-7.gdx","SupQuantity_Compare",names=supply_def,compress=T)
supply_8<- rgdx.param("H:/Water-SA/gdx/output_SSPs_CCAFS_CC-8.gdx","SupQuantity_Compare",names=supply_def,compress=T)
supply_9<- rgdx.param("H:/Water-SA/gdx/output_SSPs_CCAFS_CC-9.gdx","SupQuantity_Compare",names=supply_def,compress=T)
supply_10<- rgdx.param("H:/Water-SA/gdx/output_SSPs_CCAFS_CC-10.gdx","SupQuantity_Compare",names=supply_def,compress=T)
supply_11<- rgdx.param("H:/Water-SA/gdx/output_SSPs_CCAFS_CC-11.gdx","SupQuantity_Compare",names=supply_def,compress=T)
supply_12<- rgdx.param("H:/Water-SA/gdx/output_SSPs_CCAFS_CC-12.gdx","SupQuantity_Compare",names=supply_def,compress=T)
supply_13<- rgdx.param("H:/Water-SA/gdx/output_SSPs_CCAFS_CC-13.gdx","SupQuantity_Compare",names=supply_def,compress=T)
supply_14<- rgdx.param("H:/Water-SA/gdx/output_SSPs_CCAFS_CC-14.gdx","SupQuantity_Compare",names=supply_def,compress=T)
supply_15<- rgdx.param("H:/Water-SA/gdx/output_SSPs_CCAFS_CC-15.gdx","SupQuantity_Compare",names=supply_def,compress=T)
supply_16<- rgdx.param("H:/Water-SA/gdx/output_SSPs_CCAFS_CC-16.gdx","SupQuantity_Compare",names=supply_def,compress=T)
supply_17<- rgdx.param("H:/Water-SA/gdx/output_SSPs_CCAFS_CC-17.gdx","SupQuantity_Compare",names=supply_def,compress=T)
supply_18<- rgdx.param("H:/Water-SA/gdx/output_SSPs_CCAFS_CC-18.gdx","SupQuantity_Compare",names=supply_def,compress=T)
supply_19<- rgdx.param("H:/Water-SA/gdx/output_SSPs_CCAFS_CC-19.gdx","SupQuantity_Compare",names=supply_def,compress=T)
supply_20<- rgdx.param("H:/Water-SA/gdx/output_SSPs_CCAFS_CC-20.gdx","SupQuantity_Compare",names=supply_def,compress=T)
supply_21<- rgdx.param("H:/Water-SA/gdx/output_SSPs_CCAFS_CC-21.gdx","SupQuantity_Compare",names=supply_def,compress=T)
supply_22<- rgdx.param("H:/Water-SA/gdx/output_SSPs_CCAFS_CC-22.gdx","SupQuantity_Compare",names=supply_def,compress=T)
supply_23<- rgdx.param("H:/Water-SA/gdx/output_SSPs_CCAFS_CC-23.gdx","SupQuantity_Compare",names=supply_def,compress=T)
supply_24<- rgdx.param("H:/Water-SA/gdx/output_SSPs_CCAFS_CC-24.gdx","SupQuantity_Compare",names=supply_def,compress=T)
supply_25<- rgdx.param("H:/Water-SA/gdx/output_SSPs_CCAFS_CC-25.gdx","SupQuantity_Compare",names=supply_def,compress=T)
supply_26<- rgdx.param("H:/Water-SA/gdx/output_SSPs_CCAFS_CC-26.gdx","SupQuantity_Compare",names=supply_def,compress=T)
supply_27<- rgdx.param("H:/Water-SA/gdx/output_SSPs_CCAFS_CC-27.gdx","SupQuantity_Compare",names=supply_def,compress=T)
supply_28<- rgdx.param("H:/Water-SA/gdx/output_SSPs_CCAFS_CC-28.gdx","SupQuantity_Compare",names=supply_def,compress=T)
supply_29<- rgdx.param("H:/Water-SA/gdx/output_SSPs_CCAFS_CC-29.gdx","SupQuantity_Compare",names=supply_def,compress=T)
supply_30<- rgdx.param("H:/Water-SA/gdx/output_SSPs_CCAFS_CC-30.gdx","SupQuantity_Compare",names=supply_def,compress=T)
supply_31<- rgdx.param("H:/Water-SA/gdx/output_SSPs_CCAFS_CC-31.gdx","SupQuantity_Compare",names=supply_def,compress=T)
supply_32<- rgdx.param("H:/Water-SA/gdx/output_SSPs_CCAFS_CC-32.gdx","SupQuantity_Compare",names=supply_def,compress=T)
supply_33<- rgdx.param("H:/Water-SA/gdx/output_SSPs_CCAFS_CC-33.gdx","SupQuantity_Compare",names=supply_def,compress=T)


supply<-merge(supply_0,supply_1 ,all.x = TRUE, all.y = TRUE)
supply<-merge(supply,	supply_2	,all.x = TRUE, all.y = TRUE)
supply<-merge(supply,	supply_3	,all.x = TRUE, all.y = TRUE)
supply<-merge(supply,	supply_4	,all.x = TRUE, all.y = TRUE)
supply<-merge(supply,	supply_5	,all.x = TRUE, all.y = TRUE)
supply<-merge(supply,	supply_6	,all.x = TRUE, all.y = TRUE)
supply<-merge(supply,	supply_7	,all.x = TRUE, all.y = TRUE)
supply<-merge(supply,	supply_8	,all.x = TRUE, all.y = TRUE)
supply<-merge(supply,	supply_9	,all.x = TRUE, all.y = TRUE)
supply<-merge(supply, supply_10 ,all.x = TRUE, all.y = TRUE)
supply<-merge(supply, supply_11 ,all.x = TRUE, all.y = TRUE)
supply<-merge(supply,	supply_12	,all.x = TRUE, all.y = TRUE)
supply<-merge(supply,	supply_13	,all.x = TRUE, all.y = TRUE)
supply<-merge(supply,	supply_14	,all.x = TRUE, all.y = TRUE)
supply<-merge(supply,	supply_15	,all.x = TRUE, all.y = TRUE)
supply<-merge(supply,	supply_16	,all.x = TRUE, all.y = TRUE)
supply<-merge(supply,	supply_17	,all.x = TRUE, all.y = TRUE)
supply<-merge(supply,	supply_18	,all.x = TRUE, all.y = TRUE)
supply<-merge(supply,	supply_19	,all.x = TRUE, all.y = TRUE)
supply<-merge(supply, supply_20 ,all.x = TRUE, all.y = TRUE)
supply<-merge(supply, supply_21 ,all.x = TRUE, all.y = TRUE)
supply<-merge(supply,	supply_22	,all.x = TRUE, all.y = TRUE)
supply<-merge(supply,	supply_23	,all.x = TRUE, all.y = TRUE)
supply<-merge(supply,	supply_24	,all.x = TRUE, all.y = TRUE)
supply<-merge(supply,	supply_25	,all.x = TRUE, all.y = TRUE)
supply<-merge(supply,	supply_26	,all.x = TRUE, all.y = TRUE)
supply<-merge(supply,	supply_27	,all.x = TRUE, all.y = TRUE)
supply<-merge(supply,	supply_28	,all.x = TRUE, all.y = TRUE)
supply<-merge(supply,	supply_29	,all.x = TRUE, all.y = TRUE)
supply<-merge(supply, supply_30 ,all.x = TRUE, all.y = TRUE)
supply<-merge(supply, supply_31 ,all.x = TRUE, all.y = TRUE)
supply<-merge(supply,	supply_32	,all.x = TRUE, all.y = TRUE)
supply<-merge(supply,	supply_33	,all.x = TRUE, all.y = TRUE)


write.csv(supply,"R_data_figures/supply.csv", na="")


supply_0<- rgdx.param("H:/Water-SA/gdx/output_SSPs_CCAFS-0.gdx","SupQuantity_Compare",names=supply_def,compress=T)
supply_1<- rgdx.param("H:/Water-SA/gdx/output_SSPs_CCAFS-1.gdx","SupQuantity_Compare",names=supply_def,compress=T)
supply_2<- rgdx.param("H:/Water-SA/gdx/output_SSPs_CCAFS-2.gdx","SupQuantity_Compare",names=supply_def,compress=T)
supply_3<- rgdx.param("H:/Water-SA/gdx/output_SSPs_CCAFS-3.gdx","SupQuantity_Compare",names=supply_def,compress=T)
supply_4<- rgdx.param("H:/Water-SA/gdx/output_SSPs_CCAFS-4.gdx","SupQuantity_Compare",names=supply_def,compress=T)
supply_5<- rgdx.param("H:/Water-SA/gdx/output_SSPs_CCAFS-5.gdx","SupQuantity_Compare",names=supply_def,compress=T)
supply_6<- rgdx.param("H:/Water-SA/gdx/output_SSPs_CCAFS-6.gdx","SupQuantity_Compare",names=supply_def,compress=T)
supply_7<- rgdx.param("H:/Water-SA/gdx/output_SSPs_CCAFS-7.gdx","SupQuantity_Compare",names=supply_def,compress=T)
supply_8<- rgdx.param("H:/Water-SA/gdx/output_SSPs_CCAFS-8.gdx","SupQuantity_Compare",names=supply_def,compress=T)
supply_9<- rgdx.param("H:/Water-SA/gdx/output_SSPs_CCAFS-9.gdx","SupQuantity_Compare",names=supply_def,compress=T)


supply<-merge(supply_0,supply_1 ,all.x = TRUE, all.y = TRUE)
supply<-merge(supply,	supply_2	,all.x = TRUE, all.y = TRUE)
supply<-merge(supply,	supply_3	,all.x = TRUE, all.y = TRUE)
supply<-merge(supply,	supply_4	,all.x = TRUE, all.y = TRUE)
supply<-merge(supply,	supply_5	,all.x = TRUE, all.y = TRUE)
supply<-merge(supply,	supply_6	,all.x = TRUE, all.y = TRUE)
supply<-merge(supply,	supply_7	,all.x = TRUE, all.y = TRUE)
supply<-merge(supply,	supply_8	,all.x = TRUE, all.y = TRUE)
supply<-merge(supply,	supply_9	,all.x = TRUE, all.y = TRUE)



write.csv(supply,"R_data_figures/supply.csv", na="")
