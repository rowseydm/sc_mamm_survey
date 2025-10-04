# Code to summarize Santa Catalinas data
#####

#setwd("/Users/n8upham/ASU\ Dropbox/Nathan\ Upham/PROJECTS/SantaCatalinas_project/MS_2024-2025/sc_mamm_survey/Fig5_compareToPlants")
setwd("/Users/unathan/ASU\ Dropbox/Nathan\ Upham/PROJECTS/SantaCatalinas_project/MS_2024-2025/sc_mamm_survey/Fig5_compareToPlants")

allDat<-read.csv("AllMammalRecords_2021-23_Refined.csv")

table(allDat$habitat)

#                                      Interior Chaparral 
#                                                     197 
#                                    Madrean Oak Woodland 
#                                                     316 
#                           Petran Montane Conifer Forest 
#                                                     576 
#                                    Semidesert Grassland 
#                                                     313 
# Upland Sonoran Desertscrub/Semidesert Grassland ecotone 
#                                                     399 

table(allDat$site)

# CFN CFP CFS ICN ICS OWN OWS SDS SGN SGS 
# 120 238 218 123  74 174 142 399 147 166 

allSamplesBySite<-as.data.frame(table(allDat$site))

#    Var1 Freq
# 1   CFN  120
# 2   CFP  238
# 3   CFS  218
# 4   ICN  123
# 5   ICS   74
# 6   OWN  174
# 7   OWS  142
# 8   SDS  399
# 9   SGN  147
# 10  SGS  166

allVoucherDat<-allDat[which(allDat$basisOfRecord=="PRESERVED_SPECIMEN"),]
# 150 records

allVouchersBySite<-as.data.frame(table(allVoucherDat$site))

#    Var1 Freq
# 1   CFN    7
# 2   CFP   18
# 3   CFS   19
# 4   ICN   13
# 5   ICS    4
# 6   OWN   12
# 7   OWS    9
# 8   SDS   35
# 9   SGN   17
# 10  SGS   16


listOfSites<-as.vector(allVouchersBySite$Var1)
allSpp<-list(length(listOfSites))
whichSpp<-list(length(listOfSites))
numSpp<-list(length(listOfSites))
for(i in 1:length(listOfSites)){
	site_i<-listOfSites[i]
	allSpp[[i]]<-allVoucherDat[which(allVoucherDat$site==site_i),"species"]
	whichSpp[[i]]<-as.data.frame(table(allSpp[[i]]))
	numSpp[[i]]<-length(whichSpp[[i]][,1])
}
numSpp_ALL<-do.call(rbind, numSpp)

finalDat<-cbind.data.frame(listOfSites, numSpp_ALL)

#    listOfSites numSpp_ALL
# 1          CFN          3
# 2          CFP          4
# 3          CFS          5
# 4          ICN          4
# 5          ICS          2
# 6          OWN          5
# 7          OWS          3
# 8          SDS          6
# 9          SGN          8
# 10         SGS          5

#######
# PLOTTING
######

# Read back in data joined to Moore et al. 2013, Table 6
#######
datJoined<-read.csv("AllMammalSpecies-bySite_joinToMooreEtAl-Table6_DATA.csv")


# ALL PLANTS
####
X.VAR<-datJoined$numSpp_plantsAll
X.LAB<-"Plant species richness - all (Moore et al. 2013)"
Y.LAB<-"Mammal species richness (our survey)"

	TEST<-cor.test(x=X.VAR, y=datJoined$numSpp_mammalsAll,
              alternative = c("two.sided"),
              method = c("spearman"), conf.level = 0.95)
	TEST$estimate[[1]]
	TEST$p.value[[1]]
	#[1] 0.7099983
	#[1] 0.02142292

	# IS NORMAL?
	ks.test(y=datJoined$numSpp_mammalsAll, x=X.VAR)
		# data:  X.VAR and datJoined$numSpp_mammalsAll
		# D = 1, p-value = 1.083e-05 <<< FAILS!  So not relevant to summarize the linear model.
		# alternative hypothesis: two-sided

	TEST2<-lm(datJoined$numSpp_mammalsAll ~ X.VAR, data=datJoined)
	TEST2$coefficients[[1]]
	TEST2$coefficients[[2]]
    #           (Intercept)  datJoined$numSpp_plantsAll  
    #               2.07323                     0.04295  


pdf("AllMammalSpecies-bySite_joinToMooreEtAl_mam-v-allPlant_clean_noLine.pdf")

	plot(datJoined$numSpp_mammalsAll ~ X.VAR, type="n",data=datJoined, xlab=X.LAB, ylab=Y.LAB)
	points(datJoined$numSpp_mammalsAll ~ X.VAR, data=datJoined, cex=2, pch=20)
	text(y=datJoined$numSpp_mammalsAll, x=X.VAR, labels=datJoined$Site, adj=-0.2)

	abline(a=TEST2$coefficients[[1]], b=TEST2$coefficients[[2]])
	#mtext(text= paste0("Y = ",round(TEST2$coefficients[[1]],3), " + ",round(TEST2$coefficients[[2]],3)," X"), 
	#	side=3, line= -2, adj=0.1)
	mtext(text= paste0("r = ",round(TEST$estimate[[1]],3), "; P = ",round(TEST$p.value[[1]],3)), 
		side=3, line= -2, adj=0.1, font=3)

dev.off()


# COMMON PLANTS
####
X.VAR<-datJoined$numSpp_plantsCommon
X.LAB<-"Plant species richness - common (Moore et al. 2013)"
Y.LAB<-"Mammal species richness (our survey)"

	TEST<-cor.test(x=X.VAR, y=datJoined$numSpp_mammalsAll,
              alternative = c("two.sided"),
              method = c("spearman"), conf.level = 0.95)
	TEST$estimate[[1]]
	TEST$p.value[[1]]
	#[1] 0.5914021
	#[1] 0.07173829	

	TEST2<-lm(datJoined$numSpp_mammalsAll ~ X.VAR, data=datJoined)
	TEST2$coefficients[[1]]
	TEST2$coefficients[[2]]
	#	Coefficients:
	# (Intercept)        X.VAR  
	#    2.36547      0.07285  
pdf("AllMammalSpecies-bySite_joinToMooreEtAl_mam-v-commonPlant_clean.pdf")

	plot(datJoined$numSpp_mammalsAll ~ X.VAR, type="n",data=datJoined, xlab=X.LAB, ylab=Y.LAB)
	points(datJoined$numSpp_mammalsAll ~ X.VAR, data=datJoined, cex=2, pch=20)
	text(y=datJoined$numSpp_mammalsAll, x=X.VAR, labels=datJoined$Site, adj=-0.2)

	abline(a=TEST2$coefficients[[1]], b=TEST2$coefficients[[2]])
	mtext(text= paste0("Y = ",round(TEST2$coefficients[[1]],3), " + ",round(TEST2$coefficients[[2]],3)," X"), 
		side=3, line= -2, adj=0.1)
	mtext(text= paste0("r = ",round(TEST$estimate[[1]],3), "; P = ",round(TEST$p.value[[1]],3)), 
		side=3, line= -3, adj=0.1, font=3)

dev.off()


# UNCOMMON PLANTS
####
X.VAR<-datJoined$numSpp_plantsUncommon
X.LAB<-"Plant species richness - uncommon (Moore et al. 2013)"
Y.LAB<-"Mammal species richness (our survey)"

	TEST<-cor.test(x=X.VAR, y=datJoined$numSpp_mammalsAll,
              alternative = c("two.sided"),
              method = c("spearman"), conf.level = 0.95)
	TEST$estimate[[1]]
	TEST$p.value[[1]]
	#[1] 0.7161722
	#[1] 0.01981919	

	TEST2<-lm(datJoined$numSpp_mammalsAll ~ X.VAR, data=datJoined)
	TEST2$coefficients[[1]]
	TEST2$coefficients[[2]]
	#	Coefficients:
	# (Intercept)       X.VAR 
	#    2.137326    0.086863 
pdf("AllMammalSpecies-bySite_joinToMooreEtAl_mam-v-uncommonPlant_clean.pdf")

	plot(datJoined$numSpp_mammalsAll ~ X.VAR, type="n",data=datJoined, xlab=X.LAB, ylab=Y.LAB)
	points(datJoined$numSpp_mammalsAll ~ X.VAR, data=datJoined, cex=2, pch=20)
	text(y=datJoined$numSpp_mammalsAll, x=X.VAR, labels=datJoined$Site, adj=-0.2)

	abline(a=TEST2$coefficients[[1]], b=TEST2$coefficients[[2]])
	mtext(text= paste0("Y = ",round(TEST2$coefficients[[1]],3), " + ",round(TEST2$coefficients[[2]],3)," X"), 
		side=3, line= -2, adj=0.1)
	mtext(text= paste0("r = ",round(TEST$estimate[[1]],3), "; P = ",round(TEST$p.value[[1]],3)), 
		side=3, line= -3, adj=0.1, font=3)

dev.off()

# COMMON VS UNCOMMON PLANTS
####
X.VAR<-datJoined$numSpp_plantsUncommon
X.LAB<-"Plant species richness - uncommon (Moore et al. 2013)"
Y.LAB<-"Plant species richness - common (Moore et al. 2013)"

	TEST<-cor.test(x=X.VAR, y=datJoined$numSpp_plantsCommon,
              alternative = c("two.sided"),
              method = c("spearman"), conf.level = 0.95)
	TEST$estimate[[1]]
	TEST$p.value[[1]]
		# [1] 0.8632259
		# [1] 0.001293815

	TEST2<-lm(datJoined$numSpp_plantsCommon ~ X.VAR, data=datJoined)
	TEST2$coefficients[[1]]
	TEST2$coefficients[[2]]
	#	Coefficients:
		# (Intercept)       X.VAR 
		#    3.812376    0.937045 
	
pdf("AllMammalSpecies-bySite_joinToMooreEtAl_common-v-uncommonPlant_clean.pdf")

	plot(datJoined$numSpp_plantsCommon ~ X.VAR, type="n",data=datJoined, xlab=X.LAB, ylab=Y.LAB)
	points(datJoined$numSpp_plantsCommon ~ X.VAR, data=datJoined, cex=2, pch=20)
	text(y=datJoined$numSpp_plantsCommon, x=X.VAR, labels=datJoined$Site, adj=-0.2)

	abline(a=TEST2$coefficients[[1]], b=TEST2$coefficients[[2]])
	mtext(text= paste0("Y = ",round(TEST2$coefficients[[1]],3), " + ",round(TEST2$coefficients[[2]],3)," X"), 
		side=3, line= -2, adj=0.1)
	mtext(text= paste0("r = ",round(TEST$estimate[[1]],3), "; P = ",round(TEST$p.value[[1]],3)), 
		side=3, line= -3, adj=0.1, font=3)

dev.off()




# ELEV MID... VS MAMMALS
####
X.VAR<-datJoined$elevMid
X.LAB<-"Elevational midpoint (m)"
Y.LAB<-"Mammal species richness (our survey)"

	TEST<-cor.test(x=X.VAR, y=datJoined$numSpp_mammalsAll,
              alternative = c("two.sided"),
              method = c("spearman"), conf.level = 0.95)
	TEST$estimate[[1]]
	TEST$p.value[[1]]
		# [1] -0.4198251
		# [1] 0.2270985

	TEST2<-lm(datJoined$numSpp_mammalsAll ~ X.VAR, data=datJoined)
	TEST2$coefficients[[1]]
	TEST2$coefficients[[2]]
		# (Intercept)        X.VAR 
		# 7.428708296 -0.001546145 

pdf("AllMammalSpecies-bySite_joinToMooreEtAl_mam-v-elevMid_clean.pdf")

	plot(datJoined$numSpp_mammalsAll ~ X.VAR, type="n",data=datJoined, xlab=X.LAB, ylab=Y.LAB)
	points(datJoined$numSpp_mammalsAll ~ X.VAR, data=datJoined, cex=2, pch=20)
	text(y=datJoined$numSpp_mammalsAll, x=X.VAR, labels=datJoined$Site, adj=-0.2)

	abline(a=TEST2$coefficients[[1]], b=TEST2$coefficients[[2]])
	mtext(text= paste0("Y = ",round(TEST2$coefficients[[1]],3), " + ",round(TEST2$coefficients[[2]],3)," X"), 
		side=3, line= -2, adj=0.9)
	mtext(text= paste0("r = ",round(TEST$estimate[[1]],3), "; P = ",round(TEST$p.value[[1]],3)), 
		side=3, line= -3, adj=0.9, font=3)

dev.off()


# ELEV MID... VS PLANTS
####
X.VAR<-datJoined$elevMid
X.LAB<-"Elevational midpoint (m)"
Y.LAB<-"Plant species richness - all (Moore et al. 2013)"

	TEST<-cor.test(x=X.VAR, y=datJoined$numSpp_plantsAll,
              alternative = c("two.sided"),
              method = c("spearman"), conf.level = 0.95)
	TEST$estimate[[1]]
	TEST$p.value[[1]]
		# [1] -0.6242424
		# [1] 0.06024898

	TEST2<-lm(datJoined$numSpp_plantsAll ~ X.VAR, data=datJoined)
	TEST2$coefficients[[1]]
	TEST2$coefficients[[2]]
		# [1] 131.6602
		# [1] -0.0396791

pdf("AllMammalSpecies-bySite_joinToMooreEtAl_plant-v-elevMid_clean.pdf")

	plot(datJoined$numSpp_plantsAll ~ X.VAR, type="n",data=datJoined, xlab=X.LAB, ylab=Y.LAB)
	points(datJoined$numSpp_plantsAll ~ X.VAR, data=datJoined, cex=2, pch=20)
	text(y=datJoined$numSpp_plantsAll, x=X.VAR, labels=datJoined$Site, adj=-0.2)

	abline(a=TEST2$coefficients[[1]], b=TEST2$coefficients[[2]])
	mtext(text= paste0("Y = ",round(TEST2$coefficients[[1]],3), " + ",round(TEST2$coefficients[[2]],3)," X"), 
		side=3, line= -2, adj=0.9)
	mtext(text= paste0("r = ",round(TEST$estimate[[1]],3), "; P = ",round(TEST$p.value[[1]],3)), 
		side=3, line= -3, adj=0.9, font=3)

dev.off()






# 	Spearman's rank correlation rho
# 
# data:  datJoined$numSpp_plantsAll and datJoined$numSpp_mammalsAll
# S = 47.85, p-value = 0.02142
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#       rho 
# 0.7099983 


# Colors from other Figs
#####
# colors
# In this order: loud pink, berry, blue, olive green, light aqua
# c("#f178fa", "#62255c", "#4d6180", "#758150", "#bafdca")
cols <- c("#f178fa", "#62255c", "#416fb4", "#799943", "#bafdca")
colsplus <- c("#f178fa", "#62255c", "#416fb4", "#799943", "#bafdca", "#505050", "#959595", rep("#000000", 15))

# Shapes and colors. Shapes by family, colors by habitat.
col_corsp <- c("IntChapUB", "MadOakB", "MadOakUB", "PetConB", "PetConUB", "SDGrassUB", "EcotoneUB", rep("species", 15))

biomeCols<-cbind.data.frame(col_corsp,colsplus)
biomeCols_only<-biomeCols[c(1:7),2]
#    col_corsp colsplus
# 1  IntChapUB  #f178fa
# 2    MadOakB  #62255c
# 3   MadOakUB  #416fb4
# 4    PetConB  #799943
# 5   PetConUB  #bafdca
# 6  SDGrassUB  #505050
# 7  EcotoneUB  #959595


