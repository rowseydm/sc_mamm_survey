# Code to summarize Santa Catalinas data
#####

# FIG 2 - summary of all sampling
######
setwd("/Users/unathan/ASU\ Dropbox/Nathan\ Upham/PROJECTS/SantaCatalinas_project/MS_2024-2025/sc_mamm_survey/Figures/Fig2_samplingSummary")

allDat<-read.csv("AllMammalRecords_2021-23_Refined.csv")

prepListRaw<-as.data.frame(table(allDat$preparations))[order(as.data.frame(table(allDat$preparations))[,2], decreasing=TRUE),]

write.csv(prepListRaw, file="AllMammalRecords_2021-23_Refined_prepListRaw.csv")

# read back in the cleaned data
prepListCleaned<-read.csv(file="AllMammalRecords_2021-23_Refined_prepListCleaned.csv")
dat<-prepListCleaned
dat_rev <- dat[nrow(dat):1, ]

# PLOT these data
pdf(file="plot_prepListCleaned.pdf", width=6, height=6)
	barplot(
	  height = dat_rev$Freq,
	  names.arg = dat_rev$Var1,
	  horiz = TRUE,
	  xlab = "Frequency",
	  ylab = "Category",
	  las = 1
	)
dev.off()





# FIG 5 -- Moore et al. 2013 + Meyer et al. 2015 comparison
######
#setwd("/Users/n8upham/ASU\ Dropbox/Nathan\ Upham/PROJECTS/SantaCatalinas_project/MS_2024-2025/sc_mamm_survey/Fig5_compareToPlants")
setwd("/Users/unathan/ASU\ Dropbox/Nathan\ Upham/PROJECTS/SantaCatalinas_project/MS_2024-2025/sc_mamm_survey/Figures/Fig5_compareToPlants")

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

	allVouchersSex<-as.data.frame(table(allVoucherDat$sex))

	#   Var1 Freq
	# 1    f   76
	# 2    m   73 << 74
	# 3    M    1


allNonlethalDat<-allDat[!is.na(allDat$basisOfRecord) &
  						!is.na(allDat$preparations) &
  						allDat$basisOfRecord == "MATERIAL_SAMPLE" &
  						allDat$preparations == "ear",]

	allNonlethalBySite<-as.data.frame(table(allNonlethalDat$site))
	#    Var1 Freq
	# 1   CFN   26
	# 2   CFP   41
	# 3   CFS   26
	# 4   ICN    4
	# 5   ICS   16
	# 6   OWN   29
	# 7   OWS   29
	# 8   SDS   27
	# 9   SGN    2
	# 10  SGS   17

	allNonlethalSex<-as.data.frame(table(allNonlethalDat$sex))
	#   Var1 Freq
	# 1    unk  6
	# 2    f  114
	# 3    m   97
	

# Combined lethal + non-lethal sampling
	sumDat<-cbind.data.frame(site=allVouchersBySite[,1],lethal=allVouchersBySite[,2],nonLethal=allNonlethalBySite[,2], total=(allVouchersBySite[,2]+allNonlethalBySite[,2]), trapSuccess=(allVouchersBySite[,2]+allNonlethalBySite[,2])/400)

	sumDat_ordered<-sumDat[c(8,10,5,7,3,2,1,6,4,9),]

	write.csv(sumDat_ordered, file="AllMammalRecords_2021-23_Refined_sumSampling-lethal-non.csv")

	#    site lethal nonLethal total trapSuccess
	# 8   SDS     35        27    62      0.1550
	# 10  SGS     16        17    33      0.0825
	# 5   ICS      4        16    20      0.0500
	# 7   OWS      9        29    38      0.0950
	# 3   CFS     19        26    45      0.1125
	# 2   CFP     18        41    59      0.1475
	# 1   CFN      7        26    33      0.0825
	# 6   OWN     12        29    41      0.1025
	# 4   ICN     13         4    17      0.0425
	# 9   SGN     17         2    19      0.0475

# PLOT sites -- lethal vs non-lethal
totalScale<-10*(sumDat_ordered$total / max(sumDat_ordered$total) )
voucherScale<-10*(sumDat_ordered$lethal / max(sumDat_ordered$total) )

pdf(file="plot_sumSampling-lethal-vs-nonlethal.pdf", width=4, height=6)
	plot(x=1:10, y=c(1:6,5:2), cex=totalScale, xpd=NA, xaxt="n", yaxt="n", bty="n",xlab="",ylab="")
	points(x=1:10, y=c(1:6,5:2), cex=voucherScale, pch=20, xpd=NA)
	text(x=1:10, y=c(1:6,5:2), labels=sumDat_ordered$site, adj=0.3)
dev.off()


# Sum species richness
#####
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

# PART A
#######
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

	plot(datJoined$numSpp_mammalsAll ~ X.VAR, type="n",data=datJoined, xlab=X.LAB, ylab=Y.LAB) # plotting the axes (no points)
	points(datJoined$numSpp_mammalsAll ~ X.VAR, data=datJoined, cex=2, pch=20)	# plotting points
	text(y=datJoined$numSpp_mammalsAll, x=X.VAR, labels=datJoined$Site, adj=-0.2) # adding labels to point (site names)

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

# ELEV MID... VS ALL -- as SPLINES....
####





# PART B
#######
# Read back in data joined to Meyer at al. 2015, S2 Data
#######
datJoined<-read.csv("AllMammalSpecies-bySite_joinToMeyerEtAl-S2data_DATA.csv")


# ALL ARTHROPODS
####
X.VAR<-datJoined$TOTAL_SPP
X.LAB<-"Arthropod species richness - all (Meyer et al. 2015)"
Y.LAB<-"Mammal species richness (our survey)"

	TEST<-cor.test(x=X.VAR, y=datJoined$numSpp_mammalsAll,
              alternative = c("two.sided"),
              method = c("spearman"), conf.level = 0.95)
	TEST$estimate[[1]]
	TEST$p.value[[1]]
	#[1] 0.790259
	#[1] 0.006516335

	TEST2<-lm(datJoined$numSpp_mammalsAll ~ X.VAR, data=datJoined)
	TEST2$coefficients[[1]]
	TEST2$coefficients[[2]]
    #           (Intercept)  datJoined$TOTAL_SPP  
	#		       -0.56018      0.06333 


pdf("AllMammalSpecies-bySite_joinToMeyerEtAl_mam-v-allArthropod.pdf")

	plot(datJoined$numSpp_mammalsAll ~ X.VAR, type="n",data=datJoined, xlab=X.LAB, ylab=Y.LAB) # plotting the axes (no points)
	points(datJoined$numSpp_mammalsAll ~ X.VAR, data=datJoined, cex=2, pch=20)	# plotting points
	text(y=datJoined$numSpp_mammalsAll, x=X.VAR, labels=datJoined$Site, adj=-0.2) # adding labels to point (site names)

	abline(a=TEST2$coefficients[[1]], b=TEST2$coefficients[[2]])
	#mtext(text= paste0("Y = ",round(TEST2$coefficients[[1]],3), " + ",round(TEST2$coefficients[[2]],3)," X"), 
	#	side=3, line= -2, adj=0.1)
	mtext(text= paste0("r = ",round(TEST$estimate[[1]],3), "; P = ",round(TEST$p.value[[1]],3)), 
		side=3, line= -2, adj=0.1, font=3)

dev.off()


# Orthoptera
####
X.VAR<-datJoined$Orthoptera
X.LAB<-"Orthoptera species richness (Meyer et al. 2015)"
Y.LAB<-"Mammal species richness (our survey)"

	TEST<-cor.test(x=X.VAR, y=datJoined$numSpp_mammalsAll,
              alternative = c("two.sided"),
              method = c("spearman"), conf.level = 0.95)
	TEST$estimate[[1]]
	TEST$p.value[[1]]
	#[1] 0.4130408
	#[1] 0.2354798

	TEST2<-lm(datJoined$numSpp_mammalsAll ~ X.VAR, data=datJoined)

pdf("AllMammalSpecies-bySite_joinToMeyerEtAl_mam-v-Orthoptera.pdf")

	plot(datJoined$numSpp_mammalsAll ~ X.VAR, type="n",data=datJoined, xlab=X.LAB, ylab=Y.LAB) # plotting the axes (no points)
	points(datJoined$numSpp_mammalsAll ~ X.VAR, data=datJoined, cex=2, pch=20)	# plotting points
	text(y=datJoined$numSpp_mammalsAll, x=X.VAR, labels=datJoined$Site, adj=-0.2) # adding labels to point (site names)

	abline(a=TEST2$coefficients[[1]], b=TEST2$coefficients[[2]])
	#mtext(text= paste0("Y = ",round(TEST2$coefficients[[1]],3), " + ",round(TEST2$coefficients[[2]],3)," X"), 
	#	side=3, line= -2, adj=0.1)
	mtext(text= paste0("r = ",round(TEST$estimate[[1]],3), "; P = ",round(TEST$p.value[[1]],3)), 
		side=3, line= -2, adj=0.1, font=3)

dev.off()

# Myriapoda
####
X.VAR<-datJoined$Myriapoda
X.LAB<-"Myriapoda species richness (Meyer et al. 2015)"
Y.LAB<-"Mammal species richness (our survey)"

	TEST<-cor.test(x=X.VAR, y=datJoined$numSpp_mammalsAll,
              alternative = c("two.sided"),
              method = c("spearman"), conf.level = 0.95)
	TEST$estimate[[1]]
	TEST$p.value[[1]]
	#[1] 0.08254343
	#[1] 0.8206615

	TEST2<-lm(datJoined$numSpp_mammalsAll ~ X.VAR, data=datJoined)

pdf("AllMammalSpecies-bySite_joinToMeyerEtAl_mam-v-Myriapoda.pdf")

	plot(datJoined$numSpp_mammalsAll ~ X.VAR, type="n",data=datJoined, xlab=X.LAB, ylab=Y.LAB) # plotting the axes (no points)
	points(datJoined$numSpp_mammalsAll ~ X.VAR, data=datJoined, cex=2, pch=20)	# plotting points
	text(y=datJoined$numSpp_mammalsAll, x=X.VAR, labels=datJoined$Site, adj=-0.2) # adding labels to point (site names)

	abline(a=TEST2$coefficients[[1]], b=TEST2$coefficients[[2]])
	#mtext(text= paste0("Y = ",round(TEST2$coefficients[[1]],3), " + ",round(TEST2$coefficients[[2]],3)," X"), 
	#	side=3, line= -2, adj=0.1)
	mtext(text= paste0("r = ",round(TEST$estimate[[1]],3), "; P = ",round(TEST$p.value[[1]],3)), 
		side=3, line= -2, adj=0.1, font=3)

dev.off()

# Coleoptera
####
X.VAR<-datJoined$Coleoptera
X.LAB<-"Coleoptera species richness (Meyer et al. 2015)"
Y.LAB<-"Mammal species richness (our survey)"

	TEST<-cor.test(x=X.VAR, y=datJoined$numSpp_mammalsAll,
              alternative = c("two.sided"),
              method = c("spearman"), conf.level = 0.95)
	TEST$estimate[[1]]
	TEST$p.value[[1]]
	#[1] 0.6894942
	#[1] 0.0273886

	TEST2<-lm(datJoined$numSpp_mammalsAll ~ X.VAR, data=datJoined)

pdf("AllMammalSpecies-bySite_joinToMeyerEtAl_mam-v-Coleoptera.pdf")

	plot(datJoined$numSpp_mammalsAll ~ X.VAR, type="n",data=datJoined, xlab=X.LAB, ylab=Y.LAB) # plotting the axes (no points)
	points(datJoined$numSpp_mammalsAll ~ X.VAR, data=datJoined, cex=2, pch=20)	# plotting points
	text(y=datJoined$numSpp_mammalsAll, x=X.VAR, labels=datJoined$Site, adj=-0.2) # adding labels to point (site names)

	abline(a=TEST2$coefficients[[1]], b=TEST2$coefficients[[2]])
	#mtext(text= paste0("Y = ",round(TEST2$coefficients[[1]],3), " + ",round(TEST2$coefficients[[2]],3)," X"), 
	#	side=3, line= -2, adj=0.1)
	mtext(text= paste0("r = ",round(TEST$estimate[[1]],3), "; P = ",round(TEST$p.value[[1]],3)), 
		side=3, line= -2, adj=0.1, font=3)

dev.off()

# Araneae
####
X.VAR<-datJoined$Araneae
X.LAB<-"Araneae species richness (Meyer et al. 2015)"
Y.LAB<-"Mammal species richness (our survey)"

	TEST<-cor.test(x=X.VAR, y=datJoined$numSpp_mammalsAll,
              alternative = c("two.sided"),
              method = c("spearman"), conf.level = 0.95)
	TEST$estimate[[1]]
	TEST$p.value[[1]]
	#[1] 0.7895683
	#[1] 0.006596672

	TEST2<-lm(datJoined$numSpp_mammalsAll ~ X.VAR, data=datJoined)

pdf("AllMammalSpecies-bySite_joinToMeyerEtAl_mam-v-Araneae.pdf")

	plot(datJoined$numSpp_mammalsAll ~ X.VAR, type="n",data=datJoined, xlab=X.LAB, ylab=Y.LAB) # plotting the axes (no points)
	points(datJoined$numSpp_mammalsAll ~ X.VAR, data=datJoined, cex=2, pch=20)	# plotting points
	text(y=datJoined$numSpp_mammalsAll, x=X.VAR, labels=datJoined$Site, adj=-0.2) # adding labels to point (site names)

	abline(a=TEST2$coefficients[[1]], b=TEST2$coefficients[[2]])
	#mtext(text= paste0("Y = ",round(TEST2$coefficients[[1]],3), " + ",round(TEST2$coefficients[[2]],3)," X"), 
	#	side=3, line= -2, adj=0.1)
	mtext(text= paste0("r = ",round(TEST$estimate[[1]],3), "; P = ",round(TEST$p.value[[1]],3)), 
		side=3, line= -2, adj=0.1, font=3)

dev.off()


   # >>> Looks especially driven by spiders and beetles!








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


