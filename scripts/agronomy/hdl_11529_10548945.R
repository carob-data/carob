# R script for "carob"

carob_script <- function(path) {

"Lower yield of wheat in eastern India can be attributed to several input factors i.e., inappropriate crop management practices including other social and environmental factors. Among them, sub-optimal irrigation is one of the important factors. Recommended number of irrigations for optimal wheat harvest is 4-5 depending upon crop growth stages and availability of water. Observations by the project team and a recent survey conducted by CSISA at a landscape-level found that majority of farmers in this part of India apply 2-3 irrigations. Moreover, several studies have suggested to apply irrigation to wheat at its grain filling stage to protect the crop from terminal heat stress thereby safeguarding yield. To validate the effect of number of irrigations and irrigation at the grain-filling stage of wheat, multi-location on-farm trials were conducted continuously over six years starting from 2016-17. Krishi Vigyan Kendras, district-level extension center of national agriculture research and extension system were involved in this process. Ten districts were selected in a way that all agro-climatic zones of this area are covered. The treatment in this trial was application of additional irrigation at maturity/grain-filling stage of wheat crop against general farmer practice. Number of irrigations with farmers vary from 1 to 4, so our treatments included 1+1, 2+1, 3+1, and 4+1 irrigations. We applied these treatments in two sets of wheat crop establishment i.e., zero-tillage and conventional tillage methods. There is asymmetry in distribution of samples within treatments and over years. That happened as trial was in farmer’s participatory mode and numbers were dependent completely on willingness of farmers to participate. Altogether, the trial was conducted at 1810 sites, and we captured 63 variables including yield and yield attributing traits. (2023-08-17)"

	uri <- "hdl:11529/10548945"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=3, minor=1,
		data_organization = "CIMMYT;IRRI;CU",
		project = "CSISA",
		publication=NA,
		data_type = "experiment",
		treatment_vars = "land_prep_method;irrigation_number",
		response_vars = "yield", 
		carob_contributor = "Shumirai Manzvera",
		carob_date = "2024-08-29",
		notes = "herbicide amounts per product needs improvement"
	)
	
## read data 
							 
	f <- ff[basename(ff) == "CSISA_IND_Irrigation Trial_Data_2017-22_Final.csv"]
	r <- suppressWarnings(read.csv(f, fileEncoding = "UTF-8"))
	#rr <- read.csv("~/Downloads/CSISA_IND_Irrigation Trial_Data_2017-22v2.csv", fileEncoding = "UTF-8")
	#x = unique(cbind(r$GradeNPK, rr$GradeNPK))
	
	d <- data.frame(
		country = "India",
		crop= "wheat",
		adm1 = r$State, 
		adm2= r$District,
		adm3 = trimws(r$Block),
		location = r$Village,
		latitude=r$Latitude,
		longitude=r$Longitude,
		previous_crop=tolower(r$PreviousCrop),
		variety=r$Variety,
		land_prep_method=r$CropEstablishment,
		planting_date=r$SowingDate,
		harvest_date=r$HarvestDate,
		dmy_total=r$BiomassYield* 1000,
		yield=r$GrainYield * 1000,
		irrigation_number=r$IrrigationNumber,
		herbicide_used= TRUE,
		herbicide_product=r$HerbicideName,
		herbicide_amount=r$HerbicideDose,
		herbicide_dates=r$HerbicideDate,
		previous_crop_residue_perc=r$PrevCropResidue,
		N_splits= 3L)
	
	d$irrigation_number <- match(d$irrigation_number, c("One", "Two", "Three", "Four")) 
	d$herbicide_amount <- gsub("gm", "", d$herbicide_amount)

	d$yield_part <- "grain"

	d$on_farm <- TRUE
	d$is_survey <-FALSE 
	d$irrigated <-TRUE
	d$geo_from_source <- TRUE

	d$planting_date<- gsub("\\.", "-", d$planting_date)
	d$harvest_date <- gsub("\\.", "-", d$harvest_date)
	d$herbicide_dates <- gsub("\\.", "-", d$herbicide_dates)
	d$herbicide_dates[d$herbicide_dates == ""] <- NA
	d$harvest_date[d$harvest_date == ""] <- NA
	
	#d$harvest_date <- as.character(as.Date(d$harvest_date))
	#d$herbicide_dates <- as.character(as.Date(d$herbicide_dates))
	

# in the current version values of "r$GradeNPK" cannot be interpreted: 0.5224074 0.8473727 0.7819444
# this is the key, based on email from AA to RH
# 0.522407407 12:32:16
# 0.781944444 18:46:00
# 0.847372685 20:20:13 

	npk <- matrix(0, nrow=nrow(d), ncol=3)
	i <- r$GradeNPK != ""
	vnpk <- do.call(rbind, strsplit(r$GradeNPK[i], ":")) 
	vnpk <- matrix(as.numeric(vnpk), nrow=nrow(vnpk))
	npk[i, ] <- vnpk

	npk <- npk / 100
	
	d$N_fertilizer <- rowSums(r[, c("Split1Urea", "Split2Urea", "Split3Urea")], na.rm = TRUE) * 0.46
	d$N_fertilizer <- rowSums(cbind(d$N_fertilizer, r$BasalDAP * 0.18, npk[,1] * r$BasalNPK), na.rm = TRUE)
	d$P_fertilizer <- rowSums(cbind(r$BasalDAP * 0.201,  r$BasalNPK * (npk[,2] / 2.29)), na.rm = TRUE)
	d$K_fertilizer <- rowSums(cbind(r$BasalMOP * 0.498 , r$BasalNPK * (npk[,3] / 1.2051)), na.rm=TRUE)
	d$Zn_fertilizer <- r$BasalZn
   
   d$land_prep_method<- gsub("CT", "conventional", d$land_prep_method)
   d$land_prep_method<- gsub("ZT", "none", d$land_prep_method)
   
   d$herbicide_product[d$herbicide_product =="Clodinafop Propargyl 15% + Metsulfuron Methyl 1% WP (Vesta)"]<- "clodinafop;metsulfuron"
   d$herbicide_product[d$herbicide_product=="Sulphosulfuron 75% (Leader)+Carfentrazone Ethyl 40DF (Affinity)"] <-"sulfosulfuron;carfentrazone-ethyl"
   d$herbicide_product[d$herbicide_product =="Carfentrazone Ethyl 40DF (Affinity)+Clodinofop 15w.p. (Topic)"] <- "carfentrazone-ethyl;clodinafop"
   d$herbicide_product[d$herbicide_product =="Clodinafop 15 W.P. (Topik)"] <- "clodinafop"
   d$herbicide_product[d$herbicide_product =="Carfentrazone Ethyl 40DF (Affinity)"]<-"carfentrazone-ethyl"
   d$herbicide_product[d$herbicide_product =="Sulfosulfuron+Metsulfuron"] <- "sulfosulfuron;metsulfuron"
   d$herbicide_product[d$herbicide_product =="Metsulfuron+Sulfosulfuron (Total)"] <- "sulfosulfuron;metsulfuron"
   d$herbicide_product[d$herbicide_product =="Sulfosulfuron 75% WG (Leader), Carfentrazone Ethyl 40DF (Affinity)"] <- "sulfosulfuron;carfentrazone-ethyl"
   d$herbicide_product[d$herbicide_product =="Metsulfuron+Sulphosulfuron (Total)+Carfentrazone Ethyl 40DF (Affinity)"] <- "metsulfuron;sulfosulfuron;carfentrazone-ethyl"
   d$herbicide_product[d$herbicide_product =="Carfentrazone+Sulphosulfuron 45% (Broadway)"] <- "carfentrazone;sulfosulfuron"
   d$herbicide_product[d$herbicide_product =="Clodinafop 15 W.P. (Topik)+Metsulfuron 20 W.P. (Algrip)"] <- "clodinafop;metsulfuron"
   d$herbicide_product[d$herbicide_product =="Sulfosulfuron 75% WG (Leader)"] <- "sulfosulfuron"
   d$herbicide_product[d$herbicide_product =="2,4-dichlorophenoxyacetic acid (2,4-D)"] <- "2,4-D"
   d$herbicide_product[d$herbicide_product =="2,4-Dichlorophenoxyacetic acid"] <- "2,4-D"
   
   d$herbicide_amount <- gsub("g", "", d$herbicide_amount)
   d$herbicide_amount <- gsub(",", "+", d$herbicide_amount)
   d$herbicide_amount[d$herbicide_amount == ""] <- NA
   d$herbicide_amount <- sapply(d$herbicide_amount, \(i) eval(parse(text=i)))
   
   y <- substr(d$planting_date, 1, 4)   
   d$trial_id <- as.character(as.integer(as.factor(paste(y, d$longitude, d$latitude))))

	d$yield_moisture <- as.numeric(NA) #needs to be checked

   carobiner::write_files(path, meta, d)
}

