# R script for "carob"


carob_script <- function(path) {
  
"Farmer participatory on-farm trials with CA technologies comparing with farmers’ practices (CT), were conducted in several fields in each community. Likewise, farmer-participatory intercropping leafy vegetables in maize trials were conducted comparing to exiting practices and to find out suitable and more profitable crop production practices, prioritized to increase visibility and to avoid implementation and  management problems that emerge when utilizing small plots with significant edge effects. Most trials were replicated in several fields within each community and were farmer-managed with backstopping from project staff and NARES partners. Project partners and staff coordinated monitoring and data acquisition. Where possible, collaborating farmers were selected by the community, and the project worked with existing farmer groups, with groups of both men and women farmers."
 
 
	uri <- "hdl:11529/10548002"
	group <- "agronomy"
	ff	<- carobiner::get_data(uri, path, group)
	
	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=2,
		data_organization = "CIMMYT;RDRS;BARI",
		publication= NA,
		project=NA,
		data_type= "experiment",
		carob_contributor= "Mitchelle Njukuya",
		carob_date="2024-04-19",
		treatment_vars=NA, 
		response_vars="yield"
	)
	

	f1 <- ff[basename(ff) == "Inter-cropping 2014-15-all nodes-Rangpur.xlsx"]
	
	r1 <- carobiner::read.excel.hdr(f1, sheet ="2 - Site information", skip=4, hdr=3)
	d1 <- data.frame(
			season=r1$Season, 
			location=r1$Node, 
			trial_id=paste0(r1$Node, "_", r1$Site.No.Unique.farmer.ID),
			treatment=r1$Tmnt.Short.abbreviation.as.mentioned.in.protocol,
			soil_type=r1$Soil.texture.sand.silt.clay.etc
		)
		
	r2 <- carobiner::read.excel.hdr(f1, sheet ="4- Stand counts & Phenology", skip=4, hdr=3)
	d2 <- data.frame(
			treatment=r2$Tmnt, 
			trial_id=paste0(r2$Node, "_", r2$Site.No),
			variety=r2$Variety,
			row_spacing=r2$Row.spacing.cm,
			crop=tolower(r2$Crop),
			planting_date=as.character(r2$Date.of.seeding.dd.mm.yy),
			harvest_date=as.character(r2$Date.of.harvest.dd.mm.yy),
			flowering_days=r2$X50pct.first.flowering.DAS,
			anthesis_days=r2$X50pct.anthesis.DAS,
			maturity_days=r2$X80pct.physiological.maturity.DAS,
			harvest_days=r2$Harvesting.DAS
		)
		
	r3 <- carobiner::read.excel.hdr(f1, sheet ="6 - Fertilizer amounts ", skip=4, hdr=3)
		
	d3 <- data.frame(
			treatment=r3$Tmnt, 
			trial_id=paste0(r3$Node, "_", r3$Site.No),
			N_fertilizer=r3$N.kg.ha, 
			P_fertilizer=r3$P2O5.kg.ha / 2.29, 
			gypsum=r3$Gypsum.Kg.ha,
			Zn_fertilizer=r3$ZnSO4.kg.ha,
			K_fertilizer=r3$K2O.kg.ha / 1.2051,
			S_fertilizer = NA
		)
		
	d3$fertilizer_type <- apply(r3[, grep("Application_Product.used", names(r3))], 1, 
									\(i) paste(unique(i), collapse=";"))
		
		
	r4 <- carobiner::read.excel.hdr(f1, sheet ="13 - Grain Harvest ", skip=4, hdr=3)	
	 
	d4 <- data.frame(
			treatment=r4$Tmnt, 
			trial_id=paste0(r4$Node, "_", r4$Site.No),
			yield=r4$Grain.yield.t.ha * 1000,
			fwy_residue=r4$Straw.yield.t.ha * 1000,
			dmy_total=r4$Biomass.t.ha * 1000
	)

	x1 <- merge(d1, d2, by=c("treatment", "trial_id"), all.x=TRUE)
	x1 <- merge(x1, d3, by=c("treatment", "trial_id"), all.x=TRUE)
	x1 <- merge(x1, d4, by=c("treatment", "trial_id"), all.x=TRUE) 
		
		
	f2 <- ff[basename(ff) == "Inter-cropping 2015-16-all nodes-Rangpur.xlsx"]
	
	r5 <- carobiner::read.excel.hdr(f2, sheet ="2 - Site information", skip=5, hdr=4)
	d5 <- data.frame(
			season=r5$Season, 
			location=r5$Node, 
			trial_id=paste0(r5$Node, "_", r5$Site.No.Unique.farmer.ID),
			treatment=r5$Tmnt.Short.abbreviation.as.mentioned.in.protocol,
			soil_type=r5$Soil.texture.sand.silt.clay.etc
		)
		
	r6 <- carobiner::read.excel.hdr(f2, sheet ="4- Stand counts & Phenology", skip=4, hdr=3)
	d6 <- data.frame(
			treatment=r6$Tmnt, 
			trial_id=paste0(r6$Node, "_", r6$Site.No),
			variety=r6$Variety,
			row_spacing=r6$Row.spacing.cm,
			crop=tolower(r6$Crop),
			planting_date=as.character(r6$Date.of.seeding.dd.mm.yy),
			harvest_date=as.character(r6$Datw.of.harvest.dd.mm.yy),
			flowering_days=r6$X50pct.first.flowering.DAS,
			anthesis_days=r6$X50pct.anthesis.DAS,
			maturity_days=r6$X80pct.physiological.maturity.DAS,
			harvest_days=r6$Harvesting.DAS
		)
		
	r7 <- carobiner::read.excel.hdr(f2, sheet ="6 - Fertilizer amounts ", skip=4, hdr=3)
		
	d7 <- data.frame(
			treatment=r7$Tmnt, 
			trial_id=paste0(r7$Node, "_", r7$Site.No),
			N_fertilizer=r7$N.kg.ha, 
			P_fertilizer=r7$P2O5.kg.ha / 2.29,
			gypsum = NA,
			Zn_fertilizer=r7$Zn.kg.ha,
			K_fertilizer=r7$K.kg.ha,
			S_fertilizer=r7$S.kg.ha
		)
		
	d7$fertilizer_type <- apply(r7[, grep("Application_Product.used", names(r7))], 1, 
											\(i) paste(unique(i), collapse=";"))
		
	d7$fertilizer_type  <- gsub(";NA", "", d7$fertilizer_type)
			
			
	r8 <- carobiner::read.excel.hdr(f2, sheet ="13 - Grain Harvest ", skip=4, hdr=3)			
	d8 <- data.frame(
			treatment=r8$Tmnt, 
			trial_id=paste0(r8$X.6, "_", r8$Site.No),
			yield=r8$Grain.yield.t.ha * 1000,
			fwy_residue=r8$Straw.yield.t.ha * 1000,
			dmy_total=r8$Biomass.t.ha * 1000
	)
		
	x2 <- merge(d5, d6, by=c("treatment", "trial_id"), all.x=TRUE)
	x2 <- merge(x2, d7, by=c("treatment", "trial_id"), all.x=TRUE)
	x2 <- merge(x2, d8, by=c("treatment", "trial_id"), all.x=TRUE)

	rr1 <- carobiner::read.excel.hdr(f1, sheet ="15-Intercrop", skip=4, hdr=3)
	dd1 <- data.frame(season = rr1$Season, 
		trial_id=paste0(rr1$Node, "_", rr1$Site.No),
		treatment = rr1$Tmnt, 
		intercrops = rr1$Type.of.trial,
		crop = rr1$Crop.information.and.phenology.of.inter.crop_Name.of.Inter.Crop,
		variety = rr1$Variety,
		planting_date = as.character(as.Date(rr1$Date.of.seeding.dd.mm.yy)),
		yield = rr1$Ccalculaation_Intercrop.t.ha *1000 
	)
	hd <- rr1[, c("Date.of.harvesting.of.amaranth", "Date.of.harvesting.of.spinach","Date.of.harvesting.of.Napa.shak")]
	dd1$harvest_date <- gsub("NA", "", apply(hd, 1, \(i) paste(i, collapse="")))
	dd1 <- dd1[dd1$intercrop != "Sole Maize", ]

	nms <- names(x1)
	nms <- c("treatment", "trial_id", nms[!nms %in% names(dd1)])

	z1 <- merge(dd1, x1[,nms], by=c("treatment", "trial_id"))
	x1 <- carobiner::bindr(x1, z1)
	
	rr2 <- carobiner::read.excel.hdr(f2, sheet ="15-Intercrop", skip=4, hdr=3)
	dd2 <- data.frame(season = rr2$Season, 
		trial_id=paste0(rr2$X.6, "_", rr2$Site.No),
		treatment = rr2$Tmnt, 
		intercrops = rr2$X.1,
		crop = rr2$Crop.information.and.phenology.of.inter.crop_Name.of.Inter.Crop,
		variety = rr2$Variety,
		planting_date = rr2$Date.of.seeding.dd.mm.yy,
		harvest_date = rr2$Date.of.harvest.dd.mm.yy ,
		yield = rr2$Intercrop.t.ha *1000
	)
	
	dfun <- function(x) {
		i <- grep("/15$", x)
		x[i] <- as.character(as.Date(gsub("/15", "/2015", x[i]), "%d/%m/%Y"))
		i <- grep("/16$", x)
		x[i] <- as.character(as.Date(gsub("/16", "/2016", x[i]), "%d/%m/%Y"))
		i <- grep("^42", x)
		x[i] <- as.character(as.Date("1899-12-31") + as.numeric(x[i]))
	}
	dd2$planting_date = dfun(dd2$planting_date)
	dd2$harvest_date = dfun(dd2$harvest_date)
	dd2 <- dd2[dd2$intercrop != "Sole Maize", ]

	z2 <- merge(dd2, x2[,nms], by=c("treatment", "trial_id"))
	x2 <- carobiner::bindr(x2, z2)

	d <- carobiner::bindr(x1, x2)

	d$fertilizer_type <- gsub("Urea", "urea", d$fertilizer_type)
	d$fertilizer_type <- gsub("Gypsum", "gypsum", d$fertilizer_type)
	d$fertilizer_type <- gsub("MOP", "KCl", d$fertilizer_type)

	d$intercrops <- tolower(d$intercrops)
	d$intercrops <- gsub("\\+", ";", d$intercrops)
	d$intercrops <- gsub("napashak", "napa shak", d$intercrops)
	d$crop <- tolower(d$crop)
	d$crop[d$crop=="napa"] <- "napa shak"

	d$yield_part <- "grain" 
	d$yield_part[d$crop!="maize"] <- "leaves"

	d$country <- "Bangladesh"
	d$adm1 <- "Rangpur"
	d$on_farm <- TRUE
	d$is_survey <- FALSE
# are you sure?
#	d$irrigated <- FALSE

	geo <- data.frame(
		location=c("Durgapur","Borodarga","Kolkondo","Mohanpur","Lakhitari"), 
		latitude=c(25.55461, 25.82308, 25.86245, 25.93235, 25.88404), 
		longitude=c(89.29195, 89.25810, 89.20563, 88.69496, 89.25188),
		geo_from_source=FALSE
	)
	
	d <- merge(d, geo, by="location", all.x = TRUE)	

	d$irrigated <- NA
	
	carobiner::write_files(path, meta, d)
}
