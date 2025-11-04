# R script for "carob"


carob_script <- function(path) {

"Farmers’ participatory researchers managed long-term trails aimed to improve the productivity, profitability, and sustainability of smallholder agriculture in the EGP by activities carried out to address the objectives: 1. Understand farmer circumstances with respect to cropping systems, natural and economic resources base, livelihood strategies, and capacity to bear risk and undertake technological innovation. 2. Develop with farmers more productive and sustainable technologies that are resilient to climate risks and profitable for small holders. 3. Facilitate widespread adoption of sustainable, resilient, and more profitable farming systems. (2018-02-18)]"

	uri <- "hdl:11529/10547970"
	group <- "agronomy"

	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=1,
		project=NA,
		publication= NA,
		data_organization = "CIMMYT;UBKV",
		data_type="experiment", 
		carob_contributor="Mitchelle Njukuya",
		carob_date="2023-10-10",
		response_vars= "yield",
		treatment_vars="N_fertilizer;P_fertilizer;K_fertilizer;B_fertilizer"				
	)


	get_raw_data <- function(f) {
		if (basename(f) == "Rabi Maize 2016-17-LT-all nodes-Malda.xlsx") {
			skip1 = 3
		} else {
			skip1=4
		}
		r1 <- carobiner::read.excel.hdr(f, sheet ="4- Stand counts & Phenology", skip=skip1, hdr=2)
		r2 <- carobiner::read.excel.hdr(f, sheet ="14 - Grain Harvest ", skip=4, hdr=2)
		r3 <- carobiner::read.excel.hdr(f, sheet ="6 - Fertilizer amounts ", skip=4, hdr=2)

		colnames(r2) <- gsub("Calculation_", "", colnames(r2))
		colnames(r2) <- gsub("Straw.Stover.yield.t.ha" , "Straw.yield.t.ha" , colnames(r2))
		
		nms <- c("Site.No", "Tmnt", "Grain.yield.t.ha", "TGW.g", "Biomass.t.ha", "Straw.yield.t.ha")
		r2 <- r2[, nms]
		
		colnames(r3) <- gsub("Kg.ha_N.kg.ha", "N.kg.ha", colnames(r3))
		nms <- c("Site.No", "Tmnt", "N.kg.ha", "P2O5.kg.ha", "K2O.kg.ha", "Gypsum.kg.ha", "ZnSO4.kg.ha", "Boric.acid.kg.ha", grep("Product.used", names(r3), value=TRUE))
		r3 <- r3[, nms]

		r <- merge(r1, r2, by=c("Site.No", "Tmnt"))
		merge(r, r3, by=c("Site.No", "Tmnt"))
	}


### 4- Stand counts & Phenology #####

	process_data <- function(r) {

		## do not do this and then subset
		#	d <- r
		# rather start a new data.frame
	   d <- data.frame(
			trial_id = as.character(r$Trial.Code), 
			treatment = r$Tmnt,
			crop=tolower(r$Crop), variety= r$Variety, 
			season=r$Season, 
			dmy_total = r$Biomass.t.ha * 1000,
			fwy_residue = r$Straw.yield.t.ha * 1000,
			yield = r$Grain.yield.t.ha * 1000,
			emergence_days = r$X100pct.emergence.DAS,
			anthesis_days = r$X50pct.anthesis.DAS,
			maturity_days = r$X80pct.physiological.maturity.DAS,
			N_fertilizer = r$N.kg.ha,
			P_fertilizer = r$P2O5.kg.ha / 2.29,
			K_fertilizer = r$K2O.kg.ha / 1.2051,
			B_fertilizer = r$Boric.acid.kg.ha * 0.1748,
			S_fertilizer = 0,
			Zn_fertilizer = 0,
			lime = 0,
			gypsum = 0
		)

		i <- grep("Date.of.seeding", names(r))
		d$planting_date = as.character(as.Date(r[,i]))
		i <- grep("Dat..of.harvest", names(r))
		d$harvest_date = as.character(as.Date(r[,i]))			

		d$on_farm <- TRUE
		d$is_survey <- FALSE 
		d$irrigated <- TRUE
		d$inoculated <- FALSE
		d$yield_part <- "grain"

		d$country <- "India"
		d$adm1 <- "West Bengal"
		d$adm2 <- "Malda"
		d$location <- r$Node
		
	##### Fertilizer ########
		frp <- apply(r[, grep("Product.used", names(r), value=TRUE)], 1, 
			function(i) paste(unique(i[!is.na(i)]), collapse="; "))
		
		# the excel file has encoded 10:26:26 as "time"!
		frp <- gsub("1899-12-31 10:26:26", "NPK", frp)
		frp <- gsub("UREA", "urea", frp)
		frp <- gsub("Urea", "urea", frp)
		frp <- gsub("MoP", "KCl", frp)
		frp <- gsub("Boron 20%", "borax", frp) #?
		frp <- gsub("Borax 21", "borax", frp)
		frp <- gsub("Boron", "borax", frp) #?
		frp[frp == ""] <- NA
		d$fertilizer_type <- frp
		d
	}


	fun <- function(f) {
		r <- get_raw_data(f)
		process_data(r)
	}
	
	d <- lapply(ff, fun)
	d <- do.call(rbind, d)

	d$longitude <- NA
	d$latitude <- NA
	d$longitude [d$location =="Mahadipur"] <- 88.1265
	d$latitude [d$location =="Mahadipur"] <- 24.8501 
	d$longitude [d$location =="Bidyanandapur"] <- 87.9903
	d$latitude [d$location =="Bidyanandapur"] <- 25.9517
	d$longitude [d$location =="Urgitola"] <- 88.1411
	d$latitude [d$location =="Urgitola"] <- 25.0108
	d$longitude [d$location =="Gaurangapur"] <- 87.878
	d$latitude [d$location =="Gaurangapur"] <- 22.287
	d$longitude [d$location =="Kalinagar"] <- 88.316
	d$latitude [d$location =="Kalinagar"] <- 22.673
	d$geo_from_source <- FALSE

	d$crop <- gsub("kidneybean", "kidney bean", d$crop)
	d$season <- "rabi"

	d$yield_moisture <- as.numeric(NA) #needs to be checked

	carobiner::write_files(meta, d, path=path)
}

