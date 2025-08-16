# R script for "carob"


carob_script <- function(path) {

"Replication data for: Identification of QTL for early vigor and stay-green conferring tolerance to drought in two connected advanced backcross populations in tropical maize (Zea mays L.) We aimed to identify quantitative trait loci (QTL) for secondary traits related to grain yield (GY) in two BC1F2:3 backcross populations (LPSpop and DTPpop) under well-watered (4 environments; WW) and drought stressed (6; DS) conditions to facilitate breeding efforts towards drought tolerant maize. Out of the 105 detected QTL, 53 were overdominant indicative of strong heterosis. For 14 out of 18 detected vigor QTL, as well as for eight flowering time QTL the trait increasing allele was derived from CML491. Improving drought tolerance while at the same time maintaining yield potential could be achieved by combining alleles conferring early vigor from the recurrent parent with alleles advancing flowering from the donor. The highest yielding ten entries for all population-by-irrigation treatment combination (except LPSpop WW) used in this study outyielded the best check (CML312/CML444) by 32.5% (DTPpop WW) to 60% (DTPpop DS). Moreover three entries (((CML491/DTPWC9F104)//CML491)B2/CML503; ((CML491/LPSC7F64)//CML491)B154/CML503; ((CML491/LPSC7F64)//CML491)B218/CML503) ranked within the top ten across irrigation treatments. Best performing entries identified here under drought can therefore be used as new trait donor using phenotypic and/or molecular selection. (2016-02-17)"

	uri <- "hdl:11529/10612"
	group <- "varieties_maize"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		data_organization = "CIMMYT",
		publication = NA,
		project = NA,
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "yield", 
		carob_contributor = "Mitchelle Njukuya",
		carob_date = "2024-09-05",
		notes = "not clear which entries were irrigated and which were under induced drought"
	)
	
	#to specify variety codes
	f1 <- ff[basename(ff) == "Entrylist.xlsx"]
	r1 <- carobiner::read.excel(f1, sheet = "EntrylistDTP")
	r1 <- r1[, 1:2]
	r2 <- carobiner::read.excel(f1, sheet = "EntrylistLPS")
	r2 <- r2[, 1:2]
	
	get_data <- function(fname, id) {
	  f <- ff[basename(ff) == fname]
	  r <- read.csv(f, na.strings = ".") 
	  if (is.null(r$mRootLodgingPer)) r$mRootLodgingPer <- NA
	  data.frame( 
	    trial_id = id,
	    crop = "maize",
	    on_farm = TRUE,
	    striga_trial = FALSE, 
	    striga_infected = FALSE,
	    borer_trial = FALSE,
	    rep = r$REP,
	#    location = as.character(r$LOC),
	    variety =as.character(r$ENTRY),
	    anthesis_days = as.numeric(r$mAnthesisDate),
	    silking_days = as.numeric(r$mDaysToSilk),
	    asi = as.numeric(r$mASI),
	    rlper = r$mRootLodgingPer,
	    slper = r$mStemLodgingPer,
	    husk = r$mBadHuskCoverPer,
	    e_rot = r$mEarRotTotalPer,
	    plant_height = as.numeric(r$mPlantHeightCm),
	    ear_height = as.numeric(r$mEarHeightCm),
	    #plant_density = as.numeric(r$mPlantStand_NumPerPlot),
	    yield_part = "grain", 
	    yield = as.numeric(r$mGrainYieldTons_GrainWt) * 1000
	  )
	 }

	d1 <- get_data("DTP_Multiloc_RR_14A---.csv", 1)
	d1$variety_code <- r1$Pedigree[match(d1$variety,r1$Entry)]
	d2 <- get_data("LP_Multiloc_14A_RR_V2.csv", 2)
	d2$variety_code <- r2$Genealogia[match(d2$variety,r2$Entry)]
	d3 <- get_data("Multiloc_DTP_WW_14A.csv", 3)
	d3$variety_code <- r1$Pedigree[match(d3$variety,r1$Entry)]
	d4 <- get_data("Multiloc_LP_WW_14A----.csv", 4)
	d4$variety_code <- r2$Genealogia[match(d4$variety,r2$Entry)]
	
	#binding datasets
	d <- rbind(d1, d2, d3, d4)

	d$country <- "Mexico"
	d$adm1 <- "Mexico"
	d$adm2 <- "Texcoco"
	d$location <- "El Batán"
	d$site <- "CIMMYT"
	d$elevation <- 2200
	d$latitude<-19.5293
	d$longitude<- -98.8492
	d$geo_from_source <- FALSE

	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$irrigated <- NA
	d$planting_date <- as.character(NA)
	
	d$trial_id <- as.character(d$trial_id)
	
	d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
  d$anthesis_days[d$anthesis_days == 0] <- NA
  d$silking_days[d$silking_days==0] <- NA
  d$ear_height[d$ear_height < 2] <- NA
  d$plant_height[d$plant_height==0] <- NA
  d$yield[d$yield < 0] <- NA
  
  carobiner::write_files(path, meta, d)
}



