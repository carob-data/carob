# R script for "carob"

carob_script <- function(path) {

"The present data is from a long-term trial set at Msekera Research Station in Zambia to monitor and evaluate the the longer term effects of conservation agriculture practices on soil quality, soil water dynamics, weeds, pests/diseases and crop yield. The treatments set to investigate this are: T1: Control plot 1 (CPM1); traditional farmers practice mouldboard plough on the flat, maize as a sole crop, no residue retention, stubble incorporated into the row for the following season. T2: Control plot 2 (CPM2); ridge and furrow system dug by hand, maize as a sole crop, no residue retention, stubble incorporated into the row for the following season T3: Basins (BAM), residue retention on the surface, maize as a sole crop T4: Dibble stick (DISM), residue retention on the surface, maize as a sole crop T5: Direct seeder (DSM), residue retention on the surface, maize as a sole crop T6: Direct seeding maize/cowpea intercropping (DS-M/C), 90cm rows, residue retention on the surface T7: Direct seeding cowpea (Cowpea-maize rotation) (DS-MC), residue retention on the surface T8:Direct seeding maize (Maize-cowpea rotation) (DS-CM), residue retention on the surface T9:Direct seeding soya (Soybean-maize r otation) (DS-MS), residue retention on the surface T10: Direct seeding maize (Maize-soybean rotation) (DS-SM), residue retention on the surface. The present data set is from 2012 to 2016. (2016)"

	uri <- "hdl:11529/10844"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=1,
		data_organization = "CIMMYT",
		# Seems also doi_10.1017_S1742170521000028 refers to this dataset?
		publication = "doi:10.1016/j.agee.2021.107812", 
		project = NA,
		data_type = "on-farm experiment",
		carob_contributor = "Blessing Dzuda",
		# If trial_id == 1 for all, then planting_date needs to be added here
		treatment_vars = "land_prep_method; intercrops; residue_prevcrop_used; planting_method; previous_crop",
		response_vars = "yield; dmy_total",
		carob_date = "2024-05-03",
		last_modified = "2024-07-31"
	)



	f <- ff[basename(ff) == "Msekera 2012.2016.xlsx"]
	r1 <- carobiner::read.excel(f, sheet ="All maize yield Msekera")
	r2 <- carobiner::read.excel(f, sheet = "All legume yield Msekera")


### MAIZE TRIAL
	
	d1 <- data.frame(
		trial_id = "1",
		treatment=r1$Label,
		crop=r1$Crop,
		rep=as.integer(r1$Replicate),
		dmy_total=r1$Biomass,
		yield=r1$Grain,
		planting_date=r1$Year
	)

	
### LEGUME TRIAL
	d2 <- data.frame(
		trial_id = "2",
		treatment=r2$Label,
		crop=r2$Crop,
		rep=as.integer(r2$Rep),
		dmy_total=r2$`Biomass yield (kg/ha)`,
		yield=r2$`Grain/cotton yield (kg/ha)`,
		planting_date=r2$Year
	)
	d2$crop <- gsub("SOYABEAN|SOYABEANS|SOYBEAN", "soybean", d2$crop)
	
	d <- rbind(d1, d2)

	d$crop <- tolower(d$crop)

	d$on_farm <- TRUE
	d$is_survey <- FALSE 
	d$irrigated <- FALSE
	d$yield_part <- "grain"
	
	# Management
	d$land_prep_method <- ifelse(d$treatment %in% c("CPM", "CPM1", "CRFM"), "conventional tilled beds", 
	                      ifelse(d$treatment %in% c("CPM2"), "ridge tillage",
	                      ifelse(d$treatment %in% c("BAM"), "basins", NA)))
	d$intercrops <- ifelse(d$treatment %in% c("DS-M/C"), "cowpea", NA)
	d$residue_prevcrop_used <- ifelse(d$treatment %in% c("CPM", "CPM1", "CRFM", "CPM2"), FALSE, TRUE)
	d$planting_method <- ifelse(d$treatment %in% c("DiSM", "DISM"), "dibbling",
	                     ifelse(grepl(c("DS"), d$treatment), "mechanized line sowing", NA))
	d$row_spacing <- ifelse(d$treatment %in% c("DS-M/C"), 90, NA)
	d$seed_density <- 44444
	
	d$country <- "Zambia" 
	d$location <- "Msekera Research Station"
	d$adm1 <- "Eastern Province"
	d$adm2 <- "Chipata"

	d$elevation <- 1018
	d$longitude <- 32.5585
	d$latitude <- -13.645
	d$geo_from_source <- FALSE
	# EGB:
  # # As per publication (supplementary material)
	d$N_fertilizer <- 17 + 92
	d$N_splits <- 2L
	d$P_fertilizer <- 33 * 0.436 # P2O5 to  P
	d$K_fertilizer <- 17 / 1.21 # K2O to K
	
	# EGB:
	# # implement the crop rotations
	
	d$crop <- ifelse(d$treatment %in% c("CPM", "CPM1", "CRFM", "CPM2", "BAM", "DiSM", "DISM", "DSM", "DS-M/C"), "maize",
	          ifelse(d$treatment %in% c("DS-MC", "DS-MS") & as.integer(d$planting_date) %in% c(2012, 2014, 2016), "maize",
	          ifelse(d$treatment %in% c("DS-MC") & as.integer(d$planting_date) %in% c(2013, 2015), "cowpea", "soybean")))
	d$previous_crop <- 
              ifelse(d$treatment %in% c("CPM", "CPM1", "CRFM", "CPM2", "BAM", "DiSM", "DISM", "DSM", "DS-M/C"), "maize",
	          ifelse(d$treatment %in% c("DS-MC", "DS-MS") & as.integer(d$planting_date) %in% c(2013, 2015), "maize",
	          ifelse(d$treatment %in% c("DS-MC") & as.integer(d$planting_date) %in% c(2014, 2016), "cowpea", "soybean")))

	# # implement trial_id
	# # If each year is a different implementation of the experiment then:
#	for (year in seq_along(unique(d$planting_date))) {
#		d$trial_id[grepl(unique(d$planting_date)[year], d$planting_date)] <- year
#	}
	# # else:
	# d$trial_id <- 1
	d$trial_id <- as.character(as.integer(as.factor(paste(d$crop, d$planting_date))))
	
	# # dates
	d$planting_date <- ifelse(d$planting_date == 2012, "2012-12-13",
	                   ifelse(d$planting_date == 2013, "2013-12-24",
	                   ifelse(d$planting_date == 2014, "2015-01-02",
	                   ifelse(d$planting_date == 2015, "2015-12-21", "2016-11-30"))))
	d$harvest_date <- ifelse(d$planting_date == "2012-12-13", "2013-05-01",
	                  ifelse(d$planting_date == "2013-12-24", "2014-05-09",
	                  ifelse(d$planting_date == "2015-01-02", "2015-06-02",
	                  ifelse(d$planting_date == "2015-12-21", "2016-05-08", "2017-05-08"))))

	# EGB:
	# # Descriptive treatments
	d$treatment <- ifelse(d$treatment %in% c("CPM", "CPM1", "CRFM"), "Control plot 1 (CP); traditional farmers practice mouldboard plough on the flat, maize as a sole crop, no residue retention, stubble incorporated into the row for the following season.", 
	               ifelse(d$treatment %in% c("DiSM", "DISM"), "Dibble stick (DiS), residue retention on the surface, maize as a sole crop",
	               ifelse(d$treatment %in% c("CPM2"), "Control plot 2 (CP2); ridge and furrow system dug by hand, maize as a sole crop, no residue retention, stubble incorporated into the row for the following season",
	               ifelse(d$treatment %in% c("BAM"), "Basins (BAM), residue retention on the surface, maize as a sole crop",
	               ifelse(d$treatment %in% c("DSM"), "Direct seeder (DS), residue retention on the surface, maize as a sole crop",
	               ifelse(d$treatment %in% c("DS-MC"), "Direct seeding cowpea (Cowpea-maize rotation) (DS-MC), residue retention on the surface",
	               ifelse(d$treatment %in% c("DS-M/C"), "Direct seeding maize/cowpea intercropping (DS-M+C),  residue retention on the surface",
	               ifelse(d$treatment %in% c("DS-MS"), "Direct seeding with maize-soybean (DS-MSy), residue retention on the surface", NA))))))))
	

	d <- d[!is.na(d$yield), ]
		
	d$yield_moisture <- as.numeric(NA) #needs to be checked

	carobiner::write_files(path, meta, d)
}

