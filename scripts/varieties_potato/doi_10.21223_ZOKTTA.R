# R script for "carob"

## planting dates?

carob_script <- function(path) {
  
"B3C3 Population is the resulted fromcrosses between the elite clones from the previous cycle of the same population made at 2011. Since 2012 until 2019 this population was evaluated  for late blight resistance.  51 clones were selected having high levels of late blight resistance in Oxapampa and good tuber yield in Huancayo, both better than in the control varieties. AUDPC values are in the range of 17.50 to 705.83, lower than the AUDPC values of the Yungay and Amarilis control varieties with 1586.67 and 1534.17 respectively. The tuber yield on average in Highlands was from 20.67 to 56.41 th-1 compared to the Yungay and Amarilis varieties with 26.29 and 35.66 th-1 respectively. Under high temperature conditions, the tuber yield in the heat tolerant clones was in the range of 16.32 to 33.71th-1, higher than the Desiree and Amarilis varieties with 13.39 and 5.24 th-1 respectively). Six clones were selected that have some potential for drought tolerance, but further trials are required to confirm these results. The dry matter content of the elite clones is in the range of 17.94 to 26.92% and 33 clones have good frying quality. Twenty-eight clones showed extreme resistance to PVX and 17 to PVY. Fifteen clones have a high parental value to be used as parents in a new selection cycle or in improvement programs in the regions or countries of Latin America, Africa and Asia. Fifteen clones show phenotypic stability for tuber yield.  The process of introduction of these clones to the genebank for international distribution is in progress."


  uri <- "doi:10.21223/ZOKTTA"
  group <- "varieties_potato"
  ff  <- carobiner::get_data(uri, path, group)

  meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
      data_organization = "CIP",
      publication = NA,
      project = NA,
      data_type = "experiment",
      treatment_vars = "variety",
      response_vars = "yield;yield_marketable", 
      carob_contributor = "Henry Juarez",
      carob_effort = NA,
      carob_date = "2024-09-11",
      notes = NA
  )
  
  f <- ff[grep("Data.xls", basename(ff))]
  
#  d <- carobiner::read.excel(f = f, sheet="Table")
	r <- carobiner::read.excel.hdr(f = f, sheet="Table", skip=1, hdr=1)
	d <- data.frame(
		variety = r$Clone,
		`HYO_2015-2018` = r$Marketable.tuber.yield.tons.MTYNA._HYO.2015.2018,
		SRA_2017 = r$SRA.2017,
		`OXA_2015-2018` = r$OXA.2015.2018,
		`MAJ.NI_2018-2019` = r$MAJ.normal.irrigation.NI.2018.2019,
		`MAJ.RI_2018-2019` = r$MAJ.restricted.irrigation.RI.2018.2019,
		AUDPC = r$LB.Average.2015.2018_AUDPC / 100
		# scale = r$Scale,
#		PVX = r$Virus.Resistance_PVX,
#		PVY = r$PVY 
#		heat_tolerance = r$Tolerance_Heat
#		drought_tolerance = r$Drought
#		DM.pct.HYO.2018.2019 
#		Chips.color 
#		Phenotypic.Stability_MTY
	)

  
	vc <- grep("_", colnames(d), value=TRUE)
	d <- reshape(d, direction = "long", idvar = "variety", varying = list(vc), 
						timevar = "trial_id", times = vc, v.names="yield_marketable")
	
	d$loc <- substr(d$trial_id,1, 3)
	d$irrigated <- d$loc == "MAJ"
	d$irrigation_desc <- "none"
	d$irrigation_desc[grep("NI", d$trial_id)] <- "normal"
	d$irrigation_desc[grep("RI", d$trial_id)] <- "restricted"
	
	
	d$yield_marketable <- d$yield_marketable * 1000
	d$rep <- 1L
	d$yield <- as.numeric(NA)
	d$on_farm <- FALSE
	d$is_survey <- FALSE
	d$crop <- "potato"
	d$pathogen <- "Phytophthora infestans"
	d$country <- "Peru"
	d$yield_part <- "tubers"
 

	geo <- data.frame(
		loc = c("HYO", "OXA", "SRA", "MAJ"),
		location=c("Huancayo", "Oxapampa", "San Ramón", "Majes"), 
		adm1 = c("Junin", "Pasco", "Junin", "Arequipa"), 
		adm2 = c("Huancayo", "Oxapampa", "Chanchamayo", "Majes"), 
		longitude = c(-75.2237, -75.3564,  -75.35835, -75.3868), 
		latitude = c(-12.0093, -11.1286, -11.12787, -10.5954)
	)

 
	d <- merge(d, geo, by = "loc", all.x = TRUE)
	d$loc <- NULL
	d$geo_from_source = FALSE
	
	### how is this possible?
	# d$planting_date <- "2017-05-04"
	# d$harvest_date  <- "2017-11-17"
	d$planting_date <- as.character(NA)

	d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <-  as.numeric(NA)

	d <- d[!is.na(d$yield_marketable), ] 
	
	carobiner::write_files(path = path, metadata = meta,wide=d)

}

