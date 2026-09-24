# R script for "carob"
# license: GPL (>=3)

## ISSUES
#1. added new variables

carob_script <- function(path) {

"
Biophysical and Socio-Economic Benefits of Contour Farming

Contour farming with banks stabilization using fodder crops (trees and grass) was introduced and validated by Africa RISING researchers in Kongwa and Kiteto district. The technology involves the construction of Fanya chini (bank-below-ditch) along a contour on the upper edge of the field as the first line of defense to capture run-off entering the field, and Fanya juu (bank-over-ditches) is constructed along contours within the field to intercept run-off water. Food crops are grown in the alleys between the Fanya juus. Fodder trees ( e.g. Gliricidia sepium), grass fodder (e.g. Guatemala; Tripsacum spp), and/or cover crops can be planted on the banked soil to stabilize the ridges and provide multiple benefits to farmers. The benefits of this technology include the reduction of soil erosion and the loss of nutrients to rehabilitate land for crop production. In addition, it produces fodder and fuelwood from vegetation grown on the banks to diverse production and income sources as a risk mitigation and climate adaptation strategy. This dataset contains data collected to validate this technology with farmers.
"

	uri <- "doi:10.7910/DVN/FJQ387"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "ICRAF",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "on-farm experiment",
		treatment_vars = "contours_used",
		response_vars = "yield", 
		notes = NA,
		carob_contributor = "Blessing Dzuda",
		carob_date = "2026-09-24",
		carob_completion = 99,	
		carob_effort = 5
	)

	f2 <- ff[basename(ff) == "002_yield_cf_contourFarming-1.csv"]
	f3 <- ff[basename(ff) == "003_yield_ff_contourFarming.csv"]
	f4 <- ff[basename(ff) == "004_fodder_contourFarming.csv"]
	f5 <- ff[basename(ff) == "soil_contourFarming.csv"]

	r2 <- read.csv(f2)
	r3 <- read.csv(f3)
	r4 <- read.csv(f4)
	r5 <- read.csv(f5, fileEncoding = "latin1")

	cont1 <- data.frame(
	  date=r2$Year,
	  terrain=tolower(r2$Site),
	  plot_id=as.character(r2$Plot),
	  treatment=tolower(r2$Contour.),
	  yield=r2$Grain..t.ha.*1000,
	  dmy_residue=r2$Stover..t.ha.*1000,
	  cob_weight=r2$Cob..t.ha.*1000
	)
	
	cont2 <- data.frame(
	  date=r4$Year,
	  terrain=tolower(r4$Site),
	  plot_id=as.character(r4$Plot),
	  treatment=tolower(r4$Contour.),
	  yield=r4$Grain..t.ha.*1000,
	  dmy_residue=r4$Stover..t.ha.*1000,
	  cob_weight=r4$Cob..t.ha.*1000
	)
	
	ctrl <- data.frame(
	  date=NA,
	  terrain=NA,
	  plot_id=NA,
	  treatment="control",
	  yield=r3$Grain..t.ha.*1000,
	  dmy_residue=r3$Stover..t.ha.*1000,
	  cob_weight=r3$Cob..t.ha.*1000
	)
	
	d <- rbind(cont1,cont2,ctrl)
	d$treatment <- gsub("^contour([1-6])$", "contour \\1", d$treatment)
	
	soil <- data.frame(
	  treatment=tolower(r5$Plot.ID),
	  soil_pH=r5$pHw,
	  soil_N=r5$Total.N....,
	  soil_SOC=r5$OC....,
	  soil_P=r5$Bry.1_.P..mg.kg.soil..,
	  soil_S=r5$SO4...S..mg.kg.soil..*0.3338,#extracting sulphur only
	  soil_Ca_exch=r5$Exch..Ca2...Cmolkg.1.,
	  soil_Mg_exch=r5$Exch..Mg2...Cmolkg.1.,
	  soil_Na_exch=r5$Exch.Na...Cmolkg.1.,
	  soil_K_exch=r5$Exch..K...Cmolkg.1.,
	  soil_bd=r5$Bulky.density..g.cm3.,
	  soil_sand=r5$Sand,
	  soil_silt=r5$Silt....,
	  soil_clay=r5$Clay....,
	  soil_texture=tolower(r5$Textural.Class),
	  soil_P_method="Bray-1"
	)
	
	d <- merge(d,soil,by="treatment", all.x = TRUE)
	
	d$country <- "Tanzania"
	d$adm1 <- "Dodoma"
	d$adm2 <- "Kongwa"
	d$adm3 <- "Mlali"
	#above data was obtained from the data webpage on the file's details
	d$longitude <- 36.7439
	d$latitude <- -6.2791	
	d$geo_uncertainty <- 9320
	d$geo_source <- "GADM 4.1, adm3"
	d$crop <- "maize"
	
	d$trial_id <- "1"
	
	d$date <- as.character(d$date)
	d$contours_used <- !d$treatment=="control"
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$irrigated <- FALSE
	d$geo_from_source <- FALSE

	d$planting_date <- NA
	d$harvest_date  <- NA
  d$P_fertilizer <- d$K_fertilizer <- d$N_fertilizer <- as.numeric(NA)
	d$yield_part <- "grain"
	d$yield_moisture <- NA
	d$yield_isfresh <- NA

	d$soil_texture <- gsub("\u00A0", " ", d$soil_texture)
	d$soil_texture <- trimws(d$soil_texture)	  
	
	d <- unique(d)#rows seem duplicated because of omission of other differentiating variables like caloric and protein yield
	
	carobiner::write_files(path, meta, d)
}

