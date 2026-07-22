# R script for "carob"
# license: GPL (>=3)

## ISSUES
#1. new crop added
#2.new variables added

carob_script <- function(path) {

"
Greenhouse gas emissions from soils of a long-term trial in Western Kenya

Four seasons of N2O and CO2 emission from four selected treatments of CIAT's long-term trial INM3 located in Western Kenya
"

	uri <- "doi:10.7910/DVN/SC0VKF"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=5,
		data_organization = "CIAT",
		publication = "doi.org/10.1007/s10705-015-9693-6",
		project = NA,
		carob_date = "2026-07-19",
		design = "split–split–split plot",
		data_type = NA,
		treatment_vars = "N_fertilizer;P_fertilizer;OM_used;residue_prevcrop_used;crop_rotation",
		response_vars = "methane_flux;nitrous_oxide_flux;carbon_dioxide_flux", 
		carob_contributor = "Blessing Dzuda",
		carob_completion = 100,	
		carob_effort = 6,
		notes = NA
	)
	

	f <- ff[basename(ff) == "02. GHG Data.csv"]
	r <- read.csv(f, fileEncoding = "latin1")
	
	d <- data.frame(
	  country="Kenya",
	  adm1="Siaya",
	  adm2="Gem",
	  adm3="West Gem",
	  longitude=34.4112,
	  latitude=0.0378,
	  geo_uncertainty=7994,
	  geo_source="GADM 4.1, adm3",
	  plot_id=as.character(r$Plot),
	  crop="maize",
	  yield_part="grain",
	  yield=as.numeric(NA),
	  yield_moisture=as.numeric(NA),
	  yield_isfresh=NA,
	  planting_date=NA,
	  rep=r$Rep.,
	  #treatment=r$Treatm.,numeric codes not defined, silencing the column
	  crop_rotation=r$Rotation,
	  methane_flux=r$CH4.FLUX..mg.m..h.,
	  carbon_dioxide_flux=r$CO2.FLUX..mg.C.m..h.,
	  nitrous_oxide_flux=r$N2O.FLUX..µg.m..h.
	)
	
	d$date <- as.character(as.Date(r$Date, format = "%d-%b-%y"))
	d$trial_id <- paste(d$adm3,d$plot_id,sep = "_")
	d$OM_used <- r$FYM=="Plus FYM"
	d$OM_type[d$OM_used==TRUE] <- "farmyard manure"
	d$OM_amount[d$OM_type=="farmyard manure"] <- 4000
	d$residue_prevcrop_used <- r$Residues=="R+"
	d$residue_prevcrop <- ifelse(d$residue_prevcrop_used, 2000, 0)
	
	# Extract fertilizer values
	d$N_fertilizer <- as.numeric(sub("N([0-9]+)_P([0-9]+)", "\\1", r$N.P))
	d$P_fertilizer <- as.numeric(sub("N([0-9]+)_P([0-9]+)", "\\2", r$N.P))
	d$fertilizer_used <- !(d$N_fertilizer == 0 & d$P_fertilizer == 0)
	d$K_fertilizer <- ifelse(d$fertilizer_used,29.9,0)#determined from the publication
	
	#rotations
	rotations <- c(
	  "M-M"="maize;maize",
	  "T-M"="tephrosia;maize",
	  "M-T"="maize;tephrosia")
	
	d$crop_rotation <- rotations[d$crop_rotation]
	
	d$on_farm <- FALSE
	d$is_survey <- FALSE
  d$irrigated <- FALSE
  d$geo_from_source <- FALSE
  d$harvest_date <- NA
  
	carobiner::write_files(path, meta, d)
	
}
