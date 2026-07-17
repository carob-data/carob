# R script for "carob"
# license: GPL (>=3)

## ISSUES
#1. bad value in r$ppt_DM_roots (3.833333). Should bre a fraction.


carob_script <- function(path) {

"
Cassava storage root yield as affected by planting stake orientation, diameter, fertilizer application and variety in three sites in SW Nigeria

The aim of this investment is to verify best planting practices for cassava and achieve impact at smallholder level at scale through agronomic decision support and tailored advice on simple low cost measures. Research results will be integrated with the AKILIMO service for smallholder cassava growers, so that these benefit the wider research-and-development community involved in agronomy-at-scale.

Orientation of the planting stake in the soil at 3 levels	
	Horizontal = stakes buried at 5-7 cm depth in the soil in horizontal position
	Slanted = stakes inserted to two thirds of their length into the soil at an angle of about 45 degrees
	Vertical = stakes inserted to two thirds of their length into the soil at an angle of 90 dagrees thus vertically
"

	uri <- "doi:10.25502/35xn-j342/d"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		data_organization = "IITA",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety;fertilizer_used;planting_orientation",#orientation of cassava cuttings when planting
		response_vars = "yield", 
		notes = NA,
		carob_contributor = "Blessing Dzuda",
		carob_date = "2026-07-17",
		carob_completion = 90,	
		carob_effort = 4
	)

	f <- ff[basename(ff) == "staggered-planting-stake-orientation-expt-1.csv"]
	r <- read.csv(f)
	#removing empty fields without data
	r <- r[!is.na(r$PLOT), ]
 
	d <- data.frame(
	  country="Nigeria",
	  plot_id=as.character(r$PLOT),
	  location=trimws(r$Site),
	  variety=r$VAR,
	  planting_orientation=r$Orientation,
	  crop="cassava",
	  fertilizer_used=r$FERT,
	  rep=as.integer(r$REP),
	  plant_density=r$plt_ha_52,# since cassava is harvested in 9-12months, this is the true plant popilation
	  plant_height=r$PHT24, # taking the last recorded height as true plant height
	  yield=r$FRYield*1000, # only considering the fresh useful mass 
	  dmy_roots=r$DRY_root_yield_Mg_ha*1000,
	  yield_moisture=(1 - r$ppt_DM_roots) * 100,
	  yield_part="roots",
	  yield_isfresh=TRUE
	)
  
	d$yield_moisture[d$yield_moisture < 0] <- NA
	d$location <- ifelse(d$location %in% c("D23", "WB_South"), "IITA HQ, Ibadan", 
				ifelse(d$location == "Ile_Ife", "Obafemi Awolowo University, Ile-Ife", NA))
	
	d$on_farm <- FALSE
	d$is_survey <- FALSE
	d$irrigated <- FALSE
	d$geo_from_source <- FALSE
	d$planting_date <- NA
	d$fertilizer_used <- d$fertilizer_used == "Fert"
	d$P_fertilizer <- d$K_fertilizer <- d$N_fertilizer <- 0
	d$P_fertilizer[d$fertilizer_used] <- 20
	d$K_fertilizer[d$fertilizer_used] <- 90
	d$N_fertilizer[d$fertilizer_used] <- 75
	d$fertilizer_type[d$fertilizer_used] <- "NPK" 
	d$trial_id <- paste(d$site, d$plot_id, sep = "_")
  
  #adm levels generated from site of experiment
  geo <- data.frame(
    location=c("Obafemi Awolowo University, Ile-Ife", "IITA HQ, Ibadan"),
    adm1 = c("Osun", "Oyo"),
    adm2 = c("Ife Central", "Akinyele"),
    ## site=c("Ajebandele", "Moniya"), ?where is that from?
    longitude = c(4.51956, 3.8989),
    latitude = c(7.51789, 7.4979), 
    geo_uncertainty = c(7430, 1000),
    geo_source = c("GADM 4.1, adm2", "GADM 4.1, adm2")
  )
  
  d <- merge(d, geo, by="location", all.x = TRUE)
   
	carobiner::write_files(path, meta, d)
}
