# R script for "carob"
# license: GPL (>=3)

## ISSUES
#1. ambiguous values in the moisture column (-2.833, 0.811), derived from dry matter content, originate from the raw dataset

carob_script <- function(path) {

"
Cassava storage root yield as affected by planting stake orientation, diameter, fertilizer application and variety in three sites in SW Nigeria

The aim of this investment is to verify best planting practices for cassava and achieve impact at smallholder level at scale through agronomic decision support and tailored advice on simple low cost measures. Research results will be integrated with the AKILIMO service for smallholder cassava growers, so that these benefit the wider research-and-development community involved in agronomy-at-scale.
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
		treatment_vars = "variety;fertilizer_used;sowing_orientation",#orientation of cassava cuttings when planting
		response_vars = "yield", 
		notes = NA,
		carob_contributor = "Blessing Dzuda",
		carob_date = "2026-07-17",
		carob_completion = 90,	
		carob_effort = 4
	)

	f <- ff[basename(ff) == "staggered-planting-stake-orientation-expt-1.csv"]
	r <- read.csv(f)
 
	d <- data.frame(
	  country="Nigeria",
	  plot_id=as.character(r$PLOT),
	  site=r$Site,
	  variety=r$VAR,
	  sowing_orientation=r$Orientation,
	  crop="cassava",
	  fertilizer_used=r$FERT,
	  rep=as.integer(r$REP),
	  plant_density=r$plt_ha_52,#since cassava is harvested in 9-12months, this is the true plant popilation
	  plant_height=r$PHT24,#taking the last recorded height as true plant height
	  yield=r$FRYield*1000,#only considering the fresh useful mass 
	  dmy_roots=r$DRY_root_yield_Mg_ha*1000,
	  yield_moisture=(1 - r$ppt_DM_roots) * 100,
	  yield_part="roots",
	  yield_isfresh=TRUE
	  )
  
	d$site <- ifelse(trimws(d$site) %in% c("D23", "WB_South"), "IITA Research Centre", "Obafemi Awolowo University")
	
	d$on_farm <- FALSE
	d$is_survey <- FALSE
	d$irrigated <- FALSE
	d$geo_from_source <- FALSE
	d$planting_date <- NA
	d$fertilizer_used <- tolower(trimws(d$fertilizer_used)) == "fert"
	d$P_fertilizer[d$fertilizer_used] <- 20
  d$K_fertilizer[d$fertilizer_used] <- 90
  d$N_fertilizer[d$fertilizer_used] <- 75
  d$fertilizer_type[d$fertilizer_used] <- "NPK" 
  d$trial_id <- paste(d$site,d$plot_id,sep = "_")
  
  #location data
  #adm levels generated from site of experiment
  loc <- data.frame(
    site=c("Obafemi Awolowo University","IITA Research Centre"),
    adm1 = c("Osun", "Oyo"),
    adm2 = c("Ife Central", "Akinyele"),
    location=c("Ajebandele","Moniya"),
    longitude = c(4.6055, 3.9162),
    latitude = c(7.4562, 7.5612),
    geo_uncertainty = c(7430, 19916),
    geo_source = c("GADM 4.1, adm2", "GADM 4.1, adm2")
  )

  d <- merge(d,loc,by="site",all.x = T)
  
  #removing empty fields without data
  d <- d[!is.na(d$plot_id), ]
  
	carobiner::write_files(path, meta, d)
}
