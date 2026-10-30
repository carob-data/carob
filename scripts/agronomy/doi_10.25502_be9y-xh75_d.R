# R script for "carob"
# license: GPL (>=3)

## ISSUES
#The dataset has some missing values for OM_amount

carob_script <- function(path) {

"
ISFM/SOM long-term trials maize

Collecting long-term data from ISFM maize trials and modelling them with the DayCent model
"

	uri <- "doi:10.25502/be9y-xh75/d"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)


	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		data_organization = "IITA;ETH",
		publication = "doi.org/10.1016/j.fcr.2022.108788",
		project = NA,
		design = "split plot design",
		data_type = "on-farm experiment",
		treatment_vars = "OM_amount;OM_type;N_fertilizer",
		response_vars = "yield", 
		carob_contributor = "Premrose Masunungure",
		carob_date = "2026-10-16",
		carob_completion = 90,	
		carob_effort = 6
	)
	

	f1 <- ff[basename(ff) == "yield_data_combined.csv"]
	f2 <- ff[basename(ff) == "data_dictionary_yield.csv"]

	r1 <- read.csv(f1)
	r2 <- read.csv(f2)


	r1$OM_type[r1$OM_type == ""] <- NA
	r1$OM_type[is.na(r1$OM_type) & r1$OM_amount == 0] <- "CT"
	
	d <- data.frame(
		plot_id = as.character(r1$Plot_NO),
		rep = r1$Rep,
		OM_amount = r1$OM_amount,
		yield = r1$grainYield * 1000,
		dmy_total = r1$totalYield *1000,
		dmy_residue = r1$stoverYield * 1000,
		location = r1$Site,
		K_fertilizer = r1$K_amount,
		P_fertilizer = r1$P_amount,
		N_fertilizer = r1$N_amount,
		season = ifelse(r1$season == "LRS", "short rains",
		                ifelse(r1$season == "SRS", "long rains", r1$season)),
		OM_type = ifelse(r1$OM_type == "CT", "none",
		                 ifelse(r1$OM_type == "FYM", "farmyard manure",
		                        ifelse(r1$OM_type == "SD", "sawdust",
		                               (ifelse(r1$OM_type == "CC", "foliage",
		                                             ifelse(r1$OM_type == "TD", "foliage",
		                                                    ifelse(r1$OM_type == "MS", "unknown",
		                                                    ifelse(r1$OM_type == "CL4", "unknown", r1$OM_type)))))))),
		harvest_date = format(as.Date(r1$Harvest_date, format = "%d/%m/%Y"), "%Y-%m-%d"),
		country = "Kenya"
	)
	
	loc <- data.frame(
	  location = c("Sidada", "Aludeka", "Embu", "Machanga"),
	  latitude = c(0.143, 0.574, -0.517, -0.793 ),
	  longitude = c(34.422, 34.191, 37.459, 37.664 )
	    )

	d <- merge(d, loc, by = "location", all.x = TRUE, sort = FALSE)
	
	d$trial_id <- "1"
	d$geo_from_source <- TRUE    #The co-ordinates were obtained from publication
	
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$irrigated <- NA
	d$planting_date <- NA


  d$fertilizer_type <- "CAN; TSP; KCl"
	d$yield_part <- "grain"
	d$crop <- "maize"
	d$yield_isfresh <- NA
	d$yield_moisture <- as.numeric(NA)

	carobiner::write_files(path, meta, d)
}


