# R script for "carob"
# license: GPL (>=3)

## ISSUES
# No location below country level exists in the source.
# No planting_date in the source - only HarvestDate is available.
#

carob_script <- function(path) {

"
Cassava maize intercropping in Nigeria

ACAI is a 5 year Bill & Melinda Gates Foundation funded project in 5 countries in Africa (Nigeria and Tanzania) providing tailored agronomic advice to small scale cassava growers in the target countries. The project delivers agronomic solutions to imporve cassava root yield and quality and the necessary knowledge base and applications for accessing this knowledge to cassava scaling partners and ultimately farmers in the target countries while instituting the necessary capacity and skills for national system scientists to engage in transformative cassava agronomy.
"

	uri <- "doi:10.25502/dvff-xx49/d"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		data_organization = "IITA",
		publication = NA,
		project = "ACAI",
		design = NA,
		data_type = "on-farm experiment",
		treatment_vars = "intercrop;plant_density;N_fertilizer;P_fertilizer;K_fertilizer;land_prep_method",
		response_vars = "yield;yield_marketable;root_infection", 
		carob_contributor = "Premrose Masunungure",
		carob_date = "2026-07-17",
		carob_completion = 80,	
		carob_effort = 8
	)
	

	f1 <- ff[basename(ff) == "cim_ckan.csv"]
	#f2 <- ff[basename(ff) == "cim_ckan_treatments.csv"]
	#f3 <- ff[basename(ff) == "data_dictionary_cim_ckan.csv"],
	r <- read.csv(f1) 

    d1 <- data.frame(
		crop="cassava",
		country = r$country,
		trial_id = r$trial_ID,
		plot_id = r$plot_ID,
		treatment = r$trt_code,
		fertilizer = r$Fertilizer,
		plant_density=r$cassava_density,
		intercrops=ifelse(!is.na(r$maize_density), "maize", "none"),
		intercroped=r$maize_density > 0,
		intercrop_density =ifelse(r$maize_density > 0, r$maize_density, 0),
		harvest_date = as.character(as.Date(r$HarvestDate, format = "%d/%m/%Y")),
		variety = ifelse(trimws(r$Variety) == "", NA, r$Variety),
		land_prep_method = ifelse(trimws(r$flat_ridge) == "", NA, r$flat_ridge),
	  # assumed the weights provided are in ton/ha as plant density is reported as per ha
		yield = rowSums(r[, c("tuberizedMarketableRootsFW","tuberizedDiseasedRootsFW","tuberizedSmallRootsFW")], na.rm = FALSE),
		yield_marketable = r$tuberizedMarketableRootsFW,
		root_infection = r$tuberizedDiseasedRootsFW
	)

    d2 <- data.frame(
		crop="maize",
		country = r$country,
		trial_id = r$trial_ID,
		plot_id = r$plot_ID,
		treatment = r$trt_code,	
		fertilizer = r$Fertilizer,
		plant_density=r$maize_density,
		intercrop_density =ifelse(r$cassava_density > 0, r$cassava_density, 0),	
		intercrops=ifelse(r$cassava_density > 0, "cassava", "none"),
		intercroped=r$cassava_density > 0
	)
	d2 <- d2[!is.na(d2$plant_density), ]
	
	d <- carobiner::bindr(d1, d2)
	d$root_infection <- d$root_infection / d$yield

	d$fertilizer[d$fertilizer == ""] <- "0:0:0 none"

	frt1 <- do.call(rbind, strsplit(d$fertilizer, " "))
	d$fertilizer_type <- paste0("urea;", frt1[,2])
	d$fertilizer_type[grepl("none", d$fertilizer_type)] <- ""
	d$fertilizer_type <- gsub("MOP", "KCl", d$fertilizer_type)

	frt2 <- do.call(rbind, strsplit(frt1[,1], ":"))
	d$N_fertilizer <- as.numeric(frt2[,1])
	d$P_fertilizer <- as.numeric(frt2[,2])
	d$K_fertilizer <- as.numeric(frt2[,3])
		
	d$on_farm <- NA
	d$is_survey <- FALSE
	d$irrigated <- NA	
	d$longitude <- NA
	d$latitude <- NA
	d$geo_from_source <- NA
	d$yield_part <- "roots"
	d$yield_moisture <- as.numeric(NA)
	d$yield_isfresh <- TRUE
	
	carobiner::write_files(path, meta, d)
}


