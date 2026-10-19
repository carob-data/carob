# R script for "carob"
# license: GPL (>=3)

## ISSUES


carob_script <- function(path) {

"
Data from: Effect of coupled reduced irrigation and nitrogen fertilizer on soil mite community composition in a wheat field

Overuse of groundwater and nitrogen  fertilizer constitute a severe threat to crop production, thus, low inputs of irrigation  and nitrogen fertilizer were required in current ecological agriculture. The effects of combined reduced irrigation and nitrogen fertilizer addition on soil organism (e.g., mite) community and biodiversity remain poorly understood. We analyzed soil mite community composition, wheat grain yields, and soil characteristics in a ten-year manipulation experiment with two levels of irrigation (reduced irrigation and conventional irrigation) and five levels of nitrogen fertilizer (0, 70, 140, 210, 280 kg N ha-1). The study showed that reduced irrigation (20% reduction, from 280 to 220 mm) and nitrogen fertilizer (25% reduction, from 280 to 210 kg N ha-1) addition did not significantly influence soil mite community and wheat yields. The relative abundances of fungivore and predator showed negative quadratic relationships with wheat yields, while that of plant parasite was positive. The relationships between soil mite trophic groups and wheat yields revealed that we can elevated the impacts of reduced irrigation and nitrogen fertilizer addition from the perspective of soil fauna. The soil mite community composition was altered by soil abiotic factors prior to reduced irrigation and nitrogen fertilizer addition. On the whole, moderate reductions of irrigation and nitrogen fertilizer may not threaten to soil mite community and diversity or decrease crop production, on the contrary, it will benefit to the development of mite community and the sustainable agriculture.
"

	uri <- "doi:10.5061/dryad.50dh670"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=NA,
		publication = "doi:10.1002/ece3.5638",
		project = NA,
		design = "RCBD",
		data_type = "experiment",
		treatment_vars = "N_fertilizer;irrigation_amount",
		response_vars = "count", 
		notes = NA,
		carob_contributor = "Cedric Ngakou",
		carob_date = "2026-10-19",
		carob_completion = 100,	
		carob_effort = 2
	)
	

	f1 <- ff[basename(ff) == "Soil mite taxon abundance.xlsx"]

	r1 <- carobiner::read.excel(f1)
	taxon <- names(r1)[!grepl("Year|Nitrogen|Irrigation", names(r1))]
  
	r <- reshape(r1, varying = taxon, v.names = "count", times = taxon, timevar = "taxon" , direction = "long")
	
	## process
	
	d <- data.frame(
	  planting_date = as.character(r$Year),
		N_fertilizer = r$Nitrogen,
		treatment = r$Irrigation,
		taxon = tolower(r$taxon),
		count = r$count,
		irrigated = TRUE,
		crop = "wheat",
		yield = NA,
	  geo_from_source = TRUE,
		country = "China",
		harvest_date = NA,
		location = "Yucheng Comprehensive Experimental Station",
		longitude = 116.6, ## from publication
		latitude = 36.95,
		elevation = 20
	)
	
	d$N_fertilizer <- c("N0" = 0,"N1" = 70 , "N2" = 140, "N3" = 210, "N4" = 280)[d$N_fertilizer]
  d$irrigation_amount <- ifelse(grepl("CIR", d$treatment), 280, 220)
	d$treatment <- c("CIR" = "conventional irrigation", "RIR" = "reduced irrigation")[d$treatment]
	
	d$is_survey <- FALSE
	d$on_farm <- TRUE
	d$trial_id <- "1"
	d$yield_moisture <- NA
	d$yield_isfresh <- NA
	d$yield_part <- "none"
	d$K_fertilizer <- d$P_fertilizer <- as.numeric(NA)

	
	
	carobiner::write_files(path, meta, d)
}


