# R script for "carob"
# license: GPL (>=3)

## ISSUES


carob_script <- function(path) {

"
Effect of inter cropping forages with maize in conventional tillage and conservation agriculture

Effect of intercropping forages with maize in conventional tillage and conservation agriculture
"


	uri <- "doi:10.7910/DVN/JCEOQJ"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)


	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "TAMU", #TAMU : Texas A & M University
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "intercrops;land_prep_method",
		response_vars = "yield", 
		notes = NA,
		carob_contributor = "Cedric Ngakou",
		carob_date = "2026-09-27",
		carob_completion = 100,	
		carob_effort = 1
	)
	

	f1 <- ff[basename(ff) == "SIPSIN_Effect of intercropping forages with maize in conventional tillage and conservation agriculture  .xlsx"]

	r1 <- carobiner::read.excel(f1, sheet="Yield_Maize_Forage", skip= 4, na= c("Average"))
  #r2 <- carobiner::read.excel(f1, sheet="description")

	d1 <- data.frame(
	  yield_mz_CT = r1$`Maize Yield (kg/ha)`,
	  yield_mz_CA = r1$`Maize Yield(kg/ha)`,
	  lab_CT = r1$`Lablab Yield (kg/ha)`,
	  lab_CA = r1$`Lablab Yield (kg/ha)`,
	  vect_CT = r1$`Vetch Yield (kg/ha)`,
	  vect_CA = r1$`Vetch Yield (kg/ha)`,
	  plot_id = as.character(r1$...1),
	  adm1 = "Amhara",
	  location = "Dangila",
	  longitude = 36.841 ,
	  latitude = 11.257,
	  country = "Ethiopia",
	  trial_id = "1"
	)
	
	d <- reshape(d1, varying = c("yield_mz_CT", "lab_CT", "vect_CT", "yield_mz_CA", "lab_CA", "vect_CA"), v.names = "yield",
	             timevar = "treatment",
	             direction = "long")
  d$intercrops <- c("lablab", rep("maize", 2), "lablab", rep("maize", 2))[d$treatment]
  d$crop <- c("maize", "lablab", "vetch", "maize", "lablab", "vetch")[d$treatment]
  d$land_prep_method <- c(rep("conventional", 3), rep("minimum tillage", 3))[d$treatment]
  d$treatment <- c(rep("conventional", 3), rep("minimum tillage", 3))[d$treatment]
  d$yield <- as.numeric(gsub(" ", "", d$yield))
  d$id <- NULL
 
  d <- d[!is.na(d$plot_id),]
 
  d$is_survey <- FALSE
  d$on_farm <- TRUE
  d$yield_moisture <- NA
  d$yield_isfresh <- NA
  d$yield_part <- "grain"
  d$yield_part <- ifelse(grepl("lablab|vetch", d$crop), "aboveground biomass", d$yield_part)
  d$geo_from_source <- FALSE
  d$geo_source <- "Google Maps"
  d$irrigated <- NA
  d$K_fertilizer <- d$N_fertilizer <- d$P_fertilizer <- as.numeric(NA)
  d$planting_date <- NA
  d$harvest_date <- NA
 
 
	carobiner::write_files(path, meta, d)
}


