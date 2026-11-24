# R script for "carob"
# license: GPL (>=3)

## NOTES
# Sorghum ridging and tie-ridging at Gumara-Maksegnit, 2014 and 2015. Grain and stover are in ton/ha in the dictionary and are stored as kg/ha.
# The dictionary's replicate sentence says teff, and its treatment sentence says N and P2O5 rates. The treatment table and the study title are sorghum ridging and tie-ridging, and that is what is stored. No N or P rate is in the data.
# Year 1 is 2014-06-01 to 2014-12-01 and year 2 is 2015-06-01 to 2015-12-31. Those are the dictionary's season windows.
# Latitude_Location 346012 and Longitude_Location 1373577 are a projected grid. The dictionary says the coordinate reference system is unknown, so they are not stored as latitude and longitude.
# Stover is ton/ha and is not labeled fresh or dry, so it is not dmy_residue or fwy_residue.

## ISSUES
# Grain yield is not adjusted for moisture. No moisture value is given.
# No coordinates in decimal degrees, and on-farm is not stated.
# write_files: , on_farm, yield_moisture, latitude, longitude, irrigated, K_fertilizer, N_fertilizer, P_fertilizer. unknown variables: stover_yield_kg_ha_, head_length_cm_, head_weight_g_, tillers_per_plant_.

## NEW VARIABLES
# head_length: head length, cm.
# head_weight: head weight, g.
# tillers_per_plant_: tillers per plant at harvest.


## NOT PROCESSED
# Stand count is plants per field, and the field area is not given.
# DataDictionary_ElementDescription.csv: documentation or a codebook, not an observation table.
# DataDictionary_Introduction.csv: documentation or a codebook, not an observation table.
# DataDictionary_UniqueIdentifier.csv: documentation or a codebook, not an observation table.


carob_script <- function(path) {

"
Effect of ridging and tie-ridging time on the yield performance of sorghum at Gumara-Maksegnit, north-western Ethiopia. Year 1 ran from 2014-06-01 to 2014-12-01 and year 2 from 2015-06-01 to 2015-12-31. Eight treatments and three replications.
"

	uri <- "hdl:20.500.11766.1/FK2/P024VP"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=3, minor=0,
		data_organization = "ICARDA",
		publication = NA,
		project = "Reducing land degradation and farmers' vulnerability to climate change in the highland dry areas of north-western Ethiopia",
		design = NA,
		data_type = "experiment",
		treatment_vars = "land_prep_method;land_prep_date",
		response_vars = "yield",
		carob_contributor = "Cedric Ngakou",
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 80,
		carob_effort = 2
	)

	f <- ff[basename(ff) == "ExperimentalData.csv"]
	f1 <- ff[basename(ff) == "Treatment.csv"]
	r <- read.csv(f, sep=";", check.names=FALSE)
	r1 <- read.csv(f1, sep=";", check.names=FALSE, fill=TRUE)
	r1 <- r1[, 1:2]
	names(r1) <- c("Treatment", "treatment")
	r <- r[!is.na(r$Year), ]
	r$treatment <- r1$treatment[match(r$Treatment, r1$Treatment)]

	start <- c("1" = "2014-06-01", "2" = "2015-06-01")
	end <- c("1" = "2014-12-01", "2" = "2015-12-31")
	yr <- as.character(r$Year)
  
	d <- data.frame(
		trial_id = paste0("year", yr),
		country = "Ethiopia",
		location = "Gumara-Maksegnit",
		plot_id = paste(yr, r$Treatment, r$Replicate, sep="_"),
		rep = r$Replicate,
		crop = "sorghum",
		treatment = trimws(r$treatment),
		planting_date = unname(start[yr]),
		harvest_date = unname(end[yr]),
		yield = as.numeric(r$Yield) * 1000,
		yield_part = "grain",
		y_residue = as.numeric(r$Stover) * 1000,
		plant_height = as.numeric(r$Plant_Height),
		head_length = as.numeric(r$Head_Lenght), # cm
		head_weight = as.numeric(r$Head_Weight), # cm
		#tillers_per_plant = as.numeric(r$Tillers_N),
		geo_from_source = FALSE,
		is_survey = FALSE,
		latitude = 12.3836  ,
		longitude = 37.5487 ,
		geo_source = "Goole Maps",
		on_farm = TRUE,
		yield_moisture = NA,  ## not provided
		yield_isfresh = NA,
		irrigated = NA,
		land_prep_method = ifelse(grepl("Control",  r$treatment), "none", "tied ridges"),
		dap1 = c(0, 0, 3, 3, 3, 3, 8, NA)[r$Treatment]*7, ## from weeks to days
		dap2 = c(0, 7, 3, 6, 9, 12, 8, NA)[r$Treatment]*7
	)
	
	dat1 <- as.Date(d$planting_date) + d$dap1
	dat2 <- as.Date(d$planting_date) + d$dap2
	d$land_prep_date <- mapply(function(x, y) { if (!is.na(x) && !is.na(y) && x == y) {as.character(x) } else {paste(na.omit(c(x, y)), collapse = ";")}}, dat1, dat2)
  d$dap2 <- d$dap1 <- NULL
  d$land_prep_date <- ifelse(d$land_prep_method =="none", NA, d$land_prep_date)
	
 
  d$K_fertilizer <- d$N_fertilizer <- d$P_fertilizer <- as.numeric(NA)
  
  
	carobiner::write_files(path, meta, d)
}
