# R script for "carob"
# license: GPL (>=3)

## ISSUES
# "__agwise_wly_modelcalibrationdata_metadata.xlsx" dataset does'nt have usable data so it is ignored
# "Water_Limited_Yield" (kg/ha per the data dictionary) is sometimes given as a range (e.g. "1400-1700"), in that case the midpoint was used
# fertilizer, irrigation, rotation, or inoculation information --> not provided
# planting windows are used to estimate planting and harvest dates, but the actual dates are not reported
# "Crop_variety" indicates class maturity (Early/Medium/Late), and not a named cultivar
# no trial-level coordinates are provided; longitude/latitude are estimated from country/adm1 (region) only
# 

carob_script <- function(path) {
 
"
Summary agronomic data for model calibration and validation for key crops in the sub-Saharan Africa

Overview of agronomic data for the calibration and validation of models for major crops in sub-Saharan Africa.
"
	uri <- "doi:10.25502/akvy-ft37/d"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)


	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		data_organization = "IITA; CGIAR System Organisation; UFL; Alliance of Bioversity and CIAT; CIMMYT; ICARDA; IRRI",
		publication = NA,
		project = "CGIAR Excellence in Agronomy (EiA)",
		design = NA,
		data_type = "compilation",
		treatment_vars = "",
		response_vars = "yield", 
		notes = "this dataset is a compilation of literature-derived reference water-limited yield by maturity class and  not measurements from individual field trials or a survey",
		carob_contributor = "Kudzaishe M. Muzata",
		carob_date = "2026-08-24",
		carob_completion = 0,	
		carob_effort = 7
	)
	

	# f1 <- ff[basename(ff) == "agwise_wly_modelcalibrationdata_metadata.xlsx"]
	f2 <- ff[basename(ff) == "data_dictionary.csv"]
	f3 <- ff[basename(ff) == "eia_agwise_model_calibration_data.csv"]

	# r1 <- carobiner::read.excel(f1)
	r2 <- read.csv(f2)
	r3 <- read.csv(f3)

## some rows list several countries at once (rows 41 - 46) so i've expanded those into one row per country
	splitcountry <- function(x) trimws(strsplit(x, ",")[[1]])
	idx  <- rep(seq_len(nrow(r3)), sapply(r3$Country, function(x) length(splitcountry(x))))
	cntry <- unlist(lapply(r3$Country, splitcountry))
	r <- r3[idx, ]
	r$Country <- cntry
	rownames(r) <- NULL

	d <- data.frame(
		country = trimws(r[["Country"]]),
		adm1    = carobiner::fix_name(trimws(r[["Region"]]), "title"),
		crop    = tolower(trimws(r[["Crop"]])),
		
		variety = r[["Crop_variety"]]
	)

	d$trial_id <- as.character(as.integer(as.factor(paste(r$ID, d$country))))

	d$on_farm   <- FALSE
	d$is_survey <- FALSE

	# "Water_Limited_Yield" is the yield attained under water-limited conditions, so the trials are assumed to be rainfed
	d$irrigated <- FALSE

## crop rotation: not reported
	d$crop_rotation <- NA

# no trial-level coordinates are provided in the source dataset. terra and geodata::gadm() were used to estimate coordinates from the country

	country_coords <- data.frame(
		country = c(
			"Ethiopia",
			"Kenya",
			"Ghana",
			"Nigeria",
			"Zambia",
			"Malawi",
			"Mozambique",
			"Rwanda",
			"Senegal"
		),
		longitude = c(
			39.6250,
			37.8457,
			-1.2081,
			8.0976 ,
			27.7904,
			34.3022,
			35.5507,
			29.9264,
			-14.4704
		),
		latitude = c(
			8.6287,
			0.5527,
			7.9696,
			9.5923,
			-13.4572,
			-13.2139,
			-17.2589,
			-2.0004,
			14.3591
		)
	)

	d <- merge(
		d,
		country_coords,
		by = "country",
		all.x = TRUE,
		sort = FALSE
	)

	d$geo_from_source <- FALSE
	d$planting_date <- NA
	d$harvest_date  <- NA

### fertilisers -- not reported in this dataset
	d$P_fertilizer  <- NA
	d$K_fertilizer  <- NA
	d$N_fertilizer  <- NA
	d$S_fertilizer  <- NA
	d$lime          <- NA
	d$fertilizer_type <- NA

## for legumes --> not reported
	d$inoculated <- NA
	d$inoculant  <- NA

### yield
	parse_yield <- function(x) {
	x <- trimws(as.character(x))
	sapply(x, function(v) {
		if (grepl("-", v)) {
			parts <- strsplit(v, "-")[[1]]
			parts <- parts[parts != ""]
			mean(as.numeric(parts))
		} else {
			as.numeric(v)
		}
	}, USE.NAMES = FALSE)
	}
	yield <- parse_yield(r[["Water_Limited_Yield"]])

	# assigning yield_part
	tuber_crops <- c("potato", "cassava")
	legume_crops <- c("common bean", "soybean")
	# grain_crops <- c("maize", "sorghum", "barley", "rice", "wheat", "teff")

	d$yield <- yield
	d$yield_part <- ifelse(d$crop %in% tuber_crops, "tubers",
		ifelse(d$crop %in% legume_crops, "seed", "grain"))

	# no moisture content is reported
	d$yield_moisture <- NA
	d$yield_isfresh <- TRUE

# NOTE: yield is the _fresh weight_ production (kg/ha) of the "yield_part"
	d$fwy_storage <- NA
	d$dmy_storage <- NA

	carobiner::write_files(path, meta, d)
}

## now test your function in a _clean_ R environment (no packages loaded, no other objects available)
# carob_script(path=_____)

