# R script for "carob"


# treatment var "herbicide_product" is not a variable

carob_script <- function(path) {

"
Effectiveness of 14 herbicides were compared during 2016 in El Batán, México. The trial was set up in rainy cycle of 2016, to evaluate herbicides for developing weed management strategies for maize. (2016-07-01)
"
	uri <- "hdl:11529/10548651"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

## metadata 
	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=2,
		data_organization = "CIMMYT",
		publication = NA,
		project = NA,
		data_type = "experiment",
		treatment_vars = "herbicide_product",
		response_vars = "yield", 
		carob_contributor = "Blessing Dzuda",
		carob_date = "2024-08-20",
		notes = NA
	)
	
	f <- ff[basename(ff) == "DAT-BV101_MAIZE-16.xlsx"]
	r <- carobiner::read.excel(f, sheet = "Calculations_Maize")

	d <- data.frame(
		country = "Mexico",
		adm1 = "Mexico",
		adm2 = "Texcoco",
		location = "El Batán",
		site = "CIMMYT",
		elevation = 2200,
		latitude=19.5275,
		longitude=-98.8559,
		trial_id="1",
		planting_date="2016",
		crop= "maize",
		treatment= as.character(r$TrT),
		rep= as.integer(r$Rep),
		tassling_days=r$Tasseling,
		silking_days=r$Silking,
		maturity_days=r$Maturity,
		plant_height=r$Height,
		harvest_index=r$HI,
		dmy_stems=r$DryBiomass10,
		dmy_total=r$TotBiomass10,
		yield_moisture=r$`%HumGrain` * 1000,
		yield=r$`Yield_12%H2O`,
		fwy_storage=r$`Yield/BiomCobs`*1000,
		yield_part="grain",
		seed_weight=r$Thou,
## RH I doubt this is (input) seed, it is more likely output?
## seed_density=r$`Grains/m²`,
		asi=r$ASI,
		flowering_days=r$Flowering
	)

## about the data (TRUE/FALSE)
	d$on_farm <- FALSE
	d$is_survey <- FALSE
	d$irrigated <-FALSE
	d$geo_from_source <- TRUE
	d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
	# all scripts must end like this
	carobiner::write_files(path, meta, d)

}

