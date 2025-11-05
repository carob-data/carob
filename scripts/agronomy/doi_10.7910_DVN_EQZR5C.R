# R script for "carob"


carob_script <- function(path) {

"International Maize and Wheat Improvement Center (CIMMYT); Zambian Agriculture Research Institute (ZARI), 2021, Pigeonpea Ratooning Trial Under Conservation Agriculture, 2020"

	uri <- "doi:10.7910/DVN/EQZR5C"
	group <- "agronomy"

	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIMMYT;ZARI",
		publication= NA,
		project="AfricaRISING",
		response_vars = "yield",
		treatment_vars = "intercrops; crop_rotation",
		data_type= "on-farm experiment",
		carob_contributor= "Shumirai Manzvera",
		carob_date="2024-02-29",
		modified_by = "Siyabusa Mkuhlani",
		last_modified = "2024-07-07"
	)
	
	f <- ff[basename(ff) == "AR_ZAM_CIMMYT_ratooning_onstation_2020.csv"]
	r <- read.csv(f)
	
	d <- data.frame(
		trial_id = (paste0(r$Location, r$Year)), 
		crop=r$Crop, 
		treatment=r$Intercropstrategy,
		rep=r$Rep,
		dmy_residue=r$biomass,
		yield=r$grainyield,
		country = r$Country,
		adm2 = r$District,
		location=r$Location,
		planting_date = as.character(r$Year),
		yield_part = "grain",
		longitude = 32.6447,
		latitude = -13.64451,
		geo_from_source = FALSE,
		intercrop=r$Intercrop
	)

	#Correct crop names   
	d$crop <- carobiner::replace_values(d$crop,"pigeonpea","pigeon pea")
	d$intercrop <- carobiner::replace_values(d$intercrop,"Pigeonpea","pigeon pea")

	##Add cropping systems information
	d$crop_rotation <- NA
	d$crop_rotation[grep('rotation',d$treatment)] <- d$intercrop[grep('rotation',d$treatment)]
	d$intercrops <- NA
	d$intercrops[grep('Mz/Pp|Mz/PP|MZ/Pp|MZ/PP',d$treatment)] <- d$intercrop[grep('Mz/Pp|Mz/PP|MZ/Pp|MZ/PP',d$treatment)]
	d <- d[,!names(d) %in% "intercrop"] #Droppig variable here. Could not be 
	#dropped earlier as it was neeeded  for the Intercrop and rotation columns.	
		
## about the data
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$irrigated <- FALSE

## Fertilizers
	d$fertilizer_type <- "D-compound;urea"
	d$N_fertilizer <- (10/100 * 100) + 46
	d$P_fertilizer <- (20/100 * 100) * 0.437
	d$K_fertilizer <- (10/100 * 100) * 0.83

	d$yield_moisture <- as.numeric(NA) #needs to be checked

	carobiner::write_files(meta, d, path=path)
}


