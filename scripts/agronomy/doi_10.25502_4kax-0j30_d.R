# R script for "carob"
# license: GPL (>=3)

## ISSUES
#1. NAs in fertilizer columns are a result of misplaced variety names in the fertilizer columns
#2. large yield figure originate from the raw data before yield was calculated
carob_script <- function(path) {

"
Optimal planting and harvest time for cassava in Nigeria and Tanzania

ACAI is a 5 year Bill & Melinda Gates Foundation funded project in 5 countries in Africa (Nigeria and Tanzania) providing tailored agronomic advice to small scale cassava growers in the target countries. The project delivers agronomic solutions to improve cassava root yield and quality and the necessary knowledge base and applications for accessing this knowledge to cassava scaling partners and ultimately farmers in the target countries while instituting the necessary capacity and skills for national system scientists to engage in transformative cassava agronomy.
"

	uri <- "doi:10.25502/4kax-0j30/d"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		data_organization = "IITA",
		publication = NA,
		project ="ACAI",
		design = NA,
		data_type ="experiment",
		treatment_vars = "variety;N_fertilizer,P_fertilizer,K_fertilizer",
		response_vars = "yield", 
		notes = NA,
		carob_contributor = "Blessing Dzuda",
		carob_date = "2026-07-18",
		carob_completion = 90,	
		carob_effort = 5
	)
	
	f <- ff[basename(ff) == "spt_ckan.csv"]

	r <- read.csv(f)
	r <- unique(r)#removing duplicates from raw data

	d <- data.frame(
	  country=r$country,
	  trial_id=r$trial_ID,
	  plot_id=r$plot_ID,
	  treatment=r$trt_name,
	  harvest_date=as.character(as.Date(r$harvestDate, format = "%d/%m/%Y")),
	  plot_area = ifelse(r$Net_plot_size=="5m by 4m ", 20, 25),
	  crop="cassava",
	  variety=r$Variety,
	  fertilizer_used=trimws(r$Fertilizer),
	  irrigated=r$Rainfed_Irrigation,
	  yield_part="roots",
	  yield_moisture=as.numeric(NA),
	  yield_isfresh=TRUE
	)
	
	d$plant_density <- (r$num_of_plant/d$plot_area)*10000
	
	##determining yield from the production columns per plot
	prod_cols <- c("tuberizedDiseasedRootsFW", "tuberizedSmallRootsFW", "tuberizedMarketableRootsFW")
	
	all_na <- rowSums(!is.na(r[, prod_cols])) == 0
	r$prod_fw_kg <- ifelse(!is.na(r$tuberizedRootsFW),
	                        r$tuberizedRootsFW,
	                        ifelse(all_na, NA, rowSums(r[, prod_cols], na.rm = TRUE)))
	d$yield <- (r$prod_fw_kg/d$plot_area)*10000
	
	##deriving fertilizer amount from treatments
	fert_clean <- sub(" NPK$", "", d$fertilizer_used)
	fert_clean[d$fertilizer_used == "control"] <- "0:0:0"
	fert_clean[fert_clean %in% c("", "Mkombozi or Mkuranga1")] <- "NA:NA:NA"
	
	npk <- read.table(text = fert_clean, sep = ":", fill=TRUE, col.names = c("N", "P", "K"))
	
	d$N_fertilizer <- npk$N
	d$P_fertilizer <- npk$P
	d$K_fertilizer <- npk$K

	d$fertilizer_type <- ifelse(d$fertilizer_used=="control", "none", "NPK")
	d$fertilizer_type <- ifelse(is.na(d$N_fertilizer), NA, d$fertilizer_type)
	d$fertilizer_used <- d$N_fertilizer > 0

	d$irrigated <- tolower(trimws(d$irrigated)) == "irrigation supplemented"
	d$irrigated[r$Rainfed_Irrigation==""] <- NA
	
	d$on_farm <- NA
	d$is_survey <- FALSE
	d$geo_from_source <- FALSE
	
	#extracting planting year since the date is not provided

	harv <- gsub(" months after planting|sub-plot harvest at |Harvest at ", "", r$Harvestmonth)
	harv <- as.numeric(harv)
	harv[harv == 63] <- NA
	d$planting_date <- as.character(as.Date(d$harvest_date) - harv * 30.42)

	##to be added
	#d$longitude <- 
	#d$latitude <- 

	carobiner::write_files(path, meta, d)
}
