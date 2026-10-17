# R script for "carob"
# license: GPL (>=3)

## ISSUES

### Treatment
    #Impact of four planting dates, seeding rates, and nitrogen application rates on rye cover crop performance.
    #Cotton and peanut grown in rotation following the rye cover crop.
    #The dataset contains multiple files, most of which are included in summary_files.csv.



carob_script <- function(path) {

"
Data from: Rye planting date impacts biomass production more than seeding rate and nitrogen fertilizer

This zipped folder contains the code and datasets used for an experiment conducted over six years (2015–2020) at the Wiregrass Research and Extension Center in Headland, AL to examine rye biomass production. Treatments included four planting dates (late October, early November, late November, and early December), two seeding rates ( 67 and 101 kg ha-1), and four nitrogen rates (0, 34, 67, and 101 kg ha-1) replicated four times for a total of 128 observations each year. Measured and/or calculated variables from the data set included biomass, total C and N concentrations for the plant material, C:N ratio, N uptake values, as well as cost info for the biomass. In addition, there are three years of peanut yield data (2015, 2017, 2019) and only two years of cotton yield data (2016, 2020).  No cotton data was collected for 2018 due to hurricane damage.  Initial and final soil total C values were also measured across the 0 and 101 kg N ha-1 rates for the 67 kg ha-1 seeding rate.  
"

	uri <- "doi:10.5061/dryad.1jwstqk0w"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)


	meta <- carobiner::get_metadata(uri, path, group, major=7, minor=NA,
		data_organization = "USDA-ARS; UAu", # UAu: University of Auburn
		publication = "doi:10.1002/agj2.21418",
		project = NA,
		design = "RCBD",
		data_type = "experiment",
		treatment_vars = "cover_crop;crop_rotation;N_fertilizer;planting_date",
		response_vars = "yield;dmy_total",
		notes = NA,
		carob_contributor = "Cedric Ngakou",
		carob_date = "2026-10-15",
		carob_completion = 80,	
		carob_effort = 5
	)
	
	
	f1 <- ff[basename(ff) == "summary_file.csv"]
	f2 <- ff[basename(ff) == "cover_plant_dates.csv"]
	f3 <- ff[basename(ff) == "cash_plant_dates.csv"]
	f4 <- ff[basename(ff) == "wgs_normal_weather.csv"]
	f5 <- ff[basename(ff) == "carbon.csv"]
	
	
	r1 <- read.csv(f1, na= "")
	r2 <- read.csv(f2, na= "")
	r3 <- read.csv(f3, na= "")
	r4 <- read.csv(f4, na= "")
	r5 <- read.csv(f5, na= "")
	
	
####  process 
	
	## process rye cover crop 
	d1 <- data.frame(
	  trial_id = r1$env,
	  location = r1$loc,
	  year = r1$year,
	  rep = r1$block,
	  plot_id = as.character(r1$plot),
	  treatment = gsub("_", " ", r1$plant_date),
	  seed_rate = round(r1$seed_rate*1.120851), # from lb/ac to kg/ha 
	  N_fertilizer = r1$n_rate,
	  harvest_date = as.character(r1$biomass_date),
	  dmy_total = r1$dm_kgha,
	  residue_C = r1$pct_C/100, # mg/g
	  residue_N = r1$pct_N/100,
	  seed_cost = r1$seed_cost_ac/0.405,
	  planting_cost = r1$plant_cost_ac/0.405,
	  fertilizer_cost = r1$fert_cost_ac/0.405,
	  fertilizer_app_cost = r1$fertapp_cost_ac/0.405,
	  crop = "rye",
	  cover_crop = "none",
	  yield_part = "aboveground biomass", 
	  residue_prevcrop = 0 ,
	  crop_rotation = "none"
	)
	
	### planting_date of rye crop
	
	dp <- data.frame(
	  treatment = gsub("\\.", "", tolower(r2$treatment)),
	  year = as.numeric(substr(zoo::na.locf(r2$season), 6, 7)),
	  planting_date = as.character(as.Date(carobiner::eng_months_to_nr(r2$plant_date), "%d-%m-%y")),
	  fertilizer_date1 = as.character(as.Date(carobiner::eng_months_to_nr(r2$first_N_application), "%d-%m-%y")),
	  fertilizer_date2 = as.character(as.Date(carobiner::eng_months_to_nr(r2$second_N_application), "%d-%m-%y"))
	)
	dp$year <- as.numeric(paste0(20, dp$year))
	d1 <- merge(d1, dp, by= c("year", "treatment"), all.x = TRUE)
	d1$fertilizer_date <- paste(d1$fertilizer_date1, d1$fertilizer_date2, sep= ";") 
	d1$fertilizer_date1 <- d1$fertilizer_date2 <- NULL
	
	### process cotton and peanut cash crop
	
	d21 <- data.frame(
	  trial_id = r1$env,
	  location = r1$loc,
	  year = r1$year,
	  rep = r1$block,
	  plot_id = as.character(r1$plot),
	  crop = tolower(r1$crop),
	  harvest_date = as.character(gsub("NA", NA, r1$harvest_date)),
	  residue_prevcrop = r1$dm_kgha,
	  yield = as.numeric(gsub("NA", NA, ifelse(is.na(r1$peanut_kgha), r1$cotton_kgha, r1$peanut_kgha))),
	  cover_crop = "rye",
	  season = "kharif",
	  N_fertilizer = 0,
	  yield_part = ifelse(grepl("cotton", r1$crop), "lint", "grain"),
	  crop_rotation = ifelse(grepl("cotton", r1$crop), "cotton;rhizoma peanut", "rhizoma peanut;cotton")
	  
	)
	
	## Adding planting date of cash crop 
	d22 <- data.frame(
	  year = r3$growing_season,
	  crop = tolower(r3$cash_crop),
	  planting_date = as.character(as.Date(r3$planting_date, "%m/%d/%Y")),
	  irrigation_amount = r3$irrigation.totals..mm.
	)
	
	d2 <- merge(d21, d22, by = c("year", "crop"), all.x = TRUE)
	
  d <- carobiner::bindr(d1, d2)
	d$crop <- gsub("peanut", "rhizoma peanut", d$crop)

	
	## process soil SOC data 
	
	ds <- data.frame(
	  plot_id = as.character(r5$plot),
	  rep = r5$block,
	  depth = r5$depth,
	  treatment = gsub("_", " ", r5$plant_date),
	  seed_rate = round(r5$seed_rate*1.120851),
	  N_fertilizer = round(r5$n_rate*1.120851),
	  soil_SOC = r5$final_C,
	  year = 2020
	)
	
	## Adding record id
	
	rec <- unique(d[, c("year", "plot_id", "rep", "treatment", "seed_rate", "N_fertilizer")])
	rec$record_id <- seq_len(nrow(rec))
	
	ds <- merge(ds, rec, by= c("year", "plot_id", "rep", "treatment", "seed_rate", "N_fertilizer"), all.x = TRUE)
  ds <- ds[, c("record_id", "depth", "soil_SOC")] 
  ds$depth_bottom <- as.numeric(trimws(gsub("-|c", "", substr(ds$depth, 3, 5))))
  ds$depth_top <- as.numeric(gsub("-", "", substr(ds$depth, 1, 2)))
  d <- merge(d, rec, by= c("year", "plot_id", "rep", "treatment", "seed_rate", "N_fertilizer"), all.x = TRUE )
  d <- d[order(d$record_id),]
  d$year <- ds$depth <- NULL
  
  ### process weather data 
	
	dw <- data.frame(
	  location = r4$loc,
	  date = r4$date,
	  prec = as.numeric(gsub("NA", NA, r4$precip_in))*25.4, # mm
	  tmax = (as.numeric(gsub("NA", NA, r4$max_tempF))-32)*5/9, # in celcius degree
	  tmin = (as.numeric(gsub("NA", NA, r4$min_tempF))- 32)*5/9,
	  geo_from_source = TRUE,
	  latitude = 31.35667,
	  longitude = -85.32333,
	  country = "United States"
	)
	
	
	d$is_survey <- FALSE
	d$on_farm <- TRUE
	d$yield_moisture <- NA
	d$yield_isfresh <- NA
	d$country <- "United States"
	d$geo_from_source <- TRUE
	d$latitude <- 31.35667
	d$longitude <- -85.32333
	d$irrigated <- !is.na(d$irrigation_amount)
	d$K_fertilizer <- d$P_fertilizer <- as.numeric(NA)
	
	
	carobiner::write_files(path, meta, d, wth = dw, long = ds)
}

