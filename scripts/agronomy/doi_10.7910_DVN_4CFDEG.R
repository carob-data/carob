# R script for "carob"
# license: GPL (>=3)

## ISSUES


carob_script <- function(path) {

"
Replication Data for: Replication Data for: Long-term fertility experiments (LTFEs)

To assess long-term sustainability of intensive irrigated lowland rice in semi-arid condition
"

	uri <- "doi:10.7910/DVN/4CFDEG"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "AfricaRice",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "N_fertilizer;P_fertilizer;K_fertilizer",
		response_vars = "yield", 
		notes = NA,
		carob_contributor = "Cedric Ngakou",
		carob_date = "2026-09-29",
		carob_completion = 100,	
		carob_effort = 2
	)
	
	
	#f1 <- ff[basename(ff) == "Dictionnary.txt"]
	#r1 <- read.table(f5, sep=";", encoding = "latin1")
	ff <- ff[grepl("AfricaRice", basename(ff))] 
	
#### process
	
	proc <- function(f){
	  
	  r1 <- carobiner::read.excel(f)
	 data.frame(
	    year = r1$Year,
	    country = r1$Country,
	    location = r1$Site,
	    season = tolower(r1$Season),
	    treatment = r1$Treatment,
	    variety = r1$Variety,
	    planting_date = as.character(r1$`Sowing date`),
	    transplanting_date = as.character(r1$`Transplanting date`),
	    yield = r1$YIELD,
	    yield_moisture = 14,
	    flowering_days = r1$FLWR,
	    crop = "rice",
	    trial_id = gsub("AfricaRice Long Term Trial - |.xlsx", "", basename(f)),
	    geo_from_source = FALSE
	  )
	  
	}
	
	d <- lapply(ff, proc) 
	d <- do.call(rbind, d)
	
	### Adding fertilizer
	fert <- data.frame(
	  treatment = c("T1", "T2", "T3", "T4", "T5", "T6"),
	  N_fertilizer = c(0, 120, 120, 120, 180, 60),
	  P_fertilizer = c(0, 26, 52, 0, 26, 26),
	  K_fertilizer = c(0, 50, 100, 0, 50, 50)
	)
	
	d <- merge(d, fert, by= "treatment", all.x = TRUE)
	trt <- c("T1"= "0-0-0", "T2"= "120-26-50", "T3"= "120-52-100", "T4"= "120-0-0", "T5" = "26-26-50", "T6"= "60-26-50")
	d$treatment <- trt[d$treatment]
	
	#### Adding longitude and latitude 
	i <- grepl("Fanaye", d$location)
	d$longitude[i] <- -15.222
	d$latitude[i] <- 16.5258
	i <- grepl("Ndiaye", d$location)
	d$longitude[i] <- -16.3484
	d$latitude[i] <- 13.9608
	
	i <- is.na(d$planting_date)
	d$planting_date[i] <- d$year[i]
	d$year <- NULL
	
	d$is_survey <- FALSE
	d$on_farm <- TRUE
	d$yield_part <- "grain"
	d$irrigated <- TRUE
	d$harvest_date <- NA
	d$geo_source <- "Google Maps"
	
	
	carobiner::write_files(path, meta, d)
}

