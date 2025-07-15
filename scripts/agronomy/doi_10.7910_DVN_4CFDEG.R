# R script for "carob"
# license: GPL (>=3)

## ISSUES
# list processing issues here so that an editor can look at them


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
		publication = "doi:10.1016/S0378-4290(03)00153-9; doi:10.1016/S0378-4290(02)00117-X",
		project = "WARDA LTE",
		carob_date = "2025-07-15",
		design = NA,		
		data_type = "experiment",
		treatment_vars = "variety;N_fertilizer;P_fertilizer;K_fertilizer",		
		response_vars = "yield", 
		carob_contributor = "Robert Hijmans",
		completion = 100
	)
	

	f1 <- ff[basename(ff) == "AfricaRice Long Term Trial - Fanaye (dry season).xlsx"]
	f2 <- ff[basename(ff) == "AfricaRice Long Term Trial - Fanaye (wet season).xlsx"]
	f3 <- ff[basename(ff) == "AfricaRice Long Term Trial - Ndiaye (dry season).xlsx"]
	f4 <- ff[basename(ff) == "AfricaRice Long Term Trial - Ndiaye (wet season).xlsx"]
	#f5 <- ff[basename(ff) == "Dictionnary.txt"]

	rfun <- function(f) {
		r <- carobiner::read.excel(f)
		data.frame(
			year = r[["Year"]],
			country = r[["Country"]],
			location = r[["Site"]],
			season = tolower(r[["Season"]]),
			treatment = r[["Treatment"]],
			variety = r[["Variety"]],
			planting_date = as.Date(r[["Sowing date"]]),
			transplanting_date = as.character(r[["Transplanting date"]]),
			flowering_date = r[["FLWR"]],
			maturity_date = r[["MATURITY"]],
			grain_fill = r[["FIL"]],		
			yield = r[["YIELD"]]
		)
	}
	
	d <- do.call(rbind, lapply(c(f1, f2, f3, f4), rfun))
	
	d$flowering_date <- as.character(d$planting_date + d$flowering_date)
	d$maturity_date <- as.character(d$planting_date + d$maturity_date)
	d$planting_date <- as.character(d$planting_date)
	
	d$yield_moisture <- 14
	d$trial_id <- paste0(d$location, d$year, d$season, sep="_")
	d$year <- NULL

	# from "Dictionnary.txt"
	npk <- data.frame(rbind(c(0,0,0), c(120,26,50), c(120,52,100), c(120,0,0), c(180,26,50), c(60,26,50)))
	colnames(npk) <- c("N_fertilizer", "P_fertilizer", "K_fertilizer")
	npk$treatment <- paste0("T", 1:6)
	
	d <- merge(d, npk, by="treatment")

	# from article
	geo <- data.frame(
		location = c("Ndiaye", "Fanaye"),
		longitude = c(-16.23333, -15.76667),
		latitude = c(16.23333, 16.42424)
	)

	d <- merge(d, geo, by="location")
	d$geo_from_source <- FALSE

	d$fw_yield <- d$yield
	d$dm_yield <- d$yield / (1 + d$yield_moisture/100) 

	d$on_farm <- FALSE
	d$is_survey <- FALSE
	d$irrigated <- TRUE

	d$crop <- "rice"
	d$yield_part <- "grain"
	d$LTE_name = "WARDA"
	
	carobiner::write_files(path, meta, d)
}

