# R script for "carob"

carob_script <- function(path) {
  
"The dataset contains the result of a series of experiments conducted in Morocco between 2014 and 2015, about land and water productivity.  The overall objective of this study is the sustainable increase of wheat yield in dry areas of WANA. The purpose is the development of options that improve the adaptation of wheat to high temperature and drought."

	uri <- "hdl:20.500.11766.1/FK2/XOSFYG"
	group <- "varieties_wheat"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=15, minor=0,
		project=NA,
		publication="doi:20.500.11766/4516",
		data_organization = "ICARDA;INAT;INRAMAR",
		carob_contributor="Samar Attaher",
		carob_date="2023-03-15",
		data_type="experiment",
		response_vars = "yield",
		treatment_vars = "variety"
    )
  

	f <- ff[basename(ff) == "Grain_Yield.csv"] 	
	r <- read.csv(f, sep=";")
 
	d <- data.frame(
		trial_id = "1",
		rep=r$Bloc,
		# from quintal/ha to kg/ha
		yield=r$Yield * 100,
		country = "Morocco",
		adm1 = "Béni Mellal-Khénifra Region",
		adm2 = "Fquih Ben Salah Province",
		adm3 = "Afourar",
		location = "Afourer experiment station",
		latitude = 32.262,
		longitude = -6.535,
		geo_from_source = FALSE,
		crop = "durum wheat",
		yield_part = "grain",
		seed_density = 160,
		rain = 378,
		P_fertilizer = 200,
		N_fertilizer = 60
	)

	d$K_fertilizer <- as.numeric(NA)

	r$Planting_Date <- trimws(r$Planting_Date)

	d$treatment <- tolower(apply(r[, c("Water_Regime", "Planting_Date")], 1, \(i) paste(i, collapse="_")))
	
	i <- match(r$Variety, paste0("V", 1:4))	
	d$variety <- c("Karim", "Louiza", "Nassira", "PM9")[i]

	d$planting_date[r$Planting_Date=="Early"] <- "2014-11-11"
	d$planting_date[r$Planting_Date=="Late"] <- "2014-12-24"

	d$irrigation_amount <- 0
	d$irrigation_number <- 0L

	i <- d$treatment == "irrigated_early"
	d$irrigation_amount[i] <- 160
	d$irrigation_number[i] <- 3L

	i <- d$treatment == "irrigated_late"
	d$irrigation_amount[i] <- 275
	d$irrigation_number[i] <- 5L
	d$irrigated <- d$irrigation_amount > 0
	
	d$on_farm <- FALSE
	d$is_survey <- FALSE

	carobiner::write_files (path, meta, d) 
}
