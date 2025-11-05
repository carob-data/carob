# R script for "carob"


carob_script <- function(path) {

"Nine varieties of chickpeas were evaluated. Some of the variables measured were Total biomass weight (g), Fresh weigth of the subsample (g), Dry weight of the subsample (g), Dry biomass yield (kg/ha) and Harvest area (m²). The experiment was conducted in El Batán, México and the results will be used to make recommendations to farmers about sustainable diversification. (2017-12-30)"

	uri <- "hdl:11529/10548681"
	group <- "varieties"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		data_organization = "CIMMYT",
		publication = NA,
		project = NA,
		data_type = "experiment",
		treatment_vars = "variety_code",
		response_vars = "yield", 
		carob_contributor = "Blessing Dzuda",
		carob_date = "2024-07-18"
	)
	
	f1 <- ff[basename(ff) == "DAT-BV106-17-Biomass_Chickpea.xlsx"]
	f2 <- ff[basename(ff) == "DAT-BV106-17-Yield_Chickpea.xlsx"]
	r1 <- carobiner::read.excel(f1, sheet ="Calculation")
	r2 <- carobiner::read.excel(f2, sheet = "Calculations")

	r1$WidthArea <- r1$LengthArea <- r1$Area <- NULL
	colnames(r1) <- gsub("^W", "BM_", colnames(r1))
	r <- merge(r1, r2, by=c("Rep", "Trt"))
	
	d <- data.frame(
		country = "Mexico",
		adm1 = "Mexico",
		adm2 = "Texcoco",
		location = "El Batán",
		site = "CIMMYT",
		elevation = 2200,
		latitude=19.5293,
		longitude= -98.8492,

		rep=r$Rep,
		variety_code=as.character(r$Trt),
		dmy_total=r$Yield_Dry,
		fwy_total=r$BM_Total,		   
		yield=r$WGrain,
		fwy_storage = r$WGrain,
		dmy_storage = r$WGrain * r$WDry / r$WFresh   ,
		flowering_days=r$Flowering,
		maturity_days=r$Maturity,
		seed_weight=r$Thou,
		yield_part="grain",
		crop="chickpea",
		planting_date="2017",
		trial_id="1",
		plot_length = r$LengthArea,
		plot_width = r$WidthArea,
		plot_area = r$Area
	)

	d$on_farm <- FALSE
	d$is_survey <- FALSE
	d$irrigated <- NA
	d$geo_from_source <- FALSE
	d$P_fertilizer <- d$K_fertilizer <- d$N_fertilizer <- as.numeric(NA)
	d$inoculated <- NA
  
	d$rep <- as.integer(d$rep)

	d$yield_moisture <- as.numeric(NA) #needs to be checked

	carobiner::write_files(path, meta, d)
}


