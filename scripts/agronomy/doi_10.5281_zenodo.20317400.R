# R script for "carob"
# license: GPL (>=3)

## ISSUES
## Treatment:
   # - evaluating the integration of off-season vegetable and legume crops under different irrigation regimes over two cropping years (2020 and 2021).


carob_script <- function(path) {

"
Data on rice, okra, cucumber, tomato, and green bean yields, system rice equivalent yield, water use, water productivity, benefits, and benefit-cost ratio under different irrigation scheduling across two sites over two years.

&nbsp;This dataset contains field experimental data from lowland rice-based systems in West Africa, specifically at two sites (Loka and Mbe), evaluating the integration of off-season vegetable and legume crops under different irrigation regimes over two cropping years (2020 and 2021). &nbsp; 1.&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Study Design and Scope Sites: Loka and Mbe experimental stations Years: Two production years (2020 and 2021), represented as Year1 and Year2 Main system: Rice-based cropping systems with off-season diversification Off-season crops: Cucumber, Green bean, Okra, Tomato Irrigation treatments: Four irrigation schedules (Ir1, Ir2, Ir3, Ir4) Replication: Four field replications (R1&ndash;R4) &nbsp; 2.&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Key Variables Measured The dataset integrates agronomic, economic, and water productivity indicators at system level: &middot;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Site, crop, irrigation, replication, and year identifiers (categorical variables defining experimental structure) &middot;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Wet yield (t/ha): Yield of off-season crop under irrigation &middot;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; System rice equivalent yield (t/ha): Combined productivity of rice-based system standardized into rice equivalent &middot;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; System benefit (USD/ha): Economic return from the integrated system &middot;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; System benefit-cost ratio: Economic efficiency indicator of the system &middot;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Off-season contribution to system benefit (%): Share of off-season crops in total system profitability (can be positive or negative) &middot;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Water amount (mm): Total irrigation water applied per treatment &middot;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; System water use efficiency (kg/m&sup3;/ha): Efficiency of water use in producing system yield &nbsp; 3.&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Experimental Purpose The dataset is designed to evaluate: &middot;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; The performance of different irrigation scheduling strategies in diversified rice-based systems &middot;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Productivity gains from integrating vegetables and legumes into rice systems &middot;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Economic profitability and benefit&ndash;cost trade-offs of system diversification &middot;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Water productivity and irrigation efficiency under climate-smart agriculture practices &nbsp; 4.&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Analytical Potential This dataset supports analyses such as: &middot;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Comparison of irrigation strategies across crops and sites &middot;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Trade-offs between yield, profit, and water use efficiency &middot;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Climate-resilient agricultural system optimization &middot;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Modeling of rice-based system intensification with off-season diversification &nbsp; 5.&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Overall Structure &nbsp; The data is a multi-factorial field experiment dataset structured by: &nbsp; Site &times; Year &times; Crop &times; Irrigation &times; Replication &nbsp; It combines agronomic performance, economic outcomes, and water-use indicators, making it suitable for integrated sustainability and climate-smart agriculture assessments.
"

	uri <- "doi:10.5281/zenodo.20317400"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=4, minor=NA,
		data_organization = "AfricaRice",
		publication = "https://cgspace.cgiar.org/bitstreams/583a4a57-707b-4711-a575-c1d94e89cb18/download",
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "irrigation_amount",
		response_vars = "yield",
		notes = NA,
		carob_contributor = "Cedric Ngakou",
		carob_date = "2026-11-04",
		carob_completion = 100,	
		carob_effort = 1
	)
	

	f <- ff[basename(ff) == "20%20May%202026_Irrigation%20Database.xlsx"]

	r <- carobiner::read.excel(f)

	d <- data.frame(
		location = r$Site,
		rep = as.integer(gsub("R", "", r$Rep)),
		planting_date = as.character(r$Year2),
		yield = r$`Wet yield (t/ha)`*1000,
		irrigation_number = as.integer(gsub("Ir", "", r$Irrigation)),
		crop = gsub("greenbean", "green bean", tolower(r$offseason)),
		irrigation_amount = r$`Water amount (mm)`,
		net_benefit = r$`System benefit (USD/ha)`,
		currency = "USD",
		irrigated = TRUE,
		crop_rotation = paste(gsub("greenbean", "green bean", tolower(r$offseason)), "rice", sep = ";"),
		country = "Côte d'Ivoire",
		harvest_date = NA,
		trial_id = as.character(match(r$Site, c("Loka", "Mbe")))
		
	)
	
	## Adding geo coordinates 
	
	geo <- data.frame(
	  location = c("Loka", "Mbe" ),
	  longitude = c(-4.9797, - 5.559167),
	  latitude = c(7.91459, 7.899444),
	  geo_source = c("Google Maps", NA),
	  geo_from_source = c(FALSE, TRUE)
	)
	
	d <- merge(d, geo, by= "location", all.x = TRUE)
	
	
	d$is_survey <- FALSE
	d$on_farm <- FALSE
	d$yield_moisture <- NA
	d$yield_isfresh <- TRUE
	d$yield_part <- "fruit"
	d$K_fertilizer <- d$N_fertilizer <- d$P_fertilizer <- as.numeric(NA)

	carobiner::write_files(path, meta, d)
}


