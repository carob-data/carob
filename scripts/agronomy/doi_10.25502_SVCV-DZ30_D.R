# R script for "carob"
# license: GPL (>=3)

## ISSUES


carob_script <- function(path) {

"
Validation exercise with cassava, plot by plot comparison, of 2 fertilizer rates

The objective of the EiA SAA Nigeria Use Case is to combine 3 fertilizer recommendation tools (AKILIMO for cassava, Nutrient Expert (NE) for maize and Rice Advice for rice) in one interface. Following the request of the “demand partner” Sasakwa Africa Association (SAA), Nigeria, the decision support tools (DSTs) for fertilizer will be combined with advice on the planting or sowing windows. For cassava, this is already available as a module in AKILIMO, for maize and rice, this is under development. Modified versions of the Rice Advice tool, Rice Advice Lite (RAL) and AKILIMO, were/are tested in 2021 and 2022 (for AKILIMO, only the growing season 2021/2022).



The data were collected in the frame of the EiA Sasakawa Nigeria  Use Case in the context of a “validation exercise”, a side by side comparison to test the performance of 2 alternative agronomic treatments under farming conditions, implemented on farm with minimal supervision from research. In this case, 2 different fertilizer application rates to cassava were tested. Yield was obtained from the whole plots of 96 m2. However, plot sizes were measured to confirm the plot area by extension agents at harvest and the measured plot size was used for yield calculation as tonne per hectare. Measured plot sizes may deviate from the 96 m2 and may not respect the theoretical plot boundaries at 50% inter and intra row distances. Planting of the validation exercise started in July in 2021 and harvest ended in August 2022. Target states were  Benue and Kogi states of Nigeria.
"

	uri <- "doi:10.25502/SVCV-DZ30/D"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		data_organization = "AfricaRice; IITA",
		publication = NA,
		project = NA,
		carob_date = "2025-09-24",
		design = NA,
		data_type = "experiment",
		treatment_vars = "N_fertilizer;P_fertilizer;K_fertilizer",
	   response_vars = "yield", 
		carob_contributor = "Cedric Ngakou",
   	completion = 100,	
		notes = NA
	)
	
	
	f <- ff[basename(ff) == "fert.csv"]
	#f2 <- ff[basename(ff) == "data_dictionary.csv"]
	
	r <- read.csv(f)
	#r2 <- read.csv(f2)

  ### process
	d <- data.frame(
		treatment = ifelse(grepl("F1", r$plot), "fertilizer treatment1", "fertilizer treatment2"),
		trial_id = r$HHID,
		location = r$state,
		latitude = r$Lat,
		longitude = r$Long,
		N_fertilizer = r$rateUrea*0.46 + r$rateNPK*0.15,
		P_fertilizer = r$rateNPK*0.15/2.29,
		K_fertilizer = r$rateNPK*0.15/1.2051,
		yield = r$Yield*1000, ## in kg/ha
		variety = r$variety,
		country = "Nigeria",
		crop = "cassava",
		planting_date = "2021-07",
		harvest_date = "2022-08",
		yield_part = "roots",
		on_farm = TRUE,
		is_survey = FALSE,
		geo_from_source = TRUE,
		irrigated = NA,
		yield_moisture = as.numeric(NA)
	)

 
carobiner::write_files(path, meta, d)

}

