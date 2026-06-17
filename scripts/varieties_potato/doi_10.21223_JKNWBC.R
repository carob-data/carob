# R script for "carob"


carob_script <- function(path) {

"In 2019-2020 season, three clones with high levels of resistance to late blight were evaluated in adaptation and efficiency trials for tuber yield, dry matter content, frying and baking quality throughout Peru in six locations, compared to two varieties planted by farmers and very well accepted by final consumers, Canchan and Unica, these are currently also used for frying in sticks, but without stability in all crops due to the genotype x environment interaction. The randomized complete block design was used with three replications of 150 plants, the fertilization dose was 200-220-180 Kg of NPK, using potassium sulfate as a source of potassium to improve frying quality. At harvest, samples were taken to determine dry matter, reducing sugar content, traditional and blanched frying color, and baking quality. The clone was equal to or superior to the controls for the yield of tubers, it presented good quality of frying color in all localities compared to the control varieties that did not present good quality of frying color in all localities, It is expected to complete all the documents requested by the Peruvian Seed Authority (SENASA) to be registered as a new potato variety with resistance to late blight and quality for frying and / or baking."


	uri <- "doi:10.21223/JKNWBC"
	group <- "varieties_potato"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- 	   carobiner::get_metadata(uri, path, group, major=1, minor=2,
	   data_organization = "CIP",
	   publication= NA,
	   project=NA,
	   data_type= "experiment",
	   response_vars = "yield",
	   treatment_vars = "variety",
	   carob_contributor= "Cedric Ngakou",
	   carob_effort = NA,
	   carob_date="2024-06-16"
	)

	r <- carobiner::read.excel(ff[basename(ff)=="PTYield2019-2020_Combinado_CIP395123.6_exp.xlsx"], sheet = "Fieldbook")

	d <- data.frame(
		country="Peru",
		crop="potato",
		variety= r$INSTN,
		rep= as.integer(r$REP),
		yield=r$MTYNA*1000, # to kg/ha
		location=r$LOC,
		on_farm= TRUE,
		irrigated= FALSE,
		inoculated= FALSE,
		yield_part= "tubers",
		trial_id= "1",
		season="2019-2020",
		is_survey = FALSE
	)


	## Get planting and harvest date from data description of each file 
	dta <- data.frame(
	   location=c("Uñigan","Licame","Majes2", "Chinchao", "Quilcas","Majes1"),
	   latitude=c(-6.5516594, -7.7667731, -16.3242988, -9.6146422, -11.9374892, -16.3242988),
	   longitude=c(-78.3891572, -77.834892, -72.2878298, -76.1287916, -75.2592954, -72.2878298),
	   planting_date=c( "2019-12-18", "2020-01-15", "2020-08-20", "2020-01-24", "2019-12-17", "2020-01-28"),
	   harvest_date=c("2020-06-03", "2020-06-29", "2021-01-20", "2020-06-16","2020-06-23", "2020-06-01"),
	   geo_from_source = FALSE
	)  

	d <- merge(d, dta, by="location", all.x = TRUE)

	## From data description 
	d$N_fertilizer <- 200   
	d$P_fertilizer <- 220/2.29
	d$K_fertilizer <- 180/1.2051


	carobiner::write_files(path, meta, d)
}

