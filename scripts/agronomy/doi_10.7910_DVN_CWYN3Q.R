# R script for "carob"

carob_script <- function(path) {

"This dataset presents the data used in the study undertaken in the semi-arid area of Kiteto District in the Babati Region to address the challenge of soil plow layer compaction mainly associated with continued use of tractor mounted plow discs and cattle trampling. Trials were arranged in a mother-baby set up during the 2018/2019 cropping season whereby baby trials allow wide exposure that enables appropriate socio-economic study conditions. The mother factorial experiment was arranged in a split-plot design with two tillage treatments: Conventional farmer practice i.e. conventional tillage which involves tractor-mounted plow (CT) and rip tillage (RT) and two improved maize varieties (commercial maize variety & DT maize Variety) thus giving a total of four treatment combinations."

	uri <- "doi:10.7910/DVN/CWYN3Q"
	group <- "agronomy"

	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=2,
		data_organization = "TARI;IITA",
		publication = NA,
		project = NA,
		data_type = "experiment",
		response_vars = "yield",
		treatment_vars = "land_prep_method;variety", 
		carob_contributor = "Shumirai Manzvera",
		carob_date = "2024-05-21"
	)
	
	f1 <- ff[basename(ff) =="Grain yield 2019 Kiperesa.xlsx"]
	r1 <-carobiner::read.excel(f1, sheet="Grain yield Kiperesa 2019")

## ignoring bulk density results for now; not clear how the relate to the baby trials.
#	f2 <- ff[basename(ff) == "Bulk Density at Planting_use of Tractor Mounted trial 2019 Kiperesa.xlsx"]
#	r2 <- carobiner::read.excel(f2, sheet="Bulk Density at planting")

	d1 <- data.frame(
		variety=r1$Variety,
		treatment=r1$`Tillage method`,
		rep=r1$REP,
		yield=r1$`Maize grain yield, kg/ha`
	)

#	d2 <- data.frame(
#		variety=r2$Variety,
#		treatment=r2$`Tillage Method`,
#		rep=r2$Replication
#	)  
#	d <- carobiner::bindr(d1, d2)

	d <- d1

	i <- match(d$treatment, c("CT", "RT"))
	d$land_prep_method <- c("conventional", "ripping")[i]
	
	d$rep <- as.integer(d$rep)
	d$on_farm <- TRUE
	d$planting_date <- "2018"
	d$harvest_date <- "2019"

	d$country <- "Tanzania"
	d$location <- "Manyara"
	d$adm2 <- "Kiteto"
	d$longitude <- 36.29271 
	d$latitude <- -5.24399
	d$geo_from_source <- FALSE

	d$crop <- "maize"
	d$yield_part <- "grain"
	d$trial_id <- "1"

	d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
	d$is_survey <- FALSE
	d$irrigated <- FALSE
	
	carobiner::write_files(path, meta, d)
}
