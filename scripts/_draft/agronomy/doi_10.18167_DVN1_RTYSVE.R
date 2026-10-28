# R script for "carob"
# license: GPL (>=3)

## ISSUES
# root biomass (aRB_gm2) was recorded per soil layer (0-10cm, 10-30cm, 30-50cm). the layers were summed (0-50cm) from the rows and then averaged to get one value per plot and date
# N treatment rates are from the preprint (346 kg/ha plant crop, 336 kg/ha 1st ratoon). 
# P and K rates for the 2nd ratoon (R2) are in table S1 of the paper (not available); so these are currently NA

carob_script <- function(path) {

"
Dataset of sugarcane root production and turnover: influence of crop harvest and nitrogen fertilization

Backgrounds and Aims Root production and turnover are key drivers of belowground carbon inputs, but remain poorly quantified in tropical cropping systems such as sugarcane. This study aimed to provide the first quantification of sugarcane root production and turnover across a full crop cycle, including plant crop and first ratoon, and to examine how nitrogen fertilization modulates biomass allocation, fine root traits, and turnover. Methods A two-year field experiment (plant and ratoon crop) was conducted on Reunion Island under contrasting nitrogen fertilization with the R579 variety. Sequential soil coring was used to monitor living root biomass, necromass, and fine root traits down to 50 cm depth every two months.
"

	uri <- "doi:10.18167/DVN1/RTYSVE"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIRAD",
		publication = "hal-05563134",
		project = NA,
		design = "Two N fertilisation treatments, arranged in three blocks(TA3, TA4, TA5) and monitored over two years",
		data_type = "experiment",
		treatment_vars = "N_fertilizer",
		response_vars = "dmy_roots;dmy_storage;dmy_total", 
		notes = "Roots were sampled with an auger (9 cm diameter) to 50 cm depth in 3 layers (0-10, 10-30, 30-50 cm) and at 3 distances from the row (12.5, 37.5, 62.5 cm). dmy_roots is the alive root dry mass (structural + fine) summed over the layers and averaged over the three distances. dmy_total is the aboveground dry mass, dmy_storage the stalk dry mass.",
		carob_contributor = "Kudzaishe M. Muzata",
		carob_date = "2026-10-26",
		carob_completion = 80,	
		carob_effort = 5
	)

	f1 <- ff[basename(ff) == "DATA_Aboveground_biomass.xlsx"]
	f2 <- ff[basename(ff) == "DATA_Root_biomass_traits.xlsx"]

	r1 <- carobiner::read.excel(f1, sheet="DATA")
	r2 <- carobiner::read.excel(f2, sheet="DATA")

	d1 <- data.frame(
		plot_id = r1$Plot,
		DAP = as.integer(r1$DAP),
		dmy_storage = r1$SDM * 1000, # Mg/ha to Kg/ha
		dmy_total = r1$ADM * 1000
	)
	
	r2 <- r2[order(r2$Plot, r2$DAP), ] 
	r1 <- aggregate(aRB_gm2 ~ Plot + Date + DAP + Cycle + Block + Date_harvest + Position, data=r2, FUN=sum) # sum of rb for all 3 layers
	rp <- aggregate(aRB_gm2 ~ Plot + Date + DAP + Cycle + Block + Date_harvest, data=r1, FUN=mean) # avg root biomass per m2

	d2 <- data.frame(
		plot_id = rp$Plot,
		date = as.character(as.Date(rp$Date)),
		# planting_date is not a great term here, this is a permenant crop and the date is the date of the previous harvest 
		planting_date = as.character(as.Date(rp$Date_harvest)),
		season = c(P0 = "plant crop", R1 = "1st ratoon", R2 = "2nd ratoon")[rp$Cycle],
		DAP = as.integer(rp$DAP),
		rep = as.integer(gsub("TA", "", rp$Block)),
		treatment = ifelse(grepl("_NF$", rp$Plot), "no N", "N"),
		dmy_roots = rp$aRB_gm2 * 10 # convert to kg/ha
	)

	d <- merge(d2, d1, by=c("plot_id", "DAP"))

	# why?
	d <- d[d$season != "2nd ratoon", ]
	
	d$site <- "Soere"
	d$location <- "La Mare"
	d$country <- "Réunion"
	d$trial_id <- "1"
	
	d$on_farm <- FALSE
	d$is_survey <- FALSE

	# from publication
	# the trial was irrigated three times per week, except during the last two months of each crop cycle 
	d$irrigated <- TRUE 
	d$crop <- "sugarcane"
	d$variety <- "R579"

	d$longitude <- 55.5297
	d$latitude <- -20.9031
	d$geo_from_source <- TRUE
	
	d$harvest_date  <- d$date

	d$P_fertilizer <- NA
	d$K_fertilizer <- d$P_fertilizer
	d$N_fertilizer <- 0
	d$N_fertilizer[d$treatment == "N" & d$season == "plant crop"] <- 346
	d$N_fertilizer[d$treatment == "N" & d$season == "1st ratoon"] <- 336
	d$N_fertilizer[d$treatment == "N" & d$season == "2nd ratoon"] <- NA  # sampling

	carobiner::write_files(path, meta, d)
}


