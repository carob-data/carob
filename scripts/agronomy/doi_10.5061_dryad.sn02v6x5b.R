# R script for "carob"
# license: GPL (>=3)

## ISSUES


carob_script <- function(path) {

"
Cover crop and irrigation impacts on weeds and maize yield

Winter cover crops (CC) may facilitate weed management by inhibiting weed seed germination and seedling emergence and suppressing weed growth within the cash crop. In southern New Mexico, with scarce winter precipitation and limited irrigation water, producing sufficient CC biomass for effective weed suppression while conserving water resources is challenging. This study assessed the water requirement to produce a CC with enough biomass for weed suppression benefits during cash crop growth at two locations in New Mexico. Three winter CC species, barley, Austrian winter pea and mustard, grown singly and in a three‐way mix, under three differential irrigation treatments (one, two or three irrigations after emergence) were evaluated for their weed‐suppressive potential. Maize was planted as a cash crop four weeks after winter CC termination. Number of irrigations had no effect on the CC and weed biomass production. All CCs had lower weed density prior to maize planting compared with fallow. Barley and the three‐way mix reduced weed density by 56–96% and 68–95%, respectively. All CC treatments had lower weed biomass at the end of critical period for weed control in maize compared with fallow. Weed biomass at maize harvest did not differ between treatments. The maize yield was consistently higher in conventionally managed, weed‐free subplots, than in unsprayed weedy subplots, suggesting that CCs did not suppress weeds throughout the maize growing season. Except for barley, CCs did not cause reductions in maize yield compared with fallow. Overall, the study suggested that with adequate winter precipitation, weed‐suppressive winter cover crop stands can be produced with just one irrigation at seeding and one supplemental irrigation, making them a viable option in water‐limited agroecosystems.
"

	uri <- "doi:10.5061/dryad.sn02v6x5b"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=3, minor=NA,
		data_organization = "VT; NMSU", # NMSU: New Mexico State University
		publication = "doi:10.1111/wre.12519",
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "cover_crop;irrigation_amount",
		response_vars = "yield", 
		notes = NA,
		carob_contributor = "Cedric Ngakou",
		carob_date = "2026-10-27",
		carob_completion = 100,	
		carob_effort = 2
	)
	

	f1 <- ff[basename(ff) == "Final_datasheet.csv"]
	#f2 <- ff[basename(ff) == "README_CoverCrops.txt"]

	r1 <- read.csv(f1)
	
	#####
	d1 <- data.frame(
		year = r1$Year,
		location = c("LL" = "Los Lunas", "LD"= "Leyendecker")[r1$Site],
		rep = r1$Replicate,
		plot_id = as.character(r1$Plot),
		treatment = r1$Treat,
		cover_crop = c("F" = "none",  "M" = "mustard", "P" = "pea" ,  "B" = "barley",   "Mx" = "mustard;pea;barley")[r1$Treat],
		irrigation_number = r1$Irrigations,
		irrigation_amount = r1$Water_mm,
		residue_prevcrop = r1$CC_Bio_plot*10/2,
		yield = r1$Cob_Wt_NW*10000, ##  from kg/m2 to kg/ha
		weed_biomass = r1$Weed_Bio_Corn_Harvest*10,
		country = "United States",
		crop = "maize"
	)
	
	### Adding information's from publication
	
	geo <- data.frame(
	  location = c(rep("Leyendecker", 2),rep("Los Lunas", 2)),
	  longitude = c(rep(-106.74565, 2), rep(-106.75455, 2)),
	  latitude = c(rep(32.20299, 2), rep(34.76752, 2)),
	  geo_from_source = TRUE,
	  year = c(2019, 2020, 2019, 2020),
	  planting_date = c("2019-06-17", "2020-05-26", "2019-07-03", NA),
	  harvest_date = c("2019-09-06", "2020-08-10", "2019-09-24", NA)
	)

	d <- merge(d1, geo, by = c("location", "year"), all.x = TRUE)
	d$year <- NULL
	
	d$is_survey <- FALSE
	d$on_farm <- TRUE
	d$trial_id <- ifelse(grepl("Leyendecker", d$location), "1", "2")
	d$yield_moisture <- NA
	d$yield_part <- "grain"
	d$irrigated <- TRUE
	d$yield_isfresh <- NA
	d$K_fertilizer <- d$N_fertilizer <- d$P_fertilizer <- as.numeric(NA)
	
	
	carobiner::write_files(path, meta, d)
}


