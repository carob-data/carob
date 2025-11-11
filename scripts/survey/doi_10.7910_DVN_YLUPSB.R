# R script for "carob"
# license: GPL (>=3)

## ISSUES
## missing yield 

carob_script <- function(path) {

"
Replication Data for: Diversity of Inland Valleys and Opportunities for Agricultural Development in Sierra Leone

Inland valleys are becoming increasingly important agricultural production areas for rural households in sub-Saharan Africa due to their relative high and secure water availability and soil fertility. In addition, inland valleys are important as water buffer and biodiversity hot spots and they provide local communities with forest, forage, and ﬁshing resources. As different inland-valley ecosystem functions may conflict with agricultural objectives, indiscriminate development should be avoided. This study aims to analyze the diversity of inland valleys in Sierra Leone and to develop guidelines for their sustainable use. Land use, biophysical and socio-economic data were analyzed on 257 inland valleys using spatial and multivariate techniques. Five cluster groups of inland valleys were identified: (i) semi-permanently flooded with good soil fertility, mostly under natural vegetation; (ii) semi-permanently flooded with very low soil fertility, abandoned by farmers; (iii) seasonally flooded with low soil fertility under low input levels, used for rainfed rice and off-season vegetables for household consumption and market; (iv) well drained with moderate soil fertility under medium input levels, used for rainfed rice and off-season vegetables for household consumption and market; and (v) well drained with moderate soil fertility under low input levels, used for household consumption. Soil fertility, hydrological regime, physical and market accessibility were the major factors affecting agricultural intensification of inland valleys. Opening up the areas in which inland valleys occur through improved roads and markets, and better water control through drainage infrastructures along with an integrated nutrient management would promote the sustainable agricultural use of inland valleys.
"

	uri <- "doi:10.7910/DVN/YLUPSB"
	group <- "survey"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "AfricaRice; SLARI",
		publication = "doi:10.1371/journal.pone.0180059",
		project = NA,
		carob_date = "2025-10-23",
		design = NA,
		data_type = "survey",
		treatment_vars = "none",
		response_vars = "none", 
		carob_contributor = "Cedric Ngakou",
		completion = 100,	
		notes = NA
	)
	

	
	f <- ff[basename(ff) == "Diversity of Inland Valleys and Opportunities for Agricultural Development in Sierra Leone.csv"]
	#f1 <- ff[basename(ff) == "Dictionary.xlsx"]
	
	
	r <- read.csv(f)
  #r1 <- carobiner::read.excel(f1)

	### process data
	
	d <- data.frame(
	   hhid = as.character(r$IV),
	   adm1 = r$Dis,
	   location = r$Chief,
	   farmland = r$Area,
	   soil_sand = r$Sand,
	   soil_clay = r$Clay,
	   soil_SOC = r$OrgC,
	   soil_P = r$Av.P,
	   soil_N_total = r$TotN,
	   rain = r$Rainf,
	   variety = r$Var,
	   land_ownedby = r$Ld_own,
	   planting_method = r$Plant,
	   fertilizer_used = grepl("1|2", r$Soil_mgt),
	   OM_used = grepl("2", r$Soil_mgt),
	   crop = "rice",
	   country = "Sierra Leone",
	   irrigated = grepl("1", r$Watsourc),
	   on_farm = FALSE,
	   is_survey = TRUE,
	   yield_part = "none",
	   yield_moisture = as.numeric(NA)
	)
	
	d$adm1 <- c("Bo", "Bemena")[d$adm1]
	d$location <- c("Bagbe", "Boama", "Bumpeh", "Dama", "Gaura", "Gbo", "Gorama", "Jiama", "Kakua", 
				"Kando" , "Komboya", "Koya", "Langrama", "Lugbu", "Malegohoun", "Niawa", "Nomo", "Nongowa",
				"Selenga", "Simbaru", "Small Bo", "Tikonko", "Tunkia", "Valunia", "Wandor", "Wunde")[d$location]
	
	d$variety <- c("improved",  "local", "local and improuve", NA)	[d$variety]

	d$land_ownedby <- c("Families", "Village", "Individual", "not known")[d$land_ownedby]
	d$planting_method <- c("direct seeding", "transplanting", "none")[d$planting_method]

	d$trial_id <- paste(d$location, d$hhid, sep = "-")

	geo <- data.frame(
	   location = c("Kakua", "Selenga", "Valunia", "Niawa", "Bagbe", "Tikonko", "Bumpeh", "Boama", "Gbo", "Jiama", "Lugbu", "Wunde", "Simbaru", "Langrama", "Tunkia", "Gaura", "Nomo", "Koya", "Dama", "Malegohoun", "Small Bo", "Nongowa", "Kando", "Gorama", "Wandor", "Komboya"),
	   longitude = c(-11.737, -12.915, -11.381 , -12.614, -11.2001, -11.746, -12.3589, -11.665, -12.7207, -11.2497, -13.1624, -11.693, -11.2646, -11.266, -11.097, -10.912, -11.1974, -11.50, -11.189, -13.111, -12.356, -11.473, -11.712, -12.641, -11.905, -11.782),
	   latitude = c(7.952, 9.125, 8.286, 8.976, 7.385, 7.958, 8.644, 7.993, 9.0347, 8.696, 8.398, 8.310, 8.4207, 8.383, 8.731, 7.747, 7.863, 8.101, 7.868, 8.206, 7.8054, 7.445, 7.721, 9.235, 7.8911, 8.835),
	   geo_from_source = FALSE
	)

	d <- merge(d, geo, by = "location", all.x = TRUE)

	d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)

	carobiner::write_files(path, meta, d)

}

