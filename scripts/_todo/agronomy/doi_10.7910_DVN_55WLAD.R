# R script for "carob"
# license: GPL (>=3)

## NOTES
# GAP fertilizer rates from Senthilkumar et al. (doi:10.1007/s10333-018-0666-7): 80 kg N/ha and 40 kg P2O5/ha as urea and DAP; K not recommended.
# FP fertilizer 0 (paper Table 1: conventionally no fertilizer).
# Yield t/ha at 14% moisture; panicles/m2 * 10000 = spike_density; weed g/m2 * 10 = kg/ha.

## ISSUES
# Georeference villages Mkula, Msufini, Ichonde, Kisawasawa, Idete, Namwawala (Kilombero).
# All records use paper bounding-box midpoint (8°11'S–36°15'E to 7°52'S–36°53'E); geo_uncertainty 40000 m.
# Farmer plot size from questionnaire 6 is ~0.2 ha (out of plot_area bounds); omitted.
# Questionnaire files on adoption and field-day opinions not processed.


carob_script <- function(path) {

"
Replication Data for: Increasing paddy yields and improving farm management: results from participatory experiments with good agricultural practices (GAP) in Tanzania

Rice is an increasingly important commodity in sub-Saharan Africa. In Tanzania, the rice yield gap is as high as 87%, due to a combination of production constraints and sub-optimal crop management. Reducing this yield gap may be partly achieved through the introduction and dissemination of good agricultural practices (GAP). We conducted 18 farmer-managed on-farm trials in Tanzania, to test a set of GAP components against conventional farmers' practices (FP) for two consecutive growing seasons in 2013 and 2014. The objectives were: (1) to understand farmers' capabilities in implementing GAP; (2) to acquire better insights into the merits, relevance and suitability of individual GAP components; and (3) to provide a case study showing that exposure to good practices combined with the farmers' own experimentations can serve to improve and, trigger a positive change in the participating farmers' crop management. Compared to the farmers' own practices, average yield increases of 1 t paddy ha−1 in 2013 and 2.7 t ha−1 in 2014 were achieved when following GAP. These yield advantages were mainly obtained by a higher panicle number, improved harvest index and improved weed control. Farmers experienced difficulties with land levelling, planting or sowing in lines and using rotary weeders, but they were convinced that these technologies are important to boost their rice yields. The case of Tanzania shows that paddy yields can be substantially improved by GAP and that adoption of GAP by smallholder rice farmers can be triggered by stimulating experimentations with such practices on their own farms.
"

	uri <- "doi:10.7910/DVN/55WLAD"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "AfricaRice",
		publication = "doi:10.1007/s10333-018-0666-7",
		project = "SARD-SC;Africa RISING",
		design = "on-farm GAP vs farmer practice; 18 farmers x 2 seasons",
		data_type = "on-farm experiment",
		treatment_vars = "treatment;N_fertilizer;P_fertilizer",
		response_vars = "yield;plant_height;seed_weight;weed_biomass;harvest_index",
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.6",
		carob_completion = 70,
		carob_effort = 0.8
	)

	f1 <- ff[basename(ff) == "Trial1_2013.xls"]
	f2 <- ff[basename(ff) == "Trial2_2014.xls"]
	fq <- ff[basename(ff) == "Questionnaire1_2013.xls"]

	r1 <- carobiner::read.excel(f1)
	r2 <- carobiner::read.excel(f2)
	rq <- carobiner::read.excel(fq)

	# Farmer, ward, village, production system from 2013 questionnaire
	loc <- data.frame(
		farmer = rq$q_3,
		adm3 = carobiner::fix_name(rq$q_1, "title"),
		location = carobiner::fix_name(rq$q_2, "title"),
		irrigated = rq$q_4 == "IL"
	)

	# Paper Methods: GAP N:P2O5 = 80:40 kg/ha as urea and DAP; K not recommended.
	# FP conventionally no fertilizer (Table 1).
	P_gap <- 40 / 2.29

	make_year <- function(r, year, has_hi) {
		fp <- data.frame(
			farmer = r$q1,
			treatment = "FP",
			variety = as.character(NA),
			yield = r$q3_1 * 1000,
			spike_density = r$q4_1 * 10000,
			plant_height = r$q5_1,
			seed_weight = r$q6_1,
			weed_biomass = r$q7_1 * 10,
			N_fertilizer = 0,
			P_fertilizer = 0,
			K_fertilizer = 0,
			fertilizer_type = "none",
			planting_date = paste0(year, "-02"),
			harvest_date = paste0(year, "-06")
		)
		gap <- data.frame(
			farmer = r$q1,
			treatment = "GAP",
			variety = "SARO5",
			yield = r$q3_2 * 1000,
			spike_density = r$q4_2 * 10000,
			plant_height = r$q5_2,
			seed_weight = r$q6_2,
			weed_biomass = r$q7_2 * 10,
			N_fertilizer = 80,
			P_fertilizer = P_gap,
			K_fertilizer = 0,
			fertilizer_type = "urea; DAP",
			planting_date = paste0(year, "-02"),
			harvest_date = paste0(year, "-06")
		)
		if (has_hi) {
			fp$harvest_index <- r$q8_1 * 100
			gap$harvest_index <- r$q8_2 * 100
		} else {
			fp$harvest_index <- as.numeric(NA)
			gap$harvest_index <- as.numeric(NA)
		}
		rbind(fp, gap)
	}

	d <- rbind(make_year(r1, "2013", FALSE), make_year(r2, "2014", TRUE))
	d <- merge(d, loc, by = "farmer")

	d$crop <- "rice"
	d$country <- "Tanzania"
	d$adm1 <- "Morogoro"
	d$adm2 <- "Kilombero"
	d$trial_id <- paste(d$farmer, substr(d$planting_date, 1, 4), sep = "_")
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$yield_part <- "grain"
	d$yield_moisture <- 14
	d$flooded <- TRUE
	d$geo_from_source <- FALSE
	# Paper study area bounding-box midpoint; replace with village georeferences
	d$latitude <- -(8 + 11/60 + 7 + 52/60) / 2
	d$longitude <- (36 + 15/60 + 36 + 53/60) / 2
	d$geo_uncertainty <- 40000
	d$geo_source <- "publication bounding box"

	d$farmer <- NULL

	carobiner::write_files(path, meta, d)
}
