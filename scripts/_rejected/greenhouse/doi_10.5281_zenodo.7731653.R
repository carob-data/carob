# R script for "carob"
# license: GPL (>=3)

## ISSUES
# Species dynamics sheets skipped: no standard variable for colony composition.
# Available P sheet skipped: no P-rate column, can't match to a record_id.
# soil_microbial_P not in terminag.
# New OM_type values not yet in terminag: Crotalaria juncea, Calliandra calothyrsus,
# cattle manure, maize stover.
# planting_date ("2016-12") is inferred from routine amendment timing, not stated
# directly; harvest_date is year-only ("2017"): exact date not given in source.
# shoot_dry_biomass not in terminag: pot-scale (g/pot), can't convert to kg/ha.

carob_script <- function(path) {

"
Effects of increasing phosphorus rate on microbial dynamics and soil available P under a Lixisol soil in Zimbabwe

Soil phosphorus (P) deficiency is a major challenge to the attainment of food security in most parts of sub-Saharan Africa, including Zimbabwe, where farmers largely depend on local organic nutrient resources in cropping. A greenhouse study was conducted to evaluate the influence of increasing inorganic P fertilizer rates (16, 26 and 36 kg P ha-1) on microbial dynamics, soil P pools and maize P uptake. using soils collected from a long-term (16 seasons) maize-monocropped field experiment where organic nutrient resources of different qualty namely Crotalaria juncea (high quality), Calliandra Calothyrus (medium quality), cattle manure ( variable quality), maize stover and Pinus patula sawdust ( both low quality) were applied at 4 t C ha-1 with 16 kg P ha-1 at the start of every season, a 3 x 6 factorial experiment was established by adding single super phosphate (8.5% P) as a basal fertilizer. Maize was used as the test crop and the experiment was conducted at Soil Productivity Research Laboratory in Zimbabwe. Using Pokovskays agar medium and serial dilution methods, phsophate-solubilizing microbes were monitored forthnightly from day 1 to 57 after maize planting. Ninetten (19) fungal and forty-two (42) bacterial colonies were identified over the study period, Fungi dominated bacteria on dayone, wth Aspergillus niger showing 20-98% abundance that depended on organic resources quality. Overall, a microbial explosion bulge characterized sucession on day 29 which coincided with a significant (P< 0.05) increase in diversity (H') and soil available P. Increasing P rate to 26 kg ha-1 amplified the explosion bulge under medium-high quality resources while under the control the bulge emerged earlier on day 15. Mucor and Bacillus had peak abundance on day 43 and 57, respectively, across treatments regardless of P rates. Treatment and P rate had a significant (P< 0.01) effect on microbial P. Bacteria were more rsponsive to added P than fungi. Increasing P to 36 kg P ha-1 also stimulated an earlier bulge under maize stover on day 15. Addition of P alone, without supplying complementary nutrients such as nitrogen,did not have a positive effect on maize P uptake. Farmers need to co-apply medium-high quality organic resources with high fertilzier P rates to increase microbial diversity, soil available P and maize growth on sandy soils (Lixisols). Our results suggest the need to recondiser existing P fertilizer recommendations, currently pegged at between 26 and 30 kg P ha-1, for maize production on sandy soils as well develop new fertilizer formulations to intensify crop production in Zimbabwe.
"

	uri <- "doi:10.5281/zenodo.7731653"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=NA,
		data_organization = "University of Zimbabwe; Marondera University of Agricultural Sciences and Technology",
		publication = "doi:10.1371/journal.pone.0291226",
		project = NA,
		carob_date = "2026-07-11",
		design = "RCBD, 3 blocks x 3 reps, 3x6 factorial",
		data_type = "experiment",
		treatment_vars = "P_fertilizer;OM_type",
		response_vars = "shoot_dry_biomass;soil_pH_CaCl2;soil_microbial_P",
		carob_contributor = "Oscar Bautista",
		carob_completion = 90, # self-assessed; the logic for estimating this needs documentation
		carob_effort = 2.5,
		notes = NA
	)
	

	f1 <- ff[basename(ff) == "Raw%20Data.xlsx"]

## main table: one record per pot (3 P rates x 6 organic-resource treatments)
	r1 <- carobiner::read.excel(f1, sheet="Maize biomass")
	om_types <- unlist(r1[1, 2:7])
	r1 <- r1[-1, ]
	colnames(r1) <- c("P_fertilizer", om_types)
	r1[, 2:7] <- lapply(r1[, 2:7], as.numeric)
	r1$P_fertilizer <- as.numeric(r1$P_fertilizer)
	d1 <- reshape(r1, direction="long", varying=list(2:7), v.names="shoot_dry_biomass", timevar="OM_type", times=om_types)
	d1$id <- NULL
	rownames(d1) <- NULL

	d <- data.frame(
		record_id = 1:nrow(d1),
		P_fertilizer = d1$P_fertilizer,
		OM_type = d1$OM_type,
		shoot_dry_biomass = d1$shoot_dry_biomass # g/pot; see ISSUES
	)

## standardize OM_type; only "sawdust" and "none" are in terminag already -- the others
## (crop/green-manure species and cattle manure) are new values, see ISSUES
	d$OM_type <- carobiner::replace_values(d$OM_type,
		c("Crotalaria", "Calliandra", "Manure", "Maize stover", "Saw dust", "Control"),
		c("Crotalaria juncea", "Calliandra calothyrsus", "cattle manure", "maize stover", "sawdust", "none"))

	d$crop <- "maize"
	d$variety <- "SC555" # Tauro et al. 2023: hybrid cultivar, ~136 days to maturity
	d$country <- "Zimbabwe"
	d$trial_id <- "1"
	d$on_farm <- FALSE
	d$is_survey <- FALSE
## pot moisture kept at 70% field capacity, ~500 mL de-ionized water every 2nd day (Tauro et al. 2023)
	d$irrigated <- TRUE

## Grasslands Research Institute, Marondera, Zimbabwe (31 28'56"E; 18 10'15"S; Tauro et al. 2023)
	d$location <- "Grasslands Research Institute"
	d$adm1 <- "Mashonaland East"
	d$adm2 <- "Marondera"
	d$longitude <- 31.482222
	d$latitude <- -18.170833
	d$geo_from_source <- TRUE

## planting inferred as Dec 2016: organic amendments (from which the pot soil was taken)
## are incorporated "in early December after the start of the rainy season" each season,
## and pot soil was collected "following" the 2016/17-season incorporation (Tauro et al. 2023)
	d$planting_date <- "2016-12"
	d$harvest_date <- "2017" # shoot biomass cut on day 57 after planting; exact date not given

## basal P:K:S (14%:13.4%:5%) was applied to the source field in prior seasons, not to
## the pots themselves -- only P was added during the pot phase (Tauro et al. 2023)
	d$N_fertilizer <- 0
	d$K_fertilizer <- 0
	d$S_fertilizer <- 0
	d$fertilizer_type <- "SSP" # single super phosphate, 14% P (Tauro et al. 2023)

## no harvestable yield was measured (greenhouse study stopped at day 57); the real
## response is shoot_dry_biomass (see ISSUES)
	d$yield_part <- "none"
	d$yield <- as.numeric(NA)
	d$yield_moisture <- as.numeric(NA)
	d$yield_isfresh <- NA

## repeated measurements (days 1, 15, 29, 43, 57): soil pH
	r2 <- carobiner::read.excel(f1, sheet="Soil pH")
	days <- as.numeric(gsub("Day ", "", colnames(r2)[3:7]))
	r2 <- r2[-1, ]
	colnames(r2) <- c("OM_type", "P_fertilizer", paste0("d", seq_along(days)))
	r2[, 3:7] <- lapply(r2[, 3:7], as.numeric)
	r2$P_fertilizer <- as.numeric(r2$P_fertilizer)
	d2 <- reshape(r2, direction="long", varying=list(3:7), v.names="soil_pH_CaCl2", timevar="DAP", times=days)
	d2$id <- NULL

## repeated measurements: soil microbial P, stacked in 5 known day-blocks
	blocks <- list(
		list(day=1,  range="A5:D10"),
		list(day=15, range="A15:D20"),
		list(day=29, range="A25:D30"),
		list(day=43, range="A35:D40"),
		list(day=57, range="A45:D50")
	)
	d3 <- NULL
	for (b in blocks) {
		r3 <- carobiner::read.excel(f1, sheet="Microbial P", range=b$range, col_names=FALSE)
		colnames(r3) <- c("OM_type", "16", "26", "36")
		r3$OM_type <- gsub(" at 4.0 t C ha-1", "", r3$OM_type)
		r3[, 2:4] <- lapply(r3[, 2:4], as.numeric)
		x <- reshape(r3, direction="long", varying=list(2:4), v.names="soil_microbial_P", timevar="P_fertilizer", times=c(16, 26, 36))
		x$id <- NULL
		x$DAP <- b$day
		d3 <- rbind(d3, x)
	}

## combine the two repeated-measures tables (exact 1:1 match, same 3x6x5 grid) and
## link them back to the main table via record_id
	d4 <- merge(d2, d3, by=c("OM_type", "P_fertilizer", "DAP"))
## match the same OM_type standardization applied to d, so the record_id join lines up
	d4$OM_type <- carobiner::replace_values(d4$OM_type,
		c("Crotalaria", "Calliandra", "Manure", "Maize stover", "Saw dust", "Control"),
		c("Crotalaria juncea", "Calliandra calothyrsus", "cattle manure", "maize stover", "sawdust", "none"))
	x <- data.frame(
		record_id = d$record_id[match(paste(d4$OM_type, d4$P_fertilizer), paste(d$OM_type, d$P_fertilizer))],
		date = "2017", # year-only, same precision as planting_date/harvest_date
		DAP = as.integer(d4$DAP),
		soil_pH_CaCl2 = d4$soil_pH_CaCl2,
		soil_microbial_P = d4$soil_microbial_P
	)

	carobiner::write_files(path, meta, d, long = x)
}
