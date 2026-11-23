# R script for "carob"
# license: GPL (>=3)

## NOTES
# Publication found by search (not in the metadata): same six field-years, treatments and harvest dates.
# "time" (called "replication" in the dictionary) is the day of the year, from 1 January of the planting year: it reproduces the publication's harvest dates to within a day.
# HarvestNumber 1 and 2 are destructive samples at about 4 and 8 months. yield is HarvestNumber 3 only. Earlier root dry matter stays in dmy_storage.

## ISSUES
# ID 35 (Cross River 2016, NfPfKf) repeats the stems and roots of ID 36 (NfPfKfMn); the publication gives 29.7 t/ha root DM for NfPfKf, not 28.7. Its stems, roots, total and yield are set to NA.
# write_files reports NA yield for the 4- and 8-month samples, and for ID 35.


carob_script <- function(path) {

"
Cassava best planting practices total biomass

The African Cassava Agronomy Initiative (ACAI) aims at improving cassava root yield and quality, and cassava supply to the processing sector. The project has 6 use cases of which best planting practices (BPP) is one. BPP is focusing on guiding farmers in choosing best-suited planting practices for cassava, with a focus on tillage operations and in close relation with improved weed control recommendations.
"

	uri <- "doi:10.25502/qm2j-4q10/d"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		data_organization = "IITA",
		publication = "doi:10.1016/j.fcr.2020.107820",
		project = "ACAI",
		carob_effort = NA,
		carob_date = "2025-11-05",
		design = "RCBD with three replicates; values are treatment means",
		data_type = "experiment",
		treatment_vars = "N_fertilizer;P_fertilizer;K_fertilizer;S_fertilizer;Ca_fertilizer;Mg_fertilizer;Zn_fertilizer;B_fertilizer",
		response_vars = "yield;dmy_storage;dmy_stems;dmy_leaves;dmy_total",
		carob_contributor = "Cedric Ngakou",
		carob_completion = 100,
		notes = "Each sampling is a record with its own date. yield is the final harvest only (HarvestNumber 3). The 4- and 8-month samples keep root dry matter in dmy_storage and cover four of the twelve treatments."
	)

	f <- ff[basename(ff) == "plantparts_totalbiomass.csv"]
	r <- read.csv(f)

	## ID 35 repeats the stems and roots of ID 36 (see ISSUES)
	copied <- r$ID == 35
	r$Dmstems_g_m2[copied] <- NA
	r$DMRoots_g_m2[copied] <- NA
	r$Biomass_g_m2[copied] <- NA

	## "time" is the day of the year, counted from 1 January of the planting year
	r$sampling_date <- as.character(as.Date(paste0(r$Year, "-01-01")) + r$time - 1)

	## all parts are in g/m2, including DMLeaves_kg_ha: g/m2 * 10 = kg/ha
	## HarvestNumber 1 and 2 are not the crop harvest.
	final <- r$HarvestNumber == 3

	d <- data.frame(
		record_id = r$ID,
		country = r$Country,
		adm1 = c(Benue="Benue", CRS="Cross River", Edo="Edo")[r$Location],
		treatment = r$Treatment,
		N_fertilizer = r$N_application_kg_ha,
		P_fertilizer = r$P_application_kg_ha,
		K_fertilizer = r$K_application_kg_ha,
		harvest_date = r$sampling_date,
		dmy_leaves = r$DMLeaves_kg_ha * 10,
		dmy_stems = r$Dmstems_g_m2 * 10,
		dmy_storage = r$DMRoots_g_m2 * 10,
		dmy_total = r$Biomass_g_m2 * 10,
		yield = ifelse(final, r$DMRoots_g_m2 * 10, NA_real_),
		yield_moisture = 0,
		yield_part = "roots",
		crop = "cassava",
		variety = "TME 419",
		plant_density = 12500,
		is_survey = FALSE,
		on_farm = TRUE,
		irrigated = NA
	)
	d$trial_id <- paste(r$Location, r$Year, sep="-")

	## secondary and micronutrients, applied to one treatment only (Methods)
	mn <- d$treatment == "NfPfKfMn"
	d$S_fertilizer <- ifelse(mn, 16, 0)
	d$Ca_fertilizer <- ifelse(mn, 10, 0)
	d$Mg_fertilizer <- ifelse(mn, 10, 0)
	d$Zn_fertilizer <- ifelse(mn, 5, 0)
	d$B_fertilizer <- ifelse(mn, 2.5, 0)

	## urea, triple superphosphate and muriate of potash (Methods)
	ftype <- cbind(ifelse(d$N_fertilizer > 0, "urea", NA), ifelse(d$P_fertilizer > 0, "TSP", NA), ifelse(d$K_fertilizer > 0, "KCl", NA))
	d$fertilizer_type <- apply(ftype, 1, function(x) paste(x[!is.na(x)], collapse=";"))
	d$fertilizer_type[d$fertilizer_type == ""] <- "none"

	## sites, dates, previous crops and soils by field-year (Table 1 of the publication)
	geo <- data.frame(
		trial_id = c("Edo-2016", "CRS-2016", "Benue-2016", "Edo-2017", "CRS-2017", "Benue-2017"),
		location = c("Ekpoma", "Ogoja", "Otukpo", "Ekpoma", "Ikom", "Otukpo"),
		latitude = c(7.05, 6.76, 7.27, 6.80, 5.96, 7.27),
		longitude = c(6.13, 8.69, 8.18, 6.23, 8.77, 8.19),
		elevation = c(214, 47, 135, 215, 105, 139),
		planting_date = c("2016-05-24", "2016-06-16", "2016-08-16", "2017-05-12", "2017-06-03", "2017-06-15"),
		previous_crop = c("cassava;maize", "cassava", "cassava;maize", "cassava;maize", "cassava;maize", "soybean"),
		soil_type = c("Nitisol", "Acrisol", "Acrisol", "Nitisol", "Nitisol", "Acrisol"),
		geo_from_source = TRUE
	)
	d <- merge(d, geo, by="trial_id", all.x=TRUE)

	carobiner::write_files(path, meta, d)
}
