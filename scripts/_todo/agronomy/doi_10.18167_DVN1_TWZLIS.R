# R script for "carob"
# license: GPL (>=3)

## NOTES
# Open sheets donini_Ngong and donini_gashiga. Grain yield is RDGRA (kg/ha on both sheets). Village is the village column (Kawtal, Normandie, Ngabala, Ngalaba). The sheet names are the two village clusters.
# Country Cameroon is the title (Nord Cameroun) and geographicCoverage. On-farm is the description ("chez des producteurs"). No coordinates in the open files.
# NPK dose is kg/ha of product and the grade is not in the codebook, so it is not converted. Where the NPK dose is 0 and the urea dose is present, N is urea times 0.46 and P and K are 0. Where NPK was applied, N, P, and K are left missing.
# The codebook says aboveground biomass is t/ha. Ngong values are 0 to 6 and Gashiga values are about 700 to 8500, so Ngong is treated as t/ha and Gashiga as kg/ha. Moisture basis is not stated, so this is not dmy_total.
# NPK grade is not in the codebook. Plots with NPK dose 0 also had urea dose 0, so elemental N, P, and K are not stored. The product doses are npk_product_ and urea_product_.

## ISSUES
# Soil analysis workbooks are restricted and were not opened. Weather, rainfall, and the protocol documents are not mapped. Recorded zeros for height, plant density, and spike density fall outside the allowed range.
# write_files: missing variables: project. missing variables: yield_moisture, geo_from_source, latitude, longitude, irrigated, K_fertilizer, N_fertilizer, P_fertilizer. unknown variable: ear_yield_ (yield?). unknown variables: npk_product_, urea_product_, emerged_density_, aboveground_biomass_. out of bounds: plant_height (0, 123.25), plant_density (0, 47812.5), spike_density (0, 46875).

## NOT PROCESSED
# HAUT1 (a second height with no date). Weed scores, fertility judgments, fallow age, operation delays, and the weather and protocol files. Restricted soil files.
# météo_station_synoptique_Garoua_2013.xlsx: not read. sheet Sang 2013 horaires: Station météo Station IRAD de Bocklé (Sanguéré), Garoua, ...2, ...3, ...4, ...5, ...6, données horaires. Observation records are missing from d until this file is standardized.
# pluviometrie_cameroun_2013_gashiga_djalingo.xls: not read. sheet Ngong 2013 (of 2): Département, ...2, ...3, ...4, ...5, ...6, Cumul annuel (mm), ...8, ...9, 691. Observation records are missing from d until this file is standardized.

## NEW VARIABLES
# npk_product_: Dose NPK, kg/ha of product, grade unknown. 0 means none applied.
# urea_product_: Dose urée, kg/ha of urea. 0 means none applied.
# aboveground_biomass_: BIOM converted to kg/ha as described above.
# ear_yield_: RDEPI, ear yield kg/ha.
# emerged_density_: DENL, plants/ha at emergence. plant_density is DENR at harvest.


carob_script <- function(path) {

"
Essai intensification du maïs au Nord Cameroun (2013)

Essais mis en place chez des producteurs dans deux grappes de villages du Nord Cameroun en 2013 dans l'objectif d'étudier la faisabilité de l'intensification de la culture du maïs. Des résultats d'analyses de sols sur chaque essai sont parvenus après ces valorisations qui peuvent donner lieu à d'autres interprétations
"

	uri <- "doi:10.18167/DVN1/TWZLIS"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIRAD",
		publication = "doi:10.1051/cagri/2016048",
		design = NA,
		data_type = "on-farm experiment",
		treatment_vars = "treatment;land_prep_method",
		response_vars = "yield;plant_height",
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 50,
		carob_effort = 0.7
	)

	f <- ff[basename(ff) == "operations culturales ensemble parcelles.xlsx"]
	r1 <- carobiner::read.excel(f, sheet = "donini_Ngong")
	r2 <- carobiner::read.excel(f, sheet = "donini_gashiga")
	r1$cluster <- "Ngong"
	r2$cluster <- "Gashiga"
	keep <- c("cluster", "code parcelle", "village", "BLOCS", "Modalités", "labour ?",
		"Date semis", "Herbicidage ?", "Apports organiques", "Date récolte",
		"Dose NPK", "Dose urée", "HAUT2", "DENL", "DENR", "NBEPI", "BIOM", "RDEPI", "RDGRA", "PCGBI")
	r <- rbind(r1[, keep], r2[, keep])

	npk <- as.numeric(r[["Dose NPK"]])
	urea <- as.numeric(r[["Dose urée"]])
	only_urea <- !is.na(npk) & npk == 0 & !is.na(urea)
	ftype <- ifelse(only_urea & urea == 0, "none",
		ifelse(only_urea, "urea",
		ifelse(!is.na(npk) & npk > 0 & !is.na(urea) & urea > 0, "NPK;urea",
		ifelse(!is.na(npk) & npk > 0, "NPK", NA))))

	# Codebook says t/ha. Ngong is 0-6; Gashiga is about 1000 times larger and matches kg/ha.
	biom <- as.numeric(r$BIOM)
	biom[r$cluster == "Ngong"] <- biom[r$cluster == "Ngong"] * 1000

	as_day <- function(x) {
		if (inherits(x, "POSIXt")) return(format(x, "%Y-%m-%d"))
		if (inherits(x, "Date")) return(as.character(x))
		as.character(x)
	}

	d <- data.frame(
		trial_id = paste(r$village, "2013", sep = "_"),
		plot_id = as.character(r[["code parcelle"]]),
		location = r$village,
		country = "Cameroon",
		crop = "maize",
		treatment = as.character(r[["Modalités"]]),
		block_id = as.character(r$BLOCS),
		planting_date = as_day(r[["Date semis"]]),
		harvest_date = as_day(r[["Date récolte"]]),
		land_prep_method = ifelse(r[["labour ?"]] == "oui", "tillage", "none"),
		herbicide_used = r[["Herbicidage ?"]] == "oui",
		OM_used = r[["Apports organiques"]] == "oui",
		npk_product_ = npk,
		urea_product_ = urea,
		fertilizer_type = ftype,
		plant_height = as.numeric(r$HAUT2),
		plant_density = as.numeric(r$DENR),
		emerged_density_ = as.numeric(r$DENL),
		spike_density = as.numeric(r$NBEPI),
		yield = as.numeric(r$RDGRA),
		yield_part = "grain",
		ear_yield_ = as.numeric(r$RDEPI),
		aboveground_biomass_ = biom,
		harvest_index = as.numeric(r$PCGBI),
		on_farm = TRUE,
		is_survey = FALSE
	)

	carobiner::write_files(path, meta, d)
}
