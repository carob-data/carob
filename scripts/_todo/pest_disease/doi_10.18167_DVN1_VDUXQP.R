# R script for "carob"
# license: GPL (>=3)

## NOTES
# One row per sample, matrix (soil or mulch), and taxon. Abundance is the file's individuals per m2. The 625 cm2 count is the same observation.
# Fertilizer codes are the Read me sheet: PL poultry litter, PS pig slurry, SF synthetic fertilizer, SS1 and SS2 sewage sludge rates. Rates in kg/ha are not in the sheet.
# Soil nutrients are merged from the Soil parameters sheet. Corg is g/100 g, so it is percent. Ntot is g/kg, multiplied by 1000 to mg/kg.
# Country is France from geographicCoverage. The workbook does not name Réunion, so no finer place is set.

## ISSUES
# Elemental N, P, and K rates are not in the workbook. SS1 and SS2 are different sludge rates but the rate is not given.
# No sugarcane yield is in this file.
# write_files: missing yield, yield_moisture, yield_part, geo_from_source, latitude, longitude, irrigated, K_fertilizer, N_fertilizer, P_fertilizer, planting_date, harvest_date.
# unknown variables: fertilization_modality_, sample_matrix_, fauna_common_, fauna_species_, fauna_abundance_. invalid terms: OM_type: pig slurry, sewage sludge.

## NOT PROCESSED
# delta.pH.
# Abundance_indiv_625cm2, because Abundance_indiv_m2 is the same count on a per m2 basis.
# Field trial design pdf.


carob_script <- function(path) {

"
Replication Data for: Does fertilization type drive soil and mulch macroinvertebrate communities in a sugarcane agroecosystem? Evidences from a 10-year field trial

Macroinvertebrates play a central role in processes supporting soil fertility. In the framework of the ecological intensification of agriculture, the choice of management practices should be guided by their ability to support these beneficial organisms supplying ecosystem services. This study aims at investigating the specific effect of partial substitution of synthetic fertilizers by locally produced organic fertilizers at a similar level of major nutrient inputs on macroinvertebrate’s abundance and diversity in sugarcane agroecosystems, on a Nitisol. Invertebrates visible to the naked eye were sampled in 2013, 2016, 2019 and 2023 using the standardized TSBF method on a long term experimental field trial in Réunion island. The individuals were identified and soil samples were analyzed for physico-chemical properties. Despite the low response of macroinvertebrates to the fertilizer type, total macroinvertebrate abundance increased over time, especially isopods and earthworms. The input of organic carbon via the return of litter to the soil surface and the root turnover after each harvest enables soil macroinvertebrates to be more abundant even after the replanting tillage. Mulching and root turnover are therefore important levers to consider for promoting macroinvertebrates in sugarcane agroecosystems.
"

	uri <- "doi:10.18167/DVN1/VDUXQP"
	group <- "pest_disease"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIRAD",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "fertilization_modality_",
		response_vars = "fauna_abundance_",
		carob_LLM = "Cursor Grok 4.7",
		carob_date = "2026-10-31",
		carob_completion = 55,
		carob_effort = 0.5
	)

	f <- ff[basename(ff) == "Dataverse_Macrofauna_240229.xlsx"]
	r <- carobiner::read.excel(f, sheet="Macrofauna Densities", na="NA(notassigned)")
	r1 <- carobiner::read.excel(f, sheet="Soil parameters")

	clean <- function(x) {
		x <- trimws(as.character(x))
		x[x %in% c("", "NA(notassigned)", "NA")] <- NA_character_
		x
	}
	om <- c(PL="poultry manure", PS="pig slurry", SF="none", SS1="sewage sludge", SS2="sewage sludge")

	d <- data.frame(
		trial_id = paste0("macrofauna_", r$Year),
		crop = "sugarcane",
		treatment = trimws(as.character(r$Modality)),
		fertilization_modality_ = trimws(as.character(r$Modality)),
		OM_type = unname(om[trimws(as.character(r$Modality))]),
		OM_used = !trimws(as.character(r$Modality)) %in% "SF",
		rep = as.integer(r$Replicate),
		plot_id = trimws(as.character(r$Site)),
		date = as.character(r$Year),
		sample_matrix_ = tolower(trimws(as.character(r$Matrix))),
		fauna_common_ = clean(r$Common.name),
		fauna_species_ = clean(r$Species),
		fauna_abundance_ = as.numeric(r$Abundance_indiv_m2),
		country = "France",
		is_survey = FALSE,
		on_farm = FALSE,
		sample_key = trimws(as.character(r$Sample))
	)

	soil <- data.frame(
		sample_key = trimws(as.character(r1$Sample)),
		soil_pH = as.numeric(r1[["pH.H2O"]]),
		soil_SOC = as.numeric(r1$Corg),
		soil_N_total = as.numeric(r1$Ntot) * 1000,
		soil_P = as.numeric(r1$Pavail),
		soil_P_method = "Olsen-Dabin",
		soil_CEC = as.numeric(r1$CEC),
		soil_K_exch = as.numeric(r1$Kexch),
		soil_Na_exch = as.numeric(r1$Naexch),
		soil_Ca_exch = as.numeric(r1$Caexch),
		soil_Mg_exch = as.numeric(r1$Mgexch)
	)
	d <- merge(d, soil, by="sample_key", all.x=TRUE)
	d$sample_key <- NULL

	carobiner::write_files(path, meta, d)
}
