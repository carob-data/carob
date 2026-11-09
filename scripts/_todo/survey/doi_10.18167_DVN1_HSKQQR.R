# R script for "carob"
# license: GPL (>=3)

## NOTES
# One row per farmer in 000_group_perf_typo_data.xlsx. Rdt_frm_cotco equals cotton sold divided by the company cotton area, and the result is in the kg/ha range, so yield is that column in kg/ha.
# Urea and NPK doses are product kg/ha from the company credit divided by cotton area (dose_uree_theorik, dose_npk_theorik). A zero is no product supplied. The NPK grade is not in the files, so elemental N, P, and K are not computed.
# Country is Côte d'Ivoire from geographicCoverage and the title. Regions, departments, sous-préfectures, and villages are the sheet names. No coordinates are in the files.

## ISSUES
# yield_part is seed. The sheet says cotton sold to the cotton company and does not say lint.
# Region and department spellings are the sheet values, without the accents used in the geographicCoverage list.
# write_files: missing geo_from_source, latitude, longitude. unknown variables: urea_product_kg_ha_, npk_product_kg_ha_.

## NOT PROCESSED
# 000_regress_perf_data.xlsx: the same farmers and a subset of the typology columns.
# Household, credit, training, and opinion columns in the typology sheet.
# Farmer-stated urea and NPK doses. The company credit doses are the ones stored.
# Organic-manure columns. They record whether manure is produced, not a kg/ha rate.
# List_variables_perf.xlsx: not read. sheet list perf: Paysan_ID, Identifiant du paysan, unique. Observation records are missing from d until this file is standardized.
# List_variables_typo.xlsx: not read. sheet list_typo: Paysan_ID, Identifiant du paysan, unique. Observation records are missing from d until this file is standardized.
# Readme.txt: documentation or a codebook, not an observation table.


carob_script <- function(path) {

"
Exploitations dans les zones cotonnières de Côte d'Ivoire, enquête 2014

Enquête dans les treize régions administratives où la culture du coton a été enregistrée lors de la campagne 2013-14. Enquête réalisée dans le cadre de l'étude \"MISSION D’OBSERVATION DES EXPLOITATIONS COTONNIERES EN CÔTE D’IVOIRE\" sous le CONTRAT N°970 FIRCA / TECHNOSERVE. Le rapport de l'étude, propriété du FIRCA (Abidjan, Côte d'Ivoire), est disponible sur demande à ce dernier.
"

	uri <- "doi:10.18167/DVN1/HSKQQR"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=0,
		data_organization = "CIRAD",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "survey",
		treatment_vars = "none",
		response_vars = "yield",
		carob_LLM = "Cursor Grok 4.7",
		carob_date = "2026-10-31",
		carob_completion = 30,
		carob_effort = 0.4
	)

	f <- ff[basename(ff) == "000_group_perf_typo_data.xlsx"]
	r <- carobiner::read.excel(f, sheet="Typo_base_data")

	d <- data.frame(
		trial_id = trimws(as.character(r$Paysan_ID)),
		hhid = trimws(as.character(r$Paysan_ID)),
		country = "Côte d'Ivoire",
		adm1 = carobiner::fix_name(trimws(as.character(r$Region)), "title"),
		adm2 = carobiner::fix_name(trimws(as.character(r$Dpmt_nom_harmonisé)), "title"),
		adm3 = carobiner::fix_name(trimws(as.character(r$Ss_pref_nom_harmonisé)), "title"),
		location = carobiner::fix_name(trimws(as.character(r$Village)), "title"),
		crop = "cotton",
		yield = as.numeric(r$Rdt_frm_cotco),
		yield_part = "seed",
		field_size = as.numeric(r$Surface_coton_2013),
		urea_product_kg_ha_ = as.numeric(r$dose_uree_theorik),
		npk_product_kg_ha_ = as.numeric(r$dose_npk_theorik),
		insecticide_times = as.integer(r$L68_Nb_traitmt_13),
		weeding_times = as.integer(r$L61_Nb_sarcla_2013),
		date = "2014",
		on_farm = TRUE,
		is_survey = TRUE
	)

	carobiner::write_files(path, meta, d)
}
