# R script for "carob"
# license: GPL (>=3)

## NOTES
# Rwanda Sweetpotato Super Foods endline. Province, district, sector, and village names are the cover-page name columns. Elevation is a15e.
# Sweetpotato questions in pg8 are counts of fields, not root yield. Person names are not copied.

## ISSUES
# The cover page has a GPS correction factor, not longitude and latitude. Sites need georeferencing.
# No kg/ha yield and no fertilizer rate.
# write_files: missing carob_contributor, geo_from_source, latitude, longitude. unknown variables: residents_, valley_bottom_, farmer_group_, sp_gender_, and the sweetpotato field-count columns.

## NOT PROCESSED
# Modules other than the cover page and RW_sf_end_pg8_sp_prod_1.dta (demography, sales, labour, seed, consumption, nutrition, assets, housing, and the questionnaire workbook). pg8_sp_prod_2.dta was not mapped.
# RW_sf_end_dictionary_20162112.do: documentation or a codebook, not an observation table.
# RW_sf_end_pg10_sp_consumption.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# RW_sf_end_pg11_contract.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# RW_sf_end_pg12_trade_relation_1.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# RW_sf_end_pg12_trade_relations_2.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# RW_sf_end_pg13_sp_labr_1.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# RW_sf_end_pg13_sp_labr_2.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# RW_sf_end_pg14_seedsystem_1.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# RW_sf_end_pg14_seedsystem_2.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# RW_sf_end_pg15_seedsystem_3.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# RW_sf_end_pg16_attitude_knowledge.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# RW_sf_end_pg18_sp_storage.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# RW_sf_end_pg19_sp_varieties.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# RW_sf_end_pg2_demog_adult.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# RW_sf_end_pg20_shocks_credit.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# RW_sf_end_pg21_livestock.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# RW_sf_end_pg22_food_security.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# RW_sf_end_pg23_nutrition.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# RW_sf_end_pg24_diet_diversity.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# RW_sf_end_pg25_Vit A_consumption.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# RW_sf_end_pg26_consumption.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# RW_sf_end_pg27_assets.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# RW_sf_end_pg28_housing.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# RW_sf_end_pg3_demog_child.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# RW_sf_end_pg3_ref_child.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# RW_sf_end_pg3_remittance.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# RW_sf_end_pg3_revenue.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# RW_sf_end_pg4_group_mem.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# RW_sf_end_pg5_cropprod_1.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# RW_sf_end_pg5_cropprod_2.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# RW_sf_end_pg6_cropprod_3.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# RW_sf_end_pg7_othercrop_sales.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# RW_sf_end_pg8_sp_prod_2.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# RW_sf_end_pg9_sp_sales.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# RW_sf_end_questionnaire 20162212.xls: design, layout, or protocol sheet, not plot or survey records.

## NEW VARIABLES
# residents_, valley_bottom_, farmer_group_, sp_gender_.
# Field-count columns sp_upland_b_ and the other season and landscape counts.


carob_script <- function(path) {

"
Dataset for: Rwanda Super Foods Project Endline Survey

Rwanda Sweetpotato Super Foods Project Endline Survey  This endline survey was designed to collect data that was to be used for assessment of impact the project had on the beneficiaries. Rwanda Super Foods Project was focused on developing economically viable sweetpotato processed product value chains. The intervention had four specific objectives namely to:  1. Develop a sweetpotato value chain based on contracting fresh roots from individual farmers. The procured roots are then boiled and mashed at the SE factory or other new processors.   2. Develop a sweetpotato value chain based on farmer group formation, value-addition via processing by linking the groups to final processors.   3. Support technology development and dissemination for high value white and orange- fleshed sweetpotato product value chains, and to evaluate the change in consumer acceptance of sweetpotato based products.
"

	uri <- "doi:10.21223/P3/IPJKPG"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=7,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "survey",
		treatment_vars = "none",
		response_vars = "none",
		notes = NA,
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 20,
		carob_effort = 0.4
	)

	num <- function(x) {
		x <- trimws(as.character(haven::zap_labels(x)))
		ok <- grepl("^-?[0-9.]+$", x)
		x[!ok] <- NA_character_
		as.numeric(x)
	}
	chr <- function(x) {
		x <- trimws(as.character(haven::zap_labels(x)))
		x[!nzchar(x) | x == "NA"] <- NA_character_
		x
	}
	ymd <- function(y, m, d) {
		y <- num(y); m <- num(m); d <- num(d)
		ok <- !is.na(y) & !is.na(m) & !is.na(d) & m >= 1 & m <= 12 & d >= 1 & d <= 31
		out <- rep(NA_character_, length(y))
		out[ok] <- as.character(as.Date(sprintf("%04d-%02d-%02d", y[ok], m[ok], d[ok])))
		out
	}

	f1 <- ff[basename(ff) == "RW_sf_end_coverpage.dta"]
	f2 <- ff[basename(ff) == "RW_sf_end_pg8_sp_prod_1.dta"]
	r1 <- haven::read_dta(f1)
	r2 <- haven::read_dta(f2)
	d <- data.frame(
		hhid = chr(r1$hhid),
		adm1 = chr(r1$a01a),
		adm2 = chr(r1$a02a),
		adm3 = chr(r1$a03a),
		location = chr(r1$a04a),
		residents_ = num(r1$a08b1),
		valley_bottom_ = chr(r1$a08b2),
		farmer_group_ = chr(r1$a08b3),
		elevation = num(r1$a15e),
		date = ymd(r1$a17c, r1$a17b, r1$a17a)
	)
	sp <- data.frame(
		hhid = chr(r2$hhid),
		sp_gender_ = chr(r2$g01c),
		sp_upland_b_ = num(r2$g02a),
		sp_upland_a_ = num(r2$g02b),
		sp_upland_c_ = num(r2$g02c),
		sp_lowland_b_ = num(r2$g03a),
		sp_lowland_a_ = num(r2$g03b),
		sp_lowland_c_ = num(r2$g03c)
	)
	sp <- sp[!duplicated(sp$hhid), ]
	d <- merge(d, sp, by="hhid", all.x=TRUE)
	d$crop <- "sweetpotato"
	d$country <- "Rwanda"
	d$trial_id <- paste0("RW_sf_", d$hhid, "_", seq_len(nrow(d)))
	d$on_farm <- TRUE
	d$is_survey <- TRUE
	d$elevation[d$elevation < -200] <- NA_real_

	carobiner::write_files(path, meta, d)
}
