# R script for "carob"
# license: GPL (>=3)

## NOTES
# Marando Bora endline, January-March 2013, plus three interviews dated 2012. Country is Tanzania from geographicCoverage and from the abstract (Lake Zone, Tanzania).
# Region, district, ward, and village are the value labels. There is no latitude or longitude. The elevation column runs from 81 to 1589 and is stored as meters. One record is 81.
# Household size is the number of resident people.

## ISSUES
# No coordinates.
# write_files: missing variables: latitude, longitude.

## NOT PROCESSED
# Project exposure, vine dissemination, consent, and interview clock times on the cover file.
# Sweetpotato production, other crops, sales, labour, groups, credit, food security, shocks, assets, housing, nutrition, and demography files. Several of those are split into part 1 and part 2.
# haconver.dta, sp_conv.dta, the questionnaire workbook, and the dictionary.
# mbend_dictionary_20173001.xls: documentation or a codebook, not an observation table.
# mbend_pg10_differences_of_MB_vines_from_others.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# mbend_pg10_received_vines_elsewhere.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# mbend_pg10_received_vines_marandobora.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# mbend_pg11_pg13_sp_knowledge.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# mbend_pg14_genderbased_labour_part1.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# mbend_pg14_genderbased_labour_part2.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# mbend_pg15_farmer_group_part 2.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# mbend_pg15_farmer_group_part1.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# mbend_pg15_genderbased_group_activities_part1.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# mbend_pg15_genderbased_group_activities_part2.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# mbend_pg15_genderbased_social networks_part1.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# mbend_pg15_genderbased_social networks_part2.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# mbend_pg16_genderbased_information_part1.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# mbend_pg16_genderbased_information_part2.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# mbend_pg16_genderbased_trading relationships_part1.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# mbend_pg16_genderbased_trading relationships_part2.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# mbend_pg17_genderbased_nutritional knowledge_dietary habits_radio_part1.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# mbend_pg17_genderbased_nutritional knowledge_dietary habits_radio_part2.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# mbend_pg17_genderbased_women knowledge of vit A_part1.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# mbend_pg17_genderbased_women knowledge of vit A_part2.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# mbend_pg18_genderbased_frequency of consumption of vit A foods_part1.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# mbend_pg18_genderbased_frequency of consumption of vit A foods_part2.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# mbend_pg19_genderbased_attitude and perception_part1.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# mbend_pg19_genderbased_attitude and perception_part2.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# mbend_pg2_adult_demog.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# mbend_pg20_genderbased_sp varietal preferences and practice_part1.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# mbend_pg20_genderbased_sp varietal preferences and practice_part2.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# mbend_pg21_genderbased_credit_part1.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# mbend_pg21_genderbased_credit_part2.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# mbend_pg21_genderbased_food security_part1.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# mbend_pg21_genderbased_food security_part2.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# mbend_pg22_genderbased_shocks_part1.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# mbend_pg22_genderbased_shocks_part2.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# mbend_pg23_genderbased_Household assets_part1.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# mbend_pg23_genderbased_Household assets_part2.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# mbend_pg24_genderbased_Housing_part1.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# mbend_pg24_genderbased_Housing_part2.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# mbend_pg3_child_demog.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# mbend_pg3_dietary_diversity.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# mbend_pg4_agricultural_land.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# mbend_pg4_project_exposure.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# mbend_pg5_HH_sp_production_J00_J07B.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# mbend_pg5_sweetpotato_production_table.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# mbend_pg6_sp_production_cont.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# mbend_pg7_production_other_crops.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# mbend_pg8_crop_sales.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# mbend_pg9_HH_gaveout_sold_vines.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# mbend_pg9_vine_diffusion.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# mbend_questionnaire.xlsx: design, layout, or protocol sheet, not plot or survey records.


carob_script <- function(path) {

"
Replication Data for: Marando Bora Project Endline Survey

The Marando Bora (MB) project was implemented by the International Potato Center (CIP) in collaboration with 12 other partners in the Lake Zone region, Tanzania from October 2009 to June 2012.
The Marando Bora endline survey was conducted from January to March 2013. The objective was to understand dietary diversity of households and children between six(6) to 24 months of age; production and marketing of sweetpotatoes and other crops; vines diffusion and access to quality planting materials; farmers' knowledge about sweetpotato production including diseases, vines conservation, and sweetpotato storage methods; labour-use; membership to farmer associations, access to credit, and access to information; households' food security status, knowledge about vitamin A; nutritional knowledge and dietary habits; frequency of consumption of vitamin A foods; farmers' attitudes and perceptions about sweetpotatoes in general and OFSP in particular; sweetpotato varietal preferences; household's exposure to shocks; and household's asset endowment.
"

	uri <- "doi:10.21223/P3/ZQJNRS"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=5,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "survey",
		notes = NA,
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 20,
		carob_effort = 0.35
	)

	from_labels <- function(x) {
		v <- attr(x, "labels")
		y <- trimws(as.character(x))
		if (is.null(v)) return(y)
		out <- names(v)[match(y, as.character(unname(v)))]
		out
	}
	plain <- function(x) {
		y <- as.character(x)
		y <- vapply(y, function(s) {
			if (is.na(s)) return(NA_character_)
			rb <- charToRaw(s)
			if (any(rb == as.raw(0x92))) {
				rb[rb == as.raw(0x92)] <- charToRaw("'")
				s <- rawToChar(rb)
			}
			if (is.na(iconv(s, "UTF-8", "UTF-8"))) {
				s2 <- iconv(s, from="CP1252", to="UTF-8")
				if (!is.na(s2)) s <- s2
			}
			s
		}, character(1), USE.NAMES=FALSE)
		y <- trimws(y)
		y[!nzchar(y)] <- NA_character_
		y
	}

	f <- ff[basename(ff) == "mbend_pg1_cover_page.dta"]
	r <- haven::read_dta(f)
	hid <- trimws(as.character(r$hhid))

	d <- data.frame(
		hhid = hid,
		country = "Tanzania",
		adm1 = carobiner::fix_name(plain(from_labels(r$Region)), "title"),
		adm2 = carobiner::fix_name(plain(from_labels(r$District)), "title"),
		adm3 = carobiner::fix_name(plain(from_labels(r$Ward)), "title"),
		location = carobiner::fix_name(plain(from_labels(r$Village)), "title"),
		elevation = as.numeric(r$elevation),
		hh_size = as.numeric(r$HHmembers),
		date = sprintf("%04d-%02d-%02d", as.integer(r$A18C), as.integer(r$A18B), as.integer(r$A18A)),
		trial_id = hid,
		geo_from_source = FALSE,
		on_farm = TRUE,
		is_survey = TRUE
	)

	carobiner::write_files(path, meta, d)
}
