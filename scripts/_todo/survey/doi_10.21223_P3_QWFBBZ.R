# R script for "carob"
# license: GPL (>=3)

## NOTES
# Marando Bora baseline, September 2010. Country is Tanzania from geographicCoverage and from the abstract (Lake Zone, Tanzania).
# Region, district, ward, and village are the value labels on A01-A04. There is no latitude or longitude column. A15G is labeled Elevation and the values are 950-1644, stored as meters.
# Year code 10 is 2010. Every interview is in month 9. L01 is land owned and L02 is land used, both labeled as hectares for the 2008/9 season.

## ISSUES
# No coordinates in the cover file.
# write_files: missing variables: latitude, longitude.

## NOT PROCESSED
# Cover-page consent, enumerator and supervisor codes, interview start and end times, and callback.
# mbbase_pg5_crop production.dta and mbbase_pg7_sweetpotato_production.dta. Harvest quantities use a unit code, and the crop is a crop code.
# Vine transactions, nutrition knowledge, dietary diversity, food security, shocks, assets, housing, demography, groups, sales, labour, and credit files.
# haconver.dta, sp_conv.dta, the questionnaire workbook, and the dictionary.
# mbase_questionnaire_20173001.xlsx: design, layout, or protocol sheet, not plot or survey records.
# mbbase_dictionary_20173001.xls: documentation or a codebook, not an observation table.
# mbbase_pg10_sold_gave_out_vines.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# mbbase_pg10_vines_transaction.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# mbbase_pg11_received_vines.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# mbbase_pg13_sweetpotato_knowledge_and_practice.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# mbbase_pg14_sp_varietal_preferences.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# mbbase_pg15_vit A knowledge_nutritional knowledge.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# mbbase_pg16_food_consumption_frequency.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# mbbase_pg17_man's knowledge of vit A.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# mbbase_pg18_food_security.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# mbbase_pg19_shocks.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# mbbase_pg2_adult_demog.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# mbbase_pg20_economic activities.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# mbbase_pg20_livestock and fishery.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# mbbase_pg21_assets.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# mbbase_pg22_main housing.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# mbbase_pg3_child_demog.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# mbbase_pg3_dietary_diversity.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# mbbase_pg4_farmer_group.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# mbbase_pg4_group_activities.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# mbbase_pg6_crop_sales.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# mbbase_pg6_road_access.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# mbbase_pg7_sp_cassava_sales.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# mbbase_pg8_labour_and_credit.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.
# mbbase_pg9_attitude_practice_knowledge.dta: not read. Stata .dta table. Observation records are missing from d until this file is standardized.


carob_script <- function(path) {

"
Replication Data for: Marando Bora Project Baseline Survey

The Marando Bora (MB) project was implemented by the International Potato Center (CIP) in collaboration with 12 other partners in the Lake Zone region, Tanzania from October 2009 to June 2012. The project's aim was to improve the quantity and quality of food for 150,000 Tanzanian households in the Lake Zone region by strengthening the availability and accessibility of quality planting material of improved and local sweetpotato varieties. The specific objectives of the MB project were to provide: i) high yielding and consumer preferred sweetpotato varieties; ii) orange-fleshed sweetpotato (OSFP) varieties with adequate supplies of beta-carotene to tackle Vitamin A deficiency (VAD) problem; iii) disease free planting materials for increased root and vine production; iv) timely access to vines for early planting; v) training for decentralized vines multipliers (DVMs) and farmers about vines conservation and maintenance of vine quality; vi) communication products to enhance awareness about benefits of using disease-free vines, benefits of OFSP varieties, and sources of quality sweetpotato vines. The MB project's theory of change (TOC) was to improve food and nutrition security and enhance farmers' incomes by implementing several activities including production and distribution of clean planting materials; training farmers' about vines conservation; training of farmer multipliers and provision of improved white and orange fleshed SP varieties among others.
The Marando Bora Baseline survey objective was to address the main problems associated with sweetpotato vine availability and distribution by developing a sustainable seed system for sweetpotato. It used voucher and mass distribution strategies to address these problems. Using decentralized vine multipliers in the communities and mass distribution, the project ensured timely access to planting vines at the beginning of the rainy season.
"

	uri <- "doi:10.21223/P3/QWFBBZ"
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
		carob_effort = 0.4
	)

	from_labels <- function(x) {
		v <- attr(x, "labels")
		y <- trimws(as.character(x))
		if (is.null(v)) return(y)
		names(v)[match(y, as.character(unname(v)))]
	}

	f <- ff[basename(ff) == "mbbase_pg1_cover_page.dta"]
	r <- haven::read_dta(f)
	f1 <- ff[basename(ff) == "mbbase_pg4_agricultural land.dta"]
	r1 <- haven::read_dta(f1)
	hid <- trimws(as.character(r$hhid))
	i <- match(hid, trimws(as.character(r1$hhid)))
	yr <- as.integer(r$A16C)
	yr <- ifelse(yr < 100L, yr + 2000L, yr)

	d <- data.frame(
		hhid = hid,
		country = "Tanzania",
		adm1 = carobiner::fix_name(from_labels(r$A01), "title"),
		adm2 = carobiner::fix_name(from_labels(r$A02), "title"),
		adm3 = carobiner::fix_name(from_labels(r$A03), "title"),
		location = carobiner::fix_name(from_labels(r$A04), "title"),
		elevation = as.numeric(r$A15G),
		hh_size = as.numeric(r$A08B1),
		farmland_owned = as.numeric(r1$L01)[i],
		farmland = as.numeric(r1$L02)[i],
		date = sprintf("%04d-%02d-%02d", yr, as.integer(r$A16B), as.integer(r$A16A)),
		trial_id = hid,
		geo_from_source = FALSE,
		on_farm = TRUE,
		is_survey = TRUE
	)

	carobiner::write_files(path, meta, d)
}
