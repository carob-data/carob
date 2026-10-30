# R script for "carob"
# license: GPL (>=3)

## NOTES
# Vitamin C of advanced clones. The sheet has a second header row. Highland Tropic is dry matter percent. yield_moisture is 100 minus that value. Vict. C (mg/100DW) and Vict. C (mg/100FW) are ascorbic acid in mg/100 g.
# Locality and year are in one column, for example Huancayo-2005.

## ISSUES
# The files do not name a country or coordinates. Huancayo still needs georeferencing.
# The abstract discusses iron and zinc. This workbook has dry matter and vitamin C, not iron or zinc.
# No fertilizer, irrigation, planting date, harvest date, or yield.
# A locality value Huancayo-2005-2006 is more than one year. It is kept as recorded in the trial id.
# write_files: missing variables: carob_contributor, yield, yield_part, country, latitude, longitude, irrigated, K_fertilizer, N_fertilizer, P_fertilizer, planting_date, harvest_date. unknown variables: ascorbic_acid_mg_100g_dw_, ascorbic_acid_mg_100g_fw_.

## NOT PROCESSED
# Standard deviations next to dry matter and vitamin C.
# Primary Trait.
# The dictionary workbook.


carob_script <- function(path) {

"
Dataset for: Standard evaluation trial for Vitamin C content documentation of CIP advanced breeding clones

Iron (Fe), zinc (Zn) and vitamins A and C are among the most widespread nutrient deficiencies in developing countries. Potato genotypes display from moderate to large ranges of mineral content. A survey of advanced breeding clones and cultivars showed broad range of iron contents extending from 18 to 65 μg/g DW, while less overall variation of Zinc contents was true, ranging from 12.5 to 20 μg/g DW.  Some evidence of consistency of genotype effects across environments suggest that significant gains for iron and zinc can be expected through breeding, particularly for iron if coupled with a high content of vitamin C considering the close association between iron availability and vitamin C. Concentration of vitamin C in potatoes is moderate to low compared with fruits and other vegetables, yet as a staple food, potatoes may be one of the highest contributors of vitamin C in the diets of people in many places around the world. Inheritance study using low and high concentration parents suggested that vitamin C concentration in tubers is highly heritable and hence increased levels could rapidly be achieved through selection of high concentration parents  Standard evaluation trials assessed iron, zinc and vitamin C contents in tuber samples from advanced breeding clones to document their nutritional values. Healthy and representative tubers with regard to tuber size of each clone  are picked up  from plots of two replications of 10 hill plots from trials conducted in pathogen and pest protected fields and grown following local procedures. Iron and Zinc content are determined by inductively coupled plasma–optical emission spectrophotometry (ICP) and concentrations expressed in mg/kg, dry weight (DW), while vitamin C is quantified by the spectrophotometric method and expressed in mg/100 g, fresh weight (FW)
"

	uri <- "doi:10.21223/P3_AZUJB4"
	group <- "varieties_potato"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=4,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "ascorbic_acid_mg_100g_fw_;yield_moisture",
		notes = NA,
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 50,
		carob_effort = 0.35
	)

	f <- ff[grepl("Vitamin C Content Data", basename(ff))]
	r <- carobiner::read.excel(f, sheet="VIT.C", na=c("", "NA"))
	names(r) <- trimws(names(r))
	num <- function(x) {
		x <- trimws(as.character(x))
		x[!grepl("^-?[0-9]+(\\.[0-9]+)?$", x)] <- NA_character_
		as.numeric(x)
	}
	clone <- trimws(as.character(r$Clone))
	loc <- trimws(as.character(r[["Locality and Year of Evaluation"]]))
	dm <- trimws(as.character(r[["Highland Tropic"]]))
	ok <- !is.na(clone) & nzchar(clone) & clone != "Clone"
	ok <- ok & !dm %in% c("Highland Tropic", "Dry Matter")
	ok <- ok & !is.na(loc) & nzchar(loc) & loc != "Locality and Year of Evaluation"
	r <- r[ok, ]
	clone <- clone[ok]
	loc <- loc[ok]
	place <- sub("-.*", "", loc)
	year <- sub("^[^-]+-?", "", loc)

	d <- data.frame(
		trial_id = paste0(place, "_", year),
		location = place,
		crop = "potato",
		variety = clone,
		variety_code = clone,
		treatment = clone,
		yield_moisture = 100 - num(r[["Highland Tropic"]]),
		ascorbic_acid_mg_100g_dw_ = num(r[["Vict. C (mg/100DW)"]]),
		ascorbic_acid_mg_100g_fw_ = num(r[["Vict. C (mg/100FW)"]]),
		on_farm = FALSE,
		is_survey = FALSE,
		geo_from_source = FALSE
	)

	carobiner::write_files(path, meta, d)
}
