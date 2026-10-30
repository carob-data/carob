# R script for "carob"
# license: GPL (>=3)

## NOTES
# Fe CONT. and Zn CONT. use two header rows. Dry matter, iron, and zinc are the row-2 labels.
# Iron and zinc in mg/kg DW and mg/100FW are stored with those header units. They are not a yield.
# Locality and year are one text field, for example Huancayo-2005. Rows for Huancayo use the bounding box southLatitude -12.0264, westLongitude -75.2449.
# Vitamin C is described in the abstract and is not a column in this workbook.
# Country Peru is the geographic coverage.

## ISSUES
# Dry matter has no unit in the header.
# No yield, fertilizer, or a calendar planting date.
# Rows with a blank clone were removed. Huancayo uses the published box. Other localities have no coordinates.
# write_files: missing variables: carob_contributor; yield, yield_moisture, yield_part, K_fertilizer, N_fertilizer, P_fertilizer, planting_date, harvest_date.
# write_files: unknown variables: eval_label_, dm_recorded_, tuber_Fe_dw_, tuber_Fe_fw_, tuber_Zn_dw_, tuber_Zn_fw_.
# write_files: duplicate coordinates: fewer coordinates than locations. NA detected: latitude, longitude. duplicates: duplicate records detected.

## NEW VARIABLES
# dm_recorded_: dry matter as recorded
# tuber_Fe_dw_: iron, mg/kg DW
# tuber_Fe_fw_: iron, mg/100FW as labeled
# tuber_Zn_dw_: zinc, mg/kg DW
# tuber_Zn_fw_: zinc, mg/100FW as labeled
# eval_label_: locality and year text

carob_script <- function(path) {

"
Dataset for: Standard Evaluation Trial for micronutrient content

Iron (Fe), zinc (Zn) and vitamins A and C are among the most widespread nutrient deficiencies in developing countries. Potato genotypes display from moderate to large ranges of mineral content. A survey of advanced breeding clones and cultivars showed broad range of iron contents extending from 18 to 65 μg/g DW, while less overall variation of Zinc contents was true, ranging from 12.5 to 20 μg/g DW. Some evidence of consistency of genotype effects across environments suggest that significant gains for iron and zinc can be expected through breeding, particularly for iron if coupled with a high content of vitamin C considering the close association between iron availability and vitamin C. Concentration of vitamin C in potatoes is moderate to low compared with fruits and other vegetables, yet as a staple food, potatoes may be one of the highest contributors of vitamin C in the diets of people in many places around the world. Inheritance study using low and high concentration parents suggested that vitamin C concentration in tubers is highly heritable and hence increased levels could rapidly be achieved through selection of high concentration parents  Standard evaluation trials assessed iron, zinc and vitamin C contents in tuber samples from advanced breeding clones to document their nutritional values. Healthy and representative tubers with regard to tuber size of each clone  are picked up  from plots of two replications of 10 hill plots from trials conducted in pathogen and pest protected fields and grown following local procedures. Iron and Zinc content are determined by inductively coupled plasma–optical emission spectrophotometry (ICP) and concentrations expressed in mg/kg, dry weight (DW), while vitamin C is quantified by the spectrophotometric method and expressed in mg/100 g, fresh weight (FW).
"

	uri <- "doi:10.21223/P3/YLQUNF"
	group <- "varieties_potato"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=3,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "tuber_Fe_dw_;tuber_Zn_dw_",
		notes = NA,
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 50,
		carob_effort = 0.4
	)

	f <- ff[basename(ff) == "Standard Evaluation Trial for micronutrient content Data.xls"]
	one <- function(sheet) {
		x <- as.data.frame(carobiner::read.excel(f, sheet=sheet, col_names=FALSE), stringsAsFactors=FALSE)
		y <- x[3:nrow(x), ]
		num <- function(v) {
			y <- trimws(as.character(v))
			y[y %in% c("", "NA", ".", "-")] <- NA_character_
			as.numeric(y)
		}
		lab <- trimws(as.character(y[[11]]))
		data.frame(
			trial_id = trimws(as.character(y[[1]])),
			variety = trimws(as.character(y[[2]])),
			dm_recorded_ = num(y[[4]]),
			dw = num(y[[6]]),
			fw = num(y[[8]]),
			eval_label_ = lab,
			stringsAsFactors = FALSE
		)
	}
	fe <- one("Fe CONT.")
	zn <- one("Zn CONT.")
	names(fe)[names(fe) == "dw"] <- "tuber_Fe_dw_"
	names(fe)[names(fe) == "fw"] <- "tuber_Fe_fw_"
	names(zn)[names(zn) == "dw"] <- "tuber_Zn_dw_"
	names(zn)[names(zn) == "fw"] <- "tuber_Zn_fw_"
	key <- c("trial_id", "variety", "eval_label_")
	d <- merge(fe, zn[, c(key, "tuber_Zn_dw_", "tuber_Zn_fw_")], by=key, all=TRUE)
	d$crop <- "potato"
	d$treatment <- d$variety
	d$country <- "Peru"
	d$location <- sub("-[0-9]{4}$", "", d$eval_label_)
	huancayo <- grepl("Huancayo", d$location)
	d$latitude <- ifelse(huancayo, -12.0264, NA_real_)
	d$longitude <- ifelse(huancayo, -75.2449, NA_real_)
	d$geo_from_source <- huancayo
	d$geo_source <- ifelse(huancayo, "Dataverse bounding box for Huancayo", NA_character_)
	d$on_farm <- FALSE
	d$is_survey <- FALSE
	d$irrigated <- NA
	d$plot_id <- paste(d$trial_id, d$variety, d$eval_label_, sep="_")
	d <- d[!is.na(d$variety) & nzchar(d$variety), ]

	carobiner::write_files(path, meta, d)
}
