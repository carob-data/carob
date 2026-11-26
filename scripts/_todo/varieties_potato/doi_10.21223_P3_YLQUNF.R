# R script for "carob"
# license: GPL (>=3)

## NOTES
# Fe CONT. and Zn CONT. use two header rows plus a repeated control-block header row (Control / CIP Number) near the bottom; those header rows are filtered out.
# Dry matter (%) is stored as yield_moisture = 100 - DM. Iron and zinc are stored in mg/kg DW and mg/100g FW.
# Locality and year are one text field (e.g. Huancayo-2005, La Victoria-2005). La Victoria is the CIP experimental station in Huancayo, Junin, Peru (-12.0264, -75.2449).
# Vitamin C is described in the abstract and is not a column in this workbook.

## ISSUES
# No yield, fertilizer, irrigation, or calendar planting/harvest date.

## NEW VARIABLES
# tuber_Fe_dw_: iron concentration, mg/kg DW
# tuber_Fe_fw_: iron concentration, mg/100g FW
# tuber_Zn_dw_: zinc concentration, mg/kg DW
# tuber_Zn_fw_: zinc concentration, mg/100g FW
# eval_label_: locality and year text as recorded


## NOT PROCESSED
# Standard Evaluation Trial for micronutrient content Dictionary.xls: documentation or a codebook, not an observation table.

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
		response_vars = "yield_moisture;tuber_Fe;tuber_Zn",
		notes = NA,
		carob_contributor = NA,
		carob_date = "202?-??-??",
		carob_LLM = "Gemini 4 Neon",
		carob_completion = 70,
		carob_effort = 0
	)

	f <- ff[basename(ff) == "Standard Evaluation Trial for micronutrient content Data.xls"]
	one <- function(sheet) {
		r <- as.data.frame(carobiner::read.excel(f, sheet=sheet, col_names=FALSE, col_types="text"))
		y <- r[3:nrow(r), ]
		t1 <- trimws(as.character(y[[1]]))
		v1 <- trimws(as.character(y[[2]]))
		# Drop embedded Control/CIP Number sub-headers and blank rows
		keep <- !is.na(v1) & nzchar(v1) & !(t1 %in% c("Set", "Control")) & !(v1 %in% c("Clone", "CIP Number"))
		y <- y[keep, ]
		num <- function(v) {
			z <- trimws(as.character(v))
			ok <- !is.na(z) & grepl("^-?[0-9]+(\\.[0-9]+)?$", z)
			out <- rep(NA_real_, length(z))
			out[ok] <- as.numeric(z[ok])
			out
		}
		lab <- trimws(as.character(y[[11]]))
		lab[!nzchar(lab)] <- NA_character_
		data.frame(
			trial_id = trimws(as.character(y[[1]])),
			variety = trimws(as.character(y[[2]])),
			yield_moisture = 100 - num(y[[4]]),
			dw = num(y[[6]]),
			fw = num(y[[8]]),
			eval_label_ = lab
		)
	}
	fe <- one("Fe CONT.")
	zn <- one("Zn CONT.")
	fe$dw <- fe$dw / 1000
	zn$dw <- zn$dw / 1000
	names(fe)[names(fe) == "dw"] <- "tuber_Fe"
	names(fe)[names(fe) == "fw"] <- "tuber_Fe_fw_"
	names(zn)[names(zn) == "dw"] <- "tuber_Zn"
	names(zn)[names(zn) == "fw"] <- "tuber_Zn_fw_"
	fe$location <- sub("-[0-9]{4}(-[0-9]{4})?$", "", fe$eval_label_)
	zn$location <- sub("-[0-9]{4}(-[0-9]{4})?$", "", zn$eval_label_)
	fe$year_ <- sub("^.*-([0-9]{4}(-[0-9]{4})?)$", "\\1", fe$eval_label_)
	zn$year_ <- sub("^.*-([0-9]{4}(-[0-9]{4})?)$", "\\1", zn$eval_label_)
	fe$row_seq_ <- ave(seq_len(nrow(fe)), fe$trial_id, fe$variety, fe$year_, FUN=seq_along)
	zn$row_seq_ <- ave(seq_len(nrow(zn)), zn$trial_id, zn$variety, zn$year_, FUN=seq_along)
	key <- c("trial_id", "variety", "year_", "row_seq_")
	d <- merge(fe, zn[, c(key, "tuber_Zn", "tuber_Zn_fw_")], by=key, all=TRUE)
	d$row_seq_ <- NULL
	d$year_ <- NULL
	d$crop <- "potato"
	d$treatment <- d$variety
	d$country <- "Peru"
	d$adm1 <- "Junín"
	d$adm2 <- "Huancayo"
	d$location[is.na(d$location) | !nzchar(d$location)] <- "Huancayo"
	# La Victoria is CIP's Huancayo station (-12.0264, -75.2449)
	d$latitude <- -12.0264
	d$longitude <- -75.2449
	d$geo_from_source <- grepl("Huancayo", d$location)
	d$geo_source <- ifelse(d$geo_from_source, "Dataverse bounding box for Huancayo", "CIP Huancayo (La Victoria)")
	d$on_farm <- FALSE
	d$is_survey <- FALSE
	d$record_id <- seq_len(nrow(d))
	d$plot_id <- as.character(d$record_id)

	carobiner::write_files(path, meta, d)
}
