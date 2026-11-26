# R script for "carob"
# license: GPL (>=3)

## NOTES
# Rows are the Boiling sheet, separated into the main clone block (rows 1-319) and the shifted control-variety block (rows 322-336).
# Cooking time is reported in minute bins ("10-15", "16-20", "21-25", "26-30", ">30") and kept as character in cooking_time_.
# Flavor AVG and texture AVG are kept under tuber_flavor and tuber_texture (1-5 scale; "Pend" and "-" treated as NA).
# Country Peru is the dataset geographic coverage. The sheet has no location, so the La Molina and Huancayo boxes are not assigned to rows.

## ISSUES
# No yield, fertilizer, or dates. The metadata spans 2000-2016 and is not one planting date.
# Sites need georeferencing. Two places (La Molina, Huancayo) are named in the metadata and are not linked to rows.

## NEW VARIABLES
# cooking_time_: boiling time category (minutes, e.g., "16-20", ">30")
# cooking_type_: cooking type or quality code (A, AB, B, C, etc.)
# cooking_darkening_: after-cooking darkening score
# flesh_color_cooked_: flesh color after cooking
# cooking_class_: classification text (e.g. slightly soggy, mealy)
# record_type_: "clone" or "control"


## NOT PROCESSED
# Standard Evaluation Trial for cooking quality Dictionary.xls: documentation or a codebook, not an observation table.

carob_script <- function(path) {

"
Dataset for:  Standard evaluation trial  for cooking quality documentation of CIP advanced breeding clones

The primary objective of evaluating post-harvest traits is obtain information about the potential or aptitude of  advanced clones for diverse end-uses, ranging from fresh consumption to processed products. The interpretation of these evaluations provides important information to guide potato breeding and selection programs, as well as for the recommendation of varieties for specific uses. Therefore, CIP has adopted standard procedure for determining cooking quality for evaluation of advanced clonal selections. Variables such as  cooking time, cooking type/quality, darkening and flesh color were evaluated for each clone. In addition if a taste panel is available taste and texture is are assessed. The clones are classified as: slightly soggy, moderately soggy  and very mealy.
"

	uri <- "doi:10.21223/P3/RM1D5W"
	group <- "varieties_potato"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=3,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "cooking_time_;tuber_flavor;tuber_texture",
		notes = NA,
		carob_contributor = NA,
		carob_date = "202?-??-??",
		carob_LLM = "Gemini 4 Neon",
		carob_completion = 65,
		carob_effort = 0.4
	)

	f <- ff[basename(ff) == "Standard Evaluation Trial for cooking quality Data.xls"]
	r <- carobiner::read.excel(f, sheet="Boiling", col_types="text")
	txt <- function(x) {
		y <- gsub("\u00a0", " ", as.character(x), fixed=TRUE)
		y <- trimws(y)
		y[!nzchar(y) | y %in% c("NA", "Pend", ".", "-")] <- NA_character_
		y
	}
	num <- function(x) {
		y <- txt(x)
		ok <- !is.na(y) & grepl("^-?[0-9]+(\\.[0-9]+)?$", y)
		out <- rep(NA_real_, length(y))
		out[ok] <- as.numeric(y[ok])
		out
	}

	hdr <- which(txt(r[[2]]) == "Control")[1]
	m <- r[seq_len(hdr - 1L), ]
	m <- m[!is.na(txt(m$Clone)), ]
	d1 <- data.frame(
		crop = "potato",
		variety = txt(m$Clone),
		variety_code = txt(m$Clone),
		treatment = txt(m$Clone),
		record_type_ = "clone",
		cooking_time_ = txt(m[["Cooking Time (minutes)"]]),
		cooking_type_ = txt(m[["Cooking Type/Quality"]]),
		cooking_darkening_ = txt(m[["After cooking darkening"]]),
		flesh_color_cooked_ = txt(m[["Flesh Color after cooking"]]),
		tuber_flavor = num(m[["Flavor AVG"]]),
		tuber_texture = num(m[["Texture of tuber after cooking AVG"]]),
		cooking_class_ = txt(m$Classification),
		country = "Peru",
		trial_id = paste0("Set_", txt(m$Set)),
		on_farm = FALSE,
		is_survey = FALSE,
		geo_from_source = FALSE
	)

	# Control block below row hdr has shifted columns:
	# 1=Set, 2=Control, 3=CIP Number, 4=Cultivar Name, 5=Cooking Time,
	# 6=Cooking Type, 7=Darkening, 8=Flesh Color, 9=Taste AVG, 10=Texture AVG
	ctrl <- r[(hdr + 1L):nrow(r), ]
	vname <- txt(ctrl[[4]])
	vcode <- txt(ctrl[[3]])
	keep_c <- !is.na(vname) | !is.na(vcode)
	ctrl <- ctrl[keep_c, ]
	vname <- vname[keep_c]
	vcode <- vcode[keep_c]
	vname[is.na(vname)] <- vcode[is.na(vname)]
	d2 <- data.frame(
		crop = "potato",
		variety = vname,
		variety_code = vcode,
		treatment = vname,
		record_type_ = "control",
		cooking_time_ = txt(ctrl[[5]]),
		cooking_type_ = txt(ctrl[[6]]),
		cooking_darkening_ = txt(ctrl[[7]]),
		flesh_color_cooked_ = txt(ctrl[[8]]),
		tuber_flavor = num(ctrl[[9]]),
		tuber_texture = num(ctrl[[10]]),
		cooking_class_ = NA_character_,
		country = "Peru",
		trial_id = paste0("Set_", txt(ctrl[[1]])),
		on_farm = FALSE,
		is_survey = FALSE,
		geo_from_source = FALSE
	)

	d <- rbind(d1, d2)
	d$plot_id <- as.character(seq_len(nrow(d)))
	rownames(d) <- NULL

	carobiner::write_files(path, meta, d)
}
