# R script for "carob"
# license: GPL (>=3)

## NOTES
# Workbook sheet names are "C Crops" / "B Fields" or, on the .xls copies, "CROP", "CCROP" and "FIELD".
# Codes follow sheet "J Codes" (11 maize, 13 pearl millet, 14 sorghum, 15 finger millet, 21 bean -> common bean, 22 soybean, 23 pigeon pea, 25 cowpea, 27 groundnut, 29 bambara, 33 tanaposi -> amaranth, 34 nkhwani -> pumpkin, 35 okra, 43 sweet potato, 45 cassava, and the other food crops on that sheet). Codes for "other", fallow and pasture are skipped.
# I is variety_type improved. T (Traditional on the form) is landrace.
# Field area is taken from sheet "B Fields" only when exactly one cell is a number plus acre, ha or m2. Acres x 0.404686, m2 / 10000. That area is the field, not the crop's share of the field (shares are on the form and were not applied).
# The free-text fertiliser line under the crop (often farmyard manure in ox-carts) is fertilizer_note_. It is not converted to N/P/K.
# There is no kilogram yield on the form (productivity is a 1-5 score and was not mapped).
# Folder names supply adm2 and the village: Babati (Manyara), Kiteto (Manyara), Kongwa (Dodoma). The dataset abstract places Kongwa in Manyara and Kiteto in Dodoma; the admin regions used here are the actual regions of those districts.
# Ba_3 and Ba_8 in the file names are not mapped to a year.

## ISSUES
# Layout is a form, not a table. Rows that do not have a bare crop code in column 1 are missed. Multi-field workbooks contribute area only when a single area cell is found.
# No coordinates. Georeference villages: Long, Sabilo, Seloto, Shaurimoyo, Matufa, Hallu (Babati); Njoro (Kiteto); Chitego, Laikala, Mlali, Moleti (Kongwa).
# Code 28 "ground bean (nzama)" is left as ground bean (code 29 is the bambara line). Code 34 nkhwani is mapped to pumpkin because it is the pumpkin-leaf vegetable; confirm.
# Fertiliser text and crop-product percentages (home, sold, feed) are not standardised.
# No year and no publication DOI in the files used.
# write_files: missing carob_contributor, geo_from_source, latitude, longitude; unknown fertilizer_note_. Code 28 "ground bean" did not occur in this run.

## NEW VARIABLES
# fertilizer_note_: fertiliser free text from the crop block

## NOT PROCESSED
# A People (household roster and labour days), D Animals, E Manures (beyond the free-text line on C Crops), F Imports, G Tools, H Buildings, I Monthly, J Codes (used only as the crop-code list above).
# B Fields columns other than the single area cell: rent, productivity score, intercrop percentages.
# C Crops labour days, cultivation costs, product-use percentages and prices.


carob_script <- function(path) {

"Rapid Characterization of Farming Systems in Africa RISING- Tanzania

This data study contains household and community survey data in support of Africa RISING farming systems analysis.

About the project   Project title: Africa RISING- Sustainable Intensification of Maize-Legume-Livestock Integrated Farming Systems in East and Southern Africa   Project abstract   Sustainable intensification of mixed crop livestock systems is a key pathway towards better food security, improved livelihoods and a healthy environment. As part of the US government's Feed the Future initiative to address hunger and food security issues in sub-Saharan Africa, the US Agency for International Development (USAID) is supporting three multi-stakeholder agricultural research projects to sustainably intensify key African farming systems. In East and Southern Africa the project is being implemented in Tanzania and Malawi, and Zambia. In Tanzania the project is being implemented in Babati and Kongwa districts in the Manyara region of northern Tanzania and Kiteto district in Dodoma region, central Tanzania. The action sites were selected to acknowledge agroecological differences, allow appropriate targeting of technologies and strategies, and complement the development efforts of another USAID-supported program, the Tanzania Staples Value Chain (NAFAKA) project. In Malawi, the project is being implemented in Ntechu and Dedza districts in central Malawi where maize-based productions systems are dominant. Agroecological considerations guided the identification of research action sites. The pilot site for the study will be Eastern and Lusaka Provinces in Zambia.  Project website:  http://africa-rising.net   Project start date: 01/04/2013    Project end date : 01/04/2014
"

	uri <- "doi:10.7910/dvn/2bsyup"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "WUR; IITA",
		publication = NA,
		project = "Africa RISING",
		design = NA,
		data_type = "survey",
		treatment_vars = NA,
		response_vars = NA,
		notes = NA,
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 30,
		carob_effort = 0.8
	)

	code_crop <- c(
		"11"="maize", "12"="wheat", "13"="pearl millet", "14"="sorghum", "15"="finger millet", "16"="rice",
		"21"="common bean", "22"="soybean", "23"="pigeon pea", "24"="chickpea", "25"="cowpea", "26"="pea",
		"27"="groundnut", "28"="ground bean", "29"="bambara groundnut",
		"31"="cabbage", "32"="tomato", "33"="amaranth", "34"="pumpkin", "35"="okra",
		"41"="onion", "42"="potato", "43"="sweetpotato", "44"="garlic", "45"="cassava",
		"51"="avocado", "52"="banana", "53"="mango", "54"="orange", "55"="papaya", "56"="coconut",
		"57"="oil palm", "58"="sugarcane", "61"="cotton"
	)

	part_of <- function(crop) {
		if (crop %in% c("maize", "wheat", "pearl millet", "sorghum", "finger millet", "rice")) return("grain")
		if (crop %in% c("common bean", "soybean", "pigeon pea", "chickpea", "cowpea", "pea", "groundnut", "bambara groundnut", "coffee")) return("seed")
		if (crop %in% c("cassava", "sweetpotato")) return("roots")
		if (crop %in% c("potato", "yam", "garlic")) return("tubers")
		if (crop %in% c("banana", "tomato", "okra", "mango", "avocado", "orange", "papaya", "pumpkin")) return("fruit")
		if (crop %in% c("cabbage", "amaranth", "onion")) return("leaves")
		if (crop == "cotton") return("lint")
		if (crop == "sugarcane") return("stems")
		if (crop == "oil palm") return("fruit")
		NA_character_
	}

	files <- ff[grepl("\\.xls[x]?$", basename(ff), ignore.case=TRUE)]
	rows <- list()
	for (f in files) {
		sheets <- readxl::excel_sheets(f)
		crop_sheet <- sheets[grepl("crop", sheets, ignore.case=TRUE)]
		field_sheet <- sheets[grepl("field", sheets, ignore.case=TRUE)]
		if (!length(crop_sheet) || !length(field_sheet)) next
		p <- gsub("\\\\", "/", f)
		adm2 <- NA_character_
		adm1 <- NA_character_
		if (grepl("/Babati/", p)) {
			adm2 <- "Babati"
			adm1 <- "Manyara"
		} else if (grepl("Kiteto", p, ignore.case=TRUE)) {
			adm2 <- "Kiteto"
			adm1 <- "Manyara"
		} else if (grepl("Kongwa", p, ignore.case=TRUE)) {
			adm2 <- "Kongwa"
			adm1 <- "Dodoma"
		}
		folder <- basename(dirname(f))
		village <- sub("^Village[[:space:]]+[0-9]+[[:space:]]+", "", folder)
		village <- sub("^[0-9]+\\.[[:space:]]*[^-]+-[[:space:]]*", "", village)

		crops <- carobiner::read.excel(f, sheet=crop_sheet[1], col_names=FALSE)
		fields <- carobiner::read.excel(f, sheet=field_sheet[1], col_names=FALSE)
		cells <- trimws(as.character(unlist(fields)))
		cells <- cells[!is.na(cells)]
		area_cell <- cells[grepl("^[0-9.]+[[:space:]]*(acre|acres|ha|hectare|hectares|m2)$", cells, ignore.case=TRUE)]
		field_size <- NA_real_
		if (length(area_cell) == 1) {
			num <- as.numeric(sub("[[:alpha:]].*$", "", area_cell))
			if (grepl("acre", area_cell, ignore.case=TRUE)) field_size <- num * 0.404686
			else if (grepl("m2", area_cell, ignore.case=TRUE)) field_size <- num / 10000
			else field_size <- num
		}

		c1 <- trimws(as.character(crops[[1]]))
		c2 <- if (ncol(crops) >= 2) trimws(as.character(crops[[2]])) else rep(NA_character_, nrow(crops))
		c3 <- if (ncol(crops) >= 3) trimws(as.character(crops[[3]])) else rep(NA_character_, nrow(crops))
		is_code <- grepl("^[0-9]+$", c1) & c1 %in% names(code_crop)
		if (!any(is_code)) next
		idx <- which(is_code)
		note <- rep(NA_character_, length(idx))
		for (k in seq_along(idx)) {
			end <- if (k < length(idx)) idx[k + 1] - 1 else min(idx[k] + 6, nrow(crops))
			block <- c1[(idx[k] + 1):end]
			block <- block[!is.na(block) & nzchar(block)]
			block <- block[!grepl("^Fertilizers used", block, ignore.case=TRUE)]
			block <- block[!grepl("^[0-9]+$", block)]
			if (length(block)) note[k] <- block[1]
		}
		vt <- rep(NA_character_, length(idx))
		vt[toupper(c3[idx]) == "I"] <- "improved"
		vt[toupper(c3[idx]) == "T"] <- "landrace"
		variety <- c2[idx]
		variety[variety %in% c("", "NA", "Cultivar")] <- NA
		crop <- unname(code_crop[c1[idx]])
		rows[[length(rows) + 1]] <- data.frame(
			hhid = paste(adm2, village, tools::file_path_sans_ext(basename(f)), sep="_"),
			country = "Tanzania",
			adm1 = adm1,
			adm2 = adm2,
			location = carobiner::fix_name(village, "title"),
			crop = crop,
			variety = variety,
			variety_type = vt,
			field_size = field_size,
			fertilizer_note_ = note,
			yield_part = vapply(crop, part_of, character(1))
		)
	}

	d <- do.call(rbind, rows)
	d$on_farm <- TRUE
	d$is_survey <- TRUE
	d$trial_id <- d$hhid

	carobiner::write_files(path, meta, d)
}
