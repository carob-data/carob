# R script for "carob"
# license: GPL (>=3)

## NOTES
# The tab-separated fieldbooks are the data tables. The Excel Minerals sheet is not used.
# Mineral and vitamin C columns are tuber concentrations, not fertilizer and not soil. Dry-weight minerals are mg/kg. Fresh-weight minerals and ascorbic acid are mg/100 g. Dry matter in the mineral file is percent. yield_moisture is 100 minus that value. The vitamin-file dry matter does not match it, so it stays as tuber_dry_matter_vitamin_.
# Accession name is B3C1 for every row, so the variety is the CIP number. B3C1 is stored as population_.
# Country, department, province, district, and locality are columns in the fieldbook. The Minimal sheet has the same place names, begin date 2003-12-04, and end date 2004-05-04, and blank latitude and longitude. The Dataverse bounding box publishes only south -11.8336 and west -75.4343. Those two values are used as the point. The installation design cell is blank. The description says a completely randomized design with three replications. The installation repetition count is 3. Plot size and spacing are blank. Crop management lists fertilizing, weed, pest, disease, and irrigation with no product or dose.

## ISSUES
# No yield in the fieldbooks.
# The coordinate is the bounding-box corner, not a plot GPS point.
# Rows with no CIP number are dropped.
# write_files: missing carob_contributor, yield, yield_part, irrigated, K_fertilizer, N_fertilizer, P_fertilizer. unknown variables: population_, tuber_Fe_dw_, tuber_Fe_fw_, tuber_Mn_dw_, tuber_Mn_fw_, tuber_B_dw_, tuber_B_fw_, tuber_Cu_dw_, tuber_Cu_fw_, tuber_Zn_dw_, tuber_Zn_fw_, tuber_Ca_dw_, tuber_Ca_fw_, tuber_Mg_dw_, tuber_Mg_fw_, tuber_K_dw_, tuber_K_fw_, tuber_P_dw_, tuber_P_fw_, tuber_S_dw_, tuber_S_fw_, tuber_Na_dw_, tuber_Na_fw_, tuber_ascorbate_dw_, tuber_ascorbate_fw_, tuber_dry_matter_vitamin_.

## NEW VARIABLES
# population_: accession or population name when it is not the clone.
# tuber_dry_matter_vitamin_: dry matter from the vitamin C file, percent.
# tuber_<element>_dw_: tuber mineral, mg/kg dry weight.
# tuber_<element>_fw_: tuber mineral, mg/100 g fresh weight.
# tuber_ascorbate_dw_, tuber_ascorbate_fw_: ascorbic acid, mg/100 g.

## NOT PROCESSED
# 04_Datadictionary.txt and the Excel summary and notes sheets.
# Crop_management: categories without a product or dose.
# Breeder code is "-" in this file.


carob_script <- function(path) {

"
Dataset for:Analysis of Minerals and Vitamin C in Varieties of the B3C1 Population in Muqui, Junin

Laboratory assessments of minerals and vitamin C were carried out on raw samples with peel, based on dry weight, for the B3C1 population group planted in the Muqui locality in Jauja, Junin. The analysis methodology involved the use of chemical spectrophotometry. Eleven clones from the B3C1 population were analyzed with three replications following a Completely Randomized Design (CRD) during the year 2004.
"

	uri <- "doi:10.21223/7S836T"
	group <- "varieties_potato"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = "CRD",
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "tuber_Fe_dw_;tuber_Zn_dw_;tuber_ascorbate_dw_",
		notes = NA,
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 65,
		carob_effort = 0.4
	)

	read_txt <- function(f) {
		raw <- readBin(f, "raw", file.info(f)$size)
		if (length(raw) && raw[length(raw)] != as.raw(10L)) raw <- c(raw, as.raw(10L))
		txt <- iconv(rawToChar(raw), from="Latin1", to="UTF-8")
		x <- read.delim(text=txt, check.names=FALSE,
			sep="\t", quote="", comment.char="", na.strings=c("", "NA", "-"))
		names(x) <- gsub("\\s+", " ", trimws(names(x)))
		x
	}
	col_one <- function(x, pattern) {
		hit <- grep(pattern, names(x))
		if (length(hit) != 1) return(rep(NA_real_, nrow(x)))
		z <- trimws(as.character(x[[hit]]))
		# Below-detection results are stored as "< 0.2" and similar. They are missing concentrations, not zeros.
		z[grepl("^<", z)] <- NA_character_
		as.numeric(z)
	}
	add_minerals <- function(x) {
		elements <- c("Fe","Mn","B","Cu","Zn","Ca","Mg","K","P","S","Na","Al","Ti","Cr","Se","Pb","Cd","Mo","Co","Ni")
		out <- list()
		for (el in elements) {
			dw <- paste0("^", el, " \\(mg/kg, ?DW\\)?$")
			fw <- paste0("^", el, " \\(mg/100 ?g, ?FW\\)?$")
			if (length(grep(dw, names(x))) == 1) out[[paste0("tuber_", el, "_dw_")]] <- col_one(x, dw)
			if (length(grep(fw, names(x))) == 1) out[[paste0("tuber_", el, "_fw_")]] <- col_one(x, fw)
		}
		as.data.frame(out)
	}

	minf <- ff[grepl("^01_Fieldbook_TMIN", basename(ff))]
	vitf <- ff[grepl("^02_Fieldbook_TVIT", basename(ff))]
	xls <- ff[grepl("^TMIN.*\\.xls$", basename(ff))]
	m <- read_txt(minf)
	v <- read_txt(vitf)
	mini <- carobiner::read.excel(xls, sheet="Minimal")
	meta_val <- function(nm) {
		hit <- mini$Value[trimws(as.character(mini$Factor)) == nm]
		x <- hit[1]
		if (inherits(x, "POSIXt") || inherits(x, "Date")) return(format(x, "%Y-%m-%d"))
		trimws(as.character(x))
	}

	instn <- trimws(as.character(m$INSTN))
	acc <- trimws(as.character(m[["Accession name"]]))
	variety <- instn
	population <- acc
	if (length(unique(stats::na.omit(acc))) >= 2) {
		variety <- acc
		miss <- is.na(variety) | !nzchar(variety)
		variety[miss] <- instn[miss]
		population <- NA_character_
	}

	country <- trimws(as.character(m$Country))
	country[country %in% c("Perú", "Peru")] <- "Peru"

	d <- data.frame(
		variety_code = instn,
		variety = variety,
		population_ = population,
		treatment = variety,
		rep = as.integer(m$REP),
		country = country,
		adm1 = trimws(as.character(m$Departament)),
		adm2 = trimws(as.character(m$Province)),
		adm3 = trimws(as.character(m$District)),
		location = trimws(as.character(m$Locality)),
		planting_date = meta_val("Begin date"),
		harvest_date = meta_val("End date"),
		trial_id = "Muqui_B3C1_2003",
		yield_moisture = 100 - col_one(m, "^Percentage Dry matter"),
		key = paste(instn, as.integer(m$REP))
	)
	mins <- add_minerals(m)
	d <- cbind(d, mins)

	vinst <- trimws(as.character(v$INSTN))
	vv <- data.frame(
		key = paste(vinst, as.integer(v$REP)),
		tuber_ascorbate_dw_ = col_one(v, "Ascorbic acid.*DW"),
		tuber_ascorbate_fw_ = col_one(v, "Ascorbic acid.*FW"),
		tuber_dry_matter_vitamin_ = col_one(v, "^Percentage Dry matter")
	)
	vv <- vv[!duplicated(vv$key), ]
	d <- merge(d, vv, by="key", all.x=TRUE)
	d$key <- NULL
	d <- d[!is.na(d$variety) & nzchar(d$variety), ]
	if (all(is.na(d$population_))) d$population_ <- NULL
	rownames(d) <- NULL
	d$crop <- "potato"
	d$latitude <- -11.8336
	d$longitude <- -75.4343
	d$geo_from_source <- TRUE
	d$geo_uncertainty <- 2000
	d$geo_source <- "CIP Dataverse bounding box (south and west only)"
	d$on_farm <- NA
	d$is_survey <- FALSE
	d$record_id <- seq_len(nrow(d))

	carobiner::write_files(path, meta, d)
}
