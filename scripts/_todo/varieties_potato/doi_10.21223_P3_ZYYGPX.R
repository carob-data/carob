# R script for "carob"
# license: GPL (>=3)

## NOTES
# Standard evaluation trials of CIP potato clones for late blight at three Peruvian highland sites: Oxapampa (Pasco), Comas (Junín), and Monobamba (Junín). Coordinates added from gazetteer (geo_from_source = FALSE).
# Dictionary trait LB1 to LB7 is percent foliage affected. AUDPC in percent-days is divided by 100 to match the terminag fraction-days standard (0-150).
# sAUDPC in this sheet is the predicted susceptibility scale (1 to 9), not the 0-1 fraction term; stored as late_blight_saudpc_.
# OXA-1, OXA-2, OXA-3, COM-1, COM-2, and MONO-1 are the site-trial AUDPC columns. Sequential readings (1st, 2nd, ...) are stored in the long table with order = 1, 2, ...

## ISSUES
# No fertilizer rates, irrigation, yield, or calendar planting/harvest dates (year only).
# Rows whose clone cell is a dash are blank labels and are not records. Set is the replicate when it is an integer.

## NEW VARIABLES
# late_blight_saudpc_: susceptibility scale printed as sAUDPC (1 to 9 standard-cultivar scale).
# primary_trait_: the PT code (for example TPS or LB).

## NOT PROCESSED
# Reaction columns and ENV 1 / ENV 2 averages. They summarize the same site trials.
# The YEAR columns beside each AUDPC are kept as planting_date only when the cell is a four-digit year. Otherwise the year is not a date.
# Standard Evaluation Trial for Late Blight Resistance_Dictionary.xls: documentation or a codebook, not an observation table.


carob_script <- function(path) {

"
Dataset for: Standard evaluation trial for late blight  resistance documentation of CIP advanced breeding clones

One of the most notorious and economically devastating  disease of potatoes is late blight caused by Phytophthora infestans (Pi). Pi  is a hemibiotroph that can affect most parts of the potato plant, leaves  stems flowers fruits stolons and tubers.  Late blight is a polycyclic disease, having several cycles of infection and inoculum production during one growing season. Infection may increase proportionally to both the initial amount of inoculum and the amount of new inoculum produced during the growing season. The disease appears as water-soaked irregular pale green lesions mostly near tips and margins of leaves that rapidly grow into large brown to purplish black necrotic spots. When stems are attacked light and dark brown lesions appear  that elongate and stems become brittle at lesion areas and break.  Infected tuber show irregular reddish-brown to purplish slightly depressed areas that extend deep into internal tissue of the tubers.  Standard evaluation trial (SET)  for assessment late blight resistance in  advanced clones are conducted  in experimental trials under field conditions in endemic locations of the subtropical highlands. The percentage of leaf area affected by late blight infection is recorded by plot throughout the season to subsequently compute the area under the disease progress curve (AUDPC). Since AUDPC is subject to variation from factors that are experiment specific it is not appropriate for use as a measure to determine the level of resistance of a genotype across trials. However, AUDPC values are used to compute the predicted susceptibility scale value of any candidate genotype within a scale where increasing numbers represent increasing susceptibility. The predicted susceptibility scale value of any candidate genotype is calculated as the scale value of the standard cultivar (1 =for the most resistance standard cultivar -9 = for the most susceptible one) multiplied by the observed disease measure (AUDPC)  of the candidate genotype divided by the disease measure (AUDPC) of the standard genotype.  This predicted susceptibility scale (sAUDPC) is a  disease measure used by SET for  classifying potato genotypes for susceptibility to P. infestans with the advantage that its calculation  is not affected by the  inoculum, location, season,  and host susceptibility variation. Thus  sAUDPC allows to compare the level of resistance of a genotype under different environments and determine its overall performance
"

	uri <- "doi:10.21223/P3/ZYYGPX"
	group <- "varieties_potato"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=3,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "AUDPC",
		notes = NA,
		carob_contributor = NA,
		carob_date = "202?-??-??",
		carob_LLM = "Gemini 4 Neon",
		carob_completion = 65,
		carob_effort = 0.55
	)

	f <- ff[basename(ff) == "Standard Evaluation Trial for Late Blight Resistance_Data.xls"]
	r <- as.data.frame(carobiner::read.excel(f, sheet="Late Blight", col_names=FALSE))
	lab1 <- trimws(as.character(unlist(r[1, ])))
	lab3 <- trimws(as.character(unlist(r[3, ])))
	lab1[lab1 == "NA" | !nzchar(lab1)] <- NA_character_
	lab3[lab3 == "NA" | !nzchar(lab3)] <- NA_character_
	for (i in seq_along(lab1)[-1]) if (is.na(lab1[i])) lab1[i] <- lab1[i - 1]

	num <- function(x) {
		z <- trimws(as.character(x))
		z[!nzchar(z) | z == "NA" | grepl("^<", z)] <- NA_character_
		ok <- is.na(z) | grepl("^-?[0-9]+(\\.[0-9]+)?$", z)
		z[!ok] <- NA_character_
		as.numeric(z)
	}

	site_of <- function(block) {
		if (grepl("^OXA", block)) return("Oxapampa")
		if (grepl("^COM", block)) return("Comas")
		if (grepl("^MONO", block)) return("Monobamba")
		NA_character_
	}
	trial_of <- function(block) as.integer(sub(".*-", "", block))

	body <- r[5:nrow(r), ]
	clone <- trimws(as.character(body[[2]]))
	# A dash is a blank label, not a clone.
	keep <- !is.na(clone) & nzchar(clone) & !clone %in% c("NA", "-")
	body <- body[keep, ]
	clone <- clone[keep]
	setn <- trimws(as.character(body[[1]]))
	repn <- rep(NA_integer_, length(setn))
	okrep <- grepl("^[0-9]+$", setn)
	repn[okrep] <- as.integer(setn[okrep])
	row_i <- seq_along(clone)
	pt <- trimws(as.character(body[[3]]))
	pt[!nzchar(pt) | pt == "NA"] <- NA_character_

	sum_cols <- which(lab3 == "AUDPC" & grepl("^(OXA|COM|MONO)-", lab1))
	parts <- lapply(sum_cols, function(j) {
		block <- lab1[j]
		year <- trimws(as.character(body[[j - 1]]))
		data.frame(
			variety = clone,
			rep = repn,
			row_i = row_i,
			primary_trait_ = pt,
			location = site_of(block),
			trial_number = trial_of(block),
			planting_date = ifelse(grepl("^[0-9]{4}$", year), year, NA_character_),
			AUDPC = num(body[[j]]) / 100,
			late_blight_saudpc_ = num(body[[j + 1]])
		)
	})
	d <- do.call(rbind, parts)
	d <- d[!is.na(d$AUDPC) | !is.na(d$late_blight_saudpc_), ]
	geo <- data.frame(
		location = c("Oxapampa", "Comas", "Monobamba"),
		adm1 = c("Pasco", "Junín", "Junín"),
		latitude = c(-10.5775, -11.747, -11.356),
		longitude = c(-75.4017, -75.131, -75.344)
	)
	gm <- match(d$location, geo$location)
	d$country <- "Peru"
	d$adm1 <- geo$adm1[gm]
	d$latitude <- geo$latitude[gm]
	d$longitude <- geo$longitude[gm]
	d$crop <- "potato"
	d$treatment <- d$variety
	d$trial_id <- paste(d$location, d$trial_number, sep="_")
	d$record_id <- seq_len(nrow(d))
	d$trial_number <- NULL
	d$pathogen <- "Phytophthora infestans"
	d$stress <- "disease"
	d$geo_from_source <- FALSE
	d$on_farm <- FALSE
	d$is_survey <- FALSE
	link <- d[, c("trial_id", "row_i", "record_id")]
	d$row_i <- NULL

	read_cols <- which(grepl("^[0-9]+(st|nd|rd|th|rh)$", lab3) & grepl("Trial$", lab1))
	loc_trial <- c(
		"OXAPAMPA First Trial" = "Oxapampa_1",
		"OXAPAMPA Second Trial" = "Oxapampa_2",
		"OXAPAMPA Third Trial" = "Oxapampa_3",
		"COMAS First Trial" = "Comas_1",
		"COMAS Second Trial" = "Comas_2",
		"MONOBAMBA First Trial" = "Monobamba_1"
	)
	longs <- lapply(read_cols, function(j) {
		tid <- loc_trial[[lab1[j]]]
		if (is.null(tid) || is.na(tid)) return(NULL)
		sev <- trimws(as.character(body[[j]]))
		sev[!nzchar(sev) | sev == "NA"] <- NA_character_
		data.frame(
			trial_id = tid,
			row_i = row_i,
			order = as.integer(sub("[^0-9].*$", "", lab3[j])),
			disease_severity = sev,
			disease = "potato late blight",
			severity_scale = "0-100"
		)
	})
	dlong <- do.call(rbind, longs)
	dlong <- merge(dlong, link, by=c("trial_id", "row_i"), all.x=FALSE)
	dlong <- dlong[!is.na(dlong$disease_severity), c("record_id", "order", "disease_severity", "disease", "severity_scale")]
	dlong$record_id <- as.integer(dlong$record_id)
	dlong$order <- as.integer(dlong$order)
	rownames(d) <- NULL
	rownames(dlong) <- NULL

	carobiner::write_files(path, meta, d, long=dlong)
}
