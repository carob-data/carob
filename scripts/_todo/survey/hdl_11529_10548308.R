# R script for "carob"
# license: GPL (>=3)

## NOTES
# TAMASA Tanzania agronomy panel: 2015 baseline yield survey, 2016 crop cuts, and 2017 crop cuts.
# The 12 March 2018 maize sheet and the 11 July 2017 sheet are used. The uncorrected 2017 raw sheet stores longitude-range numbers in the Latitude column, so that sheet is not used.
# On the 2017 sheets, region, district, and ward are codes such as N03 and N0302. Those codes are not administrative names and are not stored. Some blurred coordinates fall in Kenya and are left as recorded. EC.S is stored as published. The values are far above a typical dS/m range.
# Grain yield columns marked @12.5% are kg/ha at 12.5% moisture. yield_isfresh is FALSE.
# Plant stands are counts per 25 m2 quadrat, from the variables sheet, so plant_density is stands times 400.
# Area by GPS is in acres.
# Soil C is total carbon in percent. Soil N is total nitrogen in percent and is stored as mg/kg. Mehlich 3 nutrients are mg/kg. EC.S is dS/m, equal to mS/cm.
# 2017 biomass dry weight is joined on HHID and QID. The PP workbook is the stalk biomass file released with these quadrats.

## ISSUES
# write_files: out of bounds: soil_EC (12.99, 320.567), soil_Ca (14.268, 29959.1) coordinates in wrong country: Tanzania/Kenya, Tanzania/Kenya duplicate coordinates: fewer coordinates than locations duplicates: duplicate records detected NA detected: yield, yield_part, latitude, longitude, sample_id, hhid contributor:

## NOT PROCESSED
# Community survey and the 2016 household roster. They do not contain crop cuts.
# Farmer names.
# Uncorrected 2016 and 2017 crop-cut workbooks, the 2017 maize metadata v2 extract (same maximum yield and row count as the 2016 crop cut), and the first PP biomass workbook.
# Farmer-estimated area, cob subsample weights, shelling factor, and the raw moisture column. Yield is already adjusted to 12.5%, and some raw moisture values are above 100.
# Zone.
# TAMASA_TZ_APS_2017_CC_MaizeYield (Blurred).xlsx and TAMASA_TZ_APS_CC_2016 (Blurred).xlsx: uncorrected crop-cut workbooks. The corrected metadata sheets are the ones used.
# TAMASA_TZ_APS_CS_2016 (Blurred).xlsx: community survey. It has no crop cuts.
# TAMASA_TZ_APS_HH_2016 (Blurred).xlsx: household roster. It has no crop cuts.
# TZ_TAMASA_APS_2017_Maize_Yield_MetaData_v2 (Blurred).xlsx: same maximum yield and row count as the 2016 crop cut, so it is not a separate table.
# TZ_TAMASA_APS_2017_PP_Biomass_Yield_MetaData  (Blurred).xlsx: first biomass workbook. The V2 workbook is the one joined to the 2017 quadrats.


carob_script <- function(path) {

"
Tamasa APS Tanzania 2017

Tamasa Agronomy Panel Survey Tanzania 2017
"

	uri <- "hdl:11529/10548308"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=3, minor=0,
		data_organization = "CIMMYT",
		publication = NA,
		project = "TAMASA",
		design = NA,
		data_type = "survey",
		treatment_vars = NA,
		response_vars = "yield",
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 50,
		carob_effort = 0.8
	)

	f1 <- ff[basename(ff) == "TZ_TAMASA_APS_2016_Yield_MetaData_11July17 (Blurred).xlsx"]
	f2 <- ff[basename(ff) == "TZ_TAMASA_APS_2017_Yield_MetaData_12-03-18  (Blurred).xlsx"]
	f3 <- ff[basename(ff) == "TZ_TAMASA_BYS_Yield_2015_22June17 (Blurred).xlsx"]
	f4 <- ff[basename(ff) == "TZ_TAMASA_APS_2017_PP_Biomass_Yield_MetaData_V2 (Blurred).xlsx"]
	f5 <- ff[basename(ff) == "TAMASA_TZ_APS_Soil_2016 (Blurred).xlsx"]
	f6 <- ff[basename(ff) == "TAMASA_BYS_Soil_Data_JUN7 (Blurred).xlsx"]
	num <- function(x) {
		if (is.null(x)) return(NA_real_)
		if (is.numeric(x)) return(x)
		x <- trimws(as.character(x))
		x[x %in% c("", "NA", "n/a", "N/A")] <- NA_character_
		x[grepl("[[:alpha:]]", x)] <- NA_character_
		x[!is.na(x) & grepl("-", x) & !grepl("^-", x)] <- NA_character_
		as.numeric(x)
	}
	place <- function(x) {
		x <- trimws(as.character(x))
		x[!nzchar(x) | tolower(x) %in% c("na", "n/a")] <- NA_character_
		x[!is.na(x) & grepl("^[A-Za-z][0-9]+$", x)] <- NA_character_
		y <- carobiner::fix_name(x, "title")
		up <- !is.na(y) & nzchar(y) & y == toupper(y) & grepl("[[:alpha:]]", y)
		if (any(up)) {
			z <- tolower(y[up])
			y[up] <- gsub("(^|[^[:alpha:]])([[:alpha:]])", "\\1\\U\\2", z, perl=TRUE)
		}
		y[!is.na(y) & nchar(y) < 2] <- NA_character_
		y
	}
	read1 <- function(f, sheet) {
		r <- carobiner::read.excel(f, sheet=sheet, na=c("", "NA", "n/a", "N/A"), col_types="text")
		names(r) <- trimws(names(r))
		r
	}
	depth_pair <- function(x) {
		z <- gsub("–", "-", as.character(x))
		z <- gsub(" ", "", z)
		p <- strsplit(z, "-", fixed=TRUE)
		top <- rep(NA_real_, length(z))
		bot <- rep(NA_real_, length(z))
		ok <- lengths(p) >= 2
		if (any(ok)) {
			top[ok] <- as.numeric(vapply(p[ok], function(i) i[1], ""))
			bot[ok] <- as.numeric(vapply(p[ok], function(i) i[2], ""))
		}
		list(top=top, bot=bot)
	}

	cuts <- function(r, round_id, id_name, q_name, moist) {
		r <- r[!is.na(r$Country) & r$Country == "Tanzania", ]
		hh <- trimws(as.character(r[[id_name]]))
		qid <- trimws(as.character(r[[q_name]]))
		hh[!nzchar(hh)] <- NA_character_
		qid[!nzchar(qid)] <- NA_character_
		yn <- intersect(c("Grain yield (kg/ha@12.5%)", "Grain yield (kg/ha)"), names(r))
		sn <- intersect(c("Plant stands", "Plant Stands", "Plant stand"), names(r))
		stands <- if (length(sn) == 0) rep(NA_real_, nrow(r)) else num(r[[sn[1]]])
		yld <- if (length(yn) == 0) rep(NA_real_, nrow(r)) else num(r[[yn[1]]])
		lat <- num(r$Latitude)
		lon <- num(r$Longitude)
		acres <- num(r[["Area by GPS"]])
		loc <- place(r$Village)
		site <- place(r$Hamlet)
		if (!"Hamlet" %in% names(r)) site <- rep(NA_character_, nrow(r))
		no_loc <- is.na(loc) & !is.na(site)
		loc[no_loc] <- site[no_loc]
		site[no_loc] <- NA_character_
		data.frame(
			country = "Tanzania",
			adm1 = place(r$Region),
			adm2 = place(r$District),
			adm3 = place(r$Ward),
			location = loc,
			site = site,
			hhid = hh,
			trial_id = paste(round_id, hh, qid),
			plot_id = qid,
			crop = "maize",
			yield = yld,
			yield_part = "grain",
			yield_moisture = moist,
			yield_isfresh = FALSE,
			plant_density = stands * 400,
			field_size = acres * 0.4046863,
			elevation = num(r$Altitude),
			latitude = lat,
			longitude = lon,
			geo_from_source = !is.na(lat) & !is.na(lon),
			on_farm = TRUE,
			is_survey = TRUE
		)
	}

	r1 <- read1(f1, "Corrected-Raw-Data")
	r2 <- read1(f2, "Corrected-Raw-Data")
	r3 <- read1(f3, "Yield Data")
	d16 <- cuts(r1, "APS2016", "HHID", "QID", 12.5)
	d17 <- cuts(r2, "APS2017", "HHID", "QID", 12.5)
	db <- cuts(r3, "BYS2015", "Farm_ID", "Quadrant", NA)

	r4 <- read1(f4, "Corrected-Raw-Data")
	r4 <- r4[!is.na(r4$Country) & r4$Country == "Tanzania", ]
	bkey <- paste(trimws(as.character(r4$HHID)), trimws(as.character(r4$QID)))
	gkey <- paste(d17$hhid, d17$plot_id)
	ib <- match(gkey, bkey)
	d17$dmy_residue <- num(r4[["Biomass dry weight (kg/ha)"]])[ib]
	miss <- !bkey %in% gkey
	if (any(miss)) {
		extra <- cuts(r4[miss, ], "APS2017bio", "HHID", "QID", NA)
		extra$yield <- NA_real_
		extra$yield_moisture <- NA_real_
		extra$dmy_residue <- num(r4[["Biomass dry weight (kg/ha)"]])[miss]
		d17 <- carobiner::bindr(d17, extra)
	}

	soils <- function(r, round_id, id_name) {
		r <- r[!is.na(r$Country) & r$Country == "Tanzania", ]
		dp <- depth_pair(r$Depth)
		id <- trimws(as.character(r[[id_name]]))
		lat <- num(r$Latitude)
		lon <- num(r$Longitude)
		loc <- place(r$Village)
		data.frame(
			country = "Tanzania",
			adm1 = place(r$Region),
			adm2 = place(r$District),
			adm3 = place(r$Ward),
			location = loc,
			hhid = if (id_name == "HHID") id else NA_character_,
			trial_id = paste(round_id, id, r$Depth),
			sample_id = trimws(as.character(r$SSID)),
			crop = "maize",
			depth_top = dp$top,
			depth_bottom = dp$bot,
			soil_C_total = num(r$C),
			soil_N_total = num(r[["N"]]) * 10000,
			soil_pH = num(r$pH),
			soil_EC = num(r[["EC.S"]]),
			soil_P = num(r$P),
			soil_P_method = "Mehlich 3",
			soil_K = num(r$K),
			soil_Ca = num(r$Ca),
			soil_Mg = num(r$Mg),
			soil_Na = num(r$Na),
			soil_S = num(r$S),
			soil_Zn = num(r$Zn),
			soil_Mn = num(r$Mn),
			soil_Fe = num(r$Fe),
			soil_B = num(r$B),
			soil_Al = num(r$Al),
			elevation = num(r$Altitude),
			latitude = lat,
			longitude = lon,
			geo_from_source = !is.na(lat) & !is.na(lon),
			on_farm = TRUE,
			is_survey = TRUE
		)
	}
	r5 <- read1(f5, "Data")
	s16 <- soils(r5, "soil2016", "HHID")
	r6 <- read1(f6, "Revised_Data")
	ss <- soils(r6, "soilBYS", "SSID")
	ss$hhid <- NA_character_

	d <- carobiner::bindr(d16, d17, db, s16, ss)
	d$geo_from_source[is.na(d$latitude) | is.na(d$longitude)] <- FALSE
	carobiner::write_files(path, meta, d)
}
