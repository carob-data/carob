# R script for "carob"
# license: GPL (>=3)

## NOTES
# Africa RISING Malawi crop-cut survey, May 2015. Each household has a HIGH and a LOW plot.
# Plot GPS is DMS in PART B (high) and PART I (low); converted to decimal degrees (S negative, E positive).
# PART C LOCATION 1/2/3 are ridge spacing (m), not geographic locations.
# Herbicide this year: 1 = used, 2 = not used (matches presence/absence of product names).
# Maize cob/stover weights are plot samples at three ridge locations, not kg/ha.

## ISSUES
# No crop-code codebook in the files; maize codes 1-4 and legume/other codes 5-19 are unmapped. Human should recover the instrument codes.
# Yield (kg/ha) not computed: harvest length for the three ridge samples is not in the file. Ridge spacing is in PART C/J.
# PART E/G (legume pods), H/P (witchweed/weed ratings and biomass), and stover subsample dry weights not processed.
# RENTED (1/2/3/combinations) and SLOPE (0/1) codes not decoded.
# Plot names are field names, not admin units. No district in the file (Africa RISING Malawi is typically Dedza/Ntcheu); human may add adm from project docs.
# write_files: missing carob_contributor; unknown variables cob_weight_, maize_code_, intercrop_code_; herbicide_product names are trade names not active ingredients; possible duplicate high/low plots when SAME GPS=1.
# One PART I longitude is 0.34°E (Angola/Atlantic); likely a 34°E recording error. Left as reported. HHID 3116 low plot has no GPS.

## NEW VARIABLES
# cob_weight_: sum of maize cob fresh weights (kg) across three sample locations on the plot
# intercrop_code_: raw INTERCROP/SOLE CROP code from PART D/K (1 or 2; codebook missing)
# maize_code_: raw maize crop code from PART C/J


carob_script <- function(path) {

"Malawi: Yield Cuts Survey

This study contains data from a survey about yield cuts in Malawi.

The Malawi project has local theme 'Agro-ecological intensification in Malawi through action research with smallholder farmers' with a lot of emphasis on co-learning with farmers and other stakeholders. The purpose of the Africa RISING Malawi component is to enhance farmer knowledge and support sustainable intensification (SI) pathways for productivity gains in maize-legume diversified systems, that also integrates livestock-related enterprises such as improved fodder for intensified dairy production. The project is setting up a research approach that systematically assesses SI best-bet options that appropriately respond to the needs of resource-poor farmers - particularly female headed households. Building on successful examples of participatory action research and experiences from biophysical research on smallholder farms in Malawi over the past 2 decades, the research team has begun taping into these products of agricultural research to move towards more sustainable smallholder production systems. We envisage that farm-scale production strategies employed by different farm/farmer typologies will be further distilled through scenario analyses using farming systems simulation modeling approaches. The project works with an alliance of actors, (agro-dealers, extension services, NGOs, local government structures, etc) as R4D platforms for the two districts."

	uri <- "doi:10.7910/DVN/R4U8CZ"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=0,
		data_organization = "LUANAR; MSU; IFPRI",
		publication = NA,
		project = "Africa RISING",
		design = NA,
		data_type = "survey",
		treatment_vars = NA,
		response_vars = NA,
		notes = NA,
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.6",
		carob_completion = 30,
		carob_effort = 0.5
	)

	f <- ff[basename(ff) == "yieldcutsdata.xlsx"]

	# Excel serial or dd/mm/yyyy mixed in DATE
	parse_xl_date <- function(x) {
		x <- trimws(as.character(x))
		x[x %in% c("", ".", "NA", "na")] <- NA
		out <- rep(NA_character_, length(x))
		is_serial <- grepl("^[0-9]+(\\.[0-9]+)?$", x)
		out[is_serial] <- as.character(as.Date(as.numeric(x[is_serial]), origin="1899-12-30"))
		is_dmy <- grepl("^[0-9]{1,2}/[0-9]{1,2}/[0-9]{4}$", x)
		out[is_dmy] <- as.character(as.Date(x[is_dmy], format="%d/%m/%Y"))
		out
	}

	dms_to_dec <- function(hem, d, m, s) {
		hem <- toupper(trimws(as.character(hem)))
		# all records are in southern Malawi; missing hemisphere treated as S
		hem[is.na(hem) | hem == ""] <- "S"
		dec <- as.numeric(d) + as.numeric(m) / 60 + as.numeric(s) / 3600
		dec[hem %in% c("S", "W")] <- -dec[hem %in% c("S", "W")]
		dec
	}

	# GPS + herbicide/weeding for one plot sheet (PART B high, PART I low)
	read_gps <- function(sheet) {
		r <- carobiner::read.excel(f, sheet=sheet, skip=5, col_names=FALSE, na=".")
		names(r) <- c("hhid", "plot_name", "lat_hem", "lat_d", "lat_m", "lat_s",
			"lon_hem", "lon_d", "lon_m", "lon_s", "slope", "same_gps",
			"herb_this", "herb_name", "herb_last", "herb_name_last",
			"weedings", "weeding_labor")
		r[!is.na(r$hhid), ]
	}

	read_crops <- function(sheet) {
		r <- carobiner::read.excel(f, sheet=sheet, skip=5, col_names=FALSE, na=".")
		names(r) <- c("hhid", "fallow", "maize_code", "maize_ready", "legume1",
			"legume1_other", "legume1_ready", "legume2", "legume2_ready",
			"legume2_other", "other1", "other1_spec", "other2", "other2_spec",
			"ridge1", "ridge2", "ridge3")
		r[!is.na(r$hhid), ]
	}

	read_maize <- function(sheet) {
		r <- carobiner::read.excel(f, sheet=sheet, skip=5, col_names=FALSE, na=".")
		names(r) <- c("hhid", "maize_in_plot", "crop_code", "intercrop_code",
			"plants1", "plants2", "plants3", "cobs1", "w1", "cobs2", "w2",
			"cobs3", "w3", "rate1", "stover1", "rate2", "stover2", "rate3",
			"stover3", "meter", "stover_field", "stover_dry")
		r[!is.na(r$hhid), ]
	}

	rA <- carobiner::read.excel(f, sheet="PART A", skip=4, na=".")
	rA <- rA[!is.na(rA$HHID), ]
	rB <- read_gps("PART B")
	rC <- read_crops("PART C")
	rD <- read_maize("PART D")
	rI <- read_gps("PART I")
	rJ <- read_crops("PART J")
	rK <- read_maize("PART K")

	# skipped: PART E/G legume harvest; PART H/P witchweed and weed biomass

	make_plot <- function(gps, crops, maize, field) {
		d <- data.frame(
			hhid = as.character(gps$hhid),
			field_id = field,
			location = carobiner::fix_name(as.character(gps$plot_name), "title"),
			latitude = dms_to_dec(gps$lat_hem, gps$lat_d, gps$lat_m, gps$lat_s),
			longitude = dms_to_dec(gps$lon_hem, gps$lon_d, gps$lon_m, gps$lon_s),
			herbicide_used = ifelse(as.numeric(gps$herb_this) == 1, TRUE,
				ifelse(as.numeric(gps$herb_this) == 2, FALSE, NA)),
			herbicide_product = gps$herb_name,
			weeding_times = as.integer(as.numeric(gps$weedings)),
			maize_code_ = as.character(crops$maize_code[match(gps$hhid, crops$hhid)]),
			intercrop_code_ = as.character(maize$intercrop_code[match(gps$hhid, maize$hhid)]),
			cob_weight_ = as.numeric(maize$w1[match(gps$hhid, maize$hhid)]) +
				as.numeric(maize$w2[match(gps$hhid, maize$hhid)]) +
				as.numeric(maize$w3[match(gps$hhid, maize$hhid)]),
			maize_in_plot = as.numeric(maize$maize_in_plot[match(gps$hhid, maize$hhid)])
		)
		d
	}

	dH <- make_plot(rB, rC, rD, "high")
	dL <- make_plot(rI, rJ, rK, "low")
	d <- carobiner::bindr(dH, dL)
	d <- d[!is.na(d$hhid), ]

	d$country <- "Malawi"
	d$geo_from_source <- TRUE
	d$on_farm <- TRUE
	d$is_survey <- TRUE
	d$crop <- ifelse(d$maize_in_plot == 1, "maize", NA)
	d$harvest_date <- parse_xl_date(rA$DATE[match(d$hhid, as.character(rA$HHID))])

	# maize_in_plot was only used to set crop
	d$maize_in_plot <- NULL

	carobiner::write_files(path, meta, d)
}
