# R script for "carob"
# license: GPL (>=3)

## NOTES
# Africa RISING cereal-legume trials. Five workbooks. SAS sheets are the same measurements laid out for analysis and were not read.
# 002 Upper West rotation (Goriyiri, Pase), 2014-2015. Varieties on the label, one per crop: maize Omankwa, groundnut CRI Azivivi, cowpea Songotra, soybean Jenguma.
# In RAW 2014 the grain, stover, biomass and plant-count columns are paired: kilograms (or count) then per hectare. The per-hectare column was used. It matches kg * 10000/6.75, so the 2014 harvest area is 6.75 m2. RAW 2015 stores kilograms only; the same 6.75 m2 was applied. The Year column in RAW 2014 is 1, not a calendar year; the sheet name was used.
# 003 and 004 are strip trials. 003 (2014) already has "/ha" columns (harvest area on the label is 7.5 m2). 004 stores a raw column and a per-hectare column under duplicated names; the larger of the pair was used as kg/ha.
# 005 Upper West strip trial. Label harvest areas are 7.5 m2 (maize) and 4.0 m2 (legumes). In 2014 the per-hectare columns were used (MYD second column, CYDH, GYDH), and only where the kilogram column is filled. In 2015 those per-hectare columns are empty, so kilograms were divided by the label areas.
# 001 is a stack of on-farm forms whose banners say Ntcheu and Dedza, Malawi (Kandeu, Linthipe, Golomoti, Nsipe), not northern Ghana. Country is Malawi for that file. Grain kg was divided by 3.75 m2, which is stated on the first form ("3 row x 5 m"). The "grain yield/ha" column on that first form equals stover kg x 10000 and was not used. A repeated form in columns 13-23 of some farmers was not read.
# Strip and rotation plots with two component yields were split into one row per crop. treatment keeps the system (sole maize, 2 maize : 2 cowpea, and so on).
# Authors in the Dataverse record: IITA, Ministry of Food and Agriculture (Ghana), and Savanna Agricultural Research Institute (Ghana). The organization vocabulary uses GMOFA for the ministry. Its SARI code is Selian Agricultural Research Institute (Tanzania), so the Ghana institute is named only in ISSUES.

## ISSUES
# No coordinates. Georeference the Ghana communities (Goriyiri, Pase, GUO, Bonia, Gia, Tekuru, Samboligo, Cheyohi, Tingoli, Tibogunayili, Jana, Duko, Siriyiri, Goli) and the Malawi sections (Kampanje, Mposa, Golomoti Centre, Mpamadzi).
# Ghana's Savanna Agricultural Research Institute is a co-author. It was not added to data_organization because the vocabulary code SARI is Selian Agricultural Research Institute. The ministry is stored as GMOFA.
# File 002 treatment codes 1-8 are not defined. The label lists crops next to those numbers, but several codes are "MAIZE". treatment is "code N"; the crop column is the crop measured that year. Asterisks in the 2015 yield cells were read as missing.
# 2015 community "GUO" is not on the 2014 label. Code 1 was read as Goriyiri and 2 as Pase.
# File 003 header says REGION: NORTHERN, but Kassena Nankana and Bongo are in Ghana's Upper East. adm1 was left as Northern, as written.
# File 004 writes the first community as "Cheyoli No.2" in 2014 and "Cheyohi No2" in 2015. Both spellings were kept.
# File 005 2015 has no district column, and its per-hectare yield columns are empty. Kilograms were converted with the label areas (maize 7.5 m2, legumes 4.0 m2).
# File 001 area 3.75 m2 is printed only on the first form. Later forms say only "WEIGHT (KG)". Confirm the area did not change. Also confirm the right-hand copy of some forms is a duplicate. Some scaled yields are very high (maize about 32 t/ha, soybean about 20 t/ha, cowpea about 19 t/ha) and were left as recorded.
# NPK and compost rates are not given. N, P and K are 0 only for treatments whose name is a control without a "+" . "Mz + NPK" stays NA.
# Stover and biomass freshness is not stated (stored as fwy_residue / fwy_total).
# File 003 plant counts are tens of plants, while the label says No./ha. Density was count / 7.5 m2. The same assumption was used where a per-hectare plant column was absent.
# Harvest index was not mapped: in file 003 it is not 100 x yield / biomass.
# write_files: missing longitude, latitude, irrigated, harvest_date. NA yield on some pigeon pea rows. High crop yield as above. Unknown variables epa_ and land_equivalent_ratio_.

## NOT PROCESSED
# SAS 2014, SAS 2015, SAS, ALL Yrs SAS, For SAS, Sheet2 of file 001: analysis copies or empty.
# Cob and pod weights, where grain yield per hectare was already present.
# LNDSV in file 004 2015: not defined in the label read here.
# Farmer estimated or total-grain columns (TGYD) that add the component yields.
# AEDO names. Hosting-farmer number is in trial_id, not a person's name (the name cell is empty).

## NEW VARIABLES
# epa_: Malawi extension planning area (Kandeu, Linthipe, Golomoti, Nsipe).
# land_equivalent_ratio_: LER column in the 2015 northern strip sheet. Repeated on each component row of the plot.
# Maize days-to-tasseling columns are tassling_days (terminag spelling). The cowpea and groundnut columns with the same label were stored as flowering_days.


carob_script <- function(path) {

"
Africa RISING - A Two Year Cereal Legume in Northern Ghana

This study contains data from different cereal legume rotation systems in northern Ghana.   About the project   Project title: A Two Year Cereal Legume Rotation in Northern Ghana   Project abstract   This project compares different cereal legume rotation systems and determines farmers' preferences.  Project website:    http://africa-rising.net/where-we-work/west-africa/   Project start date: 01/06/2015   Project end date : 11/01/2015
"

	uri <- "doi:10.7910/dvn/ycmmvk"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=2,
		data_organization = "IITA; GMOFA",
		publication = NA,
		project = "Africa RISING",
		design = NA,
		data_type = "on-farm experiment",
		treatment_vars = "treatment",
		response_vars = "yield",
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 55,
		carob_effort = 2,
		notes = NA
	)

	f <- function(bn) ff[basename(ff) == bn][1]
	num <- function(x) {
		x <- gsub("\u00a0", "", as.character(x))
		x <- gsub(",", "", x)
		x <- trimws(x)
		x[x %in% c("", ".", "NA", "na", "-", "*")] <- NA
		as.numeric(x)
	}
	fill <- function(x) {
		x <- trimws(as.character(x))
		x[x %in% c("", "NA")] <- NA
		for (i in seq_along(x)) {
			if (is.na(x[i]) && i > 1) x[i] <- x[i - 1]
		}
		x
	}
	# Second of a duplicated raw/per-ha pair, when that column is the larger one.
	perha <- function(r, prefix) {
		nms <- grep(paste0("^", prefix), names(r), value=TRUE)
		if (!length(nms)) return(rep(NA_real_, nrow(r)))
		if (length(nms) == 1) return(num(r[[nms[1]]]))
		a <- num(r[[nms[1]]])
		b <- num(r[[nms[2]]])
		if (median(b, na.rm=TRUE) > median(a, na.rm=TRUE)) b else a
	}
	ypart <- function(crop) ifelse(crop == "maize", "grain", "seed")
	base_row <- function(n) {
		data.frame(
			country=rep(NA_character_, n), adm1=rep(NA_character_, n), adm2=rep(NA_character_, n),
			location=rep(NA_character_, n), epa_=rep(NA_character_, n), trial_id=rep(NA_character_, n),
			planting_date=rep(NA_character_, n), rep=rep(NA_integer_, n), plot_id=rep(NA_character_, n),
			treatment=rep(NA_character_, n), crop=rep(NA_character_, n), variety=rep(NA_character_, n),
			yield=rep(NA_real_, n), yield_part=rep(NA_character_, n), yield_moisture=rep(NA_real_, n),
			fwy_residue=rep(NA_real_, n), fwy_total=rep(NA_real_, n), plant_height=rep(NA_real_, n),
			plant_density=rep(NA_real_, n), maturity_days=rep(NA_real_, n), flowering_days=rep(NA_real_, n),
			tassling_days=rep(NA_real_, n),
			plot_area=rep(NA_real_, n), N_fertilizer=rep(NA_real_, n), P_fertilizer=rep(NA_real_, n),
			K_fertilizer=rep(NA_real_, n), land_equivalent_ratio_=rep(NA_real_, n),
			stringsAsFactors=FALSE
		)
	}

	# --- Malawi hosting-farmer forms (file 001) ---
	raw <- carobiner::read.excel(f("001_legumeMaizeSequentingYieldData2013_2015.xls"), sheet="Sheet1", col_names=FALSE)
	x <- raw[, 1:12]
	crop_of <- function(z) {
		z <- tolower(gsub("\\s+", " ", trimws(z)))
		z[z %in% c("", "na")] <- NA
		out <- rep(NA_character_, length(z))
		out[z %in% c("maize", "mz")] <- "maize"
		out[z %in% c("cowpea", "cp")] <- "cowpea"
		out[z %in% c("groundnut", "groundnuts", "g/n", "gn", "gnut")] <- "groundnut"
		out[z %in% c("soybean", "soyabean", "soya")] <- "soybean"
		out[z %in% c("pp", "pigeonpea", "pigeon pea")] <- "pigeon pea"
		out[z %in% c("beans", "bean")] <- "common bean"
		out
	}
	district <- epa <- section <- farmer <- NA_character_
	mal <- list()
	for (i in seq_len(nrow(x))) {
		a <- trimws(as.character(x[i, 1]))
		b <- trimws(as.character(x[i, 3]))
		if (grepl("^DISTRICT", a, ignore.case=TRUE)) {
			district <- trimws(sub("(?i)^DISTRICT:?\\s*", "", a))
			epa <- trimws(sub("(?i)^EPA:?\\s*", "", b))
			section <- trimws(sub("(?i)^SECTION:?\\s*", "", as.character(x[i, 5])))
			next
		}
		if (grepl("HOSTING FARMER", b, ignore.case=TRUE)) {
			farmer <- trimws(sub("(?i).*HOSTING FARMER:?\\s*", "", b))
			next
		}
		if (a %in% c("REP NO.", "REP NO") || grepl("NAME OF AEDO", a, ignore.case=TRUE)) next
		trt <- gsub("\\s+", " ", trimws(as.character(x[i, 3])))
		if (is.na(trt) || trt == "" || grepl("TREATMENT", trt, ignore.case=TRUE)) next
		crops <- c(crop_of(x[i, 4]), crop_of(x[i, 8]))
		wts <- c(num(x[i, 5]), num(x[i, 9]))
		stv <- c(num(x[i, 6]), num(x[i, 10]))
		moi <- c(num(x[i, 7]), num(x[i, 11]))
		for (k in 1:2) {
			if (is.na(crops[k]) && is.na(wts[k])) next
			if (is.na(crops[k])) next
			row <- base_row(1)
			row$country <- "Malawi"
			row$adm2 <- carobiner::fix_name(district, "title")
			row$location <- carobiner::fix_name(section, "title")
			row$epa_ <- carobiner::fix_name(epa, "title")
			row$trial_id <- paste("MW", district, epa, farmer, sep="_")
			row$rep <- as.integer(num(a))
			row$plot_id <- as.character(trimws(as.character(x[i, 2])))
			row$treatment <- trt
			row$crop <- crops[k]
			row$yield <- wts[k] / 3.75 * 10000
			row$yield_part <- ypart(crops[k])
			row$yield_moisture <- moi[k]
			row$fwy_residue <- stv[k] / 3.75 * 10000
			row$plot_area <- 3.75
			ctrl <- grepl("control", tolower(trt)) & !grepl("\\+", trt)
			if (ctrl) {
				row$N_fertilizer <- 0
				row$P_fertilizer <- 0
				row$K_fertilizer <- 0
			}
			mal[[length(mal) + 1]] <- row
		}
	}
	d1 <- do.call(rbind, mal)

	# --- Upper West rotation (file 002) ---
	rot_one <- function(sheet, year) {
		r <- carobiner::read.excel(f("002_maizeLegumeRotation2014-2015.xls"), sheet=sheet, na=c(".", ""))
		cm <- trimws(as.character(r$CMMTY))
		cm[cm %in% c("1")] <- "Goriyiri"
		cm[toupper(cm) %in% c("2", "PASE")] <- "Pase"
		code <- trimws(as.character(if ("TRTMT" %in% names(r)) r$TRTMT else r$TRT))
		crop <- c(M="maize", G="groundnut", C="cowpea", SB="soybean")[code]
		variety <- c(maize="Omankwa", groundnut="CRI Azivivi", cowpea="Songotra", soybean="Jenguma")[crop]
		col_or <- function(nm) if (nm %in% names(r)) num(r[[nm]]) else rep(NA_real_, nrow(r))
		# 2014 stores kg in the named column and kg/ha in the following blank (...n) column.
		paired <- function(nm) {
			j <- match(nm, names(r))
			if (is.na(j)) return(rep(NA_real_, nrow(r)))
			nxt <- names(r)[j + 1]
			if (!is.na(nxt) && grepl("^\\.\\.\\.", nxt)) num(r[[j + 1]]) else num(r[[nm]])
		}
		if (year == "2014") {
			yield <- ifelse(crop=="maize", paired("MGYD"),
				ifelse(crop=="groundnut", paired("GNGYD"),
				ifelse(crop=="cowpea", paired("CGYD"), paired("SBGYD"))))
			residue <- ifelse(crop=="maize", paired("MSTOVER"),
				ifelse(crop=="groundnut", paired("GSTOVER"),
				ifelse(crop=="cowpea", paired("CSTOVER"), paired("SBSTOVER"))))
			biom <- ifelse(crop=="maize", paired("MTBMSS"),
				ifelse(crop=="groundnut", paired("GTBMSS"),
				ifelse(crop=="cowpea", paired("CTBMSS"), paired("SBTBMSS"))))
			dens <- ifelse(crop=="maize", paired("MNPLTHV"),
				ifelse(crop=="groundnut", paired("GNPLTHV"),
				ifelse(crop=="cowpea", paired("CPNPLTHV"), paired("SBNPLTHV"))))
		} else {
			kg <- ifelse(crop=="maize", col_or("MGYD"),
				ifelse(crop=="groundnut", col_or("GNGYD"),
				ifelse(crop=="cowpea", col_or("CGYD"), col_or("SBGYD"))))
			yield <- kg / 6.75 * 10000
			sk <- ifelse(crop=="maize", col_or("MSTOVER"),
				ifelse(crop=="groundnut", col_or("GSTOVER"),
				ifelse(crop=="cowpea", col_or("CSTOVER"), col_or("SBSTOVER"))))
			residue <- sk / 6.75 * 10000
			bk <- ifelse(crop=="maize", col_or("MTBMSS"),
				ifelse(crop=="groundnut", col_or("GTBMSS"),
				ifelse(crop=="cowpea", col_or("CTBMSS"), col_or("SBTBMSS"))))
			biom <- bk / 6.75 * 10000
			nk <- ifelse(crop=="maize", col_or("MNPLTHV"),
				ifelse(crop=="groundnut", col_or("GNPLTHV"),
				ifelse(crop=="cowpea", col_or("CPNPLTHV"), col_or("SBNPLTHV"))))
			dens <- nk / 6.75 * 10000
		}
		ht <- ifelse(crop=="maize", col_or("MPLNTH"),
			ifelse(crop=="groundnut", col_or("GPLNTH"),
			ifelse(crop=="cowpea", col_or("CPLNTH"), col_or("SBPLNTH"))))
		md <- ifelse(crop=="maize", col_or("MDTMT"),
			ifelse(crop=="groundnut", col_or("GDTMT"),
			ifelse(crop=="cowpea", col_or("CDTMT"), col_or("SBDTMT"))))
		out <- base_row(nrow(r))
		out$country <- "Ghana"
		out$adm1 <- "Upper West"
		out$location <- carobiner::fix_name(cm, "title")
		out$trial_id <- paste("UW-rotation", out$location, year, sep="_")
		out$planting_date <- year
		out$treatment <- paste("code", trimws(as.character(r$CODE)))
		out$crop <- crop
		out$variety <- variety
		out$yield <- yield
		out$yield_part <- ypart(crop)
		out$fwy_residue <- residue
		out$fwy_total <- biom
		out$plant_height <- ht
		out$plant_density <- dens
		out$maturity_days <- md
		out$plot_area <- 6.75
		out
	}
	d2 <- rbind(rot_one("RAW 2014", "2014"), rot_one("RAW 2015", "2015"))

	# --- Northern 2014 strip (file 003), /ha columns already named ---
	r3 <- carobiner::read.excel(f("003_maizeLegumeStrip14.xls"), sheet="Raw Data", na=c(".", ""))
	dist3 <- c("1"="Kassena Nankana", "2"="Bongo")[trimws(as.character(r3$DSTRT))]
	comm3 <- carobiner::fix_name(fill(r3$COMM), "title")
	trt3 <- c("1"="sole maize", "2"="sole cowpea", "3"="sole groundnut",
		"4"="2 maize : 2 cowpea", "5"="2 maize : 2 groundnut",
		"6"="2 maize : 4 cowpea", "7"="2 maize : 4 groundnut")[trimws(as.character(r3$TRT))]
	split3 <- function(crop, kg, yld, res, ht, nplant) {
		keep <- !is.na(kg)
		out <- base_row(sum(keep))
		out$country <- "Ghana"
		out$adm1 <- "Northern"
		out$adm2 <- dist3[keep]
		out$location <- comm3[keep]
		out$trial_id <- paste("UE-strip", out$adm2, out$location, "2014", sep="_")
		out$planting_date <- "2014"
		out$rep <- as.integer(num(r3$REP))[keep]
		out$treatment <- trt3[keep]
		out$crop <- crop
		out$yield <- yld[keep]
		out$yield_part <- ypart(crop)
		out$fwy_residue <- res[keep]
		out$plant_height <- ht[keep]
		out$plant_density <- nplant[keep] / 7.5 * 10000
		out$plot_area <- 7.5
		out
	}
	d3 <- rbind(
		split3("maize", num(r3[["MGYD /kg"]]), num(r3[["MGYD /ha"]]), num(r3[["MSTOVER /ha"]]), num(r3[["MPHTASS (cm)"]]), num(r3$MNPLHV)),
		split3("cowpea", num(r3[["CGYD /kg"]]), num(r3[["CGYD /ha"]]), num(r3[["CSTOVER /ha"]]), num(r3[["CPHTASS (cm)"]]), num(r3$CNPLHV)),
		split3("groundnut", num(r3[["GGYD /kg"]]), num(r3[["GGYD /ha"]]), num(r3[["GSTOVER ha"]]), num(r3[["GPHTASS (cm)"]]), num(r3$GNPLHV))
	)

	# --- Northern strip Tolon / Savelugu (file 004) ---
	strip4 <- function(sheet, year) {
		r <- carobiner::read.excel(f("004_maizeLegumestrip_NR.xls"), sheet=sheet, na=c(".", ""))
		dcode <- trimws(as.character(r$DSTRT))
		dist <- c("1"="Tolon", "2"="Savelugu")[dcode]
		if ("CMM NAME" %in% names(r)) {
			loc <- fill(r[["CMM NAME"]])
		} else if ("comm" %in% names(r)) {
			loc <- fill(r$comm)
		} else {
			loc <- fill(r$CMMTY)
		}
		# community codes from the label, if the cell is still a number
		locmap <- c("1"="Cheyohi", "2"="Tingoli", "3"="Tibogunayili", "4"="Jana", "5"="Duko")
		loc[loc %in% names(locmap)] <- locmap[loc[loc %in% names(locmap)]]
		loc <- carobiner::fix_name(loc, "title")
		tcol <- if ("TRTMENT" %in% names(r)) r$TRTMENT else if ("trt" %in% names(r)) r$trt else r$TRT
		trt <- c("1"="sole maize", "2"="sole cowpea", "3"="sole groundnut",
			"4"="2 maize : 2 cowpea", "5"="2 maize : 2 groundnut",
			"6"="2 maize : 4 cowpea", "7"="2 maize : 4 groundnut")[trimws(as.character(tcol))]
		repv <- if ("rep" %in% names(r)) as.integer(num(r$rep)) else rep(NA_integer_, nrow(r))
		ler <- if ("LER" %in% names(r)) num(r$LER) else rep(NA_real_, nrow(r))
		add <- function(crop, yld, res, ht, dens, tass) {
			keep <- !is.na(yld)
			out <- base_row(sum(keep))
			if (!nrow(out)) return(out)
			out$country <- "Ghana"
			out$adm1 <- "Northern"
			out$adm2 <- dist[keep]
			out$location <- loc[keep]
			out$trial_id <- paste("NR-strip", out$adm2, out$location, year, sep="_")
			out$planting_date <- year
			out$rep <- repv[keep]
			out$treatment <- trt[keep]
			out$crop <- crop
			out$yield <- yld[keep]
			out$yield_part <- ypart(crop)
			out$fwy_residue <- res[keep]
			out$plant_height <- ht[keep]
			dv <- dens[keep]
			if (length(dv) && !all(is.na(dv)) && median(dv, na.rm=TRUE) < 500) dv <- dv / 7.5 * 10000
			out$plant_density <- dv
			out$tassling_days <- if (crop == "maize") tass[keep] else rep(NA_real_, sum(keep))
			out$flowering_days <- if (crop != "maize") tass[keep] else rep(NA_real_, sum(keep))
			out$plot_area <- 7.5
			out$land_equivalent_ratio_ <- ler[keep]
			out
		}
		rbind(
			add("maize", perha(r, "MGYD"), perha(r, "MSTOVER"),
				if ("MPHTASS" %in% names(r)) num(r$MPHTASS) else rep(NA_real_, nrow(r)),
				perha(r, "MNPLHV"),
				if ("MDFF" %in% names(r)) num(r$MDFF) else rep(NA_real_, nrow(r))),
			add("cowpea", perha(r, "CGYD"), perha(r, "CSTOVER"),
				if ("CPHTASS" %in% names(r)) num(r$CPHTASS) else rep(NA_real_, nrow(r)),
				perha(r, "CNPLHV"),
				if ("CDFF" %in% names(r)) num(r$CDFF) else rep(NA_real_, nrow(r))),
			add("groundnut", perha(r, "GGYD"), perha(r, "GSTOVER"),
				if ("GPHTASS" %in% names(r)) num(r$GPHTASS) else rep(NA_real_, nrow(r)),
				perha(r, "GNPLHV"),
				if ("GDFF" %in% names(r)) num(r$GDFF) else rep(NA_real_, nrow(r)))
		)
	}
	d4 <- rbind(strip4("RAW 2014", "2014"), strip4("RAW 2015", "2015"))

	# --- Upper West strip (file 005) ---
	uw5 <- function(sheet, year) {
		r <- carobiner::read.excel(f("005_sasStrip_2014_2015UW.xls"), sheet=sheet, na=c(".", ""))
		cmm <- if ("COMM" %in% names(r)) r$COMM else r$CMMTY
		loc <- c("1"="Siriyiri", "2"="Pase", "3"="Goriyiri", "4"="Goli")[trimws(as.character(cmm))]
		dist <- if ("DIST" %in% names(r)) c("1"="Wa West", "2"="Nadowli")[trimws(as.character(r$DIST))] else rep(NA_character_, nrow(r))
		rawt <- trimws(as.character(r$TRT))
		trt <- c("1"="sole maize", "2"="sole cowpea", "3"="sole groundnut",
			"4"="2 maize : 2 cowpea", "5"="2 maize : 2 groundnut",
			"6"="2 maize : 4 cowpea", "7"="2 maize : 4 groundnut",
			"M"="sole maize", "C"="sole cowpea", "G"="sole groundnut",
			"2M:2C"="2 maize : 2 cowpea", "2M:2G"="2 maize : 2 groundnut",
			"2M:4C"="2 maize : 4 cowpea", "2M:4G"="2 maize : 4 groundnut")[rawt]
		# 2014: kg in MYD...40 / CYD / GYD and kg/ha in the following column. 2015 per-ha columns are empty; kg / area.
		if (year == "2014") {
			mkg <- num(r[["MYD...40"]]); mha <- num(r[["MYD...41"]])
			ckg <- num(r$CYD); cha <- num(r$CYDH)
			gkg <- num(r$GYD); gha <- num(r$GYDH)
			mst <- num(r$MSTOVER); cst <- num(r[["C STOVER"]]); gst <- num(r$GSTOVER)
		} else {
			mkg <- num(r$MGYD); mha <- mkg / 7.5 * 10000
			ckg <- num(r$CGYD); cha <- ckg / 4 * 10000
			gkg <- num(r$GGYD); gha <- gkg / 4 * 10000
			mst <- num(r$MSTOVER) / 7.5 * 10000
			cst <- num(r[["C STOVER"]]) / 4 * 10000
			gst <- num(r$GSTOVER) / 4 * 10000
		}
		add <- function(crop, kg, yld, res, ht, area) {
			keep <- !is.na(kg)
			out <- base_row(sum(keep))
			out$country <- "Ghana"
			out$adm1 <- "Upper West"
			out$adm2 <- dist[keep]
			out$location <- loc[keep]
			out$trial_id <- paste("UW-strip", out$location, year, sep="_")
			out$planting_date <- year
			out$treatment <- trt[keep]
			out$crop <- crop
			out$yield <- yld[keep]
			out$yield_part <- ypart(crop)
			out$fwy_residue <- res[keep]
			out$plant_height <- ht[keep]
			out$plot_area <- area
			out
		}
		ht <- function(nm) if (nm %in% names(r)) num(r[[nm]]) else rep(NA_real_, nrow(r))
		rbind(
			add("maize", mkg, mha, mst, ht("MPLTH"), 7.5),
			add("cowpea", ckg, cha, cst, ht("CPLHT"), 4),
			add("groundnut", gkg, gha, gst, ht("GPLTH"), 4)
		)
	}
	d5 <- rbind(uw5("Raw 2014", "2014"), uw5("Raw 2015", "2015"))

	d <- rbind(d1, d2, d3, d4, d5)
	d <- d[!is.na(d$crop), ]
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$geo_from_source <- FALSE
	# Fresh only where a moisture percent was recorded with the weight (file 001).
	d$yield_isfresh <- ifelse(!is.na(d$yield_moisture), TRUE, NA)

	carobiner::write_files(path, meta, d)
}
