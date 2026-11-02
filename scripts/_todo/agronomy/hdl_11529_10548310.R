# R script for "carob"
# license: GPL (>=3)

## NOTES
# Nutrient omission trials in farmers' fields in Kaduna, Kano, and Katsina, 2015 and 2016.
# The treatment sheet gives the full nutrient rate by LGA. P2O5 and K2O are oxides. Control is 0. A nutrient is 0 when that treatment omits it.
# The treatment sheet states spacing 75 cm by 25 cm. Net-plot area is taken from the raw sheet. A row whose treatment is the number 41, with soil fields filled with 131, is not a plot and is dropped. Crop-name typos in the previous-crop column are repaired.
# The 2015 sheet records aboveground biomass in t/ha. The 2016 sheet records it in kg/ha.
# Leaf, stover, and grain macronutrients are percent and are stored as mg/g. Micronutrients are ppm and are stored as mg/g.
# The blurred workbook has no coordinates.
# 2016 tissue columns follow the 2015 order: ear leaf, then stover NPK, then grain NPK.

## ISSUES
# write_files: missing variables: latitude, longitude out of bounds: leaf_N (4.928, 36.56), leaf_P (0.283, 5.755), leaf_K (1.93, 34.978), leaf_Ca (0.721, 12.968), leaf_Mg (0.353, 7.046), leaf_Cu (0, 0.015), leaf_Fe (0.012, 1.739), leaf_Mn (0.004, 0.536), leaf_Zn (0.002, 0.081), leaf_B (0.001, 0.089), residue_P (-0.082, 3.966), residue_K (0.496, 77.826), grain_N (3.892, 28.487), grain_P (0.528, 6.092), grain_K (0.094, 97.483) datespan: 1 harvest_date more than 366 days after planting_date NA detected: yield contributor:

## NOT PROCESSED
# The duplicate database workbook, and the separate soil, grain, stover, and yield files. Plot yields and the 2015 soil columns are taken from TAMASA_NOTs_Database_Nigeria_2015_2016.
# Farmer-estimated yields, manure rate, and the leaf sulphur column. The sulphur unit is not labeled.
# Mg is 0 unless the treatment name lists Mg. The S fertilizer source includes magnesium sulphate.


carob_script <- function(path) {

"
TAMASA Nutrient Omission Trials (NOTs) 2015–2016: Soil, plant nutrient and maize yield data from Nigeria

This dataset contains data from nutrient omission trials (NOTs) conducted in farmers' fields in Kaduna, Kano, and Katsina States, Nigeria, during the 2015 and 2016 growing seasons, as part of the Taking Maize Agronomy to Scale in Africa (TAMASA) project. The trials evaluated maize response to different fertilizer treatments using hybrid and open-pollinated maize varieties.
"

	uri <- "hdl:11529/10548310"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=0,
		data_organization = "CIMMYT",
		publication = NA,
		project = "TAMASA",
		design = NA,
		data_type = "experiment",
		treatment_vars = "treatment",
		response_vars = "yield",
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 60,
		carob_effort = 0.8
	)

	num <- function(x) {
		if (is.null(x)) return(NA_real_)
		if (is.numeric(x)) return(as.numeric(x))
		x <- trimws(as.character(x))
		x[x %in% c("", "NA", "n/a", "N/A", "-")] <- NA_character_
		as.numeric(x)
	}
	col <- function(r, nms) {
		hit <- nms[nms %in% names(r)]
		if (!length(hit)) return(rep(NA_real_, nrow(r)))
		num(r[[hit[1]]])
	}
	txt <- function(x) {
		if (is.null(x)) return(NA_character_)
		z <- trimws(as.character(x))
		z[!nzchar(z) | z %in% c("NA", "n/a")] <- NA_character_
		z
	}
	fill_down <- function(x) {
		x <- txt(x)
		for (i in seq_along(x)) {
			if (i > 1 && is.na(x[i])) x[i] <- x[i - 1]
		}
		x
	}
	as_date <- function(x) {
		if (inherits(x, "POSIXt") || inherits(x, "Date")) return(as.character(as.Date(x)))
		if (is.numeric(x)) return(as.character(as.Date(x, origin="1899-12-30")))
		x <- txt(x)
		out <- rep(NA_character_, length(x))
		iso <- !is.na(x) & nchar(x) >= 10 & substr(x, 5, 5) == "-"
		out[iso] <- substr(x[iso], 1, 10)
		slash <- !is.na(x) & grepl("/", x)
		if (any(slash)) {
			p <- strsplit(x[slash], "/")
			keep <- lengths(p) == 3
			idx <- which(slash)[keep]
			pp <- p[keep]
			dd <- as.integer(vapply(pp, function(z) z[1], ""))
			mm <- as.integer(vapply(pp, function(z) z[2], ""))
			yy <- as.integer(vapply(pp, function(z) z[3], ""))
			yy[!is.na(yy) & yy < 100] <- yy[!is.na(yy) & yy < 100] + 2000
			good <- !is.na(dd) & !is.na(mm) & !is.na(yy) & mm >= 1 & mm <= 12 & dd >= 1 & dd <= 31
			out[idx[good]] <- sprintf("%04d-%02d-%02d", yy[good], mm[good], dd[good])
		}
		out
	}
	has_nut <- function(treatment, nut) {
		parts <- strsplit(as.character(treatment), "+", fixed=TRUE)
		vapply(parts, function(p) {
			p <- trimws(p)
			if (!length(p) || any(is.na(p)) || any(p == "Control")) return(FALSE)
			if (nut %in% c("N", "P", "K")) {
				if (any(p == "NPK")) return(TRUE)
				if (any(p == "NP") && nut %in% c("N", "P")) return(TRUE)
				if (any(p == "NK") && nut %in% c("N", "K")) return(TRUE)
				if (any(p == "PK") && nut %in% c("P", "K")) return(TRUE)
				FALSE
			} else {
				any(p == nut)
			}
		}, logical(1))
	}
	apply_rate <- function(treatment, rate, nut) {
		has <- has_nut(treatment, nut)
		out <- ifelse(has, rate, 0)
		out[!has] <- 0
		out
	}
	plot_m2 <- function(x) {
		if (is.numeric(x)) return(as.numeric(x))
		z <- tolower(txt(x))
		z <- gsub("×", "x", z)
		p <- strsplit(z, "x", fixed=TRUE)
		out <- rep(NA_real_, length(z))
		two <- !is.na(z) & lengths(p) == 2
		if (any(two)) {
			a <- as.numeric(trimws(vapply(p[two], function(i) i[1], "")))
			b <- as.numeric(trimws(vapply(p[two], function(i) i[2], "")))
			out[which(two)] <- a * b
		}
		one <- !is.na(z) & lengths(p) == 1
		if (any(one)) out[one] <- as.numeric(trimws(unlist(p[one])))
		out
	}

	f <- ff[basename(ff) == "TAMASA_NOTs_Database_Nigeria_2015_2016 (blurred).xlsx"]
	ft <- carobiner::read.excel(f, sheet="Fertilizer rates & variety", na=c("", "NA"))
	ft <- ft[!is.na(ft$LGAs), ]
	ft <- ft[!duplicated(as.character(ft$LGAs)), ]
	rates <- data.frame(
		adm2 = trimws(as.character(ft$LGAs)),
		n = num(ft[["N (kg/ha)"]]),
		p = num(ft[["P2O5 (kg/ha)"]]) / 2.29,
		k = num(ft[["K2O (kg/ha)"]]) / 1.2051,
		s = num(ft[["S (kg/ha)"]]),
		ca = num(ft[["Ca (kg/ha)"]]),
		mg = num(ft[["Mg (kg/ha)"]]),
		zn = num(ft[["Zn (kg/ha)"]]),
		b = num(ft[["B (kg/ha)"]])
	)

	r1 <- carobiner::read.excel(f, sheet="Raw_Data_2015", skip=1, na=c("", "NA", "n/a"))
	r2 <- carobiner::read.excel(f, sheet="Raw_Data_2016", skip=1, na=c("", "NA", "n/a"))
	r1 <- r1[!is.na(r1$Treatment), ]
	r2 <- r2[!is.na(r2$Treatment), ]

	chem <- function(r, leaf_n, leaf_p, leaf_k, leaf_ca, leaf_mg, leaf_cu, leaf_fe, leaf_mn, leaf_zn, leaf_b, res_n, res_p, res_k, gr_n, gr_p, gr_k) {
		data.frame(
			leaf_N = col(r, leaf_n) * 10,
			leaf_P = col(r, leaf_p) * 10,
			leaf_K = col(r, leaf_k) * 10,
			leaf_Ca = col(r, leaf_ca) * 10,
			leaf_Mg = col(r, leaf_mg) * 10,
			leaf_Cu = col(r, leaf_cu) / 1000,
			leaf_Fe = col(r, leaf_fe) / 1000,
			leaf_Mn = col(r, leaf_mn) / 1000,
			leaf_Zn = col(r, leaf_zn) / 1000,
			leaf_B = col(r, leaf_b) / 1000,
			residue_N = col(r, res_n) * 10,
			residue_P = col(r, res_p) * 10,
			residue_K = col(r, res_k) * 10,
			grain_N = col(r, gr_n) * 10,
			grain_P = col(r, gr_p) * 10,
			grain_K = col(r, gr_k) * 10
		)
	}

	build <- function(r, year_sheet) {
		trt_raw <- txt(r$Treatment)
		r <- r[!is.na(trt_raw) & grepl("[A-Za-z]", trt_raw), ]
		if ("State" %in% names(r)) adm1 <- fill_down(r$State) else adm1 <- fill_down(r[["State/Province"]])
		adm2 <- trimws(as.character(r$LGA))
		site <- txt(if ("Experiment site code = EXPsit" %in% names(r)) r[["Experiment site code = EXPsit"]] else r[["Experimental site code = EXPsit"]])
		vtype <- tolower(txt(r[["Maize variety type"]]))
		vtype[vtype == "open pollinated"] <- "opv"
		variety <- txt(r[["Maize variety name = MVnam"]])
		trt <- txt(r$Treatment)
		year <- txt(r$Year)
		loc <- carobiner::fix_name(txt(r[["Community/village"]]), "title")
		area <- plot_m2(if ("netplot size (m2)" %in% names(r)) r[["netplot size (m2)"]] else r[["Size of the net plot (m x m)"]])
		if (year_sheet == 2015) {
			dmy <- col(r, "Above ground biomass (t/ha)") * 1000
			stalk <- col(r, "Stover Yield (kg/ha)")
			plot_id <- txt(r[["Plot number = PLOTnumb"]])
		} else {
			dmy <- col(r, "Above ground biomass (Kg/ha)")
			stalk <- col(r, "Stalk Yield (kg/ha)")
			plot_id <- txt(r$sample_code)
		}
		if (year_sheet == 2015) {
			pd <- as_date(r[["Planting date = PLNdat"]])
			hd <- as_date(r[["Harvest date = HDATE"]])
		} else {
			pd <- as_date(r[["Planting date"]])
			hd <- as_date(r[["Harvest Date"]])
		}
		d <- data.frame(
			country = "Nigeria",
			adm1 = carobiner::fix_name(adm1, "title"),
			adm2 = carobiner::fix_name(adm2, "title"),
			location = loc,
			adm2_key = adm2,
			trial_id = paste(site, year, vtype),
			plot_id = plot_id,
			treatment = trt,
			variety = variety,
			variety_type = vtype,
			crop = "maize",
			yield = col(r, "Grain Yield (kg/ha)"),
			yield_part = "grain",
			yield_moisture = col(r, "Grain moisture content (%)"),
			fwy_residue = stalk,
			dmy_total = dmy,
			planting_date = pd,
			harvest_date = hd,
			plot_area = area,
			row_spacing = 75,
			plant_spacing = 25,
			field_size = col(r, "Farm Size (Ha)"),
			on_farm = TRUE,
			is_survey = FALSE,
			irrigated = NA,
			geo_from_source = FALSE
		)
		d <- merge(d, rates, by.x="adm2_key", by.y="adm2", all.x=TRUE, sort=FALSE)
		d$adm2_key <- NULL
		d$N_fertilizer <- apply_rate(d$treatment, d$n, "N")
		d$P_fertilizer <- apply_rate(d$treatment, d$p, "P")
		d$K_fertilizer <- apply_rate(d$treatment, d$k, "K")
		d$S_fertilizer <- apply_rate(d$treatment, d$s, "S")
		d$Ca_fertilizer <- apply_rate(d$treatment, d$ca, "Ca")
		d$Mg_fertilizer <- apply_rate(d$treatment, d$mg, "Mg")
		d$Zn_fertilizer <- apply_rate(d$treatment, d$zn, "Zn")
		d$B_fertilizer <- apply_rate(d$treatment, d$b, "B")
		d$n <- NULL
		d$p <- NULL
		d$k <- NULL
		d$s <- NULL
		d$ca <- NULL
		d$mg <- NULL
		d$zn <- NULL
		d$b <- NULL
		if (year_sheet == 2015) {
			ch <- chem(r,
				"% N...69", "% P...70", "% K...71", "% Ca...72", "% Mg...73",
				"ppm Cu...75", "ppm Fe...76", "ppm Mn...77", "ppm Zn...78", "ppm B",
				"% N...80", "% P...81", "% K...82",
				"% N...90", "% P...91", "% K...92")
			d$soil_pH <- col(r, "pH (Water)  1:1")
			d$soil_SOC <- col(r, "OC (%)")
			d$soil_N_total <- col(r, "N (%)") * 10000
			d$soil_P <- col(r, "MehP (ppm)")
			d$soil_P_method <- "Mehlich"
			d$soil_sand <- col(r, "Sand (%)")
			d$soil_silt <- col(r, "Silt (%)")
			d$soil_clay <- col(r, "Clay (%)")
			d$soil_Ca_exch <- col(r, "Ca (cmol/kg)")
			d$soil_Mg_exch <- col(r, "Mg (cmol/kg)")
			d$soil_K_exch <- col(r, "K (cmol/kg)")
			d$soil_Na_exch <- col(r, "Na (cmol/kg)")
			d$soil_ex_acidity <- col(r, "Exch. Acidity (cmol/kg)")
			d$soil_ECEC <- col(r, "ECEC (cmol/kg)")
			d$soil_Zn <- col(r, "Zn (ppm)")
			d$soil_Cu <- col(r, "Cu (ppm)")
			d$soil_Mn <- col(r, "Mn (ppm)")
			d$soil_Fe <- col(r, "Fe (ppm)")
			d$soil_B <- col(r, "B (ppm)")
			d$soil_S <- col(r, "S (pmm)")
			d$soil_bd <- col(r, "BD (g/cm3)")
			prev <- tolower(txt(r[["Crops grown in the past two years"]]))
			prev <- gsub("g/nut", "groundnut", prev)
			prev <- gsub("gnut", "groundnut", prev)
			prev <- gsub("mazie", "maize", prev)
			prev <- gsub(" & ", ";", prev)
			prev <- gsub(" and ", ";", prev)
			prev <- gsub(",", ";", prev)
			prev <- gsub("[[:space:]]*;[[:space:]]*", ";", prev)
			prev <- gsub(";+", ";", prev)
			prev <- gsub("^;+|;+$", "", prev)
			prev[prev == "53"] <- NA_character_
			d$previous_crop <- prev
		} else {
			ch <- chem(r,
				"% N...42", "% P...43", "% K...44", "% Ca", "% Mg",
				"ppm Cu", "ppm Fe", "ppm Mn", "ppm Zn", "ppm B",
				"% N...53", "% P...54", "% K...55",
				"% N...56", "% P...57", "% K...58")
		}
		cbind(d, ch)
	}

	d <- carobiner::bindr(build(r1, 2015), build(r2, 2016))
	tv <- "treatment"
	if (!anyNA(d$N_fertilizer) && length(unique(d$N_fertilizer)) > 1) tv <- paste(tv, "N_fertilizer", sep=";")
	if (!anyNA(d$P_fertilizer) && length(unique(d$P_fertilizer)) > 1) tv <- paste(tv, "P_fertilizer", sep=";")
	if (!anyNA(d$K_fertilizer) && length(unique(d$K_fertilizer)) > 1) tv <- paste(tv, "K_fertilizer", sep=";")
	if (!anyNA(d$variety) && length(unique(d$variety)) > 1) tv <- paste(tv, "variety", sep=";")
	meta$treatment_vars <- tv
	carobiner::write_files(path, meta, d)
}
