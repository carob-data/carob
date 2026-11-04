# R script for "carob"
# license: GPL (>=3)

## NOTES
# IRRI/BIDS "Livelihood Systems in Bangladesh" panel, 2004 round (62 villages; sam#20 links to the 2000 households).
# This dataset only contains the household file HHDataY2004 (5 parts). The Parcel and Cost&Return files described in the codebook are not included.
# HHDataY2004 parts 1-4 are Excel 2.1 (BIFF2) worksheets that readxl cannot open; parsed with a small base-R record reader (read_biff2 below).
# Crop production is only available as household totals: part 3 has area (ha) and production (tons) per season (Aus, Aman, Boro) and crop group (MV rice, TV rice, jute, sugarcane, oilseeds, pulses, potato, onion, spices, vegetables, mixed, other).
# One row per household x season x crop group with area > 0; yield = production * 1000 / area. MV/TV = modern/traditional rice varieties (variety_type).
# Wheat is only given as an annual household total in part 2 (Whetland, WhetProd); added with season NA.
# zila = adm2 (spellings adjusted to GADM), upazila = adm3, union = adm4, village = location; names from the codebook "Genral_val" sheet.

## ISSUES
# Household-level aggregates only (field_size is the total area of that crop group in the season, not a plot). No varieties, inputs or dates.
# No coordinates; 62 villages (location) and upazila/zila need georeferencing; upazila/union spellings not checked against GADM.
# Crop groups "other", "mixed" (mixed rabi crops) have no single crop: crop NA (group in crop_label_). Oilseeds/pulses/spices/vegetables mapped to generic terms.
# Non-rice yields cluster on round constants (sugarcane 25 or 70 t/ha, vegetables 4 or 15 t/ha, oilseeds 0.4 t/ha, potato 10 t/ha): production was probably imputed as area x standard yield, not measured. Check before use.
# Sugarcane appears in Aus, Aman and Boro seasons (likely the same annual crop counted per season).
# Seasonal columns do not include wheat; wheat rows use the annual totals and have no season. Totals in part 2 (TVRLand, MVRLand, PotaLand, ...) duplicate part 3 and are not used.
# Unmapped: part 1 and 2 demography/education/labour, irrigated and rainfed area (irglnd, rfdlndo), land types, part 3 capital/livestock, part 4 housing/loans, part 5 income.
# write_files: no coordinates; season "aus" not in vocabulary; yield_part NA for jute, vegetables, onion, spices, mixed/other.

## NEW VARIABLES
# crop_label_: crop group label (e.g. "MV rice", "TV rice", "Oilseeds", "Mixed crop")
# hhid_2000_: household sample number in the 2000 round


carob_script <- function(path) {

"Livelihood Systems in Bangladesh (2004)

a)  Understand the operation of the Bangladesh rural household economy;  b) Analyze the drivers of change in income distribution and poverty;  c)  Prepare technical background papers to facilitating policy dialogues under the project 'Poverty Elimination Through Rice Research Assistance (PETRRA)' in Bangladesh;  d) Assess the impact of rice research on poverty reduction in Bangladesh using the Sustainable Livelihood Framework; and  e)  Estimating parameters on the determinants of rural household incomes for use in poverty mapping for Bangladesh."

	uri <- "doi:10.7910/DVN/26205"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=3, minor=1,
		data_organization = "IRRI",
		publication = NA,
		project = "PETRRA",
		design = NA,
		data_type = "survey",
		treatment_vars = NA,
		response_vars = NA,
		notes = NA,
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.6",
		carob_completion = 30,
		carob_effort = 0.6
	)

	# Excel 2.1 (BIFF2) worksheet reader: INTEGER, NUMBER, LABEL, BOOLERR and FORMULA cell records
	read_biff2 <- function(f) {
		b <- readBin(f, "raw", file.size(f))
		bi <- as.integer(b)
		n <- length(bi)
		pos <- integer(n %/% 8L)
		ids <- integer(n %/% 8L)
		p <- 1L
		k <- 0L
		while (p + 3L <= n) {
			id <- bi[p] + 256L * bi[p + 1L]
			k <- k + 1L
			pos[k] <- p + 4L
			ids[k] <- id
			if (id == 10L) break
			p <- p + 4L + bi[p + 2L] + 256L * bi[p + 3L]
		}
		cell <- ids[1:k] %in% 2:6
		pos <- pos[1:k][cell]
		ids <- ids[1:k][cell]
		u16 <- function(i) bi[i] + 256L * bi[i + 1L]
		rows <- u16(pos) + 1L
		cols <- u16(pos + 2L) + 1L
		num <- rep(NA_real_, length(pos))
		chr <- rep(NA_character_, length(pos))
		i <- ids == 2L
		num[i] <- u16(pos[i] + 7L)
		i <- which(ids %in% c(3L, 6L))
		if (length(i) > 0) {
			idx <- as.vector(outer(0:7, pos[i] + 7L, "+"))
			num[i] <- readBin(b[idx], "double", n=length(i), size=8, endian="little")
		}
		for (j in which(ids == 4L)) {
			nc <- bi[pos[j] + 7L]
			chr[j] <- if (nc > 0) rawToChar(b[(pos[j] + 8L):(pos[j] + 7L + nc)]) else ""
		}
		nr <- max(rows)
		h <- rows == 1L
		hdr <- paste0("V", 1:max(cols))
		hdr[cols[h]] <- chr[h]
		out <- lapply(seq_along(hdr), function(j) {
			i <- which(cols == j & rows > 1L)
			if (all(is.na(chr[i]))) {
				v <- rep(NA_real_, nr - 1L)
				v[rows[i] - 1L] <- num[i]
			} else {
				v <- rep(NA_character_, nr - 1L)
				v[rows[i] - 1L] <- ifelse(is.na(chr[i]), as.character(num[i]), chr[i])
			}
			v
		})
		names(out) <- hdr
		as.data.frame(out, check.names=FALSE)
	}

	fdoc <- ff[basename(ff) == "Documentation of 2004_Bangladesh.xls"]
	# value labels from the codebook: variable name (filled down), code, label
	codes <- function(sheet, var) {
		x <- carobiner::read.excel(fdoc, sheet=sheet, col_names=FALSE)
		nm <- x[[1]]
		nm <- nm[!is.na(nm)][cumsum(!is.na(nm))]
		k <- nm %in% var & !is.na(x[[4]])
		data.frame(code=as.numeric(x[[2]][k]), label=trimws(x[[4]][k]))
	}
	lab <- function(x, cds) {
		cds$label[match(x, cds$code)]
	}

	r1 <- read_biff2(ff[basename(ff) == "HHDataY2004_part 1.xls"])
	r2 <- read_biff2(ff[basename(ff) == "HHDataY2004_part 2.xls"])
	r3 <- read_biff2(ff[basename(ff) == "HHDataY2004_part 3.xls"])
	# column names differ in case (e.g. JutAsprd, SgAsLnd)
	names(r2) <- tolower(names(r2))
	names(r3) <- tolower(names(r3))

	# season x crop group columns of part 3 (codebook HHData_Var); "wheat" from the annual totals in part 2
	spec <- data.frame(
		prefix = c("mvas", "tvas", "jutas", "sgas", "osas", "vgas", "otas",
			"mvam", "tvam", "sgam", "plam", "spam", "vgam", "otam",
			"mvbr", "tvbr", "sgbr", "osbr", "plbr", "ptbr", "onbr", "spbr", "vgbr", "mxbr", "otbr"),
		season = rep(c("aus", "aman", "boro"), c(7, 7, 11)),
		label = c("MV rice", "TV rice", "Jute", "Sugarcane", "Oilseeds", "Vegetable", "Other crop",
			"MV rice", "TV rice", "Sugarcane", "Pulses", "Spices", "Vegetable", "Other crop",
			"MV rice", "TV rice", "Sugarcane", "Oilseeds", "Pulses", "Potato", "Onion", "Spices", "Vegetable", "Mixed crop", "Other crop")
	)
	crops <- data.frame(
		label = c("MV rice", "TV rice", "Jute", "Sugarcane", "Oilseeds", "Pulses", "Potato", "Onion", "Spices", "Vegetable", "Mixed crop", "Other crop", "Wheat"),
		crop = c("rice", "rice", "jute", "sugarcane", "oilseed", "pulse", "potato", "onion", "spices", "vegetable", NA, NA, "wheat"),
		variety_type = c("modern", "traditional", rep(NA, 11)),
		yield_part = c("grain", "grain", NA, "stems", "seed", "seed", "tubers", NA, NA, NA, NA, NA, "grain")
	)

	d <- lapply(1:nrow(spec), function(i) {
		data.frame(
			sample = r3$sample,
			season = spec$season[i],
			crop_label_ = spec$label[i],
			field_size = r3[[paste0(spec$prefix[i], "lnd")]],
			production = r3[[paste0(spec$prefix[i], "prd")]]
		)
	})
	d <- do.call(rbind, d)
	dw <- data.frame(
		sample = r2$sample,
		season = NA,
		crop_label_ = "Wheat",
		field_size = r2$whetland,
		production = r2$whetprod
	)
	d <- rbind(d, dw)
	d <- d[!is.na(d$field_size) & d$field_size > 0, ]

	i <- match(d$crop_label_, crops$label)
	d$crop <- crops$crop[i]
	d$variety_type <- crops$variety_type[i]
	d$yield_part <- crops$yield_part[i]
	# production in tons, area in ha
	d$yield <- d$production * 1000 / d$field_size

	i <- match(d$sample, r1$sample)
	d$hhid <- as.character(d$sample)
	d$hhid_2000_ <- as.character(r1[["sam#20"]][i])
	d$hh_size <- r1$hhsize[i]
	d$farmland_owned <- r1$ownland[i]
	d$cropland <- r1$clvt[i]
	d$location <- lab(r1$village[i], codes("Genral_val", "village"))
	d$adm4 <- lab(r1$union[i], codes("Genral_val", "union"))
	d$adm3 <- lab(r1$upazila[i], codes("Genral_val", "upazila"))
	d$adm2 <- lab(r1$zila[i], codes("Genral_val", "zila"))
	gadm <- c("B. Baria"="Brahamanbaria", "Mouvi-Bazar"="Maulvibazar", "Lakhmipur"="Lakshmipur", "Jhalakhati"="Jhalokati", "Perojpur"="Pirojpur", "Jhenaidaha"="Jhenaidah", "Kustia"="Kushtia", "Nababganj"="Nawabganj", "Panchaghar"="Panchagarh", "Netrokona"="Netrakona", "Munsiganj"="Munshiganj")
	i <- d$adm2 %in% names(gadm)
	d$adm2[i] <- gadm[d$adm2[i]]
	d$country <- "Bangladesh"

	d$sample <- d$production <- NULL

	d$on_farm <- TRUE
	d$is_survey <- TRUE

	carobiner::write_files(path, meta, d)
}
