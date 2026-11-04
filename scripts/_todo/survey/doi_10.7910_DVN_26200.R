# R script for "carob"
# license: GPL (>=3)

## NOTES
# IRRI/BIDS "Livelihood Systems in Bangladesh" panel, 1987 round (62 villages, ~1230 households).
# Parcel87.xls is an Excel 2.1 (BIFF2) worksheet (exported from SPSS) that readxl cannot open; it is parsed with a small base-R record reader (read_biff2 below).
# Parcel87 has one row per household parcel with crop, variety code and production for the Boro, Aus and Aman seasons. Reshaped to one row per parcel x season (seasons without a crop dropped).
# Parcel area "land": the codebook says "decimal", but values are hectares (= decimals / 247; parcel sums equal household clvt in ha).
# Production prdbr/prdas/prdam: tonnes (values are multiples of 1 maund = 0.03732 t; household totals elsewhere are labelled "tons"). yield = production * 1000 / land.
# Crop codes (Parcel_val) hard-coded below; TV/MV in the crop label = traditional/modern variety (variety_type). B-Aus/B-Aman = broadcast, T-Aman = transplanted.
# Variety and village/union/upazila/zila names are decoded from the codebook "Documentation of 1987_Bangladesh.xls".
# zila = district (adm2), upazila = adm3, union = adm4, village = location. Zila spellings adjusted to GADM.

## ISSUES
# No coordinates. 62 villages (location) with upazila/zila names need georeferencing; upazila/union spellings not checked against GADM.
# Survey year 1987 but crop year/season dates not given (Boro 1986/87?); planting_date not set.
# Production unit for non-rice crops (jute, potato, vegetables, ...) assumed tonnes like rice; not documented.
# Variety code 999 (and any code not in the codebook) has no label; left as NA. Code 0 = not mentioned.
# "irg" is the parcel's irrigation facility, not necessarily used in every season; irrigated is only set to FALSE for "No irrigation".
# "B-Aus & Aman" (mixed broadcast aus + aman rice) mapped to rice; original crop label kept in crop_label_. Variety is given for few parcels.
# yield_part left NA for jute, vegetables, onion, garlic, spices (fibre/bulb/mixed products; unit and part unclear).
# Unmapped: HHDataY1987 part 1/2 (household aggregates and income), Member87 (demography), Emp4days87 (time use), most household background columns repeated in Parcel87.
# write_files: no coordinates; season "aus" is not in the season vocabulary; yield_part NA; high yields on tiny parcels (e.g. vegetable 276 t/ha on 0.008 ha, lentil 24.9 t/ha) left as is, check.

## NEW VARIABLES
# land_type_: flooding land type of the parcel (high, medium high, low, very low land)
# crop_label_: original crop/season label from the codebook (e.g. "MV Boro", "B-Aman", "Kaon etc")


carob_script <- function(path) {

"Livelihood Systems in Bangladesh (1987)

a)  Understand the operation of the Bangladesh rural household economy;  b) Analyze the drivers of change in income distribution and poverty;  c)  Prepare technical background papers to facilitating policy dialogues under the project 'Poverty Elimination Through Rice Research Assistance (PETRRA)' in Bangladesh;  d) Assess the impact of rice research on poverty reduction in Bangladesh using the Sustainable Livelihood Framework; and  e)  Estimating parameters on the determinants of rural household incomes for use in poverty mapping for Bangladesh."

	uri <- "doi:10.7910/DVN/26200"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=3, minor=0,
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
		carob_completion = 40,
		carob_effort = 1
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

	fdoc <- ff[basename(ff) == "Documentation of 1987_Bangladesh.xls"]
	# value labels from the codebook: variable name (filled down), code, label
	codes <- function(sheet, var) {
		x <- carobiner::read.excel(fdoc, sheet=sheet, col_names=FALSE)
		nm <- x[[1]]
		nm <- nm[!is.na(nm)][cumsum(!is.na(nm))]
		k <- nm == var & !is.na(x[[4]])
		data.frame(code=as.numeric(x[[2]][k]), label=trimws(x[[4]][k]))
	}
	lab <- function(x, cds) {
		cds$label[match(x, cds$code)]
	}

	r <- read_biff2(ff[basename(ff) == "Parcel87.xls"])

	vrty <- codes("Parcel_val", "vrtybr/vrtyam/vrtyas")
	tanr <- codes("Parcel_val", "tanr")
	fld <- codes("Parcel_val", "fld")
	irg <- codes("Parcel_val", "irg")

	# crop codes from sheet Parcel_val
	crops <- data.frame(code=1:41, 
		label = c("TV Aus", "MV Aus", "B-Aus & Aman", "B-Aman", "TV T-Aman", "MV Aman", "Pajam Aman", "TV Boro", "MV Boro", "TV Wheat", "MV Wheat", "Kaon etc", "Jute", "Sugarcane", "Tobacco", "Potato", "Chilli", "Vegetables", "Mustard", "Til", "Other oilseeds", "Masur", "Kheshari", "Kalai", "Mung", "Gram", "Other pulses", "Betel leaves", "Betel nut", "Ginger", "Turmaric", "Onion", "Garlic", "Other spices", "Banana", "Jackfruit", "Pineapple", "Other fruits", "Seedbed", "Bamboo", "Fish"),
		crop = c(rep("rice", 9), "wheat", "wheat", "foxtail millet", "jute", "sugarcane", "tobacco", "potato", "chili pepper", "vegetable", "mustard", "sesame", "oilseed", "lentil", "grass pea", "black gram", "mung bean", "chickpea", "pulse", "betel", "areca nut", "ginger", "turmeric", "onion", "garlic", "spices", "banana", "jackfruit", "pineapple", "fruit", "seedbed", "bamboo", "fish"),
		variety_type = c("traditional", "modern", NA, NA, "traditional", "modern", NA, "traditional", "modern", "traditional", "modern", rep(NA, 30)),
		planting_method = c(NA, NA, "broadcasting", "broadcasting", "transplanting", rep(NA, 36)),
		yield_part = c(rep("grain", 12), NA, "stems", "leaves", "tubers", "fruit", NA, rep("seed", 9), "leaves", NA, "rhizome", "rhizome", NA, NA, NA, rep("fruit", 4), NA, NA, NA)
	)

	# one row per parcel x season
	seasons <- data.frame(season=c("boro", "aus", "aman"), sfx=c("br", "as", "am"))
	d <- lapply(1:3, function(i) {
		s <- seasons$sfx[i]
		data.frame(
			hhid = as.character(r$sample),
			field_id = as.character(r[["plot#"]]),
			season = seasons$season[i],
			crop_code = r[[paste0("crp", s)]],
			variety_code = r[[paste0("vrty", s)]],
			production = r[[paste0("prd", s)]],
			field_size = r$land,
			land_tenure = lab(r$tanr, tanr),
			land_type_ = lab(r$fld, fld),
			irrigation_source = lab(r$irg, irg),
			village = r$village,
			union = r$union,
			upazila = r$upazila,
			zila = r$zila,
			hh_size = r$hhsize,
			farmland_owned = r$ownland,
			cropland = r$clvt
		)
	})
	d <- do.call(rbind, d)
	d <- d[d$crop_code > 0, ]

	i <- match(d$crop_code, crops$code)
	d$crop <- crops$crop[i]
	d$crop_label_ <- crops$label[i]
	d$variety_type <- crops$variety_type[i]
	d$planting_method <- crops$planting_method[i]
	d$yield_part <- crops$yield_part[i]
	d$variety <- lab(d$variety_code, vrty)
	d$variety[d$crop_label_ == "Pajam Aman" & is.na(d$variety)] <- "Pajam"
	# production in tonnes, area in ha
	d$yield <- d$production * 1000 / d$field_size

	d$irrigated <- ifelse(d$irrigation_source == "No irrigation", FALSE, NA)
	d$irrigation_source[d$irrigation_source == "No irrigation"] <- "none"

	d$location <- lab(d$village, codes("Genral_val", "village"))
	d$adm4 <- lab(d$union, codes("Genral_val", "union"))
	d$adm3 <- lab(d$upazila, codes("Genral_val", "upazila"))
	d$adm2 <- lab(d$zila, codes("Genral_val", "zila"))
	gadm <- c("B. Baria"="Brahamanbaria", "Mouvi-Bazar"="Maulvibazar", "Lakhmipur"="Lakshmipur", "Jhalakhati"="Jhalokati", "Perojpur"="Pirojpur", "Jhenaidaha"="Jhenaidah", "Kustia"="Kushtia", "Nababganj"="Nawabganj", "Panchaghar"="Panchagarh", "Netrokona"="Netrakona", "Munsiganj"="Munshiganj")
	i <- d$adm2 %in% names(gadm)
	d$adm2[i] <- gadm[d$adm2[i]]
	d$country <- "Bangladesh"

	d$crop_code <- d$variety_code <- d$production <- NULL
	d$village <- d$union <- d$upazila <- d$zila <- NULL

	d$on_farm <- TRUE
	d$is_survey <- TRUE

	carobiner::write_files(path, meta, d)
}
