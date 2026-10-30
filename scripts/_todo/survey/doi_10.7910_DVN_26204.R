# R script for "carob"
# license: GPL (>=3)

## NOTES
# IRRI/BIDS "Livelihood Systems in Bangladesh" panel, 2000 round (62 villages; sam#87 links to the 1987 households).
# Parcel_2000.xls and CostRet_2000.xls are Excel 2.1 (BIFF2) worksheets that readxl cannot open; parsed with a small base-R record reader (read_biff2 below).
# Two record types are combined in d (record_type_):
#  "parcel": Parcel_2000, one row per parcel x season. Boro season has two crop slots (crpbr1, crpbr2). land in ha, production in tonnes (codebook); yield = production * 1000 / land.
#  "cost_return": CostRet_2000, one parcel per household with inputs and value of output (no production quantity, so no yield). land in decimals (/247 -> ha).
# Crop codes: 1-11 are non-rice crops (codebook Cost&Retrn_val), 12 and higher are rice varieties (names decoded from the codebook). vrty = local / modern (HYV) / hybrid -> variety_type.
# Cost & return: N_fertilizer = urea kg * 0.46 / ha. Costs (Tk) divided by area -> BDT/ha. Labour = family + hired person-days for ploughing, sowing/transplanting, weeding, harvesting, threshing, per ha.
# Seed quantity (kg; "seed/for seedling") -> seed_rate kg/ha. crop loss qdmg in maund -> kg (1 maund = 37.32 kg) per ha.
# zila = adm2 (spellings adjusted to GADM), upazila = adm3, union = adm4, village = location; names from the codebook "Genral_val" sheet.

## ISSUES
# No coordinates; 62 villages (location) and upazila/zila need georeferencing; upazila/union spellings not checked against GADM.
# Crop year/season dates not given; planting_date not set.
# Cost & return records cannot be linked to a specific Parcel_2000 parcel (no parcel id); kept as separate rows without yield. The value of main product (prdr, Tk) is mapped to crop_value; no quantity.
# "Phosphet" (phq) product is not named (probably TSP, as in the 2008 round); kept as P_product_ (kg/ha) and not converted to P_fertilizer. Other chemical fertilizers only as cost (ofcst), so N_fertilizer is urea-N only.
# Crop codes 6 "Potato/Sweet potato", 10 "Mix spring harvest" and 11 "Others" have no single crop: crop left NA (label in crop_label_).
# Production unit for non-rice crops assumed tonnes (codebook says "in ton" for all). yield_part left NA for jute, vegetables, onion, spices, oilseeds/others.
# Parcel "irg" is the irrigation source of the parcel, not necessarily used in every season; irrigated only set to FALSE for "Without irrigation".
# Pest type codes (ptyp) mapped to labels in pest_species_; dmg mapped to stress (pests, flood, drought, hail, other).
# Unmapped: HHDataY2000 parts 1-5 (household aggregates, income), MemDemog_2000 (members), StockExp_2000 (storage, food, expenses); household background columns.
# write_files: no coordinates; season "aus" not in vocabulary; yield NA for cost_return rows and NA yield_part; extreme yields on very small parcels (vegetable, sugarcane > 100 t/ha); seed_rate up to 11 t/ha (probably potato seed tubers or a small-area typo) and N_fertilizer up to 874 kg/ha left as is, check.

## NEW VARIABLES
# record_type_: "parcel" (Parcel_2000) or "cost_return" (CostRet_2000)
# land_type_: flooding land type of the parcel (high, medium, low, very low land)
# crop_label_: original crop or rice variety label from the codebook
# crop_slot_: crop slot within the season (1, 2) for boro season in Parcel_2000
# hhid_1987_: household sample number in the 1987 round
# P_product_: amount of phosphate fertilizer product ("Phosphet"), kg/ha
# crop_loss_: crop loss due to damage, kg/ha
# pest_species_: labelled insect/pest causing damage


carob_script <- function(path) {

"Livelihood Systems in Bangladesh (2000)

a)  Understand the operation of the Bangladesh rural household economy;  b) Analyze the drivers of change in income distribution and poverty;  c)  Prepare technical background papers to facilitating policy dialogues under the project 'Poverty Elimination Through Rice Research Assistance (PETRRA)' in Bangladesh;  d) Assess the impact of rice research on poverty reduction in Bangladesh using the Sustainable Livelihood Framework; and  e)  Estimating parameters on the determinants of rural household incomes for use in poverty mapping for Bangladesh."

	uri <- "doi:10.7910/DVN/26204"
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
		carob_completion = 45,
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

	fdoc <- ff[basename(ff) == "Documentation of 2000_Bangladesh.xls"]
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

	# crop codes 1-12 (sheet Cost&Retrn_val); 13 and higher are rice varieties (Parcel_val)
	crops <- data.frame(code=1:11,
		crop = c("wheat", "jute", "sugarcane", "oilseed", "pulse", NA, "onion", "spices", "vegetable", NA, NA),
		yield_part = c("grain", NA, "stems", NA, "seed", NA, NA, NA, NA, NA, NA)
	)
	crplab <- rbind(codes("Cost&Retrn_val", "crp")[1:12, ], codes("Parcel_val", c("crpbr1/crpbr2/", "crpas/crpam")))
	vtype <- c("local", "modern", "hybrid")
	soils <- c("loam", "sandy loam", "clay", "clay loam", "sand")
	stress <- c("none", "pests", "flood", "drought", "hail", "other")

	setcrop <- function(d, code) {
		d$crop_label_ <- lab(code, crplab)
		i <- match(code, crops$code)
		d$crop <- ifelse(code >= 12, "rice", crops$crop[i])
		d$yield_part <- ifelse(code >= 12, "grain", crops$yield_part[i])
		d$variety <- ifelse(code >= 12, d$crop_label_, NA)
		d
	}

	## Parcel_2000
	r1 <- read_biff2(ff[basename(ff) == "Parcel_2000.xls"])
	slots <- data.frame(season=c("boro", "boro", "aus", "aman"), sfx=c("br1", "br2", "as", "am"), slot=c(1, 2, 1, 1))
	d1 <- lapply(1:4, function(i) {
		s <- slots$sfx[i]
		data.frame(
			hhid = as.character(r1$sample),
			hhid_1987_ = as.character(r1[["sam#87"]]),
			field_id = as.character(r1$plotnum),
			season = slots$season[i],
			crop_slot_ = slots$slot[i],
			crop_code = r1[[paste0("crp", s)]],
			variety_type = vtype[match(r1[[paste0("vrty", s)]], 1:3)],
			production = r1[[paste0("prd", s)]],
			field_size = r1$land,
			land_ownedby = lab(r1$owner, codes("Parcel_val", "owner")),
			land_tenure = lab(r1$tanr, codes("Parcel_val", "tanr")),
			soil_texture = soils[match(r1$soil, 1:5)],
			land_type_ = lab(r1$fld, codes("Parcel_val", "fld")),
			irrigation_source = lab(r1$irg, codes("Parcel_val", "irg")),
			village = r1$village,
			union = r1$union,
			upazila = r1$upazila,
			zila = r1$zila,
			hh_size = r1$hhsize,
			farmland_owned = r1$ownland,
			cropland = r1$clvt
		)
	})
	d1 <- do.call(rbind, d1)
	d1 <- d1[d1$crop_code > 0, ]
	d1 <- setcrop(d1, d1$crop_code)
	# production in tonnes, area in ha
	d1$yield <- d1$production * 1000 / d1$field_size
	d1$record_type_ <- "parcel"

	## CostRet_2000
	r2 <- read_biff2(ff[basename(ff) == "CostRet_2000.xls"])
	# land in decimals
	ha <- r2$land / 247
	d2 <- data.frame(
		hhid = as.character(r2$sample),
		hhid_1987_ = as.character(r2[["sam#87"]]),
		season = c("aus", "aman", "boro", "rabi")[match(r2$season, 1:4)],
		variety_type = vtype[match(r2$vrty, 1:3)],
		field_size = ha,
		land_prep_times = as.integer(r2$pln),
		seed_rate = r2$sdq / ha,
		N_fertilizer = r2$urq * 0.46 / ha,
		P_product_ = r2$phq / ha,
		irrigation_source = lab(r2$irg, codes("Cost&Retrn_val", "irg")),
		weeding_times = as.integer(r2$wdn),
		insecticide_times = as.integer(r2$ppn),
		labour = (r2$flpl + r2$hlpl + r2$flsd + r2$hlsd + r2$flwd + r2$hlwd + r2$flh + r2$hlh + r2$flt + r2$hlt) / ha,
		land_prep_cost = (r2$plcst + r2$ptcst) / ha,
		seed_cost = r2$sdcst / ha,
		fertilizer_cost = (r2$urcst + r2$phcst + r2$ofcst) / ha,
		OM_cost = r2$mncst / ha,
		irrigation_cost = r2$ircst / ha,
		insecticide_cost = r2$ppcst / ha,
		insecticide_app_cost = r2$pacst / ha,
		crop_value = r2$prdr / ha,
		stress = stress[match(r2$dmg, 0:5)],
		crop_loss_ = r2$qdmg * 37.32 / ha,
		pest_species_ = lab(r2$ptyp, codes("Cost&Retrn_val", "ptyp")),
		currency = "BDT",
		village = r2$village,
		union = r2$union,
		upazila = r2$upazila,
		zila = r2$zila,
		hh_size = r2$hhsize,
		farmland_owned = r2$ownland,
		cropland = r2$clvt
	)
	d2 <- setcrop(d2, r2$crp)
	d2$record_type_ <- "cost_return"

	d <- carobiner::bindr(d1, d2)

	d$irrigated <- ifelse(d$irrigation_source == "Without irrigation", FALSE, NA)
	d$irrigation_source[d$irrigation_source == "Without irrigation"] <- "none"

	d$location <- lab(d$village, codes("Genral_val", "village"))
	d$adm4 <- lab(d$union, codes("Genral_val", "union"))
	d$adm3 <- lab(d$upazila, codes("Genral_val", "upazila"))
	d$adm2 <- lab(d$zila, codes("Genral_val", "zila"))
	gadm <- c("B. Baria"="Brahamanbaria", "Mouvi-Bazar"="Maulvibazar", "Lakhmipur"="Lakshmipur", "Jhalakhati"="Jhalokati", "Perojpur"="Pirojpur", "Jhenaidaha"="Jhenaidah", "Kustia"="Kushtia", "Nababganj"="Nawabganj", "Panchaghar"="Panchagarh", "Netrokona"="Netrakona", "Munsiganj"="Munshiganj")
	i <- d$adm2 %in% names(gadm)
	d$adm2[i] <- gadm[d$adm2[i]]
	d$country <- "Bangladesh"

	d$crop_code <- d$production <- NULL
	d$village <- d$union <- d$upazila <- d$zila <- NULL

	d$on_farm <- TRUE
	d$is_survey <- TRUE

	carobiner::write_files(path, meta, d)
}
