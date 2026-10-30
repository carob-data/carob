# R script for "carob"
# license: GPL (>=3)

## NOTES
# NCAP/IRRI/NDUAT survey "Socio-economic dynamics of rice production systems in eastern India", Uttar Pradesh component, 2000-2001: 200 farmers in 20 villages of 4 blocks in Mahrajganj and Bahraich districts.
# Farmers are numbered 1-200 across the sample (hhid = farmer number).
# Land use (Landuse.XLS): one row per season x parcel (x plot) with crop, variety, area (ha), establishment method, production (qtl). yield = Prod * 100 / Area (kg/ha).
# Parcel topography, soil type, irrigation source and tenure from Landhold.XLS (LandDescrip) merged by farmer and parcel.
# Fertilizer from Riceinp.xls (Material), one kharif rice parcel per farmer; up to 3 products in kg. Grades: urea 46% N, Super 16% P, DAP 18-46-0, potash (MOP) 58% K; "P" and "K" taken as P2O5 and K2O; P = P2O5/2.29, K = K2O/1.2051. FYM given as a fertilizer (code 7) is mapped to OM_amount.
# Mixed crops: Prod/Byprod are text like "2.0+0.5" with one value per component crop. When the number of values equals the number of crops in the crop code, the record is split into one row per crop (yield on the full plot area, intercropped = TRUE); otherwise the mixed crop is kept as "crop1; crop2" with the total if a single number is given.
# Villages and blocks from the code description.

## ISSUES
# No coordinates; georeference the 20 villages (adm4) in blocks Nautanwa, Mithaura (Mahrajganj) and Phakharpur, Shivpur (Bahraich), Uttar Pradesh.
# No planting/harvest dates; the survey refers to 2000-2001 (kharif 2000, rabi 2000-01); planting_date not set.
# Fertilizer code 8 "Zn" (1 record) has no amount of Zn given; not converted to Zn_fertilizer.
# Riceinp Plot has values like "1-3" or "1,2" (several plots); merged by farmer, season and parcel only.
# Irrigation source code 0 (160 parcels) is not in the code description; set to NA.
# Crop name translations to check: "Lahi/Mustard/Alsi" mapped to "mustard; flax", "Patsan" to kenaf, "Karbi" (sorghum fodder) to sorghum, "Barseem" to berseem, "Parval" to pointed gourd, "Fodder" kept as "forage".
# Prod entries that are formulas (e.g. "(8+3.25)*(.25/1.25)", apparently allocating production of a parcel over plots) or whose number of components does not match the crops, are set to NA. "GF" (green fodder, fresh weight) values are used as given. Decimal commas ("1,5+2,0") were read as decimal points.
# Outliers: sugarcane yields of 375-400 t/ha (farmers 110, 118) and pigeon pea 112 t/ha on 0.04 ha (farmer 134); probably area or production errors; kept.
# Fertilizer is merged by farmer x parcel x season, so it is assigned to all rice plots of the input parcel (320 of 461 rice records).
# write_files: invalid season "zaid", invalid crop terms (mixed crops, berseem, forage, pointed gourd, vegetables, onion; garlic), fertilizer type "Zn", yield out of bounds (above), 2 duplicate records, NA yield for unsplit mixed crops.
# Records with codes 0 (crop, cropping, MVTV, CEMethod) are not documented; set to NA.
# Not processed: access, assets, changes, constraints, credit, crop changes, demography, disposal, employment, feeds, income, migration, non-rice inputs (wheat etc.; NonRiceInp.xls), labor, rice features, rice production changes.

## NEW VARIABLES
# farm_size_class_: sampling stratum (Mg=marginal, S=small, M=medium, L=large)
# topography_: parcel topography (upland, medium, lowland)


carob_script <- function(path) {

"
Socio-economic dynamics of rice production system in rainfed areas of Uttar Pradesh

The objectives of the project are: (1) to analyze the spatial patterns of rice productivity growth in the context of broader changes taking place in agriculture; (2) to analyze the patterns of rice technology adoption, constraints to adoption and importance of rice in rural livelihoods; and (3) to draw implications for technology design and policy reforms needed to accelerate rice productivity growth.
"

	uri <- "doi:10.7910/DVN/26271"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=3, minor=1,
		data_organization = "IRRI",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "survey",
		treatment_vars = NA,
		response_vars = NA,
		notes = NA,
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.6",
		carob_completion = 40,
		carob_effort = 0.5
	)

	f1 <- ff[basename(ff) == "Landuse.XLS"]
	f2 <- ff[basename(ff) == "Landhold.XLS"]
	f3 <- ff[basename(ff) == "Riceinp.xls"]
	r1 <- carobiner::read.excel(f1, sheet="Landuse", skip=7)
	r2 <- carobiner::read.excel(f2, sheet="LandDescrip", skip=5)
	r3 <- carobiner::read.excel(f3, sheet="Material", skip=5)

	vars <- c("1"="Mahsuri", "2"="Sarjoo-52", "3"="CR 1009", "4"="NDR 118", "5"="Indrasan", "6"="Pankaj", "7"="Pant-4", "8"="Pant-10", "9"="hybrid", "10"="Bisunparag", "11"="NDR 359", "12"="Sita", "13"="Saket-4", "14"="Sabitri", "15"="NDR 80/97", "16"="IR 64/49", "17"="Sarjoo-49", "18"="Usha", "19"="Prasad", "20"="Basmati", "21"="IR 8", 
		"1001"="Didei", "1002"="Bindesari", "1003"="Belas", "1004"="Chaurasia", "1005"="Ramkajra", "1006"="Mala", "1007"="China-4", "1008"="Malesia", "1009"="Parhani", "1010"="Kanchchi Masuri", "1011"="Orissawa", "1012"="Kalanamak", "1013"="Sardar", "1014"="Chaini", "1015"="Jarhan", "1016"="Satha", "1017"="Saraji", "1018"="Dhaniya", "1019"="Nagendra")
	crops <- c("rice", "mung bean", "horse gram", "vegetables", "wheat", "maize; pigeon pea; black gram", "berseem", "lentil; mustard", "sugarcane", "pointed gourd", "none", "mustard; flax", "lentil", "maize; black gram", "pigeon pea; maize; black gram; kenaf", "pigeon pea; maize; black gram; kenaf; pearl millet", "maize; pigeon pea", "maize; black gram; kenaf; pearl millet", "coriander", "barley", "pea; mustard", "maize; black gram; kenaf", "pigeon pea; black gram; pearl millet", "maize; black gram; kenaf; pearl millet", "maize", "onion; garlic", "maize; black gram; pearl millet", "pea", "maize; pearl millet; pigeon pea", "maize; pearl millet", "sorghum", "black gram", "wheat; mustard", "pigeon pea", "forage", "potato", "sugarcane; mustard", "maize", "maize", "pearl millet", "chickpea", "kenaf", "lentil; mustard; pea; flax", "lentil; mustard; flax", "pea; lentil; mustard", "cucumber", "pigeon pea")
	villages <- c("Sukrauli", "Deoghatti", "Harakhpur", "Jugauli", "Khairati", "Hardee", "Bhulna", "Rajwal", "Morwan Sonadi", "Baswar", "Agnapara", "Nandwal", "Dhakherwa", "Sisaura", "Sarai Jagna", "Intaha", "Matera Kala", "Kotawa", "Chandela Kala", "Behara")
	blocks <- c("Nautanwa", "Mithaura", "Phakharpur", "Shivpur")
	z <- function(x) { x[x %in% 0] <- NA; x }

	## Prod and Byprod are text, with "a+b+c" for the components of mixed crops
	splitprod <- function(x) {
		x <- gsub(" ", "", x)
		x <- gsub("Nill", "0", x)
		x <- gsub("G\\.?F\\.?$", "", x)
		x <- gsub(",", ".", x)
		lapply(strsplit(x, "\\+"), \(p) {
			if (length(p) == 0 || !all(grepl("^[0-9]*\\.?[0-9]+$", p))) return(NA)
			as.numeric(p)
		})
	}
	prod <- splitprod(r1$Prod)
	byprod <- splitprod(r1$Byprod)
	crop <- crops[z(r1$Crop)]
	ncrop <- lengths(strsplit(crop, "; "))
	nprod <- lengths(prod)
	r1$rec <- 1:nrow(r1)
	expand <- which(ncrop > 1 & nprod == ncrop)
	single <- setdiff(1:nrow(r1), expand)
	r1$crop <- crop
	r1$prod <- sapply(prod, \(p) if (length(p) == 1) p else NA)
	r1$byprod <- sapply(byprod, \(p) if (length(p) == 1) p else NA)
	rx <- r1[rep(expand, ncrop[expand]), ]
	rx$crop <- unlist(strsplit(crop[expand], "; "))
	rx$prod <- unlist(prod[expand])
	rx$byprod <- unlist(lapply(expand, \(i) if (length(byprod[[i]]) == ncrop[i]) byprod[[i]] else rep(NA, ncrop[i])))
	r1 <- rbind(r1[single, ], rx)

	d <- data.frame(
		record_id = r1$rec,
		farmer = r1$Farmer,
		adm2 = c("Mahrajganj", "Bahraich")[r1$District],
		adm3 = blocks[r1$Block],
		adm4 = villages[r1$Village],
		season = c("pre-kharif", "kharif", "rabi", "zaid")[r1$Season],
		parcel = r1$Parcel,
		plot_id = paste(r1$Parcel, r1$Plot, r1$Subplot, sep="-"),
		crop = r1$crop,
		field_size = r1$Area,
		yield = r1$prod * 100 / r1$Area,
		fwy_residue = r1$byprod * 100 / r1$Area
	)
	d$intercropped <- c(FALSE, TRUE)[z(r1$Cropping)]
	d$intercropped[grepl(";", crops[z(r1$Crop)])] <- TRUE
	isrice <- d$crop %in% "rice"
	d$variety <- ifelse(isrice, vars[as.character(r1$Variety)], NA)
	d$variety_type <- c("modern", "traditional")[z(r1$MVTV)]
	d$planting_method <- ifelse(isrice, c("broadcasting", "line sowing", "transplanting")[z(r1$CEMethod)], NA)

	## parcels
	lh <- data.frame(
		farmer = r2$Farmer, parcel = r2$Parcel,
		farm_size_class_ = r2$Category,
		topography_ = c("upland", "medium", "lowland")[r2$Topog],
		soil_texture = c("clay", "sandy loam", "loam", "clay loam")[r2$Soiltype],
		irrigation_source = c("canal", "deep tube well", "shallow tube well", "none")[z(r2$IrrigSrc)],
		land_tenure = c("owned", "sharecropped", "rented")[r2$Tenure]
	)
	lh$irrigated <- lh$irrigation_source != "none"
	lh <- lh[!duplicated(lh[, c("farmer", "parcel")]), ]
	d <- merge(d, lh, by=c("farmer", "parcel"), all.x=TRUE)

	## fertilizer
	grades <- data.frame(
		code = c(1:4, 8),
		name = c("urea", "SSP", "DAP", "KCl", "Zn"),
		N = c(46, 0, 18, 0, NA),
		P = c(0, 16, 46, 0, NA),
		K = c(0, 0, 0, 58, NA)
	)
	r3$rec <- 1:nrow(r3)
	fl <- do.call(rbind, lapply(1:3, \(j) data.frame(rec=r3$rec, code=r3[[paste0("Fert", j, "Type")]], qty=r3[[paste0("Fert", j, "Qty")]])))
	fl <- fl[!is.na(fl$code) & fl$code != 0 & !is.na(fl$qty) & fl$qty > 0, ]
	fym <- fl[fl$code == 7, ]
	fl <- fl[fl$code != 7, ]
	fl <- merge(fl, grades, by="code", all.x=TRUE)
	fl$N <- fl$qty * fl$N / 100
	fl$P <- fl$qty * fl$P / 100 / 2.29
	fl$K <- fl$qty * fl$K / 100 / 1.2051
	fl$N[fl$code == 8] <- fl$P[fl$code == 8] <- fl$K[fl$code == 8] <- 0
	fn <- aggregate(fl[, c("N", "P", "K")], fl["rec"], sum)
	ft <- aggregate(fl["name"], fl["rec"], \(x) paste(unique(x), collapse="; "))
	fert <- data.frame(rec = r3$rec, farmer = r3$Farmer, parcel = r3$Parcel, season = c("pre-kharif", "kharif", "rabi", "zaid")[r3$Season],
		ha = r3$Area, seed_rate = r3$SeedQty / r3$Area)
	fert <- merge(fert, merge(fn, ft, by="rec"), by="rec", all.x=TRUE)
	nofert <- !fert$rec %in% fl$rec
	fert[nofert, c("N", "P", "K")] <- 0
	fert$name[nofert] <- "none"
	fert$N_fertilizer <- fert$N / fert$ha
	fert$P_fertilizer <- fert$P / fert$ha
	fert$K_fertilizer <- fert$K / fert$ha
	fert$fertilizer_type <- fert$name
	fert$OM_amount <- 0
	i <- match(fym$rec, fert$rec)
	fert$OM_amount[i] <- fym$qty / fert$ha[i]
	fert$OM_type <- ifelse(fert$OM_amount > 0, "farmyard manure", "none")
	fert$rec <- fert$ha <- fert$N <- fert$P <- fert$K <- fert$name <- NULL
	fert <- fert[!duplicated(fert[, c("farmer", "parcel", "season")]), ]
	fert$crop <- "rice"
	d <- merge(d, fert, by=c("farmer", "parcel", "season", "crop"), all.x=TRUE)

	d$hhid <- as.character(d$farmer)
	d$trial_id <- d$hhid
	d$farmer <- d$parcel <- d$record_id <- NULL

	d$country <- "India"
	d$adm1 <- "Uttar Pradesh"
	d$currency <- "INR"
	d$on_farm <- TRUE
	d$is_survey <- TRUE
	d$yield_part <- "grain"
	d$yield_part[d$crop %in% c("mung bean", "horse gram", "lentil", "black gram", "pigeon pea", "pea", "chickpea", "coriander", "mustard; flax")] <- "seed"
	d$yield_part[d$crop %in% c("vegetables", "berseem", "forage")] <- "leaves"
	d$yield_part[d$crop %in% c("pointed gourd", "cucumber")] <- "fruit"
	d$yield_part[d$crop %in% c("sugarcane", "kenaf")] <- "stems"
	d$yield_part[d$crop == "potato"] <- "tubers"
	d$yield_part[d$crop == "none"] <- "none"

	carobiner::write_files(path, meta, d)
}
