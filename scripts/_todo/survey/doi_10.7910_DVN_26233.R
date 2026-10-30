# R script for "carob"
# license: GPL (>=3)

## NOTES
# IRRI / OUAT survey on rice varietal adoption in Orissa (Odisha), India, survey year 2005; 300 households in 6 districts (Cuttack, Bargarh, Puri, Jagatsinghpur, Angul, Nuapada), stratified by farm size (Type: L, M, S, Mg).
# Farmers are numbered within district and farm size class; hhid = district-type-farmer.
# Land use (Landuse.XLS, LandUse) gives one row per season x parcel x plot x subplot with crop, rice variety, area (ha), establishment method, planting and harvest month/week and production (qtl). yield = Prod * 100 / Area (kg/ha).
# Plot characteristics (topography, soil type, irrigation source, tenure) from Landhold.XLS were merged by farmer, parcel and plot.
# Fertilizer (RiceInp.xls, Material) is for selected rice parcels; each column block is a fixed product (urea, "Super" P 16%, DAP 18-46-0, potash 60%, Gromor 28-28-0, Rocord, ammonium sulphate 21-0-0, Iffco 10-26-26; all in kg). Grades were taken as N-P2O5-K2O, P = P2O5/2.29, K = K2O/1.2051. FYM was given in kg (column "Qty in kg") and mapped to OM_amount.
# Village names (from Demog.xlsx, household head row) and block names come from the code description.

## ISSUES
# No coordinates; georeference villages (adm4) in the blocks/districts of Odisha listed in the data.
# Season years are not given (survey year 2005; Kharif probably 2004 or 2005). Planting and harvest month and week are kept as planting_month_, planting_week_, harvest_month_, harvest_week_ instead of dates.
# "Rocord" (fertilizer code 8, 7 records) has unknown composition; N/P/K set to NA for those records.
# The second "Crop" column in LandUse ("use this variable") is used; it differs from the first for 6 records.
# Production of non-rice crops is also in qtl; crop codes 6 (gulji / horse gram) and 7 (arhar = pigeon pea).
# Fertilizer and seed data only for rice parcels covered by the Material sheet.
# Not processed: Demog (household members), Income, Labor, Nonrice-Inputs and Labor, RiceTech (variety changes), SeedRep (seed replacement), insecticides, machinery.
# Fertilizer data matched to 950 of 2901 land use records (the selected rice parcels).
# write_files: unknown variables (the NEW VARIABLES below, and establishment_method_), invalid crop "horse gram", invalid fertilizer_type terms "10-26-26", "28-28-0", "AS", "Rocord"; NA yield for some records.
# read warnings: Demog Occup1 has multiple codes ("9, 10") read as NA (not used); LandUse Remarks column read as logical (text "Damaged due to flood" lost; not used).
# 6 plots damaged by flood (Remarks) have yield 0 or low.

## NEW VARIABLES
# farm_size_class_: sampling stratum (L=large, M=medium, S=small, Mg=marginal)
# topography_: plot topography (upland bunded, upland unbunded, medium, lowland, very lowland)
# planting_month_, planting_week_, harvest_month_, harvest_week_: month (1-12) and week (1-4) of planting and harvest


carob_script <- function(path) {

"
Rice Technology Adoption in Orissa

Objectives of the study are to identify rice varieties that are recently being adopted by the farmers in the past 5 years, to estimate the extent of adoption and to identify factors influencing farmers to shift to new varieties.
"

	uri <- "doi:10.7910/DVN/26233"
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
		carob_effort = 0.6
	)

	f1 <- ff[basename(ff) == "Landuse.XLS"]
	f2 <- ff[basename(ff) == "Landhold.XLS"]
	f3 <- ff[basename(ff) == "RiceInp.xls"]
	f4 <- ff[basename(ff) == "Demog.xlsx"]
	r1 <- carobiner::read.excel(f1, sheet="LandUse", skip=7)
	r2 <- carobiner::read.excel(f2, sheet="LandDescrip", skip=5)
	r3 <- carobiner::read.excel(f3, sheet="Material", skip=5)
	r4 <- carobiner::read.excel(f4, sheet="Demog", skip=6)

	tv <- c("Saruchina", "Sebatipokhia", "Jagannath Ballabha", "Sitabhoga", "Ratanchudi", "Chandrapuri", "Madhupiri", "Bhubaniukuda", "Rajabhoga", "Kalamkathi", "Badakalamula", "Sanakalamula", "Rasapanjari", "Baunsagaja", "Machhakanta", "Sunakhala", "Budhamunda", "Bhandi", "Betanasia", "Matia", "Hajarimala", "Maurakantha", "Baula", "Kakudia", "Malabati", "Kedaragouri", "Bhanumati", "Koili", "Hiramala", "Basumati", "Pakshira", "Bankoi", "Suaaboula", "Dhalakartika", "Bangaramadhabi", "Motara", "Kakudimangi", "Kalapakhia", "Kelikadamba", "Gagabhoga", "Kalasura", "Champa", "Bachhakalama", "Bhutia", "Hanseswari", "Muktapakhia", "Chandainakhi", "Pathuria", "Sahani", "Suhabaula", "Nababi", "Masinaphula", "Jhuli", "Biramani", "Bitibaula", "Sapari", "Changuri", "Kantamali", "Bunda", "Sankari", "Khalia", "Kuadi", "Gurmati", "Jagati", "Serla", "Lachai", "Dubraj", "Yubraj", "Sukalaghata", "Jaiphula")
	mv <- c("Swarna", "Lalat/Samrat", "Balaram", "Puja", "Durga", "Ramchandi", "Chha akhi/Kalashree", "CR 141", "Kanchana", "CR 1018", "CR 1030", "CR 1009", "Saragatara", "CRLC 899", "T 1242", "Culture M11", "Khandagiri", "Culture M10, S10", "Moti", "CR 1014", "Sujata", "Lunishree", "CR 1021", "Culture S6", "Jagannath", "IET 5656", "Parijata", "IR36", "Basanti", "Debasis", "Tulashi", "CR 1590", "Sarala", "Gitanjali", "Saraswati", "Jagabalia", "CR 77", "Masuri", "Ambica", "Lakshyahira", "MTU1010", "MTU1001", "Mahamaya", "HMT", "Purnima", "Sarathi", "Annapurna", "Gangasafari", "Jajati", "Surendra", "Mahanadi", "Gajapati", "Daya", "Superstar", "Arnapurna")
	crops <- c("0"="none", "1"="rice", "2"="mung bean", "3"="black gram", "4"="jute", "5"="groundnut", "6"="horse gram", "7"="pigeon pea")
	blocks <- c(Cuttack="Salipur", Bargarh="Attabira", Puri="Puri Sadar", Jagatsinghpur="Ersama", Angul="Angul", Nuapada="Nuapada Sadar")
	villages <- c("Cuttack 1"="Pikala", "Bargarh 1"="Godabhanga", "Bargarh 2"="Bhuintikira", "Bargarh 3"="Gadtiapalli", "Bargarh 4"="Kujapalli", "Puri 1"="Khandighar", "Puri 2"="Rua", "Puri 3"="Talajanga", "Jagatsinghpur 1"="Ersama", "Jagatsinghpur 2"="Siha", "Angul 1"="Baluakata", "Nuapada 1"="Mahua Bhat")

	d <- data.frame(
		hhid = paste(r1$District, r1$Type, r1$Farmer, sep="-"),
		farm_size_class_ = r1$Type,
		adm2 = r1$District,
		season = c("kharif", "rabi", "pre-kharif")[r1$Season],
		parcel = r1$Parcel,
		plot = r1$Plot,
		plot_id = paste(r1$Parcel, r1$Plot, r1$Subplot, sep="-"),
		crop = crops[as.character(r1[["Crop...9"]])],
		intercropped = r1$Cropping == 2,
		variety_type = c("modern", "traditional")[r1$MVTV],
		field_size = r1$Area,
		planting_method = c("direct seeding", "direct seeding", "transplanting")[r1$CEMethod],
		establishment_method_ = c("DSR", "WSR", "TPR")[r1$CEMethod],
		planting_month_ = r1$CEMon,
		planting_week_ = r1$CEWk,
		harvest_month_ = r1$HarvMon,
		harvest_week_ = r1$HarvWk,
		yield = r1[["Prod...19"]] * 100 / r1$Area,
		fwy_residue = r1[["Byprod...20"]] * 100 / r1$Area,
		crop_price = r1[["Prod...21"]]
	)
	d$variety <- ifelse(r1$MVTV == 1, mv[r1$Variety], tv[r1$Variety])
	d$variety[d$crop != "rice"] <- NA
	d$variety_type[d$crop != "rice"] <- NA

	## plot characteristics
	lh <- data.frame(
		hhid = paste(r2$District, r2$Type, r2$Farmer, sep="-"),
		parcel = r2$Parcel,
		plot = r2$Plot,
		topography_ = c("upland (bunded)", "upland (unbunded)", "medium", "lowland", "very lowland")[r2$Topog],
		soil_texture = c("clay", "clay loam", "sandy clay loam", "sandy loam", "loamy sand", "loam", "sand")[r2$Soiltype],
		irrigation_source = c("canal", "deep tube well", "shallow tube well", "none")[r2$IrrigSrc],
		land_tenure = c("owned", "sharecropped", "rented", "rented out")[r2$Tenure]
	)
	lh <- unique(lh)
	lh <- lh[!duplicated(lh[, c("hhid", "parcel", "plot")]), ]
	lh$irrigated <- lh$irrigation_source != "none"
	d <- merge(d, lh, by=c("hhid", "parcel", "plot"), all.x=TRUE)

	## fertilizer
	q <- function(i) { x <- r3[[i]]; x[is.na(x)] <- 0; x }
	ha <- r3$Area
	fert <- data.frame(
		hhid = paste(r3$District, r3[["Type...1"]], r3$Farmer, sep="-"),
		parcel = r3$Parcel,
		plot = r3$Plot,
		season = c("kharif", "rabi")[r3$Season],
		seed_rate = r3$SeedQty / ha,
		OM_amount = q(17) / ha,
		N_fertilizer = (0.46*q(22) + 0.18*q(30) + 0.28*q(38) + 0.21*q(46) + 0.10*q(50)) / ha,
		P_fertilizer = (0.16*q(26) + 0.46*q(30) + 0.28*q(38) + 0.26*q(50)) / 2.29 / ha,
		K_fertilizer = (0.60*q(34) + 0.26*q(50)) / 1.2051 / ha
	)
	fert$OM_type <- ifelse(fert$OM_amount > 0, "farmyard manure", "none")
	ftype <- cbind(ifelse(q(22) > 0, "urea", NA), ifelse(q(26) > 0, "SSP", NA), ifelse(q(30) > 0, "DAP", NA), 
		ifelse(q(34) > 0, "KCl", NA), ifelse(q(38) > 0, "28-28-0", NA), ifelse(q(42) > 0, "unknown", NA), 
		ifelse(q(46) > 0, "AS", NA), ifelse(q(50) > 0, "10-26-26", NA))
	fert$fertilizer_type <- apply(ftype, 1, \(x) if (all(is.na(x))) "none" else paste(na.omit(x), collapse="; "))
	rocord <- q(42) > 0
	fert[rocord, c("N_fertilizer", "P_fertilizer", "K_fertilizer")] <- NA
	fert$fertilizer_type <- gsub("unknown", "Rocord", fert$fertilizer_type)
	fert <- fert[!duplicated(fert[, c("hhid", "parcel", "plot", "season")]), ]
	d <- merge(d, fert, by=c("hhid", "parcel", "plot", "season"), all.x=TRUE)
	d$parcel <- d$plot <- NULL

	## village
	hh <- r4[r4$Relation %in% 0, ]
	hh <- data.frame(hhid = paste(hh$District, hh$Type, hh[["Farmer...5"]], sep="-"), village = villages[paste(hh$District, hh$Village)])
	hh <- hh[!duplicated(hh$hhid), ]
	d <- merge(d, hh, by="hhid", all.x=TRUE)
	d$adm4 <- d$village
	d$village <- NULL
	d$adm3 <- blocks[d$adm2]

	d$country <- "India"
	d$adm1 <- "Odisha"
	d$currency <- "INR"
	d$trial_id <- d$hhid
	d$on_farm <- TRUE
	d$is_survey <- TRUE
	d$yield_part <- "grain"
	d$yield_part[d$crop %in% c("mung bean", "black gram", "horse gram", "pigeon pea")] <- "seed"
	d$yield_part[d$crop == "groundnut"] <- "pod"
	d$yield_part[d$crop == "jute"] <- "stems"
	d$yield_part[d$crop == "none"] <- "none"

	carobiner::write_files(path, meta, d)
}
