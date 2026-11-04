# R script for "carob"
# license: GPL (>=3)

## NOTES
# CPWF-IFAD baseline household survey (2006), Luang Prabang, Laos; districts Xiengngen (village Silalek) and Pak Ou (village Fay), ~150 farmers.
# Sheets are "Excel forms" with several header rows; short variable names are in the last header row.
# Main records: "Landhold & Landuse.xls" sheet "Landuse" (lowland 2005 and upland parcels 2001-2005). One row per household x environment x year x season x parcel x plot x crop.
# yield = Prod (kg) / Area (ha). Rice production is taken to be paddy (not stated explicitly).
# Crop, variety, establishment codes decoded from "Code description.xls". Variety is sometimes a name (e.g. "Khaikhiet") or two codes ("2,3"); kept as given (codes decoded).
# Fertilizer from "Rice input.xls" sheet RMatInput (rice, 2005), merged by district, farmer, land type, season, parcel and plot.
# FertType1 was typed as N-P-K grade and Excel turned it into a time: "46:00:00" and 1.9166667 (= 46 hours) are urea 46-0-0; "16:20:00" is 16-20-0 (code description).
# 16-20-0 taken as N-P2O5-K2O grade; P = P2O5 / 2.29.

## ISSUES
# No coordinates. Georeference villages Silalek (Xiengngen district) and Fay (Pak Ou district), Luang Prabang province. Check adm2 spellings against GADM.
# Rice production may be paddy or milled; not stated.
# Upland records 2001-2005; fertilizer only merged to 2005 rice records. Lowland plots with DS (dry season) are few (17).
# Variety "2,3"/"7,3" and Glut "1,2" are mixtures on one plot; kept as "a; b".
# Mixed cropping (Cropping = 2): unclear whether Area is the full plot or a share.
# Not processed: Asset, Changes & Problems, Consumption, Credit, Crop disposal, Farm practices, Food security, HHInfo (head age/sex/ethnicity), Income, Landhold (tenure, irrigation source), Landuse-Garden, Landuse-Others, Nonrice (costs), Rice input RLabor, input costs.
# write_files: missing coordinates; invalid crop "job's tears" and fertilizer_type "16-20-0" (not in terminag); NA yield where Prod is empty (many upland history records).
# The "Wealth" sheet of Code description.xls lists household head names (personal data); not used.

## NEW VARIABLES
# land_type_: lowland or upland parcel
# establishment_method_: rice establishment code (WSR = wet seeded, DSR = dry seeded; "DSR-dibbled" = seeds in holes dug with a tool)


carob_script <- function(path) {

"
CPWF-IFAD Landscape Management in Laos

This farm household survey is an activity under the socio-economic component of the project entitled 'Managing Rice Landscapes in the Marginal Uplands for Household Food Security and Environmental Sustainability'. The objective of the project is to improve food security of farming households in the marginal highlands through more efficient production over the rice landscape. The socio-economic component focuses on analyzing how various component technologies contribute to improving farmers' livelihoods. Specific objectives are: a)  to describe and understand existing farm practices and livelihoods in the marginal uplands; b) to identify possible points of intervention for improving productivity and food security and initiating income growth and c) to serves as a baseline for future impact assessment.
"

	uri <- "doi:10.7910/DVN/26160"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=3, minor=1,
		data_organization = "IRRI",
		publication = NA,
		project = "CPWF-IFAD",
		design = NA,
		data_type = "survey",
		treatment_vars = NA,
		response_vars = NA,
		notes = NA,
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.6",
		carob_completion = 35,
		carob_effort = 0.4
	)

	f1 <- ff[basename(ff) == "Landhold & Landuse.xls"]
	f2 <- ff[basename(ff) == "Rice input.xls"]
	r1 <- carobiner::read.excel(f1, sheet="Landuse", skip=9)
	r2 <- carobiner::read.excel(f2, sheet="RMatInput", skip=7)

	crops <- c("1"="rice", "2"="maize", "3"="job's tears", "4"="sesame", "5"="banana", "7"="soybean", "8"="groundnut", "11"="cucumber")
	varieties <- c("1"="TDK1", "2"="Khao Phekhao", "3"="Khao Chaoleuang", "4"="Khao Phedeng", "5"="Khao metgnay",
		"6"="Khaonone", "7"="Khao nok", "8"="Khao metam", "9"="Khao none noy", "10"="Khaochao Na Ngao")
	decode <- function(x, codes) {
		sapply(strsplit(as.character(x), ","), function(i) {
			if (all(is.na(i))) return(NA)
			v <- ifelse(i %in% names(codes), codes[i], i)
			paste(v, collapse="; ")
		})
	}

	d <- data.frame(
		district = r1$District,
		farmer = r1$Farmer,
		land_type_ = tolower(r1$Environment),
		planting_date = as.character(r1$Year),
		season = c(DS="dry", WS="wet")[r1$Season],
		field_id = r1$Parcel,
		plot_id = as.character(r1$Plot),
		crop = crops[as.character(r1$Crop)],
		intercropped = r1$Cropping == 2,
		field_size = r1$Area,
		yield = r1$Prod / r1$Area,
		variety = decode(r1$Variety, varieties),
		variety_type = c("modern", "traditional")[r1$MVTV],
		variety_traits = decode(r1$Glut, c("1"="glutinous", "2"="non-glutinous")),
		establishment_method_ = c("3"="WSR", "4"="DSR-dibbled")[as.character(r1$CEMethod)]
	)
	d$planting_method <- c(WSR="direct seeding", "DSR-dibbled"="dibbling")[d$establishment_method_]

	# FertType1 grades that Excel converted to times
	ftype <- c("1.9166666666666667"="urea", "46:00:00"="urea", "16:20:00"="16-20-0")
	fert <- data.frame(
		key = paste(r2$District, r2$Farmer, c("lowland", "upland")[r2$Landtype], r2$Season, r2$Parcel, r2$Plot),
		fertilizer_type = ftype[as.character(r2$FertType1)],
		seed_rate = r2$SeedQty / r2$Area
	)
	fq <- r2$FertQty1
	fq[is.na(fq)] <- 0
	fert$N_fertilizer <- fq * ifelse(fert$fertilizer_type %in% "urea", 0.46, 0.16) / r2$Area
	fert$P_fertilizer <- fq * ifelse(fert$fertilizer_type %in% "16-20-0", 0.20, 0) / 2.29 / r2$Area
	fert$K_fertilizer <- 0
	fert$fertilizer_type[is.na(fert$fertilizer_type)] <- "none"

	d$key <- paste(d$district, d$farmer, d$land_type_, r1$Season, d$field_id, d$plot_id)
	d$key[!(d$crop %in% "rice") | d$planting_date != "2005"] <- NA
	d <- merge(d, fert, by="key", all.x=TRUE)

	d$hhid <- paste(d$district, d$farmer, sep="-")
	d$adm2 <- c("Xiengngen", "Pak Ou")[d$district]
	d$location <- c("Silalek", "Fay")[d$district]
	d$district <- d$farmer <- d$key <- NULL

	d$country <- "Laos"
	d$adm1 <- "Louangphrabang"
	d$trial_id <- d$hhid
	d$on_farm <- TRUE
	d$is_survey <- TRUE
	d$irrigated <- NA
	d$yield_part <- "grain"
	d$yield_part[d$crop %in% c("banana", "cucumber")] <- "fruit"
	d$yield_part[d$crop %in% c("groundnut")] <- "pod"
	d$yield_part[d$crop %in% c("sesame", "soybean")] <- "seed"

	carobiner::write_files(path, meta, d)
}
