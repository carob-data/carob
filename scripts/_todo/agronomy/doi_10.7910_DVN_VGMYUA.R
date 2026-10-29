# R script for "carob"
# license: GPL (>=3)

## ISSUES
# GY and Biomass treated as g/m2 (IRRI plot units) and converted *10 to kg/ha; confirm in paper.
# PlantHt has values to 1888 cm (out of bounds); left as recorded.
# N/P/K fertilizer rates not in the files; omitted (do not guess screening protocol).
# Disease SES scores (blast, bacterial blight races) not reshaped; see disease compiled sheet.
# Some GY cells were "NF"/"Missing"; read as NA.
# Excel Rep column reads as logical TRUE/NA; omitted.
# write_files: missing yield_moisture, N/P/K, harvest_date; NA yield.


carob_script <- function(path) {

"
Replication Data for: Selection of rice (Oryza sativa L.) genebank accessions based on response to multiple drought types and diseases, multilocation testing, and determination of breeding values

Although multi-location testing is rarely used to assess potential donor lines/landraces, it may improve the rigor of pinpointing the most promising stress-tolerant accessions. Furthermore, consideration of additional traits such as disease resistance and agronomic traits may reduce the amount of further improvement required after crossing landraces with an elite line. This study was conducted to address these factors that affect the efficiency and effectiveness of using rice (Oryza sativa L.) landraces as stress tolerance donors. We conducted a series of drought donor screenings starting with six separate batches of ∼100 accessions (one batch per year) at the International Rice Research Institute, Philippines, that were evaluated under reproductive stage lowland and upland drought, seedling stage upland drought, lowland well-watered conditions, and in response to bacterial blight and blast, followed by multi-location testing across 11 drought-prone sites in India, Bangladesh, and Nepal. The selected lines from all batches were then evaluated together under the three types of drought and in a natural blast nursery in India. Genome-wide association study was conducted on yield data from the ∼100 selections per year with available sequence data. Total genetic values and genomically estimated breeding values were determined, and example target yield and agronomic values were used to calculate selection index values for each accession. This dataset could be used by any breeding program by adjusting the target agronomic values based on the specific aims of individual breeding programs in order to identify donors for drought tolerance while taking into account the needs of the targeted market segment.
"

	uri <- "doi:10.7910/DVN/VGMYUA"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "IRRI",
		publication = "doi:10.1002/csc2.70332",
		project = NA,
		design = "multilocation drought and disease donor screening",
		data_type = "experiment",
		treatment_vars = "variety;stress",
		response_vars = "yield;plant_height;flowering_days;harvest_index",
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.6",
		carob_completion = 65,
		carob_effort = 0.7
	)

	f <- ff[basename(ff) == "Drought Donors MASTER all 3 sets agronomic 04072025.xlsx"]
	r <- carobiner::read.excel(f, sheet = "Drought Donors MASTER", na = c("NF", "Missing", "NA"))

	seas <- as.character(r$Season)
	d <- data.frame(
		season = ifelse(grepl("WS", seas), "wet", ifelse(grepl("DS", seas), "dry", NA)),
		season_raw = seas,
		site = trimws(r$Site),
		treatment = trimws(r$Trt),
		plot_id = as.character(r$Plot),
		variety = trimws(r$Genotype),
		variety_code = trimws(r[["IRGC #"]]),
		plant_height = r$PlantHt,
		flowering_days = r$DTF,
		# GY and Biomass appear to be g/m2
		yield = r$GY * 10,
		harvest_index = r$HI * 100,
		dmy_total = r$Biomass * 10,
		crop = "rice",
		yield_part = "grain"
	)

	d$stress <- ifelse(d$treatment == "WW Lowland", "none", "drought")
	d$flooded <- grepl("Lowland", d$treatment)
	d$irrigated <- grepl("Lowland", d$treatment)
	d$on_farm <- FALSE
	d$is_survey <- FALSE

	geo <- data.frame(
		site = c("IRRI", "BAU,Sabour", "BHU, Varanasi", "BRRI, Rajshahi",
			"CRURRS, Hazaribag", "DRR,Hyderabad", "IIRR,Hyderabad",
			"ICAR, Patna", "ICAR, Tripura", "IGKV, Raipur",
			"Nepalgunj", "NRRI, Cuttack", "NRRP, Hardinath"),
		country = c("Philippines", "India", "India", "Bangladesh",
			"India", "India", "India",
			"India", "India", "India",
			"Nepal", "India", "Nepal"),
		location = c("Los Baños", "Sabour", "Varanasi", "Rajshahi",
			"Hazaribagh", "Hyderabad", "Hyderabad",
			"Patna", "Lembucherra", "Raipur",
			"Nepalgunj", "Cuttack", "Hardinath"),
		latitude = c(14.167, 25.242, 25.268, 24.375,
			24.010, 17.320, 17.320,
			25.595, 23.907, 21.236,
			28.050, 20.454, 26.806),
		longitude = c(121.255, 87.043, 82.991, 88.613,
			85.353, 78.394, 78.394,
			85.090, 91.310, 81.701,
			81.617, 85.933, 85.973)
	)
	d <- merge(d, geo, by = "site", all.x = TRUE)
	d$geo_from_source <- FALSE
	d$trial_id <- paste(d$site, d$season_raw, d$treatment, sep = "_")
	# Season year as planting_date when full date is not given
	d$planting_date <- gsub("[^0-9]", "", d$season_raw)
	d$season_raw <- NULL

	d$site <- NULL
	d$variety_code[d$variety_code == ""] <- NA

	carobiner::write_files(path, meta, d)
}
