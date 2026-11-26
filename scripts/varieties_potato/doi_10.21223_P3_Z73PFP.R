# R script for "carob"
# license: GPL (>=3)

## ISSUES
# - The description mentions three locations, but the dataset has only one location (Kachwekano ZARDI).
# - Two disease categories in RS9-ERA-NTO ("early blight", "virus") are not yet in terminag values_disease.csv.

carob_script <- function(path) {

"
Dataset for: Multi-location confined field trials (ML-CFTs) for dossier development of biotech late blight resistant potato in Uganda

Transgenic potato Vic.1 carries three resistance (R) genes from wild potato relatives that were introduced to confer resistance against potato late blight (LB) disease caused by Phytophthora infestans. Preliminary field trials show that the introduction of these R genes indeed confers extreme resistance to P. infestans. In this regulatory studies, we aim to confirm the effectiveness of the introduction of the R genes in Vic.1 by conducting confined field trials during two seasons at three locations.
"

	uri <- "doi:10.21223/P3/Z73PFP"
	group <- "varieties_potato"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		data_organization = "CIP; NARO",
		publication = NA,
		project = "3R potato ML-CFT",
		design = "RCBD",
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "yield;disease_incidence;pest_incidence;plant_height",
		carob_contributor = "Premrose Masunungure",
		carob_date = "2026-08-26",
		carob_completion = 70,
		carob_effort = 4
	)

	f2 <- ff[basename(ff) == "Data.xls"]
	r2b <- carobiner::read.excel(f2, sheet="RS 8 9 AGRO ERA-Plant Dev", na="No data")
	r2c <- carobiner::read.excel(f2, sheet="RS9-ERA-NTO")
	r2e <- carobiner::read.excel(f2, sheet="Harvest data")

	parse_dt <- function(x) {
		x <- trimws(as.character(x))
		x <- gsub("/18$", "/2018", x)
		out <- rep(NA_character_, length(x))
		i1 <- !is.na(x) & grepl("/", x)
		i2 <- !is.na(x) & grepl("-", x)
		i3 <- !is.na(x) & grepl("^[0-9]+(\\.[0-9]+)?$", x)
		if (any(i1)) out[i1] <- as.character(as.Date(x[i1], format="%d/%m/%Y"))
		if (any(i2)) out[i2] <- as.character(as.Date(x[i2], format="%m-%d-%Y"))
		if (any(i3)) out[i3] <- as.character(as.Date("1899-12-30") + as.integer(as.numeric(x[i3])))
		out
	}

	clean_var <- function(x) {
		x <- trimws(as.character(x))
		x <- gsub("820046\\.09999999998", "820046.1", x)
		x <- gsub("381381\\.20000000001", "381381.20", x)
		x
	}

	# Harvest data: 32 individual plants per 6.75 m2 plot (4 rows x 8 plants, 75 x 30 cm)
	r2e <- r2e[!is.na(r2e$Plot), ]
	r2e$plant_wt_g <- rowSums(r2e[, c("Weight of small tubers (g)", "Weight of medium tubers (g)",
		"Weight of large tubers (g)")], na.rm=TRUE)
	d <- aggregate(plant_wt_g ~ Plot, data=r2e, FUN=sum)
	d$plot_id <- as.character(d$Plot)
	d$yield <- d$plant_wt_g / 0.675  # g per 6.75 m2 -> kg/ha
	d$variety <- clean_var(r2e$Genotype[match(d$Plot, r2e$Plot)])
	d$flesh_color <- tolower(r2e$`Flesh colour`[match(d$Plot, r2e$Plot)])
	d$Plot <- NULL
	d$plant_wt_g <- NULL

	d$trial_id <- "1"
	d$plot_area <- 6.75
	d$row_spacing <- 75
	d$plant_spacing <- 30
	d$on_farm <- FALSE
	d$is_survey <- FALSE
	d$irrigated <- NA
	d$country <- "Uganda"
	# location provided in sheet "CFT Data"
	d$location <- "Kachwekano ZARDI"
	d$longitude <- 29.942
	d$latitude <- -1.254
	d$geo_source <- "Google maps"
	d$geo_from_source <- FALSE
	d$planting_date <- "2017-11-27"
	d$harvest_date <- "2018-03-20"
	d$P_fertilizer <- d$K_fertilizer <- d$N_fertilizer <- as.numeric(NA)
	d$yield_part <- "tubers"
	d$yield_moisture <- as.numeric(NA)
	d$crop <- "potato"
	d$yield_isfresh <- TRUE

	# Long table 1: plant development & late blight incidence over time
	r2b <- r2b[!is.na(r2b$Plot) & !is.na(r2b$`Actual date`), ]
	dl_lb <- data.frame(
		trial_id = "1",
		plot_id = as.character(r2b$Plot),
		date = parse_dt(r2b$`Actual date`),
		disease = "potato late blight",
		disease_incidence = as.character(r2b$`Incidence of LB (%)`),
		plant_height = as.numeric(r2b$`Estimated average plant height (cm)`)
	)

	# Long table 2: diseases and pests from RS9-ERA-NTO (split into disease vs pest_species)
	r2c <- r2c[!is.na(r2c$Plot), ]
	r2c_date <- parse_dt(r2c$`Actual date`)
	r2c_dap <- as.integer(r2c$`Recommended date (dap)`)

	dis_lookup <- c(
		"Incidence of early blight (%)"   = "early blight",
		"Incidence of Rhizoctonia (%)"    = "rhizoctonia",
		"Incidence of bacterial wilt (%)" = "bacterial wilt",
		"Incidence of black leg (%)"      = "black leg",
		"Incidence of virus (%)"          = "virus"
	)
	dl_dis <- do.call(rbind, lapply(names(dis_lookup), function(cn) {
		data.frame(
			trial_id = "1",
			plot_id = as.character(r2c$Plot),
			date = r2c_date,
			DAP = r2c_dap,
			disease = dis_lookup[[cn]],
			disease_incidence = as.character(r2c[[cn]])
		)
	}))

	pest_lookup <- c(
		"Incidence of aphids (%)"           = "aphid",
		"Incidence of white flies (%)"      = "whitefly",
		"Incidence of leaf miner flies (%)" = "Liriomyza huidobrensis",
		"Incidence of moth (%)"             = "potato tuber moth"
	)
	dl_pest <- do.call(rbind, lapply(names(pest_lookup), function(cn) {
		data.frame(
			trial_id = "1",
			plot_id = as.character(r2c$Plot),
			date = r2c_date,
			DAP = r2c_dap,
			pest_species = pest_lookup[[cn]],
			pest_incidence = as.integer(r2c[[cn]])
		)
	}))

	d_long <- carobiner::bindr(dl_lb, dl_dis, dl_pest)

	carobiner::write_files(path, meta, d, long=d_long)
}

