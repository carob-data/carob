# R script for "carob"
# license: GPL (>=3)


carob_script <- function(path) {

"
Replication Data for: Multi-location screening of rice genotypes for grain and straw P concentrations under different levels of P supply

Result of a multi-location study across Africa and Asia where different varieties were screened under different soil P conditions and P treatments, and the effect on grain and straw P concentration was evaluated.
"

	uri <- "doi:10.7910/DVN/AMAZXA"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)


## supp materials at journal are better 

	p <- dirname(ff)[1]
	baseurl <- "https://www.frontiersin.org/api/v4/articles/216077/file/"
	f1 <- file.path(p, "216077_supplementary-materials_tables_1.xlsx")
	if (!file.exists(f1)) {
		download.file(paste0(baseurl, "Table_1.XLSX/216077_supplementary-materials_tables_1_xlsx/1"), f1, mode="wb")
		ff <- c(ff, f1)
	}
	f2 <- file.path(p, "216077_supplementary-materials_datasheets_1.xlsx")
	if (!file.exists(f2)) {
		download.file(paste0(baseurl, "Data_Sheet_1.XLSX/216077_supplementary-materials_datasheets_1_xlsx/1"), f2, mode="wb")
		ff <- c(ff, f2)
	}
	
	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "AfricaRice; JIRCAS; SCU; NARI; INERA; UPE; RRDI; IRRI",
		publication = "doi:10.3389/fpls.2016.01435",
		project = NA,
		carob_date = "2025-07-15",
		design = "RCBD; Alpha lattice",
		data_type = "experiment",
		treatment_vars = "P_fertilizer; variety",
		response_vars = "yield", 
		carob_contributor = "Robert Hijmans",
		completion = 0
	)
	
	f1 <- ff[basename(ff) == "216077_supplementary-materials_datasheets_1.xlsx"]
	f2 <- ff[basename(ff) == "216077_supplementary-materials_tables_1.xlsx"]
	r2 <- carobiner::read.excel(f2)
	
	i <- which(!is.na(r2[1,]))
	names(r2)[i] <- r2[1,i]
	r2 <- r2[-1,]

	readfun <- function(f, sheet, value) {
		r <- carobiner::read.excel(f, sheet=sheet, n_max=8)
		h <- data.frame(t(r[, -1]))
		colnames(h) <- r[, 1]
		r <- carobiner::read.excel(f, sheet=sheet, skip=10)
		r <- r[1:(which(is.na(r$Variety))-1), ] 
		d <- data.frame(t(r[, -1]))
		colnames(d) <- r[, 1]
		x <- cbind(h[,-1], d)
		reshape(x, direction="long", varying=names(d), timevar="variety", times=names(d), v.names=value, idvar= "observation")
	}	
	
	d1 <- readfun(f1, "Grain yield", "yield")
	d2 <- readfun(f1, "Straw biomass", "straw")
	d3 <- readfun(f1, "Grain P concentration", "grainP")
	d4 <- readfun(f1, "Straw P concentration", "strawP")
	x <- cbind(d1, d2["straw"], d3["grainP"], d4["strawP"])	
	x$observation <- NULL


	d <- data.frame(
		ID = x$Env,
		trial_id = x$Trial, 
		country = x$Country, 
		location = x$Site,
		planting_date = x$Year,
		flooded = x[,"Rice growing environment"] == "Lowland",
		treatment = x[,"P rate"],
		variety = x$variety,
		yield = x$yield * 10,
		dmy_residue = x$straw, 
		grain_P = x$grainP,
		residue_P = x$strawP
	)

	d$yield[d$yield < 0] <- NA
	keep <- rowSums(is.na(d[, c("yield", "dmy_residue", "grain_P", "residue_P")])) < 4
	d <- d[keep, ]
	d$country[d$country == "The Gambia"] <- "Gambia"


	r2[r2 == "nd"] <- NA	
	d5 <- data.frame(
		ID = r2$Environment,
		season = tolower(r2$Season),
		soil_pH = as.numeric(r2$pH),
		soil_P = as.numeric(r2[,"Bray-P (mg kg-1)"]),
		N_fertilizer = as.numeric(r2$N),
		P_fertilizer = as.numeric(r2$P),
		K_fertilizer = as.numeric(r2$K),
		design = r2[,"Exp design"],
		rep = as.integer(r2[,"No of replicates"]),
		plot_area = r2[,"Plot dimensions (m x m)"],
		row_spacing = r2[,"Hill spacing (cm x cm)"] 
	)
		
	d5$plot_area <- gsub("×|x", "*", d5$plot_area)
	d5$plot_area <- gsub("m2", "", d5$plot_area)
	d5$plot_area <- sapply(d5$plot_area, \(txt) eval(parse(text = txt)))

	d5$row_spacing <- gsub("x", "*", d5$row_spacing)
	d5$row_spacing <- sqrt(sapply(d5$row_spacing, \(txt) eval(parse(text = txt))))
		
	d <- merge(d, d5, by="ID")

	geo <- data.frame(
		location = c('Bohicon', 'Pangil', 'Ibadan', 'Cotonou', 'Farako-Ba', 'Ikenne', 'Dakawa', 'Ruvu', 'Tsukuba', 'Bathalagoda', 'Yundum'),
		longitude = c( 2.073, 121.463, 3.899, 2.322, -4.344, 3.706, 37.537, 38.654, 140.073, 80.436, -16.677),
		latitude = c(7.185, 14.399, 7.504, 6.368, 11.0, 6.861, -6.437, -6.809, 36.076, 7.532, 13.350)
	)

	d <- merge(d, geo, by="location")
	d$is_survey <- FALSE
	d$geo_from_source <- FALSE

	d$crop <- "rice"
	d$yield_part <- "grain"
	
	d$on_farm <- FALSE
	d$irrigated <- TRUE
	d$irrigated[d$country %in% c("Gambia", "Burkina Faso")] <- FALSE
	d$ID <- d$design <- NULL
	
	d$yield_moisture <- as.numeric(NA) #needs to be checked

	carobiner::write_files(path, meta, d)
}

