# R script for "carob"

carob_script <- function(path) {

"This experiments were established with different rates of nitrogen in order to generate a wide range of values for NDVI and grain yield in order to develop a calibration model for the GreenSeeker in Yaqui Valley. (2022-10-17)"

	uri <- "hdl:11529/10548811"
	group <- "agronomy"

	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=0,
		data_organization = "CIMMYT",
		publication = NA,
		project = NA,
		data_type = "experiment",
		response_vars = "yield",
		treatment_vars = "N_fertilizer", 
		carob_contributor = "Blessing Dzuda",
		carob_date = "2024-06-25"
	)
	
	f <- ff[basename(ff) == "GreenSeeker Sonora 2000-2014.xlsx"]

	read_data <- function(sheetname, id) {  
		# note read.excel.hdr and fix_names=TRUE
		r <- carobiner::read.excel.hdr(f, sheet = sheetname, skip=1, hdr =1, fix_names=TRUE)
		data.frame(
			trial_id= as.character(id),
			adm2=r$Municipality,
			location=r$Locality,
			site=r$Geographical.location,
			planting_date=r$Planting.Date,
			land_prep_method=r$Tillage,
			variety=r$Hibrid, 
			planting_method=r$Planting.method,
			seed_density=r$planting.density.Kg.ha,
			rep=r$Inf.Experiment_REP,
			treatment=r$Number.of.Treatment,
			Zn_fertilizer=r$Rate.Zn,
			N_fertilizer=r$Rate.N.kg.ha,
			yield=r$Yield.at.12pct.hum
		)
	}
	
	d1 <- read_data("2000-2001", 1)
	## to avoid a warning
	d1$planting_date[d1$planting_date == "-"] <- NA
	d1$planting_date <- as.numeric(d1$planting_date)
	## mysterious. 
	## This returns the correct date as in the spreadsheet but why do we need the odd origin??
	d1$planting_date <- as.Date(d1$planting_date,origin = "1904-01-01" )
	d2 <- read_data("2001-2002", 2)
	d3 <- read_data("2004-2005", 3)
	d4 <- read_data("2005-2006", 4)
	d5 <- read_data("2006-2007", 5)
	d6 <- read_data("2007-2008", 6)
	d7 <- read_data("2008-2009", 7)
	d8 <- read_data("2009-2010", 8)
	d9 <- read_data("2010-2011", 9)
	d10 <- read_data("2013-2014", 10)
	
	d <- rbind(d1, d2, d3, d4, d5, d6, d7, d8, d9, d10)
	d <- unique(d)
	
	d$country="Mexico"
	# CIMMYT Obregon
	d$longitude=-109.9318
	d$latitude=27.3719
	d$geo_from_source = FALSE
	d$crop="wheat"
	d$yield_part="grain"

	d$on_farm <- FALSE
	d$is_survey <- FALSE
	d$irrigated <- TRUE

	residue <- rep("unknown", nrow(d))
	residue[grep("burn straw", d$planting_method)] <- "burned straw"
	residue[grep("incorporated", d$planting_method)] <- "incorporated straw"
	residue[grep("remove straw", d$planting_method)] <- "removed straw"
	residue[grep("retain straw", d$planting_method)] <- "retained straw"
	d$previous_crop_residue_management <- residue
	
	d$land_prep_method <- gsub("Conservation", "reduced tillage",d$land_prep_method)
	d$land_prep_method <- gsub("Conventional", "conventional",d$land_prep_method)
	d$land_prep_method <- gsub("-", "unknown",d$land_prep_method)
	d$planting_method <- gsub("-", "unknown",d$planting_method)
	d$planting_method <- gsub("Basin irrigation", "basins", d$planting_method)
	d$planting_method[grep("Bed", d$planting_method)] <- "raised beds"

	d$planting_date <- as.character(d$planting_date)
	## avoid warnings
	d$rep[d$rep == "-"] <- NA
	d$rep <- as.integer(d$rep)

	d$seed_density[d$seed_density == "-"] <- NA
	d$seed_density <- as.numeric(d$seed_density)

## you cannot just this and lose all the data!
#	d$Zn_fertilizer <- as.numeric(d$Zn_fertilizer)
#	d$N_fertilizer <- as.numeric(d$N_fertilizer)
	
	d$Zn_fertilizer[d$Zn_fertilizer=="-"] <- 0
	d$Zn_fertilizer[grep("Three foliar applications", d$Zn_fertilizer)] <- "3"
	d$Zn_fertilizer <- gsub(" ZnSO4| kg ZnSO4| kg Zn SO4| protected with glycines", "", d$Zn_fertilizer)
	d$Zn_fertilizer <- as.numeric(d$Zn_fertilizer) * .4053  # from ZnSO4 to Zn

	N <- d$N_fertilizer
	N <- gsub("N  pre-plant |mid high|mid N|-", " ", N, ignore.case = TRUE)
	N <- gsub("urea.*.applied.*", " ", N, ignore.case = TRUE)
	N[grep("150 N Basal", N)] <- "150"
	N <- gsub("before 1st post.plant irrigation.*|applied 5 days before.*", " ", N, ignore.case = TRUE)
	N <- gsub("N at platning|N at planting|N pre plant|N  pre plant", " ", N, ignore.case = TRUE)
	N <- gsub("0N applied.*", "0", N)
	N <- gsub("kg N foliar.*", "", N)
	N <- gsub("(", "+", N, fixed=TRUE)
	N <- trimws(gsub("N", "", N))
	N[N==""] <- NA
	N <- sapply(N, \(i) eval(parse(text=i)), USE.NAMES=FALSE)
	d$N_fertilizer <- N 
	d$fertilizer_type <- "none"
	d$fertilizer_type[d$N_fertilizer > 0] <- "urea"
	i <- which(d$Zn_fertilizer > 0)
	d$fertilizer_type[i] <- paste0(d$fertilizer_type[i], ";ZnSO4")
	d$fertilizer_type <- gsub("none;", "", d$fertilizer_type)

# could be added to capture the foliar applications
#	d$fertilization_method

	d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)

	# removing one missing yield value
	d <- d[!is.na(d$yield), ]
	d$yield_moisture <- as.numeric(NA) #needs to be checked

	carobiner::write_files(path, meta, d)
}
