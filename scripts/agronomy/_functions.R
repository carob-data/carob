
get_elements_from_product <- function(fertab, products) {
	used <- unique(products) |> strsplit("; ") |> unlist() |> na.omit() |> unique() 	
	stopifnot(!all(fertab$name %in% used))
	fertab <- fertab[fertab$name %in% used, c("name", "N", "P", "K", "S")]
	fmat <- as.matrix(fertab[,-1]) / 100
	out <- matrix(0, ncol=4, nrow=length(products))
	colnames(out) <- paste0(c("N", "P", "K", "S"), "_fertilizer")
	for (fertilizer in used) {
		f <- fmat[fertab$name==fertilizer, ]		
		stopifnot(!any(is.na(f)))
		i <- grep(fertilizer, products)
		out[i, ] <- out[i, ] + rep(f, each=length(i))
	}
	out[is.na(products)] <- NA
	out
}



IRRI_LTE <- function(f, path) {

	uri2 <- "doi:10.7910/DVN/HKX9SF"
	group <- "agronomy"
	ff2  <- carobiner::get_data(uri2, path, group)
	f2 <- ff2[basename(ff2) == "LTCCE-HP_2068-2023.csv"]
	v <- read.csv(f2)
	names(v)[names(v) == "GYtha"] <- "GYkgha"
## 1992 has these "varieties" with otherwise useful info:
## "D1N1 (20x20cm; 2/3 basal, 1/3PI)", "D1N2 (20x20cm; 30% basal, PI and 20% midT and FL)" 
## "D2N2 (15x15cm; 30%basal, PI and 20% midT and FL)"
	v[grep("^D.N.", v$B_label), "B_label"] <- NA

	rr <- do.call(rbind, lapply(grep("\\.csv$", f, value=TRUE), \(i) read.csv(i, na.strings =".")))
	rr$Site[rr$Site == "B5-B8"] <- "IRRI-B5-B8"
	rr <- unique(rr)
	
	stopifnot(nrow(unique(rr[,c("Expt", "Site", "Year")])) == 1)
	
	v <- v[v$Year == rr$Year[1], c("Season", "Afactor", "A_label", "Bfactor", "B_label", "Rep", "GYkgha")]
	comm <- NULL

	r <- merge(rr, v, by=c("Season", "Afactor", "Bfactor", "Rep"), all.x=TRUE)
	#stopifnot(!( any(is.na(r$GYkgha) & (!(is.na(r$GYtha))))))
	if (!all(abs(r$GYkgha/1000 - r$GYtha) < 0.01, na.rm=TRUE)) {
		comm <- "yield differences between the two data sources"
	}

	x <- data.frame(
		LTE_name = r$Expt,
		country="Philippines",
		adm1="Laguna",
		location="Los Baños",
		longitude = 121.2571,
		latitude = 14.1678,
		geo_from_source = FALSE,
		irrigated = TRUE,
		on_farm = FALSE, 
		is_survey = FALSE,
		site = r$Site,
		planting_date = as.character(r$Year),
		harvest_date = as.character(r$Year),
		season = r$Season,
		crop = tolower(r$Crop),
		yield_part = "grain",
		rep = r$Rep,
#		yield = r$GYtha * 1000,
		yield = r$GYkgha,
		N_fert_level = as.integer(gsub("F", "", r$Afactor)),
		variety_code = paste0(r$Year, "_", r$Bfactor),
		variety = r$B_label,
		trial_id = as.character(r$Crop_no)
	)
	# from https://lte.irri.org/ltcce 2025/06/13

	i <- x$season == "DS"
	x$N_fertilizer[i] <- c(0,65,130,195)[x$N_fert_level[i]]
	x$N_fertilizer[!i] <- c(0,45,90,135)[x$N_fert_level[!i]]
	x$N_fert_level <- NULL
	
	if (!all(x$N_fertilizer == r$A_label)) {
		comm <- paste0(c(comm, "fertilizer app differences between sources"), collapse="; ")
	}

	x$P_fertilizer <- 26
	x$K_fertilizer <- 40
	x$Zn_fertilizer <- 5
#	x$fertilizer_type <- "ZnSO4"
	
	x$plant_density <- 250000 # 20x20 cm hills, each with three plants
	x$transplanting_days <- 14
	x$herbicide_used <- TRUE
	
	pmnt <- c("01", "05", "09")
	hmnt <- c("04", "08", "12")
	
	i <- match(x$season, c("DS", "EWS", "LWS"))
	x$planting_date = paste0(x$planting_date, "-", pmnt[i])
	x$harvest_date = paste0(x$harvest_date, "-", hmnt[i])

	x$season <- c("dry", "early wet", "late wet")[i]

	x <- x[!is.na(x$yield), ]
	x$yield_isfresh <- NA
	x$yield_moisture <- as.numeric(NA) 
	
	if (!is.null(comm)) print(comm)
	list(x=unique(x), comm=comm)
}



