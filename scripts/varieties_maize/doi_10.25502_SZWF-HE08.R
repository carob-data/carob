# R script for "carob"

# border tbd

carob_script <- function(path) {

"Yield gains and associated changes in an early yellow bi-parental maize population following Genomic Selection for Striga resistance and drought tolerance."
				
	uri <- "doi:10.25502/szwf-he08"
	group <- "varieties_maize"	
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
 	    publication="doi:10.1186/s12870-019-1740-z",
		carob_contributor = "Siyabusa Mkuhlani",
		carob_effort = NA,
		carob_date="2024-01-17",
		data_type = "experiment",
		response_vars = "yield",
		treatment_vars = "variety;longitude;latitude",
		project=NA,
		data_organization="IITA"
	)

	read_data <- function(f) {
		r <- read.csv(f)
		colnames(r) <- toupper(colnames(r))
		d <- data.frame(
			trial_id=gsub(" ", "_", gsub(".csv", "", basename(f))),
			location = r$LOC,
			## RH: were all locations planted on 04-01?
			## that seems rather unlikely.
			planting_date = paste(r$YEAR, "04", "01", sep="-"),
			variety = r$PEDIGREE,
			rep = r$REP,
			yield = r$YIELD,
			anthesis_days = r$POLLEN,
			silking_days = r$DYSK,
			asi = r$ASI,
			plant_height = r$PLHT,
			ear_height = r$EHT,
			aspect = r$PLASP,
			ear_aspect = r$EASP, 
			e_rot = r$E_ROT
		)

		if (is.null(r$RL_PERC)) {
			d$root_lodging <- ifelse(is.na(r$RL * 100), NA_character_, paste0(r$RL * 100, "%"))

		} else {
			d$root_lodging <- ifelse(is.na(r$RL_PERC * 100), NA_character_, paste0(r$RL_PERC * 100, "%"))

		}
		if (is.null(r$SL_PERC)) {
			d$stalk_lodging <- ifelse(is.na(r$SL * 100), NA_character_, paste0(r$SL * 100, "%"))
		} else {
			d$stalk_lodging <- ifelse(is.na(r$SL_PERC * 100), NA_character_, paste0(r$SL_PERC * 100, "%"))
		}
		hc <- r$HC
		if (is.null(hc)) hc <- r$HUSK
		if (!is.null(hc)) d$husk <- as.character(hc)
		d
	}

	
	#data for IKDS
	f1 <- ff[grep("EARLY.Drought.csv|Extra.Early.Drought.csv", basename(ff), ignore.case=TRUE)]
	stopifnot(length(f1) == 2)
	d1 <- do.call(rbind, lapply(f1, read_data))
	d1$adm1 <- "Ogun"
	d1$adm2 <- "Ikenne"
	d1$longitude <- 3.711
	d1$latitude <- 6.872

#data set2 Early Drought+Heat.csv

	f2 <- ff[grep("Early.DroughtHeat.csv|Extra.Early.DroughtHeat.csv|Extra.Early.Drought.Heat.csv|Early.Heat.csv", basename(ff), ignore.case=TRUE)]
#	stopifnot(length(f2) == 3)

	d2 <- do.call(carobiner::bindr, lapply(f2, read_data))

	d2$adm1 <- "Kano"
	d2$adm2 <- "Garum Mallam"
	d2$location <- "Kadawa"
	d2$longitude <- 8.448
	d2$latitude <- 11.645

	d <- rbind(d1, d2)
	
	d$country <- "Nigeria"
	d$yield_part <- "grain"
	d$N_fertilizer <- 120
	d$P_fertilizer <- 60
	d$K_fertilizer <- 60
	d$crop <- "maize"
#	d$pl_st <- NA
	d$striga_trial <- TRUE
	d$striga_infected <- TRUE
	d$borer_trial <- FALSE

	d$on_farm <- NA
	d$is_survey <- FALSE
	d$irrigated <- FALSE

	d$variety[d$variety == "Check 3 - 2015 TZE \x96Y DT STR Syn C0"] = "Check 3 - 2015 TZE DT STR Syn C0"

	d$yield_moisture <- as.numeric(NA)
	d$yield_isfresh <- NA
	d$geo_from_source <- FALSE

	d$record_id <- as.integer(seq_len(nrow(d)))
	long <- carobiner::get_function("mz_pd_long", path, group)(d)
	d[intersect(c("e_rot", "blight", "curv", "gls", "rust", "streak", "borer",
		"str_co1", "str_co2", "str_co3", "str_rat1", "str_rat2"), names(d))] <- NULL
	carobiner::write_files(meta, d, path=path, long=long)
}


