
## to do
## extract fertilizer use (and more?) from "description"
## standardize variable names specific to this dataset


intmztrial_striga <- function(ff, sf=NULL) {

	doit <- function(sf, striga=FALSE, borer=FALSE) {
		d  <- read.csv(sf)
		names(d) <- tolower(names(d))
		d$x <- NULL
		names(d)[1] <- "id"
		d$id <- NULL
		d$gwt <- NULL
		if (!is.null(d$str_rat1)) {
			d$str_ra1_in <- NULL
			d$str_ra1_un <- NULL
		}
		if (!is.null(d$str_rat2)) {
			d$str_ra2_in <- NULL
			d$str_ra2_un <- NULL
		}

		if (!is.null(d$str_co1)) {
			d$str_co1_in <- NULL
			d$str_co1_un <- NULL
		}
		if (!is.null(d$str_co2)) {
			d$str_co2_in <- NULL
			d$str_co2_un <- NULL
		}
		if (!is.null(d$str_co3)) {
			d$str_co3_in <- NULL
			d$str_co3_un <- NULL
		}
		
		d <- carobiner::change_names(d, 
		 c("trl_titl", "entry", "entryno", "instinf", "bltin_m", "bltun_m", "x1000gwt", "x_1000gwt", "cobdamco", "cobdamrt", "borerdmrat", "sbdamat", "nitr_rat"), 
		 c("trial_name", "variety", "variety_code", "instin", "blight_in", "blight_un", "seed_weight", "seed_weight", "cob_dam_co", "cob_dam_rt", "borer_dam_rat", "sb_dam_rat", "grain_N")
		 , must_have=FALSE)

		d$planting_date <- d$year
		d$planting_date[d$year==199] <- 1990 #??
		d$planting_date[d$year==2] <- 2000
		d$planting_date[d$year==21] <- 2001
		d$planting_date[d$year==22] <- 2002
		d$planting_date[d$year==23] <- 2003
		d$planting_date[d$year==24] <- 2004
		d$planting_date[d$year==25] <- 2005
		d$planting_date[d$year==26] <- 2006
		d$planting_date[d$year==27] <- 2007
		d$planting_date[d$year==28] <- 2008
		d$planting_date[d$year==29] <- 2009
		d$planting_date[d$year==211] <- 2011
		d$planting_date[d$year==212] <- 2012
		d$planting_date[d$year==213] <- 2013
		d$planting_date[d$year==214] <- 2014
		d$planting_date[d$year==215] <- 2015

		d$planting_date <- as.character(d$planting_date)
		
		d$country <- carobiner::fix_name(d$country, "title")
		d$location <- carobiner::fix_name(d$location, "title")

		d$crop <- "maize"
		d$season <- "first"
		d$on_farm <- FALSE
		d$year <- NULL
		d$aezone <- NULL

		d <- carobiner::change_names(d,  
		 c("str_ra1", "str_ra2", "yld", "r_l", "s_l", "plst", "eld", "borer_dm_rat", "deadheart", "yieldin1", "yieldun2"), 
		 c("str_rat1", "str_rat2", "yield", "rl", "sl", "pl_st", "eldana", "borer_dam_rat", "dead_heart", "yieldin", "yield2"), must_have=FALSE)

		if (!is.null(d$e_dam_rat)) {
			if (all(is.na(d$e_dam_rat))) {
				d$ear_dam_rat <- d$e_dam_rat
			}
			d$e_dam_rat <- NULL
		}

		if (!is.null(d$variety_code)) {
			d$variety_code <- as.character(d$variety_code)
		}
		if (!is.null(d$yield2)) {
			d$yield2 <- as.numeric(d$yield2)	
			dd <- d[!is.na(d$yield2), ]
			if (nrow(dd) > 0) {
				dd$season <- "second"
				if (!is.null(dd$yieldin2)) {
					dd$yieldin <- dd$yieldin2
					d$yieldin2 <- NA
				}
				d <- rbind(d, dd)
				d$yieldin2 <- NULL
			}
			d$yield2 <- NULL
		}

		if (!is.null(d$yieldin2)) {
			if (!all(is.na(d$yieldin2))) {
				message("     yieldin2. this should not happen")
			}
			d$yieldin2 <- NULL
		}
		if (!is.null(d$sesamia)) {
			if (!all(is.na(d$sesamia))) {
				message("     sesamia. this should not happen")
			}
			d$sesamia <- NULL
		}

		if (is.null(d$seed_weight)) {
			d$seed_weight <- as.numeric(NA)
		} else {
			d$seed_weight <- as.numeric(d$seed_weight)	
		}

		if (striga || borer) {
			# paired infested (*in) / uninfested (*un) plot measurements → two rows
			# after change_names so aliases such as plst already share the canonical name
			iu_stem <- function(v, sfx) {
				b <- sub(paste0("(_)?", sfx, "$"), "", v)
				tr <- c(easp="e_asp", eharv="e_harv", eht="e_ht", pasp="p_asp", pharv="p_harv",
					plht="pl_ht", plst="pl_st", dysk="dy_sk", eardam="ear_dam_rat", erot="e_rot",
					yld="yield", r_l="rl", s_l="sl", eld="eldana")
				if (b %in% names(tr)) tr[[b]] else b
			}
			apply_iu <- function(dd, vars, sfx) {
				for (v in vars) {
					b <- iu_stem(v, sfx)
					if (b == "" || b == v) next
					dd[[b]] <- dd[[v]]
				}
				dd
			}
			nms <- names(d)
			inf <- grep("in$", nms, value=TRUE)
			unf <- grep("un$", nms, value=TRUE)
			filled <- function(x) {
				x <- as.character(x)
				!is.na(x) & x != ""
			}
			has_inf <- rep(FALSE, nrow(d))
			for (v in inf) {
				has_inf <- has_inf | filled(d[[v]])
			}
			din <- apply_iu(d, inf, "in")
			dun <- apply_iu(d, unf, "un")
			drop <- unique(c(inf, unf, "yieldin1", "yieldin"))
			drop <- drop[drop %in% names(din)]
			din[drop] <- NULL
			dun[drop] <- NULL
			din$striga_infected <- striga
			dun$striga_infected <- FALSE
			if (any(has_inf)) {
				d <- carobiner::bindr(dun, din[has_inf, , drop=FALSE])
			} else {
				d <- dun
			}
		} else {
			d$striga_infected <- FALSE
		}

		qv <- c("yield", "pl_ht", "e_ht", "asi", "dy_sk", "p_asp", "p_harv", "e_asp", "husk", "rl", "sl", "e_rot", "rust", "anthesis", "moist", "pl_st")
		for (v in qv) {
			if (!is.null(d[[v]])) {
				d[[v]][d[[v]] == "."] <- NA
				d[[v]][d[[v]] == "S"] <- NA
				d[[v]] <- as.numeric(d[[v]])			
			}
		}
		if (!is.null(d$husk)) d$husk <- as.character(d$husk)
		
		d$yield_part <- "grain"
		d$striga_trial <- striga
		d$borer_trial <- borer
		d$variety <- trimws(d$variety)
		d$variety[d$variety == ""] <- NA
		if (!is.null(d$trial_name)) d$trial_name <- trimws(d$trial_name)
		d$description <- as.character(d$description)
		d$description[d$description==""] <- NA

		if (inherits(d$latitude, "character")) {
			d$latitude <- gsub("\u00a0", "", d$latitude)
			d$latitude <- trimws(d$latitude)
			d$latitude <- gsub(" ", "", d$latitude)
			d$latitude[d$latitude == ""] <- NA
			d$latitude <- as.numeric(d$latitude)
		}
		if (inherits(d$longitude, "character")) {
			d$longitude <- gsub("\u00a0", "", d$longitude)
			d$longitude <- trimws(d$longitude)
			d$longitude <- gsub(" ", "", d$longitude)
			d$longitude[d$longitude == ""] <- NA
			d$longitude <- as.numeric(d$longitude)
		}
		d <- d[!is.na(d$yield), ]
		d
	}

	if (is.null(sf)) {
		sf <- grep("regular", ff, ignore.case=TRUE, value=TRUE)
		d <- doit(sf)
		sf <- grep("striga", ff, ignore.case=TRUE, value=TRUE)		
		if (length(sf) > 0) {
			e <- doit(sf, striga=TRUE)
			if (nrow(e) > 0) {
				d <- carobiner::bindr(d, e)
			}
		}
		sf <- grep("borer", ff, ignore.case=TRUE, value=TRUE)		
		if (length(sf) > 0) {
			e <- doit(sf, borer=TRUE)
			if (nrow(e) > 0) {
				d <- carobiner::bindr(d, e)
			}
		}
	} else {
		sf <- grep(sf, ff, ignore.case=TRUE, value=TRUE)
		d <- doit(sf)
	}
	
	d <- carobiner::change_names(d, c("pl_ht", "dy_sk", "dy_tass", "pl_st", "anthesis", "e_ht", "p_asp", "e_asp", "gtext"), 
				c("plant_height", "silking_days", "tassling_days", "plant_density", "anthesis_days", "ear_height", "aspect", "ear_aspect", "grain_texture"), must_have=FALSE)

	if (!is.null(d$dy_poll)) {
		x <- d$dy_poll
		if (!is.numeric(x)) {
			x <- trimws(as.character(x))
			x[x %in% c("", ".", "S", "NA", "na")] <- NA
			x <- as.numeric(x)
		}
		if (is.null(d$silking_days)) {
			d$silking_days <- x
		} else {
			i <- is.na(d$silking_days) & !is.na(x)
			d$silking_days[i] <- x[i]
		}
		d$dy_poll <- NULL
	}

	if (!is.null(d$grain_N)) d$grain_N <- as.numeric(d$grain_N)
	if (!is.null(d$tassling_days)) d$tassling_days <- as.numeric(d$tassling_days)
	if (!is.null(d$plant_density)) d$plant_density <- as.numeric(d$plant_density) * 10000
	if (!is.null(d$anthesis_days)) d$anthesis_days <- round(d$anthesis_days)

	d$yield[d$yield < 0] <- NA
	for (v in c("silking_days", "tassling_days", "anthesis_days", "plant_height")) {
		if (!is.null(d[[v]])) d[[v]][d[[v]] < 10] <- NA
	}
	
	d <- carobiner::change_names(d, c("gwt"), 
				c("seed_weight"), must_have=FALSE)
	
	d$is_survey <- FALSE	
	d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
	d$irrigated <- as.logical(NA)
	d$geo_from_source <- TRUE

	if (!is.null(d$moist)) {
		d$yield_moisture <- as.numeric(d$moist)
		d$moist <- NULL
		if (all(is.na(d$yield_moisture))) {
			d$yield_isfresh <- as.logical(NA)
		}
	}

	fmt_lodg <- function(x, suffix="") {
		if (is.null(x)) return(NULL)
		if (!is.numeric(x)) {
			x <- trimws(as.character(x))
			x[x %in% c("", ".", "S", "NA", "na")] <- NA
		}
		out <- rep(NA_character_, length(x))
		i <- !is.na(x)
		out[i] <- paste0(as.character(x[i]), suffix)
		out
	}
	comb_lodg <- function(...) {
		args <- list(...)
		args <- args[!vapply(args, is.null, TRUE)]
		if (length(args) == 0) return(NULL)
		out <- args[[1]]
		for (x in args[-1]) {
			i <- !is.na(x)
			j <- i & is.na(out)
			k <- i & !is.na(out)
			out[j] <- x[j]
			out[k] <- paste(out[k], x[k], sep="; ")
		}
		out
	}
	d$root_lodging <- comb_lodg(fmt_lodg(d$rl), fmt_lodg(d$rl_rat), fmt_lodg(d$rlper, "%"))
	d$stalk_lodging <- comb_lodg(fmt_lodg(d$sl), fmt_lodg(d$sl_rat), fmt_lodg(d$slper, "%"))
	d[intersect(c("rl", "rl_rat", "rlper", "sl", "sl_rat", "slper"), names(d))] <- NULL

	if (!is.null(d$gr_maize)) {
		if (is.logical(d$gr_maize)) {
			if (any(!is.na(d$gr_maize))) {
				d$variety_traits <- ifelse(d$gr_maize, "glyphosate resistant", "not glyphosate resistant")
			}
			d$gr_maize <- NULL
		} else {
			x <- toupper(trimws(as.character(d$gr_maize)))
			x[x %in% c("", ".", "S", "NA", "NAN")] <- NA
			tf <- x %in% c("T", "TRUE", "F", "FALSE")
			if (any(tf)) {
				d$variety_traits <- ifelse(x %in% c("T", "TRUE"), "glyphosate resistant",
					ifelse(x %in% c("F", "FALSE"), "not glyphosate resistant", NA_character_))
			}
			if (all(is.na(x) | tf)) {
				d$gr_maize <- NULL
			}
		}
	}

	d$record_id <- as.integer(seq_len(nrow(d)))
	long <- mz_pd_long(d)

	pdvars <- c("blight", "curv", "diplodia", "dm", "e_rot", "gls", "helm", "hm", "rust", "stalk_rot", "streak",
		"borer", "borer1", "borer2", "borer3", "borer4", "sb_dam_rat", "borer_dam_rat", "sbdamrat", "stemtunnel", "dead_heart",
		"leaf_feed", "leaffeed", "lfeed2", "eldana", "mussidia", "insect",
		"cob_dam_co", "cob_dam_rt", "ear_dam_co", "ear_dam_rat",
		"str_co1", "str_co2", "str_co3", "str_rat1", "str_rat2")
	d[intersect(pdvars, names(d))] <- NULL

	# p_harv / e_harv are plot counts of plants and ears harvested (not per ha)
	# fwt is plot field weight of ears (not per ha); yield already records production
	d[intersect(c("p_harv", "e_harv", "fwt"), names(d))] <- NULL

	if (!is.null(d$description) && all(is.na(d$description))) {
		d$description <- NULL
	}
	if (!is.null(d$inst)) {
		d$inst <- trimws(as.character(d$inst))
		d$inst[d$inst %in% c("", ".", "NA", "na")] <- NA
		if (all(is.na(d$inst))) d$inst <- NULL
	}

	list(d=d, long=long)
}


## pest/disease columns from the IITA maize trials → long records
mz_pd_long <- function(d) {

	rid <- d$record_id
	n <- length(rid)
	long <- NULL

	num <- function(x) {
		if (is.null(x)) return(rep(NA_real_, n))
		if (is.numeric(x)) return(x)
		# empty Excel columns are often read as logical NA / FALSE
		if (is.logical(x)) return(rep(NA_real_, n))
		x <- trimws(as.character(x))
		x[x %in% c("", ".", "-", "*", "**", "***", "****")] <- NA
		x[toupper(x) %in% c("S", "NA", "N/A", "NAN", "NS", "FALSE", "TRUE")] <- NA
		out <- suppressWarnings(as.numeric(x))
		bad <- unique(x[!is.na(x) & is.na(out)])
		if (length(bad)) {
			warning("non-numeric pest/disease values: ", paste(bad, collapse=", "), call.=FALSE)
		}
		out
	}

	chr <- function(x, keep) {
		if (length(x) != n) return(NA_character_)
		z <- x[keep]
		out <- rep(NA_character_, length(z))
		i <- !is.na(z)
		out[i] <- as.character(z[i])
		out
	}

	first_num <- function(...) {
		out <- rep(NA_real_, n)
		for (x in list(...)) {
			x <- num(x)
			i <- is.na(out)
			out[i] <- x[i]
		}
		out
	}

	add <- function(keep, disease=NA_character_, disease_severity=NA, disease_incidence=NA,
			pest_species=NA_character_, pest_severity=NA, pest_incidence=NA, DAP=NA, order=NA) {
		if (!any(keep)) return(long)
		x <- data.frame(
			record_id = rid[keep],
			disease = disease,
			disease_severity = chr(disease_severity, keep),
			disease_incidence = chr(disease_incidence, keep),
			pest_species = pest_species,
			pest_severity = chr(pest_severity, keep),
			pest_incidence = if (length(pest_incidence) == n) as.integer(pest_incidence[keep]) else NA_integer_,
			DAP = if (length(DAP) == n) as.integer(DAP[keep]) else if (length(DAP) == 1 && !is.na(DAP)) as.integer(DAP) else NA_integer_,
			order = if (length(order) == n) as.integer(order[keep]) else if (length(order) == 1 && !is.na(order)) as.integer(order) else NA_integer_
		)
		if (is.null(long)) x else rbind(long, x)
	}

	# diseases
	ds <- list(
		blight = "maize leaf blight",
		curv = "curvularia leaf spot",
		diplodia = "stenocarpella ear rot",
		e_rot = "ear rot",
		gls = "gray leaf spot",
		rust = "rust",
		stalk_rot = "stalk rot",
		streak = "maize streak virus"
	)
	for (v in names(ds)) {
		x <- num(d[[v]])
		keep <- !is.na(x)
		long <- add(keep, disease=ds[[v]], disease_severity=x)
	}
	# Hm (striga files) is the same trait as HELM (borer files): Helminthosporium / maydis leaf blight
	x <- first_num(d$helm, d$hm)
	long <- add(!is.na(x), disease="southern leaf blight", disease_severity=x)
	x <- num(d$dm)
	long <- add(!is.na(x), disease="maize dwarf mosaic virus", disease_incidence=x)

	# stem borer (rating aliases combined; other measures kept as separate rows)
	x <- first_num(d$borer_dam_rat, d$sb_dam_rat, d$sbdamrat, d$borer)
	long <- add(!is.na(x), pest_species="stem borer", pest_severity=x)
	x <- num(d$dead_heart)
	long <- add(!is.na(x), pest_species="stem borer", pest_severity=x)
	x <- num(d$stemtunnel)
	long <- add(!is.na(x), pest_species="stem borer", pest_severity=x)
	x <- first_num(d$leaf_feed, d$leaffeed, d$lfeed2)
	long <- add(!is.na(x), pest_species="stem borer", pest_severity=x)
	for (i in 1:4) {
		x <- num(d[[paste0("borer", i)]])
		long <- add(!is.na(x), pest_species="borer", pest_severity=x, order=i)
	}

	x <- num(d$eldana)
	long <- add(!is.na(x), pest_species="Eldana saccharina", pest_severity=x)
	x <- num(d$mussidia)
	long <- add(!is.na(x), pest_species="Mussidia nigrivenella", pest_severity=x)
	x <- num(d$insect)
	long <- add(!is.na(x), pest_species="insect", pest_severity=x)

	co <- num(d$cob_dam_co)
	rt <- num(d$cob_dam_rt)
	keep <- !is.na(co) | !is.na(rt)
	long <- add(keep, pest_species="stem borer", pest_severity=rt, pest_incidence=co)

	co <- num(d$ear_dam_co)
	rt <- num(d$ear_dam_rat)
	keep <- !is.na(co) | !is.na(rt)
	long <- add(keep, pest_species="stem borer", pest_severity=rt, pest_incidence=co)

	# Striga counts/ratings at weeks after planting
	for (spec in list(
		list(co="str_co1", rat="str_rat1", DAP=56L),
		list(co="str_co2", rat="str_rat2", DAP=70L),
		list(co="str_co3", rat=NULL, DAP=21L)
	)) {
		co <- num(d[[spec$co]])
		rt <- if (is.null(spec$rat)) rep(NA_real_, n) else num(d[[spec$rat]])
		keep <- !is.na(co) | !is.na(rt)
		long <- add(keep, pest_species="Striga", pest_severity=rt, pest_incidence=co, DAP=spec$DAP)
	}

	if (is.null(long) || nrow(long) == 0) return(NULL)
	rownames(long) <- NULL
	long
}
