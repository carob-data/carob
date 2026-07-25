# R script for "carob"
# license: GPL (>=3)

## ISSUES
#1. bad value in r$ppt_DM_roots (3.833333). Should bre a fraction.
#2.out of bounds: plant_height (5, 405)

## NOTES
# Experiment 1 (of three) in Hauser et al. 2025. On-station trials at IITA HQ,
# Ibadan (sites D23, WB_South) and Obafemi Awolowo University, Ile-Ife (site
# Ile_Ife); 3 site-seasons. 2x2x3x2 factorial RCBD: variety x stake diameter x
# stake orientation x fertilizer, 3 reps. Plant density (12,500 plants/ha),
# coordinates, planting/harvest dates and fertilizer rates confirmed from
# Hauser et al. 2025 (J Plant Sci Phytopathol 9(1):011-022,
# doi:10.29328/journal.jpsp.1001148). Shoot counts, plant density and plant
# height over time are in the long table (record_id, DAP, variable, value).

## ISSUES
# root_density, stake_diameter: no terminag equivalent (suggested new terms), so
#   these columns are currently dropped from the written output. stake_diameter is
#   a treatment factor (2 classes: "small" = 15-24 mm, "large" = 25-40 mm), stored
#   here as the interpretable diameter range; it is a sibling of the existing
#   "stake_orientation" term and should be added to terminag.
# DRY_root_yield_Mg_ha column in the source is inconsistent (it tracks the
#   dry-matter proportion, not a yield); dry storage yield is recomputed here as
#   fresh useful root yield x root dry-matter proportion.
# Non-marketable ("bad") root counts/mass (no_bad_roots_plot,
#   fresh_bad_root_mass_plot_kg, BRYield) are not mapped (no terminag term);
#   the reported yields are for marketable/useful roots only.
# One Ile_Ife record (ID 165) has the fresh/dry sub-sample masses transposed
#   (ppt_DM_roots = 3.83 = 1/0.26); the dry-matter proportion is corrected here.

# Experiment 1 of three in Hauser et al. 2025. Fertilized plots received 75-20-90 kg/ha N-P-K (elemental) applied as NPK 15-15-15, urea and KCl; the control was unfertilized. Rainfed.

carob_script <- function(path) {

"
Cassava storage root yield as affected by planting stake orientation, diameter, fertilizer application and variety in three sites in SW Nigeria

The aim of this investment is to verify best planting practices for cassava and achieve impact at smallholder level at scale through agronomic decision support and tailored advice on simple low cost measures. Research results will be integrated with the AKILIMO service for smallholder cassava growers, so that these benefit the wider research-and-development community involved in agronomy-at-scale.

Orientation of the planting stake in the soil at 3 levels	
	Horizontal = stakes buried at 5-7 cm depth in the soil in horizontal position
	Slanted = stakes inserted to two thirds of their length into the soil at an angle of about 45 degrees
	Vertical = stakes inserted to two thirds of their length into the soil at an angle of 90 dagrees thus vertically
"

	uri <- "doi:10.25502/35xn-j342/d"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		data_organization = "IITA",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety;fertilizer_used;planting_orientation",#orientation of cassava cuttings when planting
		response_vars = "yield", 
		notes = NA,
		carob_contributor = "Blessing Dzuda",
		carob_date = "2026-07-17",
		carob_completion = 90,	
		carob_effort = 4
	)

	f <- ff[basename(ff) == "staggered-planting-stake-orientation-expt-1.csv"]
	r <- read.csv(f)
	#removing empty fields without data
	r <- r[!is.na(r$PLOT), ]
 
	d <- data.frame(
	  country="Nigeria",
	  plot_id=as.character(r$PLOT),
	  location=trimws(r$Site),
	  variety=r$VAR,
	  planting_orientation=r$Orientation,
	  crop="cassava",
	  fertilizer_used=r$FERT,
	  rep=as.integer(r$REP),
	  #plant_density=r$plt_ha_52,# since cassava is harvested in 9-12months, this is the true plant popilation
	  #plant_height=r$PHT24, # taking the last recorded height as true plant height
	  yield=r$FRYield*1000, # only considering the fresh useful mass 
	  dmy_roots=r$DRY_root_yield_Mg_ha*1000,
	  yield_moisture=(1 - r$ppt_DM_roots) * 100,
	  yield_part="roots",
	  yield_isfresh=TRUE
	)
  
	d$yield_moisture[d$yield_moisture < 0] <- NA
	d$location <- ifelse(d$location %in% c("D23", "WB_South"), "IITA HQ, Ibadan", 
				ifelse(d$location == "Ile_Ife", "Obafemi Awolowo University, Ile-Ife", NA))
	
	d$on_farm <- FALSE
	d$is_survey <- FALSE
	d$irrigated <- FALSE
	d$geo_from_source <- FALSE
	d$planting_date <- NA
	d$harvest_date <- NA
	d$fertilizer_used <- d$fertilizer_used == "Fert"
	d$P_fertilizer <- d$K_fertilizer <- d$N_fertilizer <- 0
	d$P_fertilizer[d$fertilizer_used] <- 20
	d$K_fertilizer[d$fertilizer_used] <- 90
	d$N_fertilizer[d$fertilizer_used] <- 75
	d$fertilizer_type[d$fertilizer_used] <- "NPK" 
	d$trial_id <- paste(d$site, d$plot_id, sep = "_")
  
  #adm levels generated from site of experiment
  geo <- data.frame(
    location=c("Obafemi Awolowo University, Ile-Ife", "IITA HQ, Ibadan"),
    adm1 = c("Osun", "Oyo"),
    adm2 = c("Ife Central", "Akinyele"),
    ## site=c("Ajebandele", "Moniya"), ?where is that from?
    longitude = c(4.51956, 3.8989),
    latitude = c(7.51789, 7.4979), 
    geo_uncertainty = c(7430, 1000),
    geo_source = c("GADM 4.1, adm2", "GADM 4.1, adm2")
  )
  
  d <- merge(d, geo, by="location", all.x = TRUE)
  
  #other important traits
  ## --- long-format reshape of repeated week-based measurements ---
  r$row_id <- seq_len(nrow(r))
  
  nsp_cols    <- paste0("NSP", c(1, 2, 3, 4, 6, 8, 12, 16, 20, 24, 52))
  ppp_cols    <- paste0("PPP", c(6, 8, 12, 16, 20, 24, 52))
  pht_cols    <- paste0("PHT", c(6, 8, 12, 16, 20, 24))
  plt_ha_cols <- paste0("plt_ha_", c(6, 8, 12, 16, 20, 24, 52))
  
  nsp_cols    <- nsp_cols[nsp_cols %in% names(r)]
  ppp_cols    <- ppp_cols[ppp_cols %in% names(r)]
  pht_cols    <- pht_cols[pht_cols %in% names(r)]
  plt_ha_cols <- plt_ha_cols[plt_ha_cols %in% names(r)]
  
  nsp_long <- reshape(r[, c("row_id", nsp_cols)],varying = nsp_cols,v.names = "shoots_count",timevar = "week",
                      times = nsp_cols,idvar = "row_id",direction = "long")
  rownames(nsp_long) <- NULL

  ppp_long <- reshape(r[, c("row_id", ppp_cols)],varying = ppp_cols,v.names = "plant_density_plot", timevar = "week",
                      times = ppp_cols,idvar = "row_id",direction = "long")
  rownames(ppp_long) <- NULL

  pht_long <- reshape(r[, c("row_id", pht_cols)],varying = pht_cols,v.names = "plant_height",timevar = "week",
                      times = pht_cols,idvar = "row_id",direction = "long")
  rownames(pht_long) <- NULL

  plt_ha_long <- reshape(r[, c("row_id", plt_ha_cols)],varying = plt_ha_cols,v.names = "plant_density_ha",timevar = "week",
                         times = plt_ha_cols,idvar = "row_id",direction = "long")
  rownames(plt_ha_long) <- NULL

  d_long <- Reduce(function(x, y) merge(x, y, by = c("row_id", "week"), all = TRUE),
                   list(nsp_long, ppp_long, pht_long, plt_ha_long))
  
  d_long <- merge(d_long, data.frame(row_id = r$row_id, plot_id = as.character(r$PLOT)),
                  by = "row_id", all.x = TRUE)
  d_long$row_id <- NULL
  
  #merging into main dataframe
 d <- merge(d,d_long,by="plot_id", all.x = T) 
  
 #removing duplicates created from merge
 d <- unique(d) 
 
	carobiner::write_files(path, meta, d)
		publication = "10.29328/journal.jpsp.1001148",
		project = "ACAI",
		design = "RCBD (2x2x3x2 factorial)",
		data_type = "on-station experiment",
		treatment_vars = "variety;stake_diameter;stake_orientation;N_fertilizer;P_fertilizer;K_fertilizer",
		response_vars = "yield;dmy_storage",
		notes = NA,
		carob_contributor = "Robert Hijmans",
		carob_date = "2026-07-23",
		carob_completion = 80,
		carob_effort = .1
	)

	f2 <- ff[basename(ff) == "staggered-planting-stake-orientation-expt-1.csv"]
	r2 <- read.csv(f2, na.strings="nd")
	## drop blank/trailing rows
	r2 <- r2[!is.na(r2$ID) & r2$ID != "" & !is.na(r2$Site) & r2$Site != "", ]

	## fertilized plots received 75-20-90 kg/ha N-P-K (elemental; Methods, all
	## via NPK 15-15-15 + urea + KCl); control ("Nil") received nothing.
	fert <- tolower(trimws(r2$FERT)) != "nil"

	## root dry-matter proportion; one Ile_Ife record (ID 165) has the fresh/dry
	## sub-sample masses transposed (ppt > 1), corrected here as 1/x.
	dm_prop <- as.numeric(r2$ppt_DM_roots)
	dm_prop[!is.na(dm_prop) & dm_prop > 1] <- 1 / dm_prop[!is.na(dm_prop) & dm_prop > 1]

	d <- data.frame(
		trial_id = r2$Site,               # one trial = one site x season
		plot_id  = paste0(r2$Site, "_", r2$PLOT),
		rep = as.integer(r2$REP),
		country = "Nigeria",
		site = r2$Site,                   # D23 / WB_South / Ile_Ife
		on_farm = FALSE,
		is_survey = FALSE,
		irrigated = FALSE,                # rainfed (Methods)
		crop = "cassava",
		variety = r2$VAR,
		treatment = paste(r2$VAR, r2$SIZE, r2$Orientation, r2$FERT),
		stake_orientation = r2$Orientation,           # horizontal / slanted / vertical
		## stake diameter class -> interpretable range (small = 15-24 mm, large = 25-40 mm)
		# stake_diameter = c(small="15-24 mm", large="25-40 mm")[r2$SIZE],
		stake_diameter = c(small=20, large=33)[r2$SIZE],
		N_fertilizer = ifelse(fert, 75, 0),           # kg/ha, elemental (Methods)
		P_fertilizer = ifelse(fert, 20, 0),           # kg/ha, elemental P (not P2O5)
		K_fertilizer = ifelse(fert, 90, 0),           # kg/ha, elemental K (not K2O)
		fertilizer_type = "NPK;urea;KCl",
		plant_density = as.numeric(r2$plt_ha_52),     # plants/ha at harvest (52 WAP)
		yield = as.numeric(r2$FRYield) * 1000,        # useful fresh root yield, Mg/ha -> kg/ha
		yield_part = "roots",
		yield_isfresh = TRUE,
		yield_moisture = (1 - dm_prop) * 100,          # % moisture of the fresh root yield
		dmy_storage = as.numeric(r2$FRYield) * dm_prop * 1000,   # kg/ha
		root_density = as.numeric(r2$no_useful_roots_plot) / 20 * 10000              # roots/ha (net plot = 20 m2)
	)

	## geography and dates by site (coordinates and dates from the publication)
	lon   <- c(D23=3.90360, WB_South=3.88287, Ile_Ife=4.5594)
	lat   <- c(D23=7.49238, WB_South=7.48847, Ile_Ife=7.5527)
	pdate <- c(D23="2016-05-09", WB_South="2016-05-11", Ile_Ife="2016-09-27")
	hdate <- c(D23="2017-06-04", WB_South="2017-06-06", Ile_Ife="2017-09-25")
	ife <- r2$Site == "Ile_Ife"

	d$adm1 <- ifelse(ife, "Osun", "Oyo")
	d$adm2 <- ifelse(ife, "Ife Central", "Ibadan")
	d$location <- ifelse(ife, "Obafemi Awolowo University, Ile-Ife", "IITA HQ, Ibadan")
	d$longitude <- unname(lon[r2$Site])
	d$latitude  <- unname(lat[r2$Site])
	d$geo_from_source <- TRUE
	d$planting_date <- unname(pdate[r2$Site])
	d$harvest_date  <- unname(hdate[r2$Site])

	## time series -> long records (record_id, DAP, variable, value)
	d$record_id <- 1:nrow(d)

	## turn a set of wide time columns into long records
	to_long <- function(m, weeks, varname) {
		out <- data.frame(
			record_id = rep(d$record_id, times=ncol(m)),
			DAP = as.integer(rep(weeks, each=nrow(m)) * 7),   # weeks after planting -> days
			variable = varname,
			value = as.numeric(unlist(m, use.names=FALSE))
		)
		out[!is.na(out$value), ]
	}

	wk_ha  <- c(6, 8, 12, 16, 20, 24, 52)     # weeks with plant counts (plt_ha)
	wk_pht <- c(6, 8, 12, 16, 20, 24)         # weeks with plant height

	dens <- to_long(r2[, paste0("plt_ha_", wk_ha)], wk_ha, "plant_density")   # plants/ha
	pht  <- to_long(r2[, paste0("PHT", wk_pht)], wk_pht, "plant_height")      # cm

	## stems/ha = mean shoots per plant (NSP) x plants per ha (plt_ha), for weeks
	## where both were recorded; matches the stems/ha convention used in the
	## companion datasets (doi:10.25502/c9jq-w968/d etc.).
	nsp <- as.matrix(r2[, paste0("NSP", wk_ha)]) * as.matrix(r2[, paste0("plt_ha_", wk_ha)])
	stems <- to_long(nsp, wk_ha, "stem_density")

	dlong <- rbind(dens, pht, stems)

	carobiner::write_files(path, meta, d, long=dlong)
}
