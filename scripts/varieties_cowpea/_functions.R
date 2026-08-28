# R script for "carob"
# license: GPLv3

#       Var1 Freq
#    VARIETY  457
#        GID  457
#         ID  455
#        REP  442
#   SEEDKGHA  426
#      SEEDS  422
#     PLOTNO  403
#      STAND  390
#        DFF  371
#     MATURE  304
#       PODS  201
#       THR.  188
#         BB  114
#  X_100SDWT   70
#     STRIGA   59
#   MATURITY   48
#      APHID   40
#   PODPLANT   37
#       SMUT   36
#   SEEDSPOD   36
#          X   35
#   SEED_POD   34
#        X.1   32
#     FODDER   23
#   POD_PLTS   17
#   B_BLIGHT   17
#      SEED2   16
#      SEED1   16
#      VIRUS   15
#   FODERKGH   14
#   LEAFKGHA   13
#     LEAVES   12
#     INSECT   12
#   B_BLOTCH   12
#      SMUT2   11
#       SCAB   11
#   W_BLIGHT   10
#       POD2   10
#       POD1   10
#     VIRUSE    9
#    DISEASE    9
#    ANTHRAC    9
#   STRIGASC    8
#      SMUT1    8
#     PODBUG    8
#    POD_PLT    8
#   PLANTINF    8
#    INSECT1    8
#     APHIDS    8
#   X_50.DFF    7
#       SEED    7
#   D_MATURE    7
#    PODKGHA    6
#    PLANTIF    6
#   WEBLIGHT    5
#   SEPTORIA    5
#      LEAF2    5
#      LEAF1    5
#   FRFODDER    5
#   FRFDKGHA    5
#    FODKGHA    5
#   DRFDKGHA    5
#   DISEASE2    5
#   DISEASE1    5
#    T_SEEDS    4
#        SEP    4
#    INSECT2    4
#   DRFODDER    4
#       DFFL    4
#       CAMV    4
# X__NODULES    3
#      THRIP    3
#    SEEDKHA    3
#   SECONDSD    3
#     SCABPD    3
#     SCABLF    3
#     RATING    3
#    POD_PLA    3
#    PLOT_NO    3
#   PLINFSTR    3
#   PL_STAND    3
#  NUM_S_POD    3
#   NODULEWT    3
#   LEAFSPOT    3
#    F_SEEDS    3
#     DATEFL    3
#   D_FODDER    3
#      CERCO    3
#     BLOTCH    3
#      YIELD    2
#   X__ROOTS    2
#        TRT    2
#   TOTALSDS    2
#   TOTALPOD    2
#     THRIPS    2
#    THRESHI    2
#   STRGPLOT    2
#   STEMROOT    2
#       RUST    2
#    PODSCAB    2
#   PODS_PLT    2
#       POD3    2
#   PLANTIF2    2
#     PL_STD    2
#    PD_PLTS    2
#     LFSCAB    2
#      LEAVE    2
#      FSEED    2
#   FODDKGHA    2
#   F_FODDER    2
#      DUMMY    2
#   DFODKGHA    2
#   DFODERWT    2
#   DFDWTKGH    2
#        CLS    2
#    CERCOSP    2
#     CERCOS    2
#     BLIGHT    2
#     B_PUST    2
#        B_B    2
#   ASCOCHTA    2
#     ANTRAC    2
#    ALECTRA    2
#   X__STAND    1
#    X__ROOT    1
#    X__PODS    1
# X__MILHEAD    1
#     VIRUS2    1
#     VIRUS1    1
#      TREAT    1
#     T_PODS    1
#     SWT100    1
#     STTAND    1
#   STRPRPLT    1
#     STRPLT    1
#    STRPLOT    1
#   STRIGAHT    1
#   STR_PLOT    1
#   STR_HGHT    1
#   STANDINF    1
#   ST_SCORE    1
#   ST_PPLOT    1
#      SEED3    1
#     SD_POD    1
#     S_PODS    1
#         S_    1
#   PTINFSTR    1
# PODS_Pedun    1
# PODS_pedun    1
#       POD4    1
#  POD_PLT.1    1
#   POD_PLNT    1
#   PLTINSTR    1
#   PLSTRIGA    1
#   PLINFALT    1
#      PHOMA    1
#     PD_PLT    1
#     P_SUCH    1
#    P_PLANT    1
#    P_BORER    1
#  Num_s_pod    1
#     MOSAIC    1
#     MILLET    1
#    MILHDWT    1
#   MILETKGH    1
#    LEAVES2    1
#    LEAVES1    1
#     LEAFWT    1
#      LEAF4    1
#      LEAF3    1
#       LAMB    1
#    INSECT3    1
#         Id    1
#   GRPODKGH    1
#        GMV    1
#   FRSTPODS    1
#     FRFDKG    1
#   FODDERKH    1
#    FIRSTSD    1
#   FFODKGHA    1
#   FFODERWT    1
#    FDWRKGH    1
#    F_FODER    1
#   DSTR_EMG    1
#   DRYFODDR    1
#   DISEASE3    1
#   DISAESE2    1
#       DIS2    1
#       DIS1    1
#     DIABRO    1
#   DFODDERW    1
#       DFF1    1
#   DAYALEMG    1
#   D_STRIGA    1
#    D_FODER    1
#        CTS    1
#       CSNB    1
#     CEREAL    1
#   CERCOSPR    1
#         BP    1
#   B_BLOCTH    1
#       B_B_    1
#    APHHIDS    1
#      ANTR_    1
#   ALPLTHST    1
#   ALPERPLT    1
#   ALHEIGHT    1
#    ACTOMIA    1



process_cowpea <- function(ff) {

	p_cowpea <- function(f) {

		r <- read.csv(f) |> unique()
		fd <- dirname(f)
		fj <- file.path(fd, paste0(basename(fd), ".json"))
		m <- jsonlite::fromJSON(fj)$result

		d <- data.frame(
			crop = "cowpea",
			yield_part = "seed",
			variety  =r$VARIETY,
			longitude = as.numeric(m$coverage_y), #!!
			latitude = as.numeric(m$coverage_x),
			geo_from_source = TRUE,
			country = trimws(m$coverage_country),
			planting_date = m$`coverage_start_date`,
			harvest_date = m$`coverage_end_date`
		)
		d$trial_id <- gsub(".csv$", "", basename(f))
		d$yield <- r$SEEDKGHA
		if (!is.null(r$REP)) {
			p <- gsub("REP", "", r$REP, TRUE)
			p <- gsub("ONE", "1", p, TRUE)
			p <- gsub("TWO", "2", p, TRUE)
			p <- gsub("THREE", "3", p, TRUE)
			p <- gsub("FOUR", "4", p, TRUE)
			d$rep <- as.integer(p)
		}
		d$on_farm <- FALSE
		d$is_survey <- FALSE
		d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
		d$irrigated <- NA
		unique(d)
	}

	f <- grep("\\.csv$", ff, value=TRUE)

	for (i in 1:length(f)) p_cowpea(f[i])
	
	d <- do.call(carobiner::bindr, lapply(f, p_cowpea))

	d$country[d$country == "Cape Verde"] <- "Cabo Verde"	
	d$country[d$country == "Republic of Benin"] <- "Benin"
	d$country[d$country == "Zaire"] <- "Democratic Republic of the Congo"
	d$country[d$country == "Democartic Republic of the Congo"] <- "Democratic Republic of the Congo"
	d$country[d$country == "Swaziland"] <- "Eswatini"
	d$country[d$country == "Columbia"] <- "Colombia"

	if (!"yield" %in% names(d)) d$yield <- NA_real_
	d$yield[d$yield < 0] <- NA
	d
}


xxprocess_cowpea <- function(ff) {

return(NULL)

	fcsv <- grep("\\.csv$", ff, value=TRUE)
	r <- lapply(fcsv, \(f) {
			x <- read.csv(f)
			cn <- tolower(names(x))
			cn <- gsub("aphhids|aphid|aphidss|aphidss", "aphids", cn)
			cn <- gsub("plot_no", "plotno", cn)
			cn <- gsub("sd_pod|num_s_pod", "seed_pod" , cn)
			cn <- gsub("podplant|pd_plt|pod_pla", "pod_plnt", cn)
			cn <- gsub("seedkha", "seedkgha", cn) # yield is in kg per plot
			cn <- gsub("viruse", "virus", cn)  
			cn <- gsub("dffl", "dff1", cn)  
			cn <- gsub("^x__..", "", cn)  
			cn <- gsub("x_50.dff", "dff", cn)  
			cn <- gsub("foderkgh", "fodkgha", cn) # fodder is in kg per plot
			cn <- gsub("thr.|thripp", "thrip", cn)  
			cn <- gsub("maturity", "mature", cn)  
			cn <- gsub("d_striga", "striga", cn)  
			names(x) <- cn
			x$source <- gsub(".csv$", "", basename(f))
			x
		}
	)
	
	r <- do.call(carobiner::bindr, r)
	r <- r[, colSums(is.na(r)) < nrow(r)]
	r[r == -32768] <- NA

	d <- data.frame(
		trial_id = r$source,
		crop = "cowpea",
		yield_part = "seed",
		#record_id = r$gid,
		rep = as.integer(gsub("REP|-", "", r$rep)),
		variety = r$variety,
		yield = r$seedkgha,
		fwy_storage = r$seedkgha,		
		fwy_residue = r$fodkgha,		
		flowering_days = r$dff,
		maturity_days = r$mature,
		plant_density = r$stand,
		seed_weight = as.numeric(r$x_100sdwt) * 10,
		seeds_pod = r$seeds,
		bacterial_blight_score = r$bb,
		smut_score = r$smut,
		striga_score = r$striga,
		aphid_score = r$aphids
	)

	d$on_farm <- d$is_survey <- FALSE
	d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
	d$irrigated <- NA
	d <- unique(d)

#		country = "Nigeria",
#		adm1 = "Kano", 
#		longitude = 8.59196,
#		latitude = 12.00218,



	return(NULL)
}

