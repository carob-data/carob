
do_LCAS <- function(r) {

	d <- data.frame(
		date = r$collectionDate,
		adm1 = r$A.q102_state,
		adm2 = r$A.q103_district,
		adm3 = r$A.q104_subDistrict,
		location = r$A.q105_village,
		crop = tolower(r$A.q116_crop),
		previous_crop = tolower(r$D.prevCrop),
		harvest_date = r$L.q601_harvestDate,  # L.harvDate is cleaner, but not in dictionary
		season = r$A.q117_season, # A.q118_harvYear,
		variety = r$D.q410_varName,
		variety_type = r$D.q409_varType,
		latitude = r$O.largestPlotGPS.Latitude,
		longitude = r$O.largestPlotGPS.Longitude,
		geo_from_source = TRUE,
		soil_texture = r$D.q401_soilTexture,
		soil_quality = r$D.q403_soilPerception,
		landscape_position = r$D.q402_drainClass,
		previous_crop_residue_perc = r$D.q407_cropResiduePcnt,
		previous_crop_burnt = r$D.q408_residueBurnt == "yes",
		land_prep_method = r$D.q411_LandPrep,
		is_survey = TRUE
	)
	
	d$trial_id <- as.character(1:nrow(d))
	d$country <- r$A.q101_country
	if (is.null(d$country)) d$country <- "India"
	d$country[d$country == "8"] <- "India"
#	d$country <- "India"
	d$yield_part <- "grain"

	d$planting_method = r$D.q413_CropEst
	## grep above to get d$trans_planting_method 

	plot_ha <- 0.404686 * r$C.q305_cropLargestArea / r$C.q302_acreConv

## ?? 
	d$flood_stress <- tolower(r$I.q5504_floodSeverity)
	d$drought_stress <- tolower(r$I.q5502_droughtSeverity)
	d$pest_severity <- tolower(r$I.q5506_insectSeverity)
	d$weed_severity <- tolower(r$I.q5505_weedSeverity)
	d$disease_severity <- tolower(r$I.q5509_diseaseSeverity)

	d$insecticide_product <- tolower(r$I.q5508_insecticidesName)
	d$insecticide_product[grep("remember", d$insecticide_product)] <- "unknown"
	d$insecticide_product[d$insecticide_product %in% c("chloripyriphos", "chloropyariphosh", "chloropyriphos", "chlorpyriphos")] <- "chlorpyrifos"
	d$insecticide_product[d$insecticide_product %in% c("firadon", "furadan", "furadon")] <- "carbofuran"
	d$insecticide_product <- carobiner::replace_values(d$insecticide_product, 
		c("dichlorophos", "imadiclorpid", "imidachloropid"),
		c("dichlorvos", "imidacloprid", "imidacloprid"), FALSE)

	d$biocide_product <- tolower(r$I.q5511_pesticidesName)
	d$biocide_product[grep("remember", d$biocide_product)] <- "unknown"

	d$herbicide_product <- apply(r[, c("J.q5601_1herbName", "J.q5603_2herbName", "J.q5605_3herbName")], 1, 
		\(i) {
			i <- tolower(i)
			i <- gsub("2,4-d|24d", "2,4-D", i)
			i <- gsub("clodinofop", "clodinafop", i)
			i <- gsub("idosulfuron", "iodosulfuron", i)
			i <- gsub("leader|traget", "sulfosulfuron", i)
			i <- gsub("propargyl", "clodinafop", i)
			i <- gsub("\\+", "; ", unique(i))
			i <- gsub(", ", "; ", unique(i))
			i <- gsub("; NA|NA", "", paste(unique(i), collapse="; "))
			gsub("", NA, i)
		})

	d$herbicide_times <- as.integer(rowSums(!is.na(r[, c("J.q5601_1herbName", "J.q5603_2herbName", "J.q5605_3herbName")]))) 
	d$herbicide_timing <- apply(r[, c("J.q5602_1herbAppDays", "J.q5604_2herbAppDays", "J.q5606_3herbAppDays"
)], 1, \(i) paste(na.omit(i), collapse=";"))

	d$herbicide_product[d$herbicide_product == ""] <- NA 
	d$herbicide_timing[d$herbicide_timing == ""] <- NA 
	
	d$weeding_times <- as.integer(r$J.manualWeedTimes)

	d$planting_date <- r$D.seedingSowingTransplanting
	if (is.null(d$planting_date)) d$planting_date <- r$D.q415_seedingSowingTransDate


	d$seed_density = r$D.q420_cropSeedAmt / plot_ha
	d$seed_source = ifelse(r$D.q421_seedSource == "other", 
							r$D.q422_otherSeedSource, r$D.q421_seedSource)

	fix_date <- function(x) {
		x <- gsub(", ", "-", x)
		x <- gsub(" ", "-", x)
		x <- gsub("/", "-", x)
		for (y in 16:24) {
			x <- gsub(paste0("-", y, "$"), paste0("-20", y), x)
		}
		
		month.num <- paste0("-", formatC(1:12, width=2, flag = "0"), "-")
		for (i in 1:12) {
			x <- gsub(paste0("-", month.abb[i], "-"), month.num[i], x)
		}

		dat <- rep(as.Date(NA), length(x))
		i <- grepl("-", x)
		dat[!i] <- as.Date("1899-12-31") + as.numeric(x[!i])
		dat[i] <- as.Date(x[i], "%d-%m-%Y")
		as.character(dat)
	}
	
	d$date <- fix_date(d$date)
	d$planting_date <- fix_date(d$planting_date)
	d$harvest_date <- fix_date(d$harvest_date)

	d$previous_crop <- carobiner::replace_values(d$previous_crop, 
		c("fallow", "other", "bajra", "jowar", "greenmanure", "greengram", "pulses", "mungbean"), 
		c("none", NA, "pearl millet", "sorghum", "green manure", "mung bean", "pulse", "mung bean"), 
		FALSE)


	get_fert <- function(x, product) {
		p <- paste0(c("_basal", "_1td", "_2td", "_3td"), product, "$")
		cn <- colnames(x)
		i <- sapply(p, \(v) grep(v, cn))
		stopifnot(length(i) == 4)
		rowSums(x[, i], na.rm=TRUE)
	}

	fert <- data.frame(
		DAP = get_fert(r, "DAP"), 
		NPK = get_fert(r, "NPK"), 
		urea = get_fert(r, "Urea"), 
		NPKS = get_fert(r, "NPKS"), 
		KCl = get_fert(r, "MoP"),
		SSP = get_fert(r, "SSP"), 
		TSP = get_fert(r, "TSP"), 
		ZnSO4 = get_fert(r, "ZnSO4"), 
		gypsum = get_fert(r, "Gypsum"), 
		H3BO3 = get_fert(r, "Boron")
	) / plot_ha
	
    # to get the fertilizer/ha
	ftab <- data.frame(
		name = c("AN", "ATS", "basic slag", "CAN", "C-compound", "CMP", "DAP", "DAS", "D-compound", "DSP", "ERP", "GRP", "gypsum", "KCl", "KNO", "lime", "none", "MAP", "NPK", "NPKS", "NPS", "PKS", "SCU", "SOP", "SSP", "sympal", "TSP", "unknown", "urea", "ZnSO4", "S-compound", "MgSO4", "H3BO3", "CaCO3", "borax"), 
		N = c(34L, 12L, NA, 26L, 7L, 0L, 18L, 21L, 10L, 18L, 0L, 0L, 0L, 0L, 13L, 0L, 0L, 11L, NA, 8L, 23L, 0L, 39L, 0L, 0L, 0L, 0L, NA, 46L, 0L, 8L, 0L, 0L, 0L, 0L), 
		P = c(0, 0, NA, 0, 9.156, 7, 20.1, 0, 20, 20, NA, NA, 0, 0, 0, 0, 0, 52, NA, 21, 21, NA, 0, 0, 8.74, 23, 19.23, NA, 0, 0, 21, 0, 0, 0, 0), 
		K = c(0, 0, NA, 0, 5.785, 0, 0, 0, 10, 0, 0, 0, 0, 49.8, 44, 0, 0, 0, NA, 7, 0, NA, 0, 41.5, 0, 15, 0, NA, 0, 0, 7, 0, 0, 0, 0), 
		S = c(0, 26, NA, 0, 6, 0, 0, 24, 9, 0, 0, 0, 19, 0, 0, 0, 0, 0, 0, 4, 4, NA, 13, 18, 0, 4, 0, NA, 0, 19.9, 4, 13.19, 0, 0, 0), 
		B = c(0, 0, NA, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, NA, 0, 0, 0, 0, 17.48, 0, 11.3), 
		Mg = c(0, 0, NA, 0, 0, 12, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, NA, 0, 0, 0, 20.19, 0, 0, 0), 
		Ca = c(0, 0, NA, 0, 0, 23, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, NA, 0, 0, 0, 0, 0, 0.4, 0), 
		Zn = c(0, 0, NA, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, NA, 0, 40.5, 0, 0, 0, 0, 0)
	)
		
	
	ftab <- ftab[match(colnames(fert), ftab$name), ]
## define NPK according to R script that comes with the data
	ftab[ftab$name=="NPK", c("N", "P", "K", "S")] <- c(12, 20, 13, 0)	
### none applied anyway
	ftab[ftab$name=="NPKS", c("N", "P", "K", "S")] <- c(12, 20, 13, 0)	
	fert[is.na(fert)] <- 0
  
 	# NPK percentages from the R script that was published with the data
	d$N_fertilizer <- colSums(t(fert) * ftab$N / 100)
	d$P_fertilizer <- colSums(t(fert) * ftab$P / 100)
	d$K_fertilizer <- colSums(t(fert) * ftab$K / 100)
	d$S_fertilizer <- colSums(t(fert) * ftab$S / 100)
	d$B_fertilizer <- colSums(t(fert) * ftab$B / 100)
	d$Zn_fertilizer <- colSums(t(fert) * ftab$Zn / 100)

	d$OM_used <- r$E.q5101_FYM == "yes"
	d$OM_type <- paste0("farmyard manure (", tolower(r$E.q5102_typeFYM), ")")
	d$OM_type[!d$OM_used] <- NA
	d$OM_amount <- r$E.q5103_amtFYM / plot_ha


##	p <- r[, c("F.q5112_priceDAP", "F.q5113_priceNPK", "F.q5114_priceUrea", "F.q5127_priceMoP", "F.q5115_priceZnSO4", "F.q5116_priceGypsum", "F.q5117_priceBoron", "F.q5126_priceNPKS", "F.q5128_priceSSP", "F.q5129_priceTSP")]
##	nms <- c("DAP", "NPK", "urea", "KCl", "S", "gypsum", "B", "NPKS", "SSP", "TSP")

	crop_cut_biomass <- data.frame(
		bm1 = r$B.q201_q1tagb, 
		bm2 = r$B.q204_q2tagb, 
		bm3 = r$B.q207_q3tagb
	)
	# biomass from 2*2 quadrants
	if (nrow(crop_cut_biomass) > 0) {
		d$dmy_total <- 10000 * rowMeans(crop_cut_biomass) / 4
	}

	crop_cut_yield <- data.frame(
		gw1 = r$B.q202_q1gWeight,
		gw2 = r$B.q205_q2gWeight,
		gw3 = r$B.q208_q3gWeight
	)

	if (nrow(crop_cut_yield) > 0) {
		crop_cut_yield[crop_cut_yield==0] <- NA
		crop_cut_yield <- 10000 * rowMeans(crop_cut_yield, na.rm=TRUE) / 4
	}
	moist <- data.frame(
		m1 = r$B.q203_q1gMoist,
		m2 = r$B.q206_q2gMoist,
		m3 = r$B.q209_q3gMoist
	)
	if (nrow(moist) > 0) {
		moist[moist==0] <- NA
		crop_cut_moist <- rowMeans(moist, na.rm=TRUE)
		crop_cut_moist[is.na(crop_cut_moist)] <- 14
		crop_cut_yield <- crop_cut_yield * (100 - crop_cut_moist) / 86
	}
	
	d$yield <- 10 * r$L.q606_largestPlotYieldQUNITAL / plot_ha
	
	if (!is.null(d$crop_cut)) {
		d$crop_cut <- !is.na(crop_cut_yield)
		d$yield[d$crop_cut] <- crop_cut_yield[d$crop_cut] 
	}
	
	d$crop_price <- r$M.q706_cropSP
	d
}






N2A_monitoring_2 <- function(ff, path) {	

	fix_crop <- function(p) {
	
		p[p=="busbean"] <- "bush bean"	
		p[p=="bushbean"] <- "bush bean"	
		p[p=="oignon"] <- "onion"	
	
		p[grep("^grou", p, ignore.case=TRUE)] <- "groundnut"	
		p[p=="grundnut"] <- "groundnut"	
		p[p=="grungnut"] <- "groundnut"	
		p[grep("soja", p, ignore.case=TRUE)] <- "soybean"	
		p[grep("soy", p, ignore.case=TRUE)] <- "soybean"	
		p[grep("sweet p", p, ignore.case=TRUE)] <- "sweetpotato"	
		p[grep("sweetp", p, ignore.case=TRUE)] <- "sweetpotato"	
		p[grep("swetpot", p, ignore.case=TRUE)] <- "sweetpotato"	
		p <- gsub("n/a", NA, p)
		p <- gsub("tobaco", "tobacco", p)
		p <- gsub("beans", "common bean", p)
		p <- gsub("pumpkins", "pumpkin", p)
		p <- gsub("irish potatoes", "potato", p)
		p <- gsub("irish potato", "potato", p)
		p <- gsub("patatoe", "potato", p)
		p <- gsub("patato", "potato", p)
		p <- gsub(" ma$", " maize", p)
		p <- gsub(", ", "; ", p)
		p <- gsub(" ;", ";", p)
		p <- gsub(" and ", "; ", p)
		p <- gsub("\\+|/| &|&|,", "; ", p)
		p <- gsub("maize; bean", "maize; common bean", p)
		p <- gsub("farrow", "no crop", p)
		p <- gsub("fallow", "no crop", p)
		p <- gsub("pegion pea", "pigeon pea", p)
		p <- gsub("groundnuts", "groundnut", p)
		p <- gsub("local maize", "groundnut", p)
		p <- gsub("fingermillet", "finger millet", p)
		p <- gsub("amaranthas", "amaranth", p)
		p <- gsub("amaranthus", "amaranth", p)
		p <- gsub("tomatoes", "tomato", p)
		p <- gsub("green amarantha", "amaranth", p)
		p <- gsub("rice upland", "rice", p)
		p <- gsub("kales", "kale", p)
		p <- gsub(" intercrop", "", p)
		p <- gsub("cowpeas", "cowpea", p)
		p <- gsub("simsim", "sesame", p)
		p <- gsub("sugar cane", "sugarcane", p)
		p <- gsub(" ;", ";", p)
		p <- gsub("  ", " ", p)
		trimws(p)

	}
	
	# read the data
	bn <- basename(ff)
	r0 <- read.csv(ff[bn == "a_general.csv"])
	r1 <- read.csv(ff[bn == "c_use_of_package_2.csv"])
	r1$SN <- r1$instanceid <- NULL
	r2 <- read.csv(ff[bn == "e_harvest.csv"])
	r2$SN <- r2$instanceid <- NULL
	#f3 <- ff[bn == "c_use_of_package_3.csv"]
	#r3 <- read.csv(f3) 
	r4 <- read.csv(ff[bn == "d_cropping_calendar.csv"])
	r5 <- read.csv(ff[bn == "b_info_site_2.csv"])
	
	#start processing the 1st data
	d <- data.frame(
		country = r0$country, 
		adm2 = carobiner::fix_name(r0$district, "title"), 
		adm3 = carobiner::fix_name(r0$sector_ward, "title"), 
		location = carobiner::fix_name(r0$vilage, "title"), 
		latitude = r0$gps_latitude, 
		longitude = r0$gps_longitude, 
		season = r0$season, 
		farm_id = r0$farm_id
	)



#farm_id plot_no crop_1_area_harvested crop_1_plants_no crop_1_weight_stover crop_1_weight_grain crop_1_grain_unshelled

	dd <- merge(r1, r2, by = c("id", "farm_id", "plot_no"), all.x = TRUE )

	p <- carobiner::fix_name(dd$mineral_fert_type, "upper")
	p[grepl("UREA", p)] <- "urea"
	
	p[grepl("^SYMP", p)] <- "sympal"
	p[grepl("^S ", p)] <- "S-compound"
	p[grepl("^D ", p)] <- "D-compound"
	p[grepl("^S-COM", p)] <- "S-compound"
	p[grepl("^D-COM", p)] <- "D-compound"
	p[grepl("SUPER D", p)] <- "D-compound"
	p[grepl("SINGLE SUPER PHOSPHATE", p)] <- "SSP"
	p[p == "SUPER PHOSPHATE"] <- "SSP"
	p[p %in% c("NONE", "NOON", "NON", "NO")] <- "none"
	p[p == "FERTILIZER"] <- NA
	p[p == "23:21:0+4S"] <- NA
	p[p == "0.972916667"] <- NA
	
	dd$fertilizer_type <- p

## it is not clear what the quantities refer to if there are multiple products 
## that much of each?	
	    # to get the fertilizer/ha
	ftab <- data.frame(
		name = c("AN", "ATS", "basic slag", "CAN", "C-compound", "CMP", "DAP", "DAS", "D-compound", "DSP", "ERP", "GRP", "gypsum", "KCl", "KNO", "lime", "none", "MAP", "NPK", "NPKS", "NPS", "PKS", "SCU", "SOP", "SSP", "sympal", "TSP", "unknown", "urea", "ZnSO4", "S-compound", "MgSO4", "H3BO3", "CaCO3", "borax"), 
		N = c(34L, 12L, NA, 26L, 7L, 0L, 18L, 21L, 10L, 18L, 0L, 0L, 0L, 0L, 13L, 0L, 0L, 11L, NA, 8L, 23L, 0L, 39L, 0L, 0L, 0L, 0L, NA, 46L, 0L, 8L, 0L, 0L, 0L, 0L), 
		P = c(0, 0, NA, 0, 9.156, 7, 20.1, 0, 20, 20, NA, NA, 0, 0, 0, 0, 0, 52, NA, 21, 21, NA, 0, 0, 8.74, 23, 19.23, NA, 0, 0, 21, 0, 0, 0, 0), 
		K = c(0, 0, NA, 0, 5.785, 0, 0, 0, 10, 0, 0, 0, 0, 49.8, 44, 0, 0, 0, NA, 7, 0, NA, 0, 41.5, 0, 15, 0, NA, 0, 0, 7, 0, 0, 0, 0), 
		S = c(0, 26, NA, 0, 6, 0, 0, 24, 9, 0, 0, 0, 19, 0, 0, 0, 0, 0, 0, 4, 4, NA, 13, 18, 0, 4, 0, NA, 0, 19.9, 4, 13.19, 0, 0, 0), 
		B = c(0, 0, NA, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, NA, 0, 0, 0, 0, 17.48, 0, 11.3), 
		Mg = c(0, 0, NA, 0, 0, 12, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, NA, 0, 0, 0, 20.19, 0, 0, 0), 
		Ca = c(0, 0, NA, 0, 0, 23, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, NA, 0, 0, 0, 0, 0, 0.4, 0), 
		Zn = c(0, 0, NA, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, NA, 0, 40.5, 0, 0, 0, 0, 0)
	)

	
## NPK is undefined need to check of 20-20-20 is a good guess
	ftab[ftab$name=="NPK", c("N", "P", "K", "S")] <- c(20, 20, 20, 0)	
	get_elements <- carobiner::get_function("get_elements_from_product", path, group)
	elements <- get_elements(ftab, p)
	
	dd <- cbind(dd, elements)
  
	dd$OM_amount <- as.numeric(dd$organic_fert_amount)
	dd$OM_used <- dd$organic_fert_amount > 0
	dd$OM_type <- carobiner::fix_name(dd$organic_fert_type, "tolower")
	dd$OM_type[!dd$OM_used] <- "none"

	dd$yield <- 10000 * dd$crop_1_weight_grain / dd$crop_1_area_harvested
	
	dd$inoculant_used[dd$inoculant_used == ""] <- NA
	dd$inoculated <- dd$inoculant_used == "Y"
	
	#standardizing the crops and variety
	dd$variety_type <- NA
	dd$crop <- fix_crop(dd$crop_1)
	i <- dd$crop == "bush bean"
	dd$crop[i] <- "common bean"
	dd$variety_type[i] <- "bush bean"
	i <- dd$crop == "climbing bean"
	dd$crop[i] <- "common bean"
	dd$variety_type[i] <- "climbing bean"

	dd$variety <- carobiner::fix_name(dd$variety_1, "title")
	dd$rep <- dd$plot_no
 	
## to do: also deal with crop_2/variety_2 
	dd <- dd[, c("id", "farm_id", "rep", "crop", "variety", "inoculated", "fertilizer_type", "N_fertilizer", "P_fertilizer", "K_fertilizer", "yield", "OM_used", "OM_type", "OM_amount")]
	
	#get the dates information
	if (!is.null(r4$date_planting_yyyy)) {
		dd4 <- r4[, "farm_id", drop=FALSE]	
		p <- apply(r4[, c("date_planting_yyyy", "date_planting_mm", "date_planting_dd")], 1, paste, collapse="-")
		dd4$planting_date <- as.character(as.Date(p))

		i <- r4$date_harvest_dd == 0
		j <- r4$date_harvest_yy == 0
		r4$date_harvest_dd[i] <- 15
		h <- apply(r4[, c("date_harvest_yyyy", "date_harvest_mm", "date_harvest_dd")], 1, paste, collapse="-")
		h[j] <- NA

		dd4$harvest_date <- as.character(as.Date(h))
	} else {
	## cannot merge without plot_id!!
#		pd <- r4[r4$activity == 'Date of planting', ]
#		pd$planting_date <- apply(pd[, c("yyyy", "mm", "dd")], 1, paste, collapse="_")
#		hd <- r4[r4$activity == 'Date of harvest', ]
#		hd$harvest_date <- apply(hd[, c("yyyy", "mm", "dd")], 1, paste, collapse="_")
#		v <- c("id", "farm_id")
#		m <- merge(pd[,c(v, "planting_date")], hd[,c(v, "harvest_date")], by=v)
		dd4 <- NULL
	}

	#standardizing the previous crop variable

	dd5 <- r5[, "farm_id", drop=FALSE]	
	dd5$previous_crop <- fix_crop(carobiner::fix_name(r5$main_crop_last_season, "lower"))
	
	#merge the data sets
	z <- merge(dd, d, by = "farm_id", all.x=TRUE)
	#z <- merge(z, d3, by = "farm_id", all.x=TRUE)
	z <- merge(z, dd4, by = "farm_id", all.x=TRUE)
	z <- merge(z, dd5, by = "farm_id", all.x=TRUE)
	
	z$yield_part <- "seed"
	z$trial_id <- z$farm_id
	z$farm_id <- NULL
	z$id <- NULL
	
	z
}





N2A_monitoring_1 <- function(ff) {	

	fix_crop <- function(p) {
		p[grep("^grou", p, ignore.case=TRUE)] <- "groundnut"	
		p[grep("soy", p, ignore.case=TRUE)] <- "soybean"	
		p[grep("sweet pot", p, ignore.case=TRUE)] <- "sweetpotato"	
		p <- gsub("tobaco", "tobacco", p)
		p <- gsub("beans", "common bean", p)
		p <- gsub("irish potatoes", "potato", p)
		p <- gsub(" ma$", " maize", p)
		p <- gsub(", ", "; ", p)
		p
	}

	# read the data
	bn <- basename(ff)
	f0 <- ff[bn == "a_general.csv"]
	r0 <- read.csv(f0)
	
	f1 <- ff[bn == "c_use_of_package_1.csv"]
	d1 <- read.csv(f1)
	
	f2 <- ff[bn == "e_harvest.csv"]
	d2 <- read.csv(f2)
	
	f3 <- ff[bn == "c_use_of_package_3.csv"]
	d3 <- read.csv(f3) 

	f4 <- ff[bn == "d_cropping_calendar.csv"]
	r4 <- read.csv(f4)
	
	f5 <- ff[bn == "b_info_site_2.csv"]
	d5 <- read.csv(f5)
	
	#start processing the 1st data
	d <- data.frame(
		country = r0$country, 
		adm2 = carobiner::fix_name(r0$action_site, "Title"), 
		adm3 = carobiner::fix_name(r0$sector_ward, "Title"), 
		location = carobiner::fix_name(r0$vilage, "Title"), 
		latitude = r0$gps_latitude, 
		longitude = r0$gps_longitude, 
		season = r0$season, 
		farm_id= r0$farm_id
	)

	#subset the variables of interest in d1 and d2	
	d1 <- d1[, c("farm_id", "plot_no", "crop", "variety", "inoculant_used", "min_fertilizer_type", "min_fertiliser_amount_kg", "org_fertilizer_type", "org_fertilizer_amount_kg")]
	d2 <- d2[, c("farm_id", "plot_no", "area_harvested_m2", "weight_kg")]
	dd <- merge(d1, d2, by = c("farm_id", "plot_no"), all.x = TRUE )
	
	# working on fertilizer types
	dd$min_fertilizer_type[dd$min_fertilizer_type %in% c("SSP/Urea", "SSP+Ureia", "SSP+Urea", "Urea+SSP")] <- "SSP; urea"	
	dd$min_fertilizer_type[dd$min_fertilizer_type %in% c("ssp", "Phosphor(SSP)", "SSP+Inoc", "Y", "12")] <- "SSP"
	dd$min_fertilizer_type[dd$min_fertilizer_type %in% c("Ureia", "Urea")] <- "urea"
	dd$min_fertilizer_type[dd$min_fertilizer_type %in% c( "None", "", "N")] <- "none"
	
	#working on fertilizer amounts
	dd$min_fertiliser_amount_kg[dd$min_fertiliser_amount_kg %in% c("2/0.4", "2 /0.4", "2/0,4", "2/ 0,4")] <- "2/0.4"
	dd$min_fertiliser_amount_kg <- carobiner::replace_values(dd$min_fertiliser_amount_kg, c("2kg/ha", "", "N"), c("2", "0", "0"))
	
	#split fertilizer amount column to separate urea amounts and SSP amounts
	dd$ssp_amt <- ifelse(dd$min_fertilizer_type != "urea", as.numeric(sub("/.*", "", dd$min_fertiliser_amount_kg)), 0)
	dd$urea_amt <- ifelse(dd$min_fertilizer_type == "urea", as.numeric(sub(".*?/", "", dd$min_fertiliser_amount_kg)), 0)
	dd$fertilizer_type <- dd$min_fertilizer_type
	#to get rates of N and P
	dd$P_rate_plot <- dd$ssp_amt*0.16
	
	# for 10.25502/hwdb-p578
	#v <- carobiner::replace_values(dd$area_harvested_m2, c(101, 102, 103, 104), 							c(100, 100, 100, 100))

	## how so ???
	#v[is.na(v)] <- 100
	#dd$area_harvested_m2 <- v
	
	dd$P_fertilizer <- (10000/dd$area_harvested_m2) * dd$P_rate_plot
	dd$N_rate_plot <- dd$urea_amt*0.467
	dd$N_fertilizer <- (10000/dd$area_harvested_m2) * dd$N_rate_plot
	dd$K_fertilizer <- 0
	#getting the yield
	dd$yield <- (10000/dd$area_harvested_m2) * dd$weight_kg
	
	#correcting mismatched rows in inoculated
	dd$inoculated <- dd$inoculant_used
	
	#standardizing the crops and variety
	dd$crop <- fix_crop(dd$crop)
	dd$variety <- carobiner::fix_name(dd$variety, "title")
	dd$rep <- dd$plot_no

## FIX NEEDED: ALSO USE organic_fert_type"  "organic_fert_amount
 	
	dd <- dd[, c("farm_id", "rep", "crop", "variety", "inoculated", "fertilizer_type", "N_fertilizer", "P_fertilizer", "K_fertilizer", "yield")]
	
	#get the spacing information
	dd3 <- d3[, "farm_id", drop=FALSE]
	dd3$row_spacing <- as.numeric(d3$crop_1_spacing_row_to_row)
	dd3$plant_spacing <- as.numeric(d3$crop_1_spacing_plant_to_plant)
	dd3 <- unique(dd3)
	
	dd4 <- unique(r4[, "farm_id", drop=FALSE])
	p <- r4[grepl("planting", r4$activity), ]
	h <- r4[grepl("harvest", r4$activity), ]
	p$planting_date <- with(p, paste(date_planting_yyyy, date_planting_mm, date_planting_dd, sep = "-"))
	h$harvest_date <- with(h, paste(date_planting_yyyy, date_planting_mm, date_planting_dd, sep = "-"))
	p$planting_date[p$date_planting_yyyy == 0] <- NA
	h$harvest_date[h$date_planting_yyyy == 0] <- NA

	p <- p[, c("farm_id", "planting_date")]
	h <- h[, c("farm_id", "harvest_date")]
	ph <- unique(merge(p, h, by = "farm_id", all=TRUE))
	dd4 <- merge(dd4, ph, by = "farm_id", all.x=TRUE)
	
	dd4$planting_date <- as.character(as.Date(dd4$planting_date, format = "%Y-%m-%d"))
	dd4$harvest_date <- as.character(as.Date(dd4$harvest_date, format = "%Y-%m-%d"))
	
	#standardizing the previous crop variable
	dd5 <- d5[, "farm_id", drop=FALSE]
	dd5$previous_crop <- fix_crop(carobiner::fix_name(d5$main_crop_last_season, "lower"))
	
	#merge the data sets
	z <- merge(d, dd, by = "farm_id", all.x=TRUE)
	z <- merge(z, dd3, by = "farm_id", all.x=TRUE)
	z <- merge(z, dd4, by = "farm_id", all.x=TRUE)
	z <- merge(z, dd5, by = "farm_id", all.x=TRUE)
	
	z$yield_part <- ifelse(z$crop == "groundnut", "pod", "seed")
	z$trial_id <- z$farm_id
	z$farm_id <- NULL
	z
}
