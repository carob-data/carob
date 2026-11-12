
## ISSUES
#no specification of total_production (yield) units
#Location names standardised (spelling variants merged). 
#Combined names split into admin levels: Kenya ward/village; DRC territory/groupement/village; Nigeria ward. village. Other admin levels taken from action_side
#Some data points for Ghana has missing location, hence longitude and latitude will be added soon as author responds

carob_script <- function(path) {

"N2Africa is to contribute to increasing biological nitrogen fixation and productivity of grain legumes among African smallholder farmers which will contribute to enhancing soil fertility, improving household nutrition and increasing income levels of smallholder farmers. As a vision of success, N2Africa will build sustainable, long-term partnerships to enable African smallholder farmers to benefit from symbiotic N2-fixation by grain legumes through effective production technologies including inoculants and fertilizers adapted to local settings. A strong national expertise in grain legume production and N2-fixation research and development will be the legacy of the project. The project is implemented in five core countries (Ghana, Nigeria, Tanzania, Uganda and Ethiopia) and six other countries (DR Congo, Malawi, Rwanda, Mozambique, Kenya & Zimbabwe) as tier one countries."


	uri <- "doi:10.25502/6TXR-K273/D"
	group <- "survey"

	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		data_organization = "IITA",
		publication = NA,
		project = "N2Africa",
		data_type = "survey",
		treatment_vars = "none",
		response_vars = "none",  
		carob_completion = 100,
		carob_contributor = "Mitchelle Njukuya",
		carob_date = "2026-06-11",
		carob_effort = NA,
		notes = NA, 
		design = NA
	)

	f1 <- ff[basename(ff) == "a1_a3_demographic.csv"]
	r1 <- read.csv(f1)
	
	f2 <- ff[basename(ff) == "a4_demographic.csv"]
	r2 <- read.csv(f2)
	
	f3 <- ff[basename(ff) == "c_labour.csv"]
	r3 <- read.csv(f3)
	
	f4 <- ff[basename(ff) == "d_hh_house_flooring.csv"]
	r4 <- read.csv(f4)
	
	f5 <- ff[basename(ff) == "d_hh_house_roof.csv"]
	r5 <- read.csv(f5)
	
	f6 <- ff[basename(ff) == "d_hh_house_wall.csv"]
	r6 <- read.csv(f6)
	
	f8 <- ff[basename(ff) == "d_hh_irrigation.csv"]
	r8 <- read.csv(f8)
	
	f9 <- ff[basename(ff) == "d_hh_power.csv"]
	r9 <- read.csv(f9)
	
	f10 <- ff[basename(ff) == "d_hh_water_source.csv"]
	r10 <- read.csv(f10)
	
	f11 <- ff[basename(ff) == "e_livestock_ownership.csv"]
	r11 <- read.csv(f11)
	
	f12 <- ff[basename(ff) == "f_land_holding.csv"]
	r12 <- read.csv(f12)
	
	f13 <- ff[basename(ff) == "g2_cult_legumes.csv"]
	r13 <- read.csv(f13)
	
	f14 <- ff[basename(ff) == "g3_production_legumes.csv"]
	r14 <- read.csv(f14)
	
	f15 <- ff[basename(ff) == "g4_inputs_other.csv"]
	r15 <- read.csv(f15)
	
	f16 <- ff[basename(ff) == "g5_prod_major_crops.csv"]
	r16 <- read.csv(f16)
	
	f17 <- ff[basename(ff) == "h4_legumes_hauls.csv"]
	r17 <- read.csv(f17)
	
	f18 <- ff[basename(ff) == "i_markets.csv"]
	r18 <- read.csv(f18)
	
	f19 <- ff[basename(ff) == "location.csv"]
	r19 <- read.csv(f19)

	
	d1 <- data.frame(
	  hhid = r1$id,
	  field_id = r1$farm_id,
	  hh_size = r1$household_number_of_people,
	  is_head = r1$household_head
	)
	
	d2 <- data.frame(
	  hhid = r2$id,
	  field_id = r2$farm_id,
	  age = r2$age,
	  sex = r2$gender,
	  education = r2$schooling_level
	)
	
	d3 <- data.frame(
	  hhid = r3$id,
	  field_id = r3$farm_id,
	  activity = r3$activity,
	  plot_labour = r3$no_days,
	  farm_labour_hired = r3$no_days,
	  cost = r3$cost_per_day_money
	)
	
	#multiple crops recorded in the dataset,it is however not specified if the crop were grown as intercrops/in rotation
	#d3$crop <- apply(
	  #r3[, c("crop_1", "crop_2", "crop_3", "crop_4")],1,
	  #function(x) paste(na.omit(x[x != ""]), collapse = " : "))
	
	d3$activity <- tolower(d3$activity)
	d3$land_prep_cost <- ifelse(d3$activity == "land preparation", d3$cost, 0)
	d3$planting_cost <- ifelse(d3$activity == "planting", d3$cost, 0)	
	d3$weeding_cost <- ifelse(d3$activity == "weeding", d3$cost, 0)
	d3$weeding_labour <- ifelse(d3$activity == "weeding", d3$plot_labour, 0)
	d3$irrigation_cost <- ifelse(d3$activity == "watering", d3$cost, 0)
	## OM_cost would be when you buy it. This seems to be the cost of moving your own OM around
	d3$OM_transp_cost <- ifelse(d3$activity %in% c("Transport of farmyard manure","Transport of manure"),d3$cost,0)
	## cost for fertilizer _application_ is not the same as the cost of "fertilizer"
	d3$fertilizer_app_cost <- ifelse(d3$activity == "fertilizer application", d3$cost, 	 	0)
	d3$fertilizer_labour <- ifelse(d3$activity == "fertilizer application",d3$plot_labour,0)
	d3$harvest_cost <- ifelse(d3$activity == "harvesting",d3$cost,0)
	
	d4 <- data.frame(
	  hhid = r4$id,
	  field_id = r4$farm_id,
	  floor_quality = r4$type
	)   	
	
	d5 <- data.frame(
	  hhid = r5$id,
	  field_id = r5$farm_id,
	  roof_quality = r5$type
	)
	
	d6 <- data.frame(
	  hhid = r6$id,
	  field_id = r6$farm_id,
	  wall_quality = r6$type
	)

#Aggregating done to carter for lack of a one-to-one correspondence between records across the three files.
	fagg <- function(x) paste(unique(x), collapse = ";")
	d4a <- aggregate(floor_quality ~ field_id, d4, FUN = fagg)
	d5a <- aggregate(roof_quality ~ field_id, d5, FUN = fagg)
	d6a <- aggregate(wall_quality ~ field_id, d6, FUN = fagg)
	
	d7 <- merge(d4a, d5a, by = "field_id", all = TRUE)
	d7 <- merge(d7, d6a, by = "field_id", all = TRUE)
	
	d7$housing_quality <- apply(
	  d7[, c("floor_quality", "roof_quality", "wall_quality")],  1,
	  function(x) paste(x[!is.na(x) & x != ""], collapse = ";")
	)

	d8 <- data.frame(
	  hhid = r8$id,
	  field_id = r8$farm_id,
	  irrigation_method = r8$type
	  )
	
	d9 <- data.frame(
	  hhid = r9$id,
	  field_id = r9$farm_id,
	  power_supply = r9$type
	  )
	
	d9$electricity <- grepl(
	  "electric|solar|generator|battery|batteries|bulb|pile|cell|torch|flash light|lamp",
	  tolower(trimws(d9$power_supply))
	)

	d10 <- data.frame(
	  hhid = r10$id,
	  field_id = r10$farm_id,
	  water_source = r10$type
	  )
	
	d11 <- data.frame(
	  hhid = r11$id,
	  field_id = r11$farm_id,
	  animal = trimws(tolower(r11$livestock)),
	  heads = r11$total_no
	)
	
	# Purpose first
	d11$purpose <- NA
	
	d11$purpose[grepl("dairy|caows for dairy", d11$animal)] <- "milk"
	d11$purpose[grepl("draft|oxen", d11$animal)] <- "draft-power"
	d11$purpose[grepl("meat", d11$animal)] <- "meat"
	d11$purpose[d11$animal == "broilers"] <- "meat"
	d11$purpose[d11$animal == "bees"] <- "honey"
	
	# Standardize livestock names
	d11$animal[grepl("cattle|cattke|dairy cows|caows for dairy|cows for meat|cows for draft|oxen|calves|^kid$|^kids$", d11$animal)] <- "cattle"
	
	d11$animal[grepl("goats|goast|gaots", d11$animal)] <- "goat"
	d11$animal[grepl("^sheep$", d11$animal)] <- "sheep"
	d11$animal[grepl("chickens|broilers", d11$animal)] <- "chicken"
	d11$animal[grepl("turkeys|turkey-cock", d11$animal)] <- "turkey"
	d11$animal[grepl("ducks|canard", d11$animal)] <- "duck"
	d11$animal[d11$animal == "goose"] <- "goose"
	d11$animal[grepl("guinea fowls", d11$animal)] <- "guinea fowl"
	d11$animal[grepl("doves/pigeons|doves\\|pigeons|doves/pigeons", d11$livestock)] <- "pigeon"
	d11$animal[d11$animal %in% c("pigs")] <- "pig"
	d11$animal[d11$animal %in% c("rabbits")] <- "rabbit"
	d11$animal[d11$animal %in% c("bees")] <- "bee"
	d11$animal[grepl("fish", d11$animal)] <- "fish"
	d11$animal[d11$animal == "donkeys"] <- "donkey"
	d11$animal[d11$animal == "horse"] <- "horse"
	d11$animal[d11$animal == "dogs"] <- "dog"
	d11$animal[grepl("guinea pigs|cobay", d11$animal)] <- "guinea pig"
	d11$animal[d11$animal == "pet rat"] <- "rat"
	d11$animal[d11$animal == "uc"] <- NA
	
	d12 <- data.frame(
	  hhid = r12$id,
	  field_id = r12$farm_id,
	  field_size = r12$area,
	  land_tenure = r12$ownership
	  )
	
	d13 <- data.frame(
	  hhid = r13$id,
	  field_id = r13$farm_id,
	  crop = trimws(tolower(r13$legume_type)),
	  field_size = r13$area
	)
	
	d13$fertilizer_type <- apply(r13[, c("m_fert_1", "m_fert_2", "m_fert_3")], 1,
	  function(x) paste(x[!is.na(x) & x != ""], collapse = ";"))
	
	d13$fertilizer_amount <- rowSums(r13[, grep("m_fert_,_amount", names(r13))],  na.rm = TRUE)
	
	d13$OM_type <- apply(r13[, c("o_fert_1", "o_fert_2", "o_fert_3")], 1, 
				function(x) paste(x[!is.na(x) & x != ""], collapse = ";"))
	
	d13$OM_amount <- rowSums(r13[, c("o_fert_1_amount","o_fert_2_amount","o_fert_3_amount")],na.rm = TRUE)
	
	d13$inoculated <- !(is.na(r13$inoculant) |trimws(tolower(r13$inoculant)) %in% c("", "0", "-9999", "no"))
	d13$inoculant <- r13$inoculant
	d13$inoculant[trimws(tolower(d13$inoculant)) %in% c("", "0", "-9999", "no")] <- NA
	
	d14 <- data.frame(
	  hhid = r14$id,
	  field_id = r14$farm_id,
	  crop = r14$legume,
	  yield_isfresh = TRUE,
	  yield = r14$total_production       #moisture content not recorded
	)    #yield was recorded as kg (mentioned in N2Africa report)
	
	d15 <- data.frame(
	  hhid = r15$id,
	  field_id = r15$farm_id,
	  crop = r15$crop
	  )
	
	# Fertilizer type
	d15$fertilizer_type <- apply(r15[, c("m_fert_1", "m_fert_2", "m_fert_3")], 1,
				function(x) paste(x[!is.na(x) & x != ""], collapse = ";"))
	
	# Organic matter type
	d15$OM_type <- apply(r15[, c("o_fert_1", "o_fert_2", "o_fert_3")], 1,
				function(x) paste(x[!is.na(x) & x != ""], collapse = ";"))
	
	# Insecticide product
	d15$insecticide_product <- apply(r15[, c("bio_pest_1", "bio_pest_2", "bio_pest_3")], 1,
			function(x) paste(x[!is.na(x) & x != ""], collapse = ";"))
	
	d16 <- data.frame(
	  hhid = r16$id,
	  field_id = r16$farm_id,
	  crop = r16$crop,
	  yield_isfresh = TRUE,
	  yield = r16$total_production       #moisture content not recorded
	)
	
	d17 <- data.frame(
	  hhid = r17$id,
	  field_id = r17$farm_id,
	  crop = r17$legume,
	  previous_crop_residue_management = r17$hauls_purpose
	 )
	
	d18 <- data.frame(
	  hhid = r18$id,
	  field_id = r18$farm_id,
	  market_type = r18$market_kind,
	  market_access = r18$place_name,
	  market_distance = r18$distance
	  )
	
	## coordinate typos fixed before parsing (checked against neighbouring farms in the same location)
	r19$homestead_coord_northing[r19$farm_id == "RWA037"] <- "S01.48.53.8"   # was S02.48 (Tanzania); other Gakenyeri farms S01.48-49
	r19$homestead_coord_northing[r19$farm_id == "RWA330"] <- "S01.34.56"     # was S01.3456; other Nyamusanze farms S01.34.5x
	r19$homestead_coord_northing[r19$farm_id == "MOZ018"] <- "14.08.48,4"    # was 15.08 (Malawi); other luelele farms 14.08
	r19[r19$farm_id == "MOZ027", c("homestead_coord_northing", "homestead_coord_easting")] <- c("19.32.42,3", "32.53.51,0")  # lat/lon swapped
	r19[r19$farm_id %in% c("RWA151", "MOZ048"), c("homestead_coord_northing", "homestead_coord_easting")] <- NA  # fall in DRC / Zimbabwe, no basis to correct
	
	parse_coord <- function(x) {
	  x <- toupper(trimws(as.character(x)))
	  x <- gsub("O", "0", x)                      # letter O typed for zero (EOO842, O29)
	  x <- gsub("[A-Z]", "", x)                   # drop hemisphere letters; sign is set by country
	  x <- trimws(gsub("[^0-9.,]+", " ", x))      # ? * ' - and spaces are separators
	  sapply(x, function(s) {
	    if (is.na(s) || s == "") return(NA_real_)
	    tok <- regmatches(s, gregexpr("[0-9]+", s))[[1]]
	    sep <- regmatches(s, gregexpr("[^0-9]+", s))[[1]]
	    if (length(tok) > 3 && tok[1] == "0") {tok <- tok[-1]; sep <- sep[-1]}   # "0,36.42.33,3"
	    n <- length(tok)
	    num <- as.numeric(tok)
	    if (n == 1) {
	      v <- num
	      if (v > 180) v <- floor(v/100) + (v %% 100)/60          # DDMM
	    } else if (n == 2) {
	      if (num[1] > 180) {                                   # DDMM.mmm / DDDMM.mmm
	        m <- as.numeric(paste0(substr(tok[1], nchar(tok[1])-1, nchar(tok[1])), ".", tok[2]))
	        v <- floor(num[1]/100) + m/60
	      } else {                                              # decimal degrees
	        v <- as.numeric(paste0(tok[1], ".", tok[2]))
	      }
	    } else if (n == 3) {
	      if (nchar(tok[3]) == 2 && grepl("[.]", sep[1]) == grepl("[.]", sep[2]) && !grepl(",", sep[2])) {
	        m <- num[2]; sec <- num[3]                            # D.M.S
	      } else {
	        m <- as.numeric(paste0(tok[2], ".", tok[3])); sec <- 0   # D M.mmm
	      }
	      if (m >= 60 || sec >= 60) return(NA_real_)
	      v <- num[1] + m/60 + sec/3600
	    } else if (n == 4) {                                        # D M S.s
	      sec <- as.numeric(paste0(tok[3], ".", tok[4]))
	      if (num[2] >= 60 || sec >= 60) return(NA_real_)
	      v <- num[1] + num[2]/60 + sec/3600
	    } else return(NA_real_)
	    v
	  }, USE.NAMES = FALSE)
	}
	
	cc <- c(DR = "Democratic Republic of the Congo", GH = "Ghana", KE = "Kenya", MW = "Malawi",
	        MO = "Mozambique", NI = "Nigeria", RW = "Rwanda", ZI = "Zimbabwe")
	
	d19 <- data.frame(
	  hhid = r19$id,
	  field_id = r19$farm_id,
	  country = unname(cc[substr(r19$farm_id, 1, 2)]),
	  location = r19$location,
	  action = tolower(trimws(gsub("\\s+", " ", r19$action_side))),   # temporary, used for admin levels
	  elevation = r19$homestead_coord_altitude,
	  latitude = parse_coord(r19$homestead_coord_northing),
	  longitude = parse_coord(r19$homestead_coord_easting)
	)
	
	d19$elevation <- {
	  x <- trimws(as.character(d19$elevation))
	  is_ft <- grepl("ft$", x, ignore.case = TRUE)
	  num <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", x)))
	  num[is_ft] <- num[is_ft] * 0.3048
	  num
	}
	i <- d19$country == "Zimbabwe" & !is.na(d19$elevation) & d19$elevation > 2600   # Zimbabwe max ~2590 m; these are feet
	d19$elevation[i] <- d19$elevation[i] * 0.3048
	
	south <- d19$country %in% c("Democratic Republic of the Congo", "Mozambique", "Rwanda", "Zimbabwe", "Malawi")
	d19$latitude[south] <- -d19$latitude[south]
	d19$longitude[d19$country == "Ghana"] <- -d19$longitude[d19$country == "Ghana"]
	
	#set to NA values outside the country bounding box (unrecoverable typos)
	bb <- data.frame(country = cc,
	                 xmin = c(12, -3.3, 33.9, 32.6, 30.2, 2.6, 28.8, 25.2), xmax = c(31.4, 1.2, 41.9, 35.9, 41, 14.7, 30.9, 33.1),
	                 ymin = c(-13.5, 4.5, -4.7, -17.2, -26.9, 4.2, -2.9, -22.5), ymax = c(5.4, 11.2, 5.1, -9.3, -10.4, 13.9, -1, -15.6))
	i <- match(d19$country, bb$country)
	out <- !(d19$longitude >= bb$xmin[i] & d19$longitude <= bb$xmax[i] & d19$latitude >= bb$ymin[i] & d19$latitude <= bb$ymax[i])
	out[is.na(out)] <- TRUE
	d19$latitude[out] <- d19$longitude[out] <- NA
	d19$geo_from_source <- !is.na(d19$latitude)
	
	#cleaning spelling variants in location names
	x <- tolower(trimws(gsub("\\s+", " ", d19$location)))
	x <- gsub("\\s*/\\s*", "/", x)                            # "Buhene / buhene" -> "buhene/buhene"
	x <- gsub("\\s*,\\s*", "/", x)                            # "Kako, Cegera" -> "kako/cegera"
	x[x == ""] <- NA
	ng <- d19$country == "Nigeria" & grepl("\\.", x)
	ward <- ifelse(ng, trimws(sub("\\..*$", "", x)), NA)      # Nigeria: "ward. village"
	
	fix <- c(
	  #Kenya
	  "wamuluma/mbaya" = "wamuluma/mbaya",
	  #DR Congo
	  "buhene/buhene" = "buhene", "buhene/walungu" = "walungu/buhene",
	  "mulamba/ndala" = "mulamba/ndola", "ndala" = "ndola", "ndolo" = "ndola",
	  "kabiganda" = "kabinganda", "kaviganda" = "kabinganda", "kabinganda/kabinganda" = "kabinganda",
	  "nyangezi/kabiganda" = "nyangezi/kabinganda", "nyagezi/kabiganda" = "nyangezi/kabinganda", "nyangezi/kaviganda" = "nyangezi/kabinganda",
	  "konge" = "konge 1", "kinge" = "konge 1", "konge 1er/mudaka" = "konge 1/mudaka", "mudaka/konde 1" = "konge 1/mudaka",
	  "bulonge i" = "bulonge 1", "bulonge/bulonge 1" = "bulonge 1", "mulamba/bulonge" = "walungu/bulonge",
	  "buhehe/buhehe" = "buhehe", "buhehe/buruma" = "buhehe/buhuma", "buhehe kabare" = "buhehe", "buhehe kabama" = "buhehe",
	  "kako/cegera" = "cegera/kako", "cigera/kako" = "cegera/kako",
	  "canya 1 er" = "canya 1er", "canya 1 er/bushwira" = "canya 1er/bushwira",
	  "kasheke/kahumbiro" = "kasheke/kabumbiro", "kasheke/karumbiro" = "kasheke/kabumbiro", "kasheke/kabambiro" = "kasheke/kabumbiro",
	  "kasheke/kavumbiro" = "kasheke/kabumbiro", "kabambiro" = "kabumbiro", "kabumbiro/kacheke" = "kasheke/kabumbiro",
	  "chigazi" = "cigazi", "chigazi/kacheke" = "kasheke/cigazi",
	  "mwendo ibona" = "mwendo/ibona", "ibona/mwendo" = "mwendo/ibona",
	  "cibanda/budara" = "cibanda/budaha", "madaka/cibanda budaha" = "cibanda/budaha", "madaka/cibanda" = "cibanda/madaka",
	  "chibanda" = "cibanda", "changombe/chibanda" = "cibanda/cagombe", "changombo/chibanda" = "cibanda/cagombe",
	  "changombe/chiranda" = "cibanda/cagombe", "changombe" = "cagombe", "cangombe" = "cagombe",
	  "ibamba/walungu/ikoma" = "ibamba/ikoma", "kajabu/ibamba" = "ibamba/kajabu",
	  #Rwanda
	  "kidehero" = "kadehero", "kadehe" = "kadehero", "gafunzo" = "nyagafunzo", "butimba2" = "butimba 2",
	  #Ghana
	  "manyaro" = "manyoro", "maryoro" = "manyoro", "mayaro" = "manyoro", "naa" = "naaga",
	  "tanima" = "tanina", "tamina" = "tanina", "tanani" = "tanina", "tanigna" = "tanina",
	  "tibane" = "tibani", "tibaru" = "tibani", "zambogo" = "zambogu", "goothi" = "goohi",
	  "tilli" = "tillizongo", "tilli zongo" = "tillizongo", "bawio" = "bawiu", "bawiwo" = "bawiu", "gaani" = "gaane",
	  "nyagemia" = "nayagnia", "nyagaroa" = "nayagnia",
	  "apoatoabogo" = "apotoabogo", "apotdabago" = "apotoabogo", "apadabogo" = "apotoabogo", "apotoadabogo" = "apotoabogo",
	  "kpaghlagi" = "kpaglaghi", "dafiama" = "daffiama", "dafiamah" = "daffiama",
	  "nabujoku" = "nabujako", "nabujoka" = "nabujako", "andonye manu" = "andonyemanu",
	  "dulzugu" = "dulizugu", "dolzugu" = "dulizugu", "wogandoo" = "wogando", "ugaadu" = "ugaado",
	  "tarikpali" = "tarikpaa", "napirisi" = "naprisi", "duluu kukuo" = "dalun kukuo", "jaudori kukuo" = "janjori kukuo",
	  "tonjina" = "tonginga", "tonjing" = "tonginga",
	  #Malawi
	  "m'memo" = "mmemo", "mtema2" = "mtema 2", "mtama 2" = "mtema 2", "bwalo ii" = "bwalo 2",
	  "mtupila" = "ntupila", "mtupira" = "ntupila",
	  "chikhwiri" = "chinkhwiri", "chinkhwili" = "chinkhwiri", "chinkhwila" = "chinkhwiri",
	  "chiwunjicha" = "chiunjiza", "chiunjidza" = "chiunjiza", "nkwali" = "nkhwali", "makwangala" = "makwangwala",
	  "mwenda phazo" = "mwendaphazo", "thengoliiweta" = "thengoliweta",
	  #Zimbabwe
	  "mutohwa" = "mutowa", "mataranyyika" = "mataranyika", "mataranyik" = "mataranyika",
	  "zinudza" = "zunidza", "zinhudza" = "zunidza", "tesekese" = "tsekese", "katsekesa" = "tsekese", "katsekese" = "tsekese",
	  #Nigeria
	  "bunkure. un/bunkure" = "bunkure. u/bunkure", "bunkure. bunkure u/tuta" = "bunkure. u/tuta", "bunkure. lautaye bunkura" = "bunkure. lautaye bunkure",
	  "busaye. busaya yamma" = "busaye. busaye yamma", "busaye. busaye yama village" = "busaye. busaye yamma",
	  "kogar kaza. kafar kaza" = "kogar kaza", "kogar kaza. kofar kaza" = "kogar kaza",
	  "karefa. karefe" = "karefa", "karefa. kafere" = "karefa",
	  "fadan yaya. fandan yaya" = "fadan yaya", "fadan yaya. fadanyaya" = "fadan yaya", "fadan yaya. fan danyaya" = "fadan yaya",
	  "sidire. sidere" = "sidire", "gwanairi. gwaneri" = "gwanairi",
	  "zangon-aya. zangon aya" = "zangon-aya", "zangon-aya. zangpm-aya" = "zangon-aya", "zangon-aya. zangpn-aya" = "zangon-aya",
	  "zangon-aya. zongan-aya" = "zangon-aya",
	  #Mozambique
	  "rotunda sede" = "rotanda sede", "rotanta sede" = "rotanda sede", "rotanda-sede" = "rotanda sede",
	  "maunhinga" = "munhinga", "ruace" = "ruasse", "magigi" = "magige",
	  "congernge" = "congerenge", "gongerenge" = "congerenge", "muitas" = "muita", "leulele" = "luelele"
	)
	i <- x %in% names(fix)
	x[i] <- fix[x[i]]
	
	#Nigeria: keep the village part; ward goes to adm3
	ng <- d19$country == "Nigeria" & grepl("\\.", x)
	suf <- trimws(sub("^[^.]*\\.", "", x[ng]))
	x[ng] <- ifelse(suf == "", trimws(sub("\\..*$", "", x[ng])), suf)
	d19$location <- x
	
	#assigning admin levels
	d19$adm1 <- d19$adm2 <- d19$adm3 <- d19$adm4 <- NA
	a <- d19$action
	#Kenya: county / sub-county / ward (the ward is also the first part of "ward/village" in row data)
	ke <- d19$country == "Kenya"
	kmap <- rbind(
	  c("bondo|north sakwa", "Siaya", "Bondo", NA),
	  c("rarieda", "Siaya", "Rarieda", NA),
	  c("kisumu west", "Kisumu", "Seme", NA),
	  c("south ge", "Siaya", "Gem", NA),
	  c("east kanyamkago", "Migori", "Uriri", "East Kanyamkago"),
	  c("central kanyamkago", "Migori", "Uriri", "Central Kanyamkago"),
	  c("wamuluma|mudete|masigolo", "Vihiga", NA, NA))
	for (k in 1:nrow(kmap)) {
	  i <- ke & grepl(kmap[k, 1], a)
	  d19$adm1[i] <- kmap[k, 2]; d19$adm2[i] <- kmap[k, 3]; d19$adm3[i] <- kmap[k, 4]
	}
	i <- ke & grepl("^(south gem|north sakwa|east asembo|south central seme)/", d19$location)
	d19$adm3[i] <- sub("/.*", "", d19$location[i])
	d19$location[i] <- sub(".*/", "", d19$location[i])
	
	#DRC
	dr <- d19$country == "Democratic Republic of the Congo"
	d19$adm1[dr] <- "South Kivu"
	grp <- c("nduba|ndumba" = "Nduba", "mulamba" = "Mulamba", "ikoma" = "Ikoma", "nya?n?gezi" = "Nyangezi",
	         "mushinga" = "Mushinga", "karhongo" = "Karhongo", "bugore|bughore" = "Bugorhe", "mudaka" = "Mudaka",
	         "mumosho" = "Mumosho", "bushwira" = "Bushwira", "katana" = "Irhambi-Katana",
	         "[mbp]inga sud" = "Mbinga-Sud", "[mbp]inga nord" = "Mbinga-Nord")
	terr <- c("Walungu" = "Mulamba|Nduba|Ikoma|Nyangezi|Mushinga|Karhongo", 
	          "Kabare" = "Bugorhe|Mudaka|Mumosho|Bushwira|Irhambi-Katana", "Kalehe" = "Mbinga-Sud|Mbinga-Nord")
	for (k in names(grp)) {
	  
	  parts <- strsplit(ifelse(dr & !is.na(d19$location), d19$location, ""), "/")
	  hit <- vapply(parts, function(p) any(grepl(paste0("^(", k, ")$"), p)), logical(1))
	  i <- dr & hit & is.na(d19$adm4)
	  d19$adm4[i] <- grp[k]
	  d19$location[i] <- vapply(parts[i], function(p) {q <- p[!grepl(paste0("^(", k, ")$"), p)]; paste(if (length(q)) q else p, collapse = "/")}, character(1))
	}
	for (k in names(grp)) {
	  i <- dr & is.na(d19$adm4) & grepl(k, a)
	  d19$adm4[i] <- grp[k]
	}
	d19$adm2[dr & grepl("walung", a)] <- "Walungu"
	d19$adm2[dr & grepl("kabare", a)] <- "Kabare"
	d19$adm2[dr & grepl("kalehe|kasheke", a)] <- "Kalehe"
	for (k in names(terr)) d19$adm2[dr & is.na(d19$adm2) & grepl(terr[k], d19$adm4)] <- k
	i <- dr & grepl("^(walungu|kabare)/", d19$location)            # "Walungu/Mwendo" -> territory + village
	d19$adm2[i] <- tools::toTitleCase(sub("/.*", "", d19$location[i]))
	d19$location[i] <- sub("^[^/]*/", "", d19$location[i])
	
	for (k in names(terr)) d19$adm2[dr & grepl(terr[k], d19$adm4)] <- k
	d19$adm2[dr & grepl("kasheke", d19$location)] <- "Kalehe"
	d19$adm4[dr & d19$adm2 == "Kalehe" & !grepl("Mbinga", d19$adm4)] <- NA
	
	#Ghana
	gh <- d19$country == "Ghana"
	gmap <- rbind(
	  c("bawk", "Upper East", "Bawku West"),
	  c("k/n/e|na[ns]+ena nankana|nankana", "Upper East", "Kassena Nankana East"),
	  c("chereponi", "North East", "Chereponi"),
	  c("sa[vr]e?lugu|savlugu|naprisi", "Northern", "Savelugu-Nanton"),
	  c("tolon", "Northern", "Tolon-Kumbungu"),
	  c("nado[wl]", "Upper West", "Nadowli"),
	  c("wa east", "Upper West", "Wa East"))
	for (k in 1:nrow(gmap)) {
	  i <- gh & grepl(gmap[k, 1], a)
	  d19$adm1[i] <- gmap[k, 2]; d19$adm2[i] <- gmap[k, 3]
	}
	
	#Malawi
	mw <- d19$country == "Malawi"
	d19$adm1[mw] <- "Central"
	d19$adm2[mw & grepl("dowa", a)] <- "Dowa"
	d19$adm2[mw & grepl("lilongwe", a)] <- "Lilongwe"
	d19$adm2[mw & grepl("ntch|mmemo", a)] <- "Ntcheu"
	d19$adm2[mw & grepl("salima", a)] <- "Salima"
	
	#Mozambique
	mz <- d19$country == "Mozambique"
	d19$adm1[mz & grepl("gur", a)] <- "Zambezia"; d19$adm2[mz & grepl("gur", a)] <- "Gurue"
	d19$adm1[mz & grepl("m,?andi", a)] <- "Niassa"; d19$adm2[mz & grepl("m,?andi", a)] <- "Mandimba"
	d19$adm1[mz & grepl("sussundenga|manica", a)] <- "Manica"; d19$adm2[mz & grepl("sussundenga|manica", a)] <- "Sussundenga"
	
	#Nigeria
	nga <- d19$country == "Nigeria"
	lga <- c("bunkure" = "Bunkure", "dawakin kudu" = "Dawakin Kudu", "garko" = "Garko", "tudun wada" = "Tudun Wada",
	         "igabi" = "Igabi", "giwa" = "Giwa", "kachia" = "Kachia", "zon?gon? kataf" = "Zangon Kataf")
	for (k in names(lga)) d19$adm2[nga & grepl(paste0("^", k), a)] <- lga[k]
	d19$adm1[nga & d19$adm2 %in% c("Bunkure", "Dawakin Kudu", "Garko", "Tudun Wada")] <- "Kano"
	d19$adm1[nga & d19$adm2 %in% c("Igabi", "Giwa", "Kachia", "Zangon Kataf")] <- "Kaduna"
	d19$adm3[nga] <- ward[nga]
	d19$adm3[nga & d19$location == "gumel"] <- "gumel"     # Gumel is a ward of Kachia
	
	#Rwanda
	rw <- d19$country == "Rwanda"
	rmap <- rbind(
	  c("cyabingo", "Northern", "Gakenke", "Cyabingo"),
	  c("nemba|neamba|muramba", "Northern", "Gakenke", "Nemba"),  # 'muramba' farms share Nemba's GPS
	  c("k[i]?noni", "Northern", "Burera", "Kinoni"),
	  c("burera", "Northern", "Burera", NA),
	  c("mus[a]?m[b]?ir", "Southern", "Kamonyi", "Musambira"),
	  c("n[y]+amiyaga", "Southern", "Kamonyi", "Nyamiyaga"),
	  c("kamonyi", "Southern", "Kamonyi", NA),
	  c("musenyi", "Eastern", "Bugesera", "Musenyi"),
	  c("bugesera", "Eastern", "Bugesera", NA),
	  c("rukara", "Eastern", "Kayonza", "Rukara"),
	  c("kayonza", "Eastern", "Kayonza", NA))
	for (k in 1:nrow(rmap)) {
	  i <- rw & grepl(rmap[k, 1], a)
	  d19$adm1[i] <- rmap[k, 2]; d19$adm2[i] <- rmap[k, 3]; d19$adm3[i] <- rmap[k, 4]
	}
	
	#Zimbabwe
	zw <- d19$country == "Zimbabwe"
	blk <- ceiling(as.numeric(sub("ZIM", "", d19$field_id[zw])) / 100)
	d19$adm2[zw] <- c("Guruve", "Makoni", "Mhondoro-Ngezi", "Mudzi")[blk]
	d19$adm1[zw] <- c("Mashonaland Central", "Manicaland", "Mashonaland West", "Mashonaland East")[blk]
	
	#Ghana
	d19$adm2[gh & d19$location %in% c("azupupunga", "goase", "sofo abdulai")] <- "Bawku West"
	d19$adm2[gh & d19$location %in% c("gaane", "pungu bawiwo")] <- "Kassena Nankana East"
	d19$adm1[gh & d19$adm2 %in% c("Bawku West", "Kassena Nankana East")] <- "Upper East"
	d19$adm3[rw & d19$location %in% "rukara"] <- "Rukara"
	
	#admin levels per location
	#in DRC and Ghana, where the action site is often inconsistent, all farms in a location take the most common value
	key <- paste(d19$country, d19$location)
	for (v in c("adm2", "adm4", "adm1", "adm3")) {
	  md <- ave(d19[[v]], key, FUN = function(z) {t <- table(z); if (length(t)) names(t)[which.max(t)] else NA})
	  i <- !is.na(d19$location) & !is.na(md) & (is.na(d19[[v]]) | d19$country %in% c("Democratic Republic of the Congo", "Ghana"))
	  d19[[v]][i] <- md[i]
	}
	d19$adm3[dr] <- c("Walungu" = "Ngweshe", "Kabare" = "Kabare", "Kalehe" = "Buhavu")[d19$adm2[dr]]
	ok <- mapply(function(t, g) is.na(g) || is.na(t) || grepl(terr[t], g), d19$adm2, d19$adm4)
	d19$adm4[dr & !ok] <- NA     # groupement not in that territory (e.g. "Walungu/Mudaka" = Madaka village)
	
	#missing coordinates
	#median of the other farms' GPS in the same location (Nigeria: else in the same ward)
	fill_median <- function(key, ok) {
	  mlat <- ave(d19$latitude, key, FUN = function(v) if (all(is.na(v))) NA else median(v, na.rm = TRUE))
	  mlon <- ave(d19$longitude, key, FUN = function(v) if (all(is.na(v))) NA else median(v, na.rm = TRUE))
	  i <- is.na(d19$latitude) & !is.na(mlat) & ok
	  d19$latitude[i] <<- mlat[i]
	  d19$longitude[i] <<- mlon[i]
	}
	fill_median(paste(d19$country, d19$adm2, d19$location), !is.na(d19$location))
	fill_median(paste(d19$country, d19$adm2, d19$adm3), d19$country == "Nigeria" & !is.na(d19$adm3))
	
	vil <- as.data.frame(rbind(
	  c("Kenya", "ogero", 0.00583, 34.48639),  # https://www.geonames.org/182496
	  c("Kenya", "nyabenge", -0.0571, 34.36183),  # https://www.geonames.org/11784447
	  c("Kenya", "rombe", -0.94652, 34.48553),  # https://www.geonames.org/11784587
	  c("Ghana", "daffiama", 10.42135, -2.55967),  # https://www.geonames.org/2302070
	  c("Ghana", "kpalsako", 10.93605, -0.02011),  # https://www.geonames.org/2299245
	  c("Ghana", "manyoro-sakunia", 10.97931, -0.99803),  # https://www.geonames.org/2298158
	  c("Ghana", "nabujako", 10.1864, 0.24835),  # https://www.geonames.org/2297763
	  c("Malawi", "buno", -13.62191, 33.57754),  # https://www.geonames.org/12185443
	  c("Malawi", "chinkhwiri", -13.37733, 33.74224),  # https://www.geonames.org/930748
	  c("Malawi", "chiunjiza", -13.49741, 34.29783),  # https://www.geonames.org/930388
	  c("Malawi", "gumbi", -14.98632, 34.87115),  # https://www.geonames.org/929737
	  c("Malawi", "gwaza", -14.87849, 34.81139),  # https://www.geonames.org/929717
	  c("Malawi", "kambwiri", -13.79547, 34.40808),  # https://www.geonames.org/929113
	  c("Malawi", "makwangwala", -14.8, 34.76667),  # https://www.geonames.org/927427
	  c("Malawi", "mmemo", -14.76667, 34.76667),  # https://www.geonames.org/926352
	  c("Malawi", "mtema", -13.81667, 33.66667),  # https://www.geonames.org/925918
	  c("Malawi", "mtema 2", -13.81667, 33.66667),  # https://www.geonames.org/925918
	  c("Nigeria", "sarina", 11.58438, 8.92365),  # https://www.geonames.org/2323646
	  c("Nigeria", "gumel", 9.85023, 7.97466),  # https://www.geonames.org/2340092
	  c("Zimbabwe", "nyamweda", -18.15543, 30.71411),  # https://www.geonames.org/7405288
	  c("Zimbabwe", "zunidza", -18.7, 32.32263)   # https://www.geonames.org/878577
	))
	i <- match(paste(d19$country, d19$location), paste(vil[, 1], vil[, 2]))
	j <- is.na(d19$latitude) & !is.na(i)
	d19$latitude[j] <- as.numeric(vil[i[j], 3])
	d19$longitude[j] <- as.numeric(vil[i[j], 4])
	d19[d19$field_id == "MW293", c("latitude", "longitude")] <- list(-14.76667, 34.76667)   # blank location, action site Mmemo; https://www.geonames.org/926352
	
	adm <- as.data.frame(rbind(
	  c("Kenya", "adm4", "mudete", 0.1, 34.75, 5000),                              # Gwaranda (action site Mudete); https://www.geonames.org/186026
	  c("Kenya", "adm3", "south gem", -0.01667, 34.48333, 10000),                  # https://www.geonames.org/179870
	  c("Kenya", "adm3", "north sakwa", -0.11667, 34.25, 10000),                   # https://www.geonames.org/180819
	  c("Kenya", "adm3", "east asembo", -0.13333, 34.36667, 10000),                # https://www.geonames.org/200827
	  c("Kenya", "adm3", "south central seme", -0.08333, 34.51667, 10000),         # https://www.geonames.org/180591
	  c("Kenya", "adm3", "East Kanyamkago", -0.91667, 34.43333, 10000),            # https://www.geonames.org/194304
	  c("Kenya", "adm3", "Central Kanyamkago", -0.91667, 34.43333, 10000),         # https://www.geonames.org/194304
	  c("Kenya", "adm1", "Vihiga", 0.072, 34.712, 20000),                          # https://www.geonames.org/7667638
	  c("Ghana", "adm2", "Bawku West", 10.91667, -0.51667, 20000),                 # https://www.geonames.org/7647390
	  c("Ghana", "adm2", "Chereponi", 10.13417, 0.28806, 20000),                   # https://www.geonames.org/7647639
	  c("Ghana", "adm2", "Savelugu-Nanton", 9.62472, -0.82778, 20000),             # https://www.geonames.org/7649029
	  c("Ghana", "adm2", "Tolon-Kumbungu", 9.54692, -0.94753, 20000),              # https://www.geonames.org/13609077
	  c("Ghana", "adm2", "Wa East", 9.92933, -1.86848, 30000),                     # https://www.geonames.org/7647181
	  c("Ghana", "adm2", "Nadowli", 10.28835, -2.60889, 20000),                    # https://www.geonames.org/7647177
	  c("Malawi", "adm2", "Dowa", -13.60098, 33.82378, 30000),                     # https://www.geonames.org/929975
	  c("Malawi", "adm2", "Lilongwe", -14.01962, 33.68407, 30000),                 # https://www.geonames.org/927964
	  c("Malawi", "adm2", "Ntcheu", -14.75, 34.75, 30000),                         # https://www.geonames.org/925072
	  c("Malawi", "adm2", "Salima", -13.71959, 34.40184, 30000),                   # https://www.geonames.org/924053
	  c("Nigeria", "adm3", "garko", 11.65022, 8.80782, 5000),                      # https://www.geonames.org/9410679
	  c("Nigeria", "adm3", "karefa", 11.35295, 8.44254, 5000),                     # https://www.geonames.org/9410894
	  c("Nigeria", "adm3", "shuwaki", 11.3148, 8.38277, 5000),                     # https://www.geonames.org/9412504
	  c("Nigeria", "adm3", "zangon-aya", 10.9154, 7.66376, 5000),                  # https://www.geonames.org/2317816
	  c("Nigeria", "adm2", "Garko", 11.57286, 8.83524, 15000),                     # https://www.geonames.org/8633724
	  c("Nigeria", "adm2", "Tudun Wada", 11.28272, 8.59928, 20000),                # https://www.geonames.org/8633726
	  c("Nigeria", "adm2", "Zangon Kataf", 9.841, 8.25491, 20000),                 # https://www.geonames.org/8634540
	  c("Nigeria", "adm2", "Giwa", 11.12077, 7.40565, 30000),                      # https://www.geonames.org/7932490
	  c("Zimbabwe", "adm2", "Guruve", -16.7371, 30.68406, 20000),                  # Guruve Communal Land; https://www.geonames.org/7324374
	  c("Zimbabwe", "adm2", "Makoni", -18.3355, 32.1465, 30000),                   # https://www.geonames.org/887718
	  c("Zimbabwe", "adm2", "Mhondoro-Ngezi", -18.28148, 30.59574, 20000)          # Mondoro Communal Land; https://www.geonames.org/885924
	))
	
	names(adm) <- c("country", "level", "name", "lat", "lon", "unc")
	d19$geo_uncertainty <- NA
	for (lv in c("adm4", "adm3", "adm2", "adm1")) {
	  a <- adm[adm$level == lv, ]
	  k <- if (lv == "adm4") ifelse(d19$action == "mudete", "mudete", NA) else tolower(d19[[lv]])
	  i <- match(paste(d19$country, k), paste(a$country, tolower(a$name)))
	  j <- is.na(d19$latitude) & !is.na(i)
	  d19$latitude[j] <- as.numeric(a$lat[i[j]])
	  d19$longitude[j] <- as.numeric(a$lon[i[j]])
	  d19$geo_uncertainty[j] <- as.numeric(a$unc[i[j]])
	}
	
	#merging data
	##Labour
	d3a <- aggregate(
		cbind(plot_labour, land_prep_cost, planting_cost, weeding_cost, weeding_labour, irrigation_cost, OM_transp_cost, fertilizer_app_cost, fertilizer_labour, harvest_cost) ~ hhid + field_id,  
		data = d3, sum, na.rm = TRUE )
	
	##Livestock
	d11a <- aggregate(animal ~ hhid + field_id, d11, FUN = function(x) paste(unique(x), collapse = ";"))
	
	d11b <- aggregate(purpose ~ hhid + field_id, data = d11,
			FUN = function(x) paste(unique(na.omit(x)), collapse = ";"))
	
	d11a <- merge(d11a, d11b, by = c("hhid","field_id"), all = TRUE)
	
	##Markets
## for now:
## 	d18a <- aggregate(market_distance ~ hhid + field_id,  data = d18, FUN = mean, na.rm = TRUE)
	tmp <- aggregate(market_type ~ hhid + field_id, data = d18,  FUN = function(x) paste(unique(x), collapse = ";"))
##	d18a <- merge(d18a, tmp, by = c("hhid", "field_id"), all = TRUE)
    d18a <- tmp
	
	d <- d13
	
	d <- merge(d, d12, by = c("hhid","field_id","field_size"), all = TRUE)
	d <- merge(d, d14,by = c("hhid","field_id","crop"), all = TRUE)
	d <- merge(d, d15, by = c("hhid","field_id","crop"), all = TRUE)
	d <- merge(d, d16,by = c("hhid","field_id","crop"), all = TRUE)
	d <- merge(d, d17, by = c("hhid","field_id","crop"),all = TRUE)
	d <- merge(d, d1, by = c("hhid","field_id"),all = TRUE)
	d <- merge(d, d2,by = c("hhid","field_id"), all = TRUE)
	d <- merge(d, d3a, by = c("hhid","field_id"),all = TRUE)
	d <- merge(d, d8, by = c("hhid","field_id"),all = TRUE)
	d <- merge(d, d9[, c("hhid","field_id","electricity")],by = c("hhid","field_id"), all = TRUE)
	d <- merge(d, d10, by = c("hhid","field_id"),all = TRUE)
	d <- merge(d, d11a, by = c("hhid","field_id"), all = TRUE)
	d <- merge(d, d18a, by = c("hhid","field_id"), all = TRUE)
	d <- merge(d,d19, by = c("hhid","field_id"), all = TRUE)
	
	#d$field_size <- ifelse(
	 # !is.na(d$field_size.y),
	  #d$field_size.y,
	  #d$field_size.x
	#)
	
	#d$field_size.x <- NULL
	#d$field_size.y <- NULL
	
	d$fertilizer_type <- apply(
	  d[, c("fertilizer_type.x", "fertilizer_type.y")],
	  1,
	  function(x) paste(unique(x[!is.na(x) & x != ""]), collapse = ";")
	)
	
	d$fertilizer_type.x <- NULL
	d$fertilizer_type.y <- NULL
	
	d$OM_type <- apply(
	  d[, c("OM_type.x", "OM_type.y")],
	  1,
	  function(x) paste(unique(x[!is.na(x) & x != ""]), collapse = ";")
	)
	
	d$OM_type.x <- NULL
	d$OM_type.y <- NULL
	
	#sum(!is.na(d$yield.x) & !is.na(d$yield.y))
	
	d$yield <- ifelse(
	  is.na(d$yield.x),
	  d$yield.y,
	  d$yield.x
	)
	
	d$yield.x <- NULL
	d$yield.y <- NULL
	
	d$yield_isfresh <- ifelse(
	  is.na(d$yield_isfresh.x),
	  d$yield_isfresh.y,
	  d$yield_isfresh.x
	)
	
	d$yield_isfresh.x <- NULL
	d$yield_isfresh.y <- NULL
	
	d$treatment <- NA
	d$trial_id <- as.character(as.integer(as.factor(1)))
	d$on_farm <- FALSE
	d$is_survey <- TRUE
	d$irrigated <- TRUE
	d$geo_from_source <- TRUE
	d$planting_date <- NA
	d$harvest_date  <- NA
	d$yield_moisture <- NA
	
	d$P_fertilizer <- d$K_fertilizer <- d$N_fertilizer <- d$S_fertilizer <- d$lime <- as.numeric(NA)
	
	#fixing crop names
	d$crop <- tolower(trimws(d$crop))
	d$crop[grepl("common bean|bush bean|beans, bush beans|string bean|voluble beans|wax bean|w.bean|n.bean|o.bean|j.bean|m.bean|creeper bean|sugarbeans", d$crop)] <- "kidney bean"
	d$crop[grepl("climbing bean|climbing beans|beans|common beans/bush beans", d$crop)] <- "common bean"
	d$crop[grepl("^soybeans?$|^soja$", d$crop)] <- "soybean"
	d$crop[grepl("^cowpeas?$", d$crop)] <- "cowpea"
	d$crop[grepl("^groundnuts?$", d$crop)] <- "groundnut"
	d$crop[grepl("bambara nuts|bambara groundnut|bambara groundnuts|bambara beans|mbande|b.bean|b.bena", d$crop)] <- "bambara groundnut"
	d$crop[grepl("greengram|greengrams", d$crop)] <- "mung bean"
	d$crop[grepl("garden peas|green peas|petit pois", d$crop)] <- "pea"
	d$crop[grepl("pigeonpeas|nandolo", d$crop)] <- "pigeon pea"
	d$crop[grepl("nhemba bean|boer bean|fava bean|macaco bean", d$crop)] <- "bean"
	d$crop[grepl("^maize$|msize", d$crop)] <- "maize"
	d$crop[grepl("^rice$", d$crop)] <- "rice"
	d$crop[grepl("^wheat$", d$crop)] <- "wheat"
	d$crop[grepl("sorghum|sorhum|soghun|guinea corn|mapfunde", d$crop)] <- "sorghum"
	d$crop[grepl("millet|milllet|pearl millet|millet", d$crop)] <- "millet"
	d$crop[grepl("rapoko", d$crop)] <- "finger millet"
	d$crop[grepl("^cassava$|^manioc$|^mashava$", d$crop)] <- "cassava"
	d$crop[grepl("sweet potato|wweet potato", d$crop)] <- "sweetpotato"
	d$crop[grepl("irish potato|irish patato|irish poatato|^potato$|^irish$", d$crop)] <- "potato"
	d$crop[grepl("^yam$|^yams$|^igname$", d$crop)] <- "yam"
	d$crop[grepl("cocoyam|colocase|colocasse|taro|madhumbe", d$crop)] <- "taro"
	d$crop[grepl("^sunflower$|^tournesol$", d$crop)] <- "sunflower"
	d$crop[grepl("^sesame$", d$crop)] <- "sesame"
	d$crop[grepl("^cotton$", d$crop)] <- "cotton"
	d$crop[grepl("^tomato$", d$crop)] <- "tomato"
	d$crop[grepl("^onion$", d$crop)] <- "onion"
	d$crop[grepl("^carrot$|^carrots$", d$crop)] <- "carrot"
	d$crop[grepl("eggplant|egglplant|aubergine", d$crop)] <- "eggplant"
	d$crop[grepl("^cabbage$|^cavage$|^choux$", d$crop)] <- "cabbage"
	d$crop[grepl("^okra$|^okfro$", d$crop)] <- "okra"
	
	d$crop[grepl("^pepper$|^piment$", d$crop)] <- "pepper"
	d$crop[grepl("^lettuce$", d$crop)] <- "lettuce"
	d$crop[grepl("^cucumber$", d$crop)] <- "cucumber"
	d$crop[grepl("^turnips$", d$crop)] <- "turnip"
	d$crop[grepl("^garlic$", d$crop)] <- "garlic"
	d$crop[grepl("^amaranth$|^amarenth$|^amarante$|green amarantha", d$crop)] <- "amaranth"
	d$crop[grepl("sukuma", d$crop)] <- "kale"
	d$crop[grepl("^banana$", d$crop)] <- "banana"
	d$crop[grepl("^mango$|^mangoes$", d$crop)] <- "mango"
	d$crop[grepl("^pineapples$", d$crop)] <- "pineapple"
	d$crop[grepl("^water melon$|^watermelon$", d$crop)] <- "watermelon"
	d$crop[grepl("^coffee$", d$crop)] <- "coffee"
	d$crop[grepl("^tea$", d$crop)] <- "tea"
	d$crop[grepl("^cocoa$", d$crop)] <- "cocoa"
	d$crop[grepl("^sugarcane$", d$crop)] <- "sugarcane"
	d$crop[grepl("^ginger$", d$crop)] <- "ginger"
	d$crop[grepl("fodder legume|lucina", d$crop)] <- "forage crop"
	#d$crop[grepl("^desmodium$", #d$crop)] <- "desmodium"
	d$crop[grepl("^caliandra$", d$crop)] <- "forage crop"
	d$crop[grepl("nappier|naipper", d$crop)] <- "napier grass"
	d$crop[grepl("hay grass|rusena|canada|cameroun", d$crop)] <- "forage crop"
	d$crop[d$crop %in% c("bean", "beans")] <- "kidney bean"
	d$crop[d$crop == "butternuts"] <- "pumpkin"
	d$crop[d$crop == "coals"] <- NA
	d$crop[d$crop == "masiya"] <- NA
	d$crop[d$crop == "g/corn"] <- "sorghum"
	d$crop[d$crop == "irish beans"] <- "kidney bean"
	d$crop[d$crop == "kales"] <- "kale"
	d$crop[d$crop == "lengalenga"] <- "amaranth"
	d$crop[d$crop == "lentils"] <- "lentil"
	d$crop[d$crop == "fruits"] <- "fruit"
	d$crop[d$crop == "vegetables"] <- "vegetable"
	
	d$crop[d$crop %in% c("", " ", "other (specify)", "soil", "crop", "trees", "straws", "kanannado",
      "kanan nabo", "dek", "moti", "mito", "spya", "sota", "arbre", "cana", "mexoeira", 
	  "mallaqueta", "mallauqeta", "malaguetta","tigernut","desmodium", "no crop legume planted last season")] <- NA
	
	#empty character values
	d$sex[d$sex == ""] <- NA
	d$land_tenure[d$land_tenure == ""] <- NA
	d$insecticide_product[d$insecticide_product == ""] <- NA
	d$previous_crop_residue_management[d$previous_crop_residue_management == ""] <- NA
	d$is_head[d$is_head == ""] <- NA
	d$education[d$education == ""] <- NA
	d$irrigation_method[d$irrigation_method == ""] <- NA
	d$water_source[d$water_source == ""] <- NA
	d$animal[d$animal == ""] <- NA
	d$market_type[d$market_type == ""] <- NA
	d$country[d$country == ""] <- NA
	d$location[d$location == ""] <- NA
	d$elevation[d$elevation == ""] <- NA

	d$fertilizer_type[d$fertilizer_type == ""] <- NA
	d$OM_type[d$OM_type == ""] <- NA
	d$country[d$country=="D.R. Congo"] <- "Democratic Republic of the Congo"
	
	#fixing data_types
	d$hhid <- as.character(d$hhid)
	d$is_head <- ifelse(d$is_head=="Yes", TRUE,FALSE)
	d$age <- as.numeric(d$age)
	
	## need to address units (m, ft ,...)
##	d$elevation <- as.numeric(d$elevation)

	d$treatment <- as.character(d$treatment)
	d$planting_date <- as.character(d$planting_date)
	d$harvest_date <- as.character(d$harvest_date)
	d$treatment <- as.character(d$treatment)
	d$yield_moisture <- as.numeric(d$yield_moisture)
	
	#yield_part
	d$yield_part <- NA
	
	d$yield_part[d$crop == "mung bean"] <- "seed"
	d$yield_part[d$crop == "tobacco"] <- "leaves"
	d$yield_part[d$crop == "onion"] <- "tubers"
	d$yield_part[d$crop == "garlic"] <- "tubers"
	d$yield_part[d$crop %in% c("kidney bean", "runner bean", "cowpea", "pea", "soybean", "bambara groundnut", 
						                 "pigeon pea","lentil")] <- "seed"
	d$yield_part[d$crop %in% c("maize", "sorghum", "millet", "rice", "wheat")] <- "grain"
	d$yield_part[d$crop %in% c("cassava","sweetpotato", "carrot", "turnip")] <- "roots"
	d$yield_part[d$crop %in% c("potato", "yam")] <- "tubers"
	d$yield_part[d$crop == "ginger"] <- "rhizome"
	d$yield_part[d$crop %in% c("kale","amaranth","cabbage","lettuce",
	                           "forage legume","forage crop","napier grass")] <- "leaves"
	
	d$yield_part[d$crop %in% c("banana","tomato","watermelon","pineapple","mango",
	                           "eggplant","pepper","okra","pumpkin","cucumber")] <- "fruit"
	
	d$yield_part[d$crop %in% c("groundnut","sunflower","sesame")] <- "seed"
	d$yield_part[d$crop %in% c("coffee","cocoa")] <- "seed"
	d$yield_part[d$crop == "tea"] <- "leaves"
	d$yield_part[d$crop == "cotton"] <- "fruit"
	d$yield_part[d$crop == "sugarcane"] <- "stems"
	
	#trim vars
	vars <- c("inoculant", "insecticide_product", "previous_crop_residue_management", "market_type", "country", "location","fertilizer_type", "OM_type")

	char_cols <- sapply(d, is.character)
	d[char_cols] <- lapply(d[char_cols], function(x) trimws(x))
	
	#products
	ins <- trimws(tolower(d$insecticide_product))
	
	ins[tolower(ins) %in% c("n o", "nn", "nlo", "nol", "non", "no")] <- "none"
	ins[ins %in% c("yes", "name unknown")] <- "unknown"
	ins[ins %in% c("", "-88", "0", "1", "2", "3", "4", "5", "6", "7", "8", "18", "22", "45")] <- NA
	
	
## some of these are herbicides and could be used for that (as below?). 
## also, could there be an insecticide as well? 
	ins[grepl("atraz|gramaz|gramoz|grammaz|paraquat|paraforce|round-up|butach|butaforce|weed|herbicide|habicide|harbicide|selective weedicide|stamp|condem|kombat", ins)] <- NA
	
	herb <- grepl("atraz|afrazine|adrazone|attrazine|attazine|atrizine|para-force|para force|
	para fprce|butachlor|butachcor|buta force|butta force|butoforce|butter force|butters|dara force|gram|sarosate|stump|selective|kondem|caliherb", ins)
	ins[herb] <- NA
	
	fung <- grepl("benlate|benbte|dethan|dethane|distane|cooper|copper", ins)
	ins[fung] <- NA
	
	ins[grepl("actellic", ins)] <- "unknown"
	ins[grepl("cipenetrin|cipetrin|cyper|cymetherine|dymetherine|sipmethlane", ins)] <- "cypermethrin"
	
	#ins[grepl("^ddt$", ins)] <- "ddt"
	ins[ins=="ddt"] <- "DDT"
	
	ins[grepl("diptex", ins)] <- "trichlorfon"
	ins[grepl("^seven$", ins)] <- "carbaryl"
	ins[grepl("lipcord|lipicod|cypamethin lipcod",  ins)] <- "cypermethrin"
	
	ins[grepl("sumicombi", ins)] <- "fenvalerate"
	
	ins[grepl("insecticide|hercides|power|propanol|bloodtex|bugus|bullet|bulletin|
     carack|commando|fernkill|fernq|diasol|dimophylin|distabu cophidal|r and oryzum|seprinethelean|
     simekombe|simocombe|tetan|themaron|ultrachlo|ultrachlor|ultrachol|ultrachor|
     carbohydrates|npk|urea|85% w.p an|3 grain\\*|best", ins)] <- "unknown"
	
	ins[grepl("dithane|dithan|ridomil|ridonul|ridanie|benlate|bentale|shavit|mildrex",ins)] <- NA
	ins[grepl("pesticide|pestcides|pesrcides|insecticide|insectant|biocides", ins )] <- "unknown"
	
	ins[grepl("carbaryl|cabaryl|cabary|caborly", ins)] <- "carbaryl"
	
	ins[grepl("deltamet", ins)] <- "deltamethrin"
	ins[grepl("dimothaete", ins)] <- "dimethoate"
	ins[grepl("durbuban", ins)] <- "chlorpyrifos"
	ins[grepl("thiodan|tioda|tiode|theodan", ins)] <- "endosulfan"
	ins[grepl("methodidate", ins)] <- "methidathion"
	ins[grepl("lannet", ins)] <- "methomyl"
	ins[grepl("^karate$", ins)] <- "lambda-cyhalothrin"
	ins[grepl("^roga$|^roger$", ins)] <- "dimethoate"
	ins[grepl("^ash$|^soap$", ins)] <- "unknown"
	
	valid <- c("carbaryl", "cypermethrin", "ddt", "trichlorfon", "fenvalerate", "dimethoate",
	  "deltamethrin", "chlorpyrifos", "endosulfan", "methomyl", "lambda-cyhalothrin",  "unknown")
	
	ins[!is.na(ins) & !(ins %in% valid)] <- "unknown"

	d$insecticide_product <- ins
	d$herbicide_product <- NA
	d$fungicide_product <- NA
	
	d$herbicide_product[grepl("atraz|afrazine|atrazil|atrazin|atrazone|attrazine|attazine", ins)] <- "atrazine"
	d$herbicide_product[grepl("gramaz|gramoz|grammaz|gramazoe|gramazone|gramozone", ins)] <- "paraquat"
	d$herbicide_product[grepl("paraquat|paraforce|para force|para fprce", ins)] <- "paraquat"
	d$herbicide_product[grepl("round-up|sarosate", ins)] <- "glyphosate"
	d$herbicide_product[grepl("butach|butaforce|buta force|butter force|butoforce", ins)] <- "butachlor"
	d$herbicide_product[d$herbicide_product=="butachlor"] <- "unknown"
	d$herbicide_product[d$herbicide_product=="paraquat"] <- "paraquat dichloride"
	d$fungicide_product[grepl("dithane|dithan|dithone",  ins)] <- "mancozeb"
	d$fungicide_product[grepl("ridomil|ridonul|ridanie|rhidanie|rhidonue|lidomil", ins)] <- "metalaxyl"
	d$fungicide_product[grepl("benlate|bentale", ins)] <- "benomyl"
	d$fungicide_product[grepl("shavit", ins)] <- "shavit"
	d$fungicide_product[grepl("copper|cooper", ins)] <- "copper"
	d$fungicide_product[d$fungicide_product =="copper fungicide"]  <- "copper"
	
	#Irrigation methods
	d$irrigation_method[
	  grepl("bucket|bucet|watering|sprinkling|handsprinkling|hand sprinkler|hand sprinkling
           |watering can|watering cane|calabash|perforated bowl|perforated calabash|hand sprinkler"
           ,tolower(d$irrigation_method))] <- "sprinkler"

	d$irrigation_method <- trimws(tolower(d$irrigation_method))
	d$irrigation_method[grepl("canal|channel irrigation|canalization", d$irrigation_method)] <- "surface"
	d$irrigation_method[grepl("drip irrigation", d$irrigation_method)] <- "drip"
	d$irrigation_method[grepl("diesel pump|treadle pump|water pump|dug well|surface water|water$", d$irrigation_method)] <- "surface"
	d$irrigation_method[grepl("domestic bassin|basin", d$irrigation_method)] <- "basin"
	d$irrigation_method[grepl("dam|irrigated dam|irrigation dam", d$irrigation_method)] <- "flood"	
	d$irrigation_method[grepl("^rain$|rain water", d$irrigation_method)] <- "none"
	d$irrigation_method[grepl("^axe$|^matering$|half can|neighbour's well|arosoir|dug well", d$irrigation_method)] <- "unknown"
	d$irrigation_method[d$irrigation_method == ""] <- NA
	
	#fertilizer_type
	d$fertilizer_type <- trimws(tolower(d$fertilizer_type))
	d$fertilizer_type[d$fertilizer_type %in% c("", "-88", "-9999", "`","0", "n0", "nio", "nn",
	                                           "no", "noi", "yes","inknown")] <- NA
	d$fertilizer_type[grepl("urea|uera|^u$",d$fertilizer_type)] <- "urea"
	d$fertilizer_type[grepl("^an$|ammonium nitrate|ammonia",d$fertilizer_type)] <- "AN"
	d$fertilizer_type[grepl("sulphate of ammonia|sulphate of amonia",d$fertilizer_type)] <- "AS"
	d$fertilizer_type[grepl("chitowe", d$fertilizer_type)] <- "NPS"
	d$fertilizer_type[grepl("23:21|32:21:0\\+4s|nps",d$fertilizer_type)] <- "NPS"
	d$fertilizer_type[grepl("can.*chitowe|chitowe.*can",d$fertilizer_type)] <- "CAN;NPS"
	d$fertilizer_type[grepl("urea.*chitowe|chitowe.*urea",d$fertilizer_type)] <- "urea;NPS"
	d$fertilizer_type[grepl("^dap$|dap -|dap,|dap|d p a/",d$fertilizer_type)] <- "DAP"
	d$fertilizer_type[grepl("^map$", d$fertilizer_type)] <- "MAP"
	d$fertilizer_type[grepl("npk|15-15-15|15 15 15|17-17-17|17\\.17\\.17|23:21|23\\+21|
                           23:21\\+0|23:21:0|n p k",d$fertilizer_type)] <- "NPK"
	d$fertilizer_type[grepl("^ssp$|^spp$|^sss$|s\\.s\\.p",d$fertilizer_type)] <- "SSP"
	d$fertilizer_type[grepl("^tsp$|^t/s$",d$fertilizer_type)] <- "TSP"
	d$fertilizer_type[grepl("^gypsum$",d$fertilizer_type)] <- "gypsum"
	d$fertilizer_type[grepl("compound c|d-compound cianate",d$fertilizer_type)] <- "C-compound"
	d$fertilizer_type[grepl("compound d",d$fertilizer_type)] <- "D-compound"
	d$fertilizer_type[grepl("compound l",d$fertilizer_type)] <- "L-compound"
	d$fertilizer_type[grepl("rhizatex",d$fertilizer_type)] <- NA
	
	om_idx <- grepl("animal manure|cattle manure|cow dung|goat manure|farm yard manure|
                   manure|composte|composite manure|poultry drop|organic",d$fertilizer_type)
	
	d$OM_type[om_idx] <- "manure"
	d$fertilizer_type[om_idx] <- NA
	d$fertilizer_type[grepl("chemical|chemicals|top dressing|mavuno|golden|majimaji|ero",d$fertilizer_type)] <- "unknown"
	d$fertilizer_type[grepl("dap.*can|can.*dap",d$fertilizer_type)] <- "DAP;CAN"
	d$fertilizer_type[grepl("npk.*urea|urea.*npk",d$fertilizer_type)] <- "NPK;urea"
	d$fertilizer_type[grepl("ssp.*urea|urea.*ssp",d$fertilizer_type)] <- "SSP;urea"
	d$fertilizer_type[grepl("npk.*ssp.*urea",d$fertilizer_type)] <- "NPK;SSP;urea"
	d$fertilizer_type[d$fertilizer_type %in% c("-88;-88;-88","-9999;-9999","-9999;-9999;-9999","0;0;0","n0;no;no",
	                                           "no;n0;n0","no;nn;no","no;no","no;no;nio","no;no;noi","no;no;no","no;yes;no","yes;no;no")] <- NA
	d$fertilizer_type[d$fertilizer_type == "can"] <- "CAN"
	d$fertilizer_type[d$fertilizer_type == "can;can"] <- "CAN"
	d$fertilizer_type[d$fertilizer_type == "can;no;no"] <- "CAN"
	d$fertilizer_type[d$fertilizer_type == "dap;0;0"] <- "DAP"
	d$fertilizer_type[d$fertilizer_type == "dap;ssp"] <- "DAP;SSP"
	d$fertilizer_type[d$fertilizer_type == "dap;mavuno"] <- "DAP;unknown"
	d$fertilizer_type[d$fertilizer_type == "ssp;ssp"] <- "SSP"
	d$fertilizer_type[d$fertilizer_type == "ssp;"] <- "SSP"
	d$fertilizer_type[d$fertilizer_type == "ssp; ;"] <- "SSP"
	d$fertilizer_type[d$fertilizer_type == "ssp;goat manure"] <- "SSP"
	d$OM_type[grepl("ssp;goat manure", d$fertilizer_type)] <- "manure"
	d$fertilizer_type[d$fertilizer_type == "ssp;npl"] <- "SSP"
	d$fertilizer_type[d$fertilizer_type == "ssp;oganic manual"] <- "SSP"
	d$OM_type[grepl("ssp;oganic manual", d$fertilizer_type)] <- "manure"
	d$OM_type[d$fertilizer_type == "animal farming"] <- "manure"
	d$fertilizer_type[d$fertilizer_type == "animal farming"] <- NA
	d$OM_type[d$fertilizer_type == "manure refuse"] <- "manure"
	d$fertilizer_type[d$fertilizer_type == "manure refuse"] <- NA
	d$fertilizer_type[d$fertilizer_type == "mavuno"] <- "unknown"
	d$fertilizer_type[d$fertilizer_type == "mavuno;can"] <- "CAN;unknown"
	d$fertilizer_type[d$fertilizer_type == "n.p.k, 50kg"] <- "NPK"
	d$fertilizer_type[d$fertilizer_type == "npl"] <- "NPK"
	d$fertilizer_type[d$fertilizer_type == "super"] <- "SSP"
	d$fertilizer_type[d$fertilizer_type == "super"] <- "SSP"
	d$fertilizer_type[d$fertilizer_type == "super"] <- "SSP"
	d$fertilizer_type[d$fertilizer_type == "super"] <- "SSP"
	d$fertilizer_type[d$fertilizer_type == "super"] <- "SSP"
	d$fertilizer_type[d$fertilizer_type %in% c("manure","cypermethyline")] <- NA
	d$fertilizer_type[d$fertilizer_type %in% c("compound g","compound x")] <- "unknown"
	d$fertilizer_type[d$fertilizer_type == "d p a"] <- "DAP"
	d$fertilizer_type[d$fertilizer_type=="gypsum;compound g"] <- "gypsum;unknown"
	
	#OM_type
	d$OM_type <- trimws(tolower(d$OM_type))
	d$OM_type[d$OM_type %in% c("", "-77", "-88", "-9999","0", "1", "100", "200", "500",
	                           "650", "760", "1300","3 wheebarrow","no", "yes")] <- NA
	d$OM_type[grepl("compost|composite|composte|dechet|dechets|debris des menanges",d$OM_type)] <- "compost"
	d$OM_type[grepl("farm.?yard manure|ffarmyard manure|kraal manure|organic manure|livestock manure|manure$|fumier",d$OM_type)] <- "farmyard manure"
	d$OM_type[grepl("cattle dung|catlle dung|cattle manure|cow dung|
                   cowdung|murakwani cowdung", d$OM_type)] <- "cattle dung"
	d$OM_type[grepl("animal dung|animal manure|dung$|dung manure|
                   bouse|ordure|ordures",d$OM_type)] <- "animal dung"
	d$OM_type[grepl("chicken manure|fowl droppings|poultry dropping",d$OM_type)] <- "poultry manure"
	d$OM_type[grepl("woodland litter|woodlands litter|litter manure|little manure",d$OM_type)] <- "leaf litter"
	d$OM_type[grepl("engrais vert|angrais vert|green$|crop residues|fodder|hauls|hault|haults|paillage",d$OM_type)] <- "foliage"
	d$OM_type[grepl("pig manure|fumier de porcs",d$OM_type)] <- "animal dung"
	d$OM_type[grepl("goat manure|goat dung|giat manure|fumier de chevre",d$OM_type)] <- "animal dung"
  d$OM_type[grepl("guinea pig manure|fumier de cobaye",d$OM_type)] <- "animal dung"
	d$OM_type[grepl("ash|cendres",d$OM_type)] <- "unknown"
	d$OM_type[grepl("anthill|ant hill|termitaria|termitalia|woodland termitaria",d$OM_type)] <- "unknown"
	d$OM_type[grepl("cabaryl|cabaryl|gramazoe|grammazoe|kombat|para force|copper dimethylin|drogon|dao",d$OM_type)] <- NA
	d$OM_type[grepl("animal feed|maize seed|milled seed|mineral|local|buntu|lame|farill",d$OM_type)] <- NA
	d$OM_type[d$OM_type %in% c("-9999;-9999","-9999;-9999;-9999",";","0;0","0;0;0","1300;0;0","no;no","no;no;no","yes;no;no","yes mulch;no;no")] <- NA
	d$OM_type[d$OM_type %in% c("bouse et compste","debris des menanges")] <- "compost"
	d$OM_type[grepl("^farmyard  manure, 300kg$|^manure [0-9]+kg$|^manure,[0-9 ]+kg$",d$OM_type)] <- "farmyard manure"
	d$OM_type[d$OM_type == "livestock"] <- "farmyard manure"
	d$OM_type[d$OM_type == "murakwani"] <- "farmyard manure"
	d$OM_type[d$OM_type %in% c("bouse","dung","goat dung")] <- "animal dung"
	d$OM_type[grepl("^dung;manure",d$OM_type)] <- "animal dung"
	d$OM_type[d$OM_type == "manure;dung"] <- "animal dung"
	d$OM_type[d$OM_type %in% c("cowdung murakwani","manure, cowdung","little manure;cowdung")] <- "cattle dung"
	d$OM_type[d$OM_type == "fowl droppings"] <- "poultry manure"
	d$OM_type[d$OM_type %in% c("angrais vert","crop residues","fodder","hauls","green;0;0","paillage")] <- "foliage"
	d$OM_type[d$OM_type %in% c("woodlands litter","litter manure;maize seed","litter manure;milled seed")] <- "leaf litter"
	d$OM_type[d$OM_type == "cattle manure;termitaria"] <- "cattle dung"
	d$OM_type[d$OM_type %in% c("termitaria","woodland termitaria")] <- "unknown"
	d$OM_type[d$OM_type %in% c("copper dimethylin","gramazoe atrazine","kombat","drogon")] <- NA
	d$OM_type[d$OM_type %in% c("buntu","farill","lame","local","mineral")] <- "unknown"
	d$OM_type[d$OM_type == "manure from toilet"] <- "sludge"
	d$OM_type[d$OM_type == "maize seed"] <- "unknown"
	d$OM_type[d$OM_type == "poultry dropping"] <- "poultry manure"
	d$OM_type[d$OM_type %in% c("manure","manure;maize seed") ] <- "unknown"
	
	d$country[d$country=="Ghana "] <- "Ghana"
	d$country[d$country=="Zimbabwe "] <- "Zimbabwe"
	d$country[d$country=="Nigeria "] <- "Nigeria"
	
	d$inoculant <- tolower(trimws(d$inoculant))
	d$previous_crop_residue_management <- tolower(trimws(d$previous_crop_residue_management))
	d$location <- tolower(trimws(d$location))
	
	
    #watermelon yields are 0
	crops_tonnes <- c("forage legume", "garlic", "pea", "pumpkin", "turnip", "watermelon")
	
	idx <- d$crop %in% crops_tonnes & !is.na(d$yield)
	d$yield[idx] <- d$yield[idx] * 1000

	#out of bounds
	d$fertilizer_amount[d$fertilizer_amount < 0] <- NA
	d$hh_size[d$hh_size < 0] <- NA
	d$yield[d$yield < 0] <- NA
	
	carobiner::write_files(path, meta, d)
}


