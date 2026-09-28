# R script for "carob"
# license: GPL (>=3)

## ISSUES
# 2 publications were found with regards to this dataset, both doi's were added under publication
# 1."Replicate" identifies a farm (9 in LR2014 + 10 in LR2015 = the 19 trials in the Data in Brief paper),
#    with one replicate per farm; it is used to build trial_id rather than rep.
# 2. Planting/harvest dates are not given in the papers. The IITA metadata gives data collection from
#    2013-09 to 2015-09 and harvest was at 12 MAP, so LR2014 is assumed planted 2013-09 and harvested
#    2014-09 (LR2015: 2014-09 to 2015-09). To be confirmed with the authors.


carob_script <- function(path) {

"
Datasets on economic analysis of fertilized improved and local varieties of cassava grown in the highlands of South Kivu, DR Congo

The use of mineral fertilizer and organic inputs with an improved and local variety of cassava allow firstly to identify nutrient limitations to cassava production, and secondly to investigate the effects of variety and combined application of mineral and organic inputs on cassava growth and yields in the highland conditions of the Democratic Republic of Congo (DR Congo). Data on growth parameters, yields and yield components of the improved and local varieties of cassava, economic analysis and soil parameters, collected during two growing cycles of cassava are presented. The data support a research article which is under review “Increased cassava growth and yields through improved variety use and fertilizer application in the highlands of South Kivu, Democratic Republic of Congo” [1]. Data on plant height and diameter was measured throughout the growing period of the crop while the data on the storage root, stem, tradable storage root and non-tradable storage root was determined at 12 months after planting (MAP) of the field experiments. The economic analysis was performed using a simplified financial analysis where the additional benefits were calculated relative to the respective control treatments while the total costs included the purchasing prices of fertilizer and the additional net benefits, the revenue from the increased storage root yields due to fertilizer application. The value cost ratio (VCR) was calculated as the additional net benefits over the cost of fertilizer purchase.
"

	uri <- "doi:10.25502/hyh7-bw50/d"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		data_organization = "IITA",
		publication = "doi:10.1016/j.fcr.2023.109056;doi:10.1016/j.dib.2023.109945",
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety;N_fertilizer;P_fertilizer;K_fertilizer;Ca_fertilizer;Mg_fertilizer;S_fertilizer;Zn_fertilizer;OM_amount",
		response_vars = "yield",
		notes = "Economic analysis: root price 0.40 USD/kg; fertilizer_price holds the unit price of each product, in the same order as fertilizer_type",
		carob_contributor = "Mitchelle Njukuya",
		carob_date = "2026-09-28",
		carob_completion = 100,
		carob_effort = 5
	)

	f1 <- ff[basename(ff) == "vcr_fertilizer.csv"]
	f2 <- ff[basename(ff) == "price_nutrient-response.csv"]

	r1 <- read.csv(f1)
	r1 <- r1[!is.na(r1$ID), 1:11]   # drop trailing empty rows and columns
	r2 <- read.csv(f2)

	## improved variety, all 8 treatments (r4)
	## r4 has two blocks of product columns: amounts in kg/ha (Urea ... FYM) and costs in USD/ha (Urea.1 ... FYM.1)
	prod <- c(Urea="urea", TSP="TSP", KCl="KCl", CaCO3="CaCO3", MgSO4="MgSO4", ZnSO4="ZnSO4")
	amt  <- as.matrix(r2[, names(prod)])
	cost <- as.matrix(r2[, paste0(names(prod), ".1")])
	used <- !is.na(amt) & amt > 0
	uprice <- round(cost / amt, 2)

	d2 <- data.frame(
		adm2 = r2$Site,
		location = r2$Village,
		trial_id = paste0(r2$Season, "_", r2$Replicate),
		treatment = gsub(" ", "", r2$Fertilizer),
		variety = "Sawasawa",
		variety_type = "improved",
		yield = r2$FW_StorageRoot.1,        # kg/ha
		crop_value = r2$FW_StorageRoot.2,   # USD/ha = yield * 0.40
		N_fertilizer = r2$Qt_N,
		P_fertilizer = r2$Qt_P,
		K_fertilizer = r2$Qt_K,
		Ca_fertilizer = r2$Qt_Ca,
		Mg_fertilizer = r2$Qt_Mg,
		S_fertilizer = r2$Qt_S,
		Zn_fertilizer = r2$Qt_Zn,
		OM_amount = r2$Qt_FYM,
		fertilizer_type = apply(used, 1, function(i) if (any(i)) paste(prod[i], collapse=";") else "none"),
		fertilizer_amount = rowSums(amt, na.rm=TRUE),     # kg product/ha
		fertilizer_price = sapply(1:nrow(used), function(i) if (any(used[i,])) paste(uprice[i, used[i,]], collapse=";") else NA),
		fertilizer_cost = rowSums(cost, na.rm=TRUE),      # USD/ha, mineral fertilizers only
		OM_cost = r2$FYM.1                                # USD/ha
	)
  
	d2$fertilizer_price <- as.numeric(d2$fertilizer_price)
	
	## fill the improved-variety control plot missing in r2 from r1
	imp <- r1[r1$Variety == "Improve", ]
	i <- match(paste(d2$trial_id, d2$treatment),
			paste(paste0(imp$Season, "_", imp$Replicate), gsub(" ", "", imp$Fertilizer)))
	fill <- is.na(d2$yield) & !is.na(i)
	d2$yield[fill] <- imp$FW_StorageRoot.1[i[fill]]
	d2$crop_value[fill] <- imp$FW_StorageRoot.2[i[fill]]

	## local variety, control and NPK+FYM (r1)
	loc <- r1[r1$Variety == "Local", ]
	d1 <- data.frame(
		adm2 = loc$Site,
		location = loc$Village,
		trial_id = paste0(loc$Season, "_", loc$Replicate),
		treatment = gsub(" ", "", loc$Fertilizer),
		variety = "Nambiyombiyo",
		variety_type = "local",
		yield = loc$FW_StorageRoot.1,
		crop_value = loc$FW_StorageRoot.2
	)
	# the inputs of the local-variety treatments are identical to the same treatments on the improved variety
	inputs <- unique(d2[, c("treatment", setdiff(names(d2), names(d1)))])
	d1 <- merge(d1, inputs, by="treatment", all.x=TRUE)

	## same columns, different plots: bind the rows
	d <- rbind(d2, d1[, names(d2)])
	d <- d[!is.na(d$yield), ]

	d$treatment[d$treatment == "None"] <- "control"
	d$fertilizer_used <- d$fertilizer_type != "none"
	d$N_splits <- ifelse(d$N_fertilizer > 0, 2L, 0L)   # urea: half at planting, half at 3 MAP
	d$OM_used <- d$OM_amount > 0
	d$OM_type <- ifelse(d$OM_used, "farmyard manure", "none")
	d$OM_price <- ifelse(d$OM_used, 0.025, NA)          # USD/kg (250 USD for 10 t)
	d$crop_price <- 0.40                                # USD/kg fresh roots
	d$currency <- "USD"

	d$country <- "Democratic Republic of the Congo"
	d$adm1 <- "Sud-Kivu"
	d$longitude[d$location == "Kasheke"] <- 28.8594
	d$latitude[d$location == "Kasheke"] <- -2.1386                                                                       #https://www.geonames.org/search.html?q=Kasheke&country=CD
	d$longitude[d$location == "Munanira"] <- 28.9072
	d$latitude[d$location == "Munanira"] <- -2.8061                                                                      #https://www.geonames.org/search.html?q=Munanira&country=CD
	d$longitude[d$adm2 == "Kalehe" & d$location %in% c("Cibanda","Cibandja","Muhongoza")] <- 28.9167
	d$latitude[d$adm2 == "Kalehe" & d$location %in% c("Cibanda","Cibandja","Muhongoza")] <- -2.1                      #https://www.geonames.org/search.html?q=Kalehe&country=CD
	d$geo_from_source <- FALSE

	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$irrigated <- FALSE
	d$crop <- "cassava"
	d$plant_spacing <- 100
	d$row_spacing <- 100
	d$plant_density <- 10000
	d$plot_length <- 6
	d$plot_width <- 6

	yr <- as.integer(substr(d$trial_id, 3, 6))   # "LR2014_1" -> 2014
	d$planting_date <- paste0(yr - 1, "-09")      # see ISSUES 4
	d$harvest_date  <- paste0(yr, "-09")
	d$harvest_days <- 365                         # harvested at 12 MAP
	d$yield_part <- "roots"
	d$yield_isfresh <- TRUE
	d$yield_moisture <- NA

	carobiner::write_files(path, meta, d)
}

