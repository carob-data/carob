# R script for "carob"
# license: GPL (>=3)


## NOTES
# Paper (no DOI): Juarez et al. 2001, CIP Program Report 1999-2000, p. 69-75 (hdl:10568/109461); its Oxapampa means match the data.
# 2 varieties x 3 N x 3 fungicide levels (paper) plus 3 Tomasa treatments at N160 (data only); 84 plots in 4 blocks.
# Spray dates are the Oxapampa block of file 07 (same schedule as Sprays.doc); the Huancayo trial is not in this dataset.
# Weather has daily (no time) and hourly records; hourly data from 8 Jan to 3 Feb 2000 look gap-filled.

## ISSUES
# Tomasa plots are not in the paper: P, K, fertilizer type, N splits and fungicide product left NA.
# on_farm and irrigated are not stated in the paper; left NA.
# Out of bounds, kept as reported: fungicide_times 29 (above terminag max 20, real: every 4 days); ground cover 106.5% (plot 55); slightly negative hourly ETo; one impossible last hourly record (63.7 C).
# No tuber yield in the data (only total biomass); yield left NA.

## NEW VARIABLES
# leaf_wetness: hourly leaf wetness index, 0 (dry) to 15 (wet)

## NOT PROCESSED
# paper Table 1 treatment means (tuber dry matter, rAUDPC, intercepted radiation)
# Huancayo spray dates in file 07 (other experiment)
# fungicide amount: 1.25 per mil Bravo 500, spray volume not given
# derived or unexplained weather columns (cumulative rain, day number, energy, degree days, chill, run, index, rate, volts)

carob_script <- function(path) {

"
Dataset for: Effect of nitrogen fertilization on potato late blight in the field in Oxapampa, Pasco, Peru

The effect of nitrogen (N) fertilization on the development of potato late blight disease, caused by Phytophthora infestans (Mont.) de Bary, was studied through field experiments in two locations in Peru (Huancayo and Oxapampa). These datasets correspond to the field experiments in Oxapampa. Three levels of N (0, 160, and 320 kg/ha) and three levels of fungicide were applied in all combinations on two potato (Solanum tuberosum L.) varieties with different levels of resistance (18 treatments), and in four repetitions. N had no effect on the disease when severity was very low or high. At intermediate levels of disease severity, increased N led to increased disease. In Huancayo, the effect of 0 vs. 160 kg·ha-1 of N on  the more susceptible variety was negligible because of increased late blight.  For the more resistant variety, late blight had little effect on yield when N was applied at 0 or 160 kg·ha-1. Increasing the N fertilization rate to 320 kg·ha-1 did not further increase yield in the absence of late blight, but decreased it sharply in the treatments where the disease was present because no fungicides were applied. These results indicate the importance of considering N fertilization levels in integrated management schemes for potato late blight, and in comparison of results across experiments. If farmers want to increase potato yields with N fertilizer, they may need to use more fungicides or varieties with higher resistance.
"

	uri <- "doi:10.21223/HMMWZH"
	group <- "pest_disease"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=0,
		data_organization = "CIP",
		publication = "hdl:10568/109461",
		project = NA,
		design = "RCBD, 4 blocks; factorial variety x N x fungicide",
		data_type = "experiment",
		treatment_vars = "variety;N_fertilizer;fungicide_times",
		response_vars = "disease_severity;ground_cover;dmy_total",
		notes = NA,
		carob_contributor = "Stella Muthoni",
		carob_LLM = "Claude Opus 5.5 (merged from 3 drafts)",
		carob_date = "2026-11-19",
		carob_completion = 90,
		carob_effort = 1
	)

	f1 <- ff[basename(ff) == "01_Data_LateBlightSeverity_Oxapampa.xls"]
	f2 <- ff[basename(ff) == "02_LateBlight_Evaluation_date.xls"]
	f3 <- ff[basename(ff) == "03_Data_Canopy_cover_Oxapampa.xls"]
	f4 <- ff[basename(ff) == "04_canopy_evaluation_days.xls"]
	f5 <- ff[basename(ff) == "05_TotalYield_Oxapampa.xls"]
	f6 <- ff[basename(ff) == "06_Evaluation_date_Total biomass dry matter content.xls"]
	f7 <- ff[basename(ff) == "07_SpraysOfFungicides_Oxapampa.xls"]
	f8 <- ff[basename(ff) == "08_DailyWeatherData_Oxapampa.xls"]
	f9 <- ff[basename(ff) == "09_HourlyWeatherData_Oxapampa.xlsx"]

	r1 <- carobiner::read.excel(f1)            # late blight severity (%) per plot, LB1-LB27
	r2 <- carobiner::read.excel(f2)            # dates of LB1-LB27 and emergence date
	r3 <- carobiner::read.excel(f3)            # ground cover (%) per plot, 16 dates
	r4 <- carobiner::read.excel(f4)            # dates of the ground cover observations
	r5 <- carobiner::read.excel(f5)            # total biomass dry matter (kg/ha) per plot, 4 dates
	r6 <- carobiner::read.excel(f6)            # dates of the biomass samplings
	r7 <- carobiner::read.excel(f7)            # fungicide spray dates, Huancayo (cols 2-5) and Oxapampa (cols 7-11)
	r8 <- carobiner::read.excel(f8, skip=2)    # daily weather Oxapampa
	r9 <- carobiner::read.excel(f9, skip=2, na=c("---", "--", "-32768"))   # hourly weather Oxapampa

	## d1: one record per plot (r1), with spray dates (r7) and emergence date (r2)
	sp <- r7[!is.na(r7$...7), ]                                          # Oxapampa block: rows with a spray number
	spF4 <- as.Date(as.numeric(sp$...8), origin="1899-12-30")            # Excel serial numbers
	spF10 <- as.Date(as.numeric(sp$...9), origin="1899-12-30")
	spF10 <- spF10[!is.na(spF10)]
	emergence <- as.Date(r2$Evaluation_date[r2$Abbreviation == "Emergency_date"])
	paper <- r1$Variety != "Tomasa"                                      # the 18 treatments in the paper
	stopifnot(identical(r1$id, r3$id), identical(r1$id, r5$id), identical(r1$Treatment, r5$Treatment))   # same plots, same order
	
	d1 <- data.frame(
	  plot_id = as.character(r1$id),
	  rep = as.integer(r1$Repetition),
	  block_id = as.character(r1$Repetition),
	  treatment = paste(r1$Variety, r1$Nitrogen_rate, r1$Fungicide_rate, sep="_"),
	  variety = r1$Variety,
	  N_fertilizer = as.numeric(gsub("N", "", r1$Nitrogen_rate)),
	  fungicide_used = r1$Fungicide_rate != "F0",
	  fungicide_interval = unname(c(F0=NA, F4=4L, F10=10L)[r1$Fungicide_rate]),        # days between sprays
	  fungicide_times = unname(c(F0=0L, F4=length(spF4), F10=length(spF10))[r1$Fungicide_rate]),
	  fungicide_dates = unname(c(F0=NA, F4=paste(spF4, collapse=";"), F10=paste(spF10, collapse=";"))[r1$Fungicide_rate])
	)
	
	d1$trial_id <- "1"
	d1$crop <- "potato"
	d1$variety_traits <- unname(c(Amarilis="moderately resistant to late blight", Yungay="moderately susceptible to late blight", Tomasa=NA)[d1$variety])   # paper
	d1$disease <- "potato late blight"
	d1$pathogen <- "Phytophthora infestans"
	d1$stress <- "disease"
	d1$severity_scale <- "0-100"                     # % of the foliage infected (paper)
	
	# fungicide (paper): chlorothalonil (Bravo 500), backpack sprayer; not reported for Tomasa
	# paper says spraying started at emergence; file 07 and Sprays.doc give the first spray 4 days after emergence
	d1$fungicide_product <- ifelse(d1$fungicide_used, "chlorothalonil", "none")
	d1$fungicide_product[!paper] <- NA
	d1$fungicide_implement <- ifelse(d1$fungicide_used, "backpack sprayer", NA)
	d1$fungicide_implement[!paper] <- NA
	
	# fertilizer (paper): N as NH4NO3, half at planting and half 22 days after emergence;
	# P 62 and K 100 kg/ha (as elements, as written) as Ca(H2PO4)2 and KCl at planting; not reported for Tomasa
	d1$N_splits <- ifelse(d1$N_fertilizer > 0, 2L, 0L)
	d1$P_fertilizer <- 62
	d1$K_fertilizer <- 100
	d1$fertilizer_type <- ifelse(d1$N_fertilizer > 0, "AN;TSP;KCl", "TSP;KCl")   # Ca(H2PO4)2 is closest to TSP
	d1$fertilizer_date <- ifelse(d1$N_fertilizer > 0, paste("1999-11-27", emergence + 22, sep=";"), "1999-11-27")
	d1$N_splits[!paper] <- NA
	d1$P_fertilizer[!paper] <- NA
	d1$K_fertilizer[!paper] <- NA
	d1$fertilizer_type[!paper] <- NA
	d1$fertilizer_date[!paper] <- NA
	
	# other management (paper, Methods)
	d1$planting_date <- "1999-11-27"
	d1$emergence_date <- as.character(emergence)
	d1$harvest_date <- "2000-04-05"                  # last harvest
	d1$plot_area <- 16.8                             # m2, 4 rows of 14 plants
	d1$row_spacing <- 100
	d1$plant_spacing <- 30
	d1$herbicide_used <- TRUE
	d1$herbicide_product <- "metribuzin;glyphosate"  # Sencor, Roundup
	d1$insecticide_used <- TRUE
	d1$insecticide_product <- "carbofuran"           # Furadan, against Diabrotica
	d1$on_farm <- NA
	d1$is_survey <- FALSE
	d1$irrigated <- NA
	
	# location (paper: 10d35'31" S, 75d23'0" "E", 1813 m; longitude is West)
	d1$country <- "Peru"
	d1$adm1 <- "Pasco"
	d1$adm2 <- "Oxapampa"
	d1$location <- "Oxapampa"
	d1$latitude <- -10.5919
	d1$longitude <- -75.3833
	d1$elevation <- 1813
	d1$geo_from_source <- TRUE
	d1$geo_uncertainty <- 1000                       # longitude given to the minute
    d1$yield <- NA
	d1$yield_moisture <- NA ## not provided
	d1$yield_part <- "aboveground biomass"
	d1$yield_isfresh <- NA                           # no tuber yield in the data
	
	## d1long: late blight severity per plot and date (r1, dates from r2)
	lbcol <- grep("^LB", names(r1), value=TRUE)
	lbdate <- as.character(as.Date(r2$Evaluation_date[match(lbcol, r2$Abbreviation)]))
	d1long <- data.frame(
	  plot_id = rep(as.character(r1$id), length(lbcol)),
	  date = rep(lbdate, each=nrow(r1)),
	  disease_severity = as.character(unlist(r1[, lbcol]))     # % of foliage infected
	)
	
	## d3: ground cover per plot and date (r3, dates from r4)
	cccol <- grep("^CC_G", names(r3), value=TRUE)
	ccdate <- as.character(as.Date(r4$Evaluation_date_CCG[match(cccol, r4$Abbreviation)]))
	d3 <- data.frame(
	  plot_id = rep(as.character(r3$id), length(cccol)),
	  date = rep(ccdate, each=nrow(r3)),
	  ground_cover = as.numeric(unlist(r3[, cccol]))           # %, 1 x 0.6 m grid frame over 2 plants
	)
	
	## d5: total biomass dry matter per plot and date (r5, dates from r6)
	bmcol <- grep("^TBDMC", names(r5), value=TRUE)
	bmdate <- as.character(as.Date(r6$Evaluation_date_TBDMC[match(sub(".*Ev", "", bmcol), r6$Numeration)]))
	d5 <- data.frame(
	  plot_id = rep(as.character(r5$id), length(bmcol)),
	  date = rep(bmdate, each=nrow(r5)),
	  dmy_total = as.numeric(unlist(r5[, bmcol]))              # kg/ha; 0 when the canopy was dead
	)

# we have hourly weather, no need to keep daily weather 
#	## d8: daily weather (r8); 1999-12-08, -09, -10 are copies of 12-13, 12-14, 12-26 (kept)
#	d8 <- data.frame(
#	  date = as.character(as.Date(r8$...1)),
#	  temp = r8$`T prom.`,
#	  tmax = r8$`T Hi prom.`,                  # mean of the hourly maxima
#	  tmin = r8$`T low prom`,                  # mean of the hourly minima
#	  rhum = r8$`% HR prom.`,
#	  prec = r8$`Rain/d`,
#	  rhum90_hours = r8$`Hora Hr>90`,
#	  temp_rhum90_ = r8$`T prom. HR>90`,
#	  leaf_wet_hours = r8$`Hr de LW>=1`
#	)
#	d8 <- d8[!is.na(d8$date), ]                  # summary row
	
	## wth: hourly weather (r9); "Time" is a fraction of a day, but a running count of days in the gap-filled stretch
	tday <- as.numeric(difftime(r9$Time, as.POSIXct("1899-12-31", tz="UTC"), units="days"))
	compass <- c("N", "NNE", "NE", "ENE", "E", "ESE", "SE", "SSE", "S", "SSW", "SW", "WSW", "W", "WNW", "NW", "NNW")
	wth <- data.frame(
	  date = as.character(as.Date(r9$Date)),
	  time = sprintf("%02d:00", round(24 * (tday %% 1))),
	  temp = r9$Temp,
	  tmax = r9$Hi...4,
	  tmin = r9$Low,
	  rhum = r9$Hum,
	  dewp = r9$Point,
	  prec = r9$Rain,
	  irad = r9$Rad,                           # W/m2
	  ETo = r9$ET,                             # mm per hour, computed by the station
	  pressure = r9$Bar * 1.33322,             # mmHg -> hPa (611 mmHg = 815 hPa, fits 1813 m)
	  wspd = r9$Speed,
      wspdmx = r9$Hi...13,
	  wdir = (match(r9$Dir, compass) - 1) * 22.5, #Nice # 22.5 is a standard compass interval
	  leaf_wetness = r9$Leaf
	)
	wth <- wth[!is.na(wth$date), ]                  # totals row
		
	dlong <- merge(d1long, d3, by=c("plot_id", "date"), all=TRUE)
	dlong <- merge(dlong, d5, by=c("plot_id", "date"), all=TRUE)
	dlong$DAE <- as.integer(as.Date(dlong$date) - emergence)
	
#	wth <- carobiner::bindr(d8, d9)
	wth$country <- "Peru"
	wth$location <- "Oxapampa"
	wth$latitude <- -10.5919
	wth$longitude <- -75.3833
	wth$geo_from_source <- TRUE
	
	carobiner::write_files(path, meta, d1, long=dlong, wth=wth)
}
