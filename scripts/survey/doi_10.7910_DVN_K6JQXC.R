# R script for "carob"
# license: GPL (>=3)

## ISSUES
### Absence of a converter to transform local units into standard units.

carob_script <- function(path) {

"
Household Survey Data on Cost Benefit Analysis of Climate-Smart Soil Practices in Western Kenya

This household survey was conducted among 88 respondents by CIAT in three counties of western Kenya (Bungoma, Kakamega, and Siaya) in 2016. The main aim of the project was to conduct a cost benefit analysis of eight climate-smart soil (CSS) practices, as a step toward understanding whether they were beneficial or not both from a private and social point of view. This knowledge could then be potentially used to enlighten farmers, policy makers and development practitioners about soil protection and rehabilitation practices that are most cost-effective when implemented on farms. Such knowledge also provides a rationale that can be used as a basis for promoting selected CSS practices. Farm practices were considered as 'climate smart' if they could improve the soil-nitrogen cycle, enhance soil fertility, improve crop productivity, improve soil biodiversity, promote soil conservation, increase soil biomass, reduce soil erosion, reduce volatility in crop and livestock production, and reduce water pollution. These practices could, in turn, boost food production, income, and households' ability to adapt to climate change. Variables collected include: 1) general information about each site, 2) household age, gender, education level, and farming experience, 3) farm activities (without intervention), 4) implemented CSS practices such as the use of improved seeds, agroforestry, inorganic fertilisers, liming, and organic manure  socio economic characteristics, and farm output, 5) crop and livestock yields, prices for farm inputs and outputs, the cost of implementing farm activities (both before and after intervention), 7) household financial information and, 8) environmental effects. Identifying variables such household head information, contact details and geographical locations of the households have not been provided in the data but they can be availed upon request.
"


	uri <- "doi:10.7910/DVN/K6JQXC"
	group <- "survey"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=8,
		data_organization = "CIAT",
		publication = NA,
		project = NA,
		carob_date = "2026-03-13",
		design = NA,
		data_type = "survey",
		treatment_vars = "none",
		response_vars = "none", 
		carob_contributor = "Cedric Ngakou",
		completion = 70,	
		notes = "We only process files with useful information for carob"
	)
	

	# f1 <- ff[basename(ff) == "03.01 Question 1_8 Codebook.xls"]
	# f2 <- ff[basename(ff) == "03.02 Question 9 _Table 1 Codebook.xls"]
	# f3 <- ff[basename(ff) == "03.03 Question 10_11 _Table 2_3 Codebook.xlsx"]
	# f4 <- ff[basename(ff) == "03.04 Question 12_Table 4 Codebook.xlsx"]
	# f5 <- ff[basename(ff) == "03.05 Question 13_14 Table 5 Codebook.xlsx"]
	# f6 <- ff[basename(ff) == "03.06 Question 15 Table 6 Codebook.xlsx"]
	# f7 <- ff[basename(ff) == "03.07 Question 15_17 Table 71_72 Codebook.xlsx"]
	# f8 <- ff[basename(ff) == "03.08 Question 18 Table 8_1 Codebook.xlsx"]
	# f9 <- ff[basename(ff) == "03.09 Question 19 Table 8_2 Codebook.xlsx"]
	# f10 <- ff[basename(ff) == "03.10 Question 20 Table 9_1 Codebook.xls"]
	# f11 <- ff[basename(ff) == "03.11 Question 20 Table 9_2 Codebook.xls"]
	# f12 <- ff[basename(ff) == "03.12 Question 21 Table 10_1 Codebook.xls"]
	# f13 <- ff[basename(ff) == "03.13 Question 22 Table 10_2 Codebook.xls"]
	# f14 <- ff[basename(ff) == "03.14 Question 23 Table 11 Codebook.xlsx"]
	# f15 <- ff[basename(ff) == "03.15 Question 24 Table 12 Codebook.xls"]
	# f16 <- ff[basename(ff) == "03.16 Question 26_30 Codebook.xls"]
	# f17 <- ff[basename(ff) == "03.17 Question 31_32 Codebook.xls"]
	f18 <- ff[basename(ff) == "02.01 Question 1_8.dta"]
	f19 <- ff[basename(ff) == "02.02 Question 9 _Table 1.dta"]
	f20 <- ff[basename(ff) == "02.03 Question 10_11 _Table 2_3.dta"]
	f21 <- ff[basename(ff) == "02.04 Question 12_Table 4.dta"]
	f22 <- ff[basename(ff) == "02.05 Question 13_14 Table 5.dta"]
	f23 <- ff[basename(ff) == "02.06 Question 15 Table 6.dta"]
	f24 <- ff[basename(ff) == "02.07 Question 15_17 Table 71_72.dta"]
	f25 <- ff[basename(ff) == "02.08 Question 18 Table 8_1.dta"]
	f26 <- ff[basename(ff) == "02.09 Question 19 Table 8_2.dta"]
	f27 <- ff[basename(ff) == "02.10 Question 20 Table 9_1.dta"]
	f28 <- ff[basename(ff) == "02.11 Question 20 Table 9_2.dta"]
	f29 <- ff[basename(ff) == "02.12 Question 21 Table 10_1.dta"]
	f30 <- ff[basename(ff) == "02.13 Question 22 Table 10_2.dta"]
	f31 <- ff[basename(ff) == "02.14 Question 23 Table 11.dta"]
	f32 <- ff[basename(ff) == "02.15 Question 24 Table 12.dta"]
	f33 <- ff[basename(ff) == "02.16 Question 26_30.dta"]
	f34 <- ff[basename(ff) == "02.17 Question 31_32.dta"]

	#r1 <- carobiner::read.excel(f1)
	#r2 <- carobiner::read.excel(f2)
	#r3 <- carobiner::read.excel(f3)
	#r4 <- carobiner::read.excel(f4)
	#r5 <- carobiner::read.excel(f5)
	#r6 <- carobiner::read.excel(f6)
	#r7 <- carobiner::read.excel(f7)
	#r8 <- carobiner::read.excel(f8)
	#r9 <- carobiner::read.excel(f9)
	#r10 <- carobiner::read.excel(f10)
	#r11 <- carobiner::read.excel(f11)
	#r12 <- carobiner::read.excel(f12)
	#r13 <- carobiner::read.excel(f13)
	#r14 <- carobiner::read.excel(f14)
	#r15 <- carobiner::read.excel(f15)
	#r16 <- carobiner::read.excel(f16)
	#r17 <- carobiner::read.excel(f17)
	r18 <- haven::read_dta(f18) |> carobiner:::unlabel()
	r19 <- haven::read_dta(f19) |> carobiner:::unlabel()
	r20 <- haven::read_dta(f20) |> carobiner:::unlabel()
	#r21 <- haven::read_dta(f21) |> carobiner:::unlabel()
	#r22 <- haven::read_dta(f22) |> carobiner:::unlabel()
	r23 <- haven::read_dta(f23) |> carobiner:::unlabel()
	r24 <- haven::read_dta(f24) |> carobiner:::unlabel()
	#r25 <- haven::read_dta(f25) |> carobiner:::unlabel()
	#r26 <- haven::read_dta(f26) |> carobiner:::unlabel()
	r27 <- haven::read_dta(f27) |> carobiner:::unlabel()
	# r28 <- haven::read_dta(f28) |> carobiner:::unlabel()
	# r29 <- haven::read_dta(f29) |> carobiner:::unlabel()
	# r30 <- haven::read_dta(f30) |> carobiner:::unlabel()
	# r31 <- haven::read_dta(f31) |> carobiner:::unlabel()
	# r32 <- haven::read_dta(f32) |> carobiner:::unlabel()
	# r33 <- haven::read_dta(f33) |> carobiner:::unlabel()
	# r34 <- haven::read_dta(f34) |> carobiner:::unlabel()

 #### process

 d1	<- data.frame(
  hhid =  as.character(r18$unique_hhqid),
  location = r18$county,
  farmer_education = r18$q3_2,
  farmer_gender = r18$q3_3,
  field_size = r18$q_7*0.4047,
  farmland_owned = r18$q_8owned*0.4047,
  farmland = r18$q_8total*0.4047
 )
 
 #############
 
 d2	<- data.frame(
   hhid = as.character(r19$unique_hhqid),
   plot_area = r19$q9_area*0.4047,
   plot_slope = r19$q9_slope,
   soil_color = r19$q9_soil_type,
   season = r19$q9_ssn_utilization,
   crop = tolower(r19$q9_crops_ssn_1)
 )
d2 <- d2[d2$plot_area>0,]
 ### merge d1 and d2
 
 d <- merge(d1, d2, by= intersect(names(d1), names(d2)), all = TRUE)
 
 ##########	
r20[r20== -99] <- NA
d3 <- data.frame(
  hhid = as.character(r20$unique_hhqid),
  year1 = r20$q10_yr1,
  yield1 =  ifelse(grepl("Gorogoro", r20$q10_unit_yr1), r20$q10_yield_y1*2,
            ifelse(grepl("Grams", r20$q10_unit_yr1), r20$q10_yield_y1/1000,
            ifelse(grepl("Dbe of 20 Kg", r20$q10_unit_yr1), r20$q10_yield_y1*20, 
            ifelse(grepl("Bag of 90kg", r20$q10_unit_yr1), r20$q10_yield_y1*90,
            ifelse(grepl("Bag of 50kg", r20$q10_unit_yr1), r20$q10_yield_y1*50,
            ifelse(grepl("Bag of 25kg", r20$q10_unit_yr1), r20$q10_yield_y1*25, r20$q10_yield_y1)))))),
  
  year2 = r20$q10_yr2,
  yield2 =  ifelse(grepl("Gorogoro", r20$q10_unit_a_y2), r20$q10_yield_y2*2,
            ifelse(grepl("Grams", r20$q10_unit_a_y2), r20$q10_yield_y2/1000,
            ifelse(grepl("Dbe of 20 Kg", r20$q10_unit_a_y2), r20$q10_yield_y2*20, 
            ifelse(grepl("Bag of 90kg",r20$q10_unit_a_y2), r20$q10_yield_y2*90,
            ifelse(grepl("Bag of 50kg", r20$q10_unit_a_y2), r20$q10_yield_y2*50,
            ifelse(grepl("Bag of 25kg", r20$q10_unit_a_y2), r20$q10_yield_y2*25, r20$q10_yield_y2)))))),
  year3 = r20$q10_yr3,
  yield3 = ifelse(grepl("Gorogoro", r20$q10_unit_yr3), r20$q10_yield_yr3*2,
           ifelse(grepl("Grams", r20$q10_unit_yr3), r20$q10_yield_yr3/1000,
           ifelse(grepl("Dbe of 20 Kg", r20$q10_unit_yr3), r20$q10_yield_yr3*20, 
           ifelse(grepl("Bag of 90kg",r20$q10_unit_yr3), r20$q10_yield_yr3*90,
           ifelse(grepl("Bag of 50kg", r20$q10_unit_yr3), r20$q10_yield_yr3*50,
           ifelse(grepl("Bag of 25kg", r20$q10_unit_yr3), r20$q10_yield_yr3*25, r20$q10_yield_yr3)))))),

  year4 = r20$q10_yr4,
  yield4 = ifelse(grepl("Gorogoro",  r20$q10_price_y4), r20$q10_yield_y4*2,
           ifelse(grepl("Grams",  r20$q10_price_y4), r20$q10_yield_y4/1000,
           ifelse(grepl("Dbe of 20 Kg",  r20$q10_price_y4), r20$q10_yield_y4*20, 
           ifelse(grepl("Bag of 90kg", r20$q10_price_y4), r20$q10_yield_y4*90,
           ifelse(grepl("Bag of 50kg",  r20$q10_price_y4), r20$q10_yield_y4*50,
           ifelse(grepl("Bag of 25kg",  r20$q10_price_y4), r20$q10_yield_y4*25, r20$q10_yield_y4)))))),
  
  year5 = r20$q10_yr5,
  
  yield5 = ifelse(grepl("Gorogoro",  r20$q10_unit_yr5), r20$q10_yield_yr5*2,
           ifelse(grepl("Grams",  r20$q10_unit_yr5), r20$q10_yield_yr5/1000,
           ifelse(grepl("Dbe of 20 Kg",  r20$q10_unit_yr5), r20$q10_yield_yr5*20, 
           ifelse(grepl("Bag of 90kg", r20$q10_unit_yr5), r20$q10_yield_yr5*90,
           ifelse(grepl("Bag of 50kg",  r20$q10_unit_yr5), r20$q10_yield_yr5*50,
           ifelse(grepl("Bag of 25kg",  r20$q10_unit_yr5), r20$q10_yield_yr5*25, r20$q10_yield_yr5)))))),
  
  crop_price = rowSums(r20[, c("q10_pric_yr1", "q10_price_y2", "q10_price_y3", "q10_price_y4", "q10_price_yr5")],na.rm = TRUE)
  
)	

d31 <- reshape(d3, varying = list(c("yield1","yield2", "yield3", "yield4", "yield5"),
                                 c("year1", "year2", "year3", "year4", "year5")),
              v.names = c("yield", "planting_date"),
              direction = "long")  
 
d31$id <- d31$time <- NULL
gg <- aggregate(. ~ hhid + crop_price + planting_date , d31, function(x) mean(x)) 

### merge d and d31
d <- merge(d, gg, by= intersect(names(d), names(d31)), all = TRUE)
d$record_id <- as.integer(1:nrow(d))

####
d4 <- data.frame(
  hhid = as.character(r23$unique_hhqid),
  herbicide_amount = r23$q15_herb_bau,
  herbicide_used = ifelse(!is.na(r23$q15_herb_bau) & r23$q15_herb_bau >0 , TRUE, FALSE),
  herbicide_price = r23$q15_price_adop,
  mulch_used = grepl("Yes",r23$q15_mulch_adopt)
)

### merge d and d4
d4_agg <- aggregate(. ~ hhid +herbicide_used  + mulch_used , d4, function(x) mean(x)) 
d <- merge(d, d4_agg, by= intersect(names(d), names(d4)), all = TRUE)
d <- d[!duplicated(d$record_id),]
####

d5 <- data.frame(
  hhid = as.character(r24$unique_hhqid),
  seed_type = r24$q16_adopt_type_of_seed,
  seed_rate = ifelse(grepl("Gorogoro", r24$q16_adopt_unit_of_seed), r24$q16_adopt_how_much_seed*2,
              ifelse(grepl("Grams", r24$q16_adopt_unit_of_seed), r24$q16_adopt_how_much_seed/1000,
              ifelse(grepl("Dbe of 20 Kg", r24$q16_adopt_unit_of_seed), r24$q16_adopt_how_much_seed*20, 
              ifelse(grepl("Bag of 90kg", r24$q16_adopt_unit_of_seed), r24$q16_adopt_how_much_seed*90,
              ifelse(grepl("Bag of 50kg", r24$q16_adopt_unit_of_seed), r24$q16_adopt_how_much_seed*50,
              ifelse(grepl("Bag of 25kg", r24$q16_adopt_unit_of_seed), r24$q16_adopt_how_much_seed*25, r24$q16_adopt_how_much_seed)))))),
  seed_origin = r24$q16_on_adoption_origin,
  seed_price = r24$q16_adopt_price_per_unit
  
  
)

### merge d and d5
d <- merge(d, unique(d5), by= intersect(names(d), names(d5)), all = TRUE)
d <- d[!duplicated(d$record_id),]
###########

d6 <- data.frame(
  hhid = as.character(r27$unique_hhqid),
  fertilizer_amount = rowSums(r27[, c("q9_1_adqtty_urea_9_1", "q9_1_adqtty_phosphate9_1", "q9_1_adqtty_other_fert9_1")], na.rm = TRUE),
  N_fertilizer = r27$q9_1_adqtty_urea_9_1*0.46,
  fertilizer_price = as.character(rowSums(r27[, c("q9_1_adprice_per_uniu9_1", "q9_1_adper_unit_phosphate9_1")], na.rm = TRUE)),
  P_fertilizer = r27$q9_1_adqtty_phosphate9_1,
  OM_amount = r27$q9_1_adqtty_organic_9_1,
  OM_used = ifelse(!is.na(r27$q9_1_adqtty_organic_9_1) & r27$q9_1_adqtty_organic_9_1 >0, TRUE, FALSE),
  OM_price = r27$q9_1_adprice_organic9_1,
  K_fertilizer = as.numeric(NA)
  
)

### merge d and d6
d <- merge(d, unique(d6), by= intersect(names(d), names(d6)), all = TRUE)
d <- d[!duplicated(d$record_id)& !is.na(d$record_id),]

### Adding long and lat coordinate 
geo <- data.frame(
  location = c("Kakamega", "Bungoma", "Siaya"),
  longitude = c(34.7514, 34.559, 34.2869),
  latitude = c(0.2826, 0.5842, 0.0629)
)

d <- merge(d, geo, by="location", all.x = TRUE)

d$crop_price <- ifelse(d$yield>0, d$crop_price/d$yield, d$crop_price)  ## KES/unit
d$yield <- d$yield/d$plot_area # kg/ha
d$fertilizer_amount <- d$fertilizer_amount/d$plot_area ## kg/ha
d$seed_rate <- d$seed_rate/d$plot_area  ### kg/ha
d$plot_area <- d$plot_area*10000 ## m2

### Fixing crop names
P <- carobiner::fix_name(d$crop)
P <- gsub("beans", "common bean", P)
P <- gsub("traditional vegetables|other vegetable", "vegetable", P)
P <- gsub("butternut", "winter squash", P)
#P <- gsub("calliandra", "calliandra", P)
P <- gsub("soya bean", "soybean", P)
P <- gsub("tomatoes", "tomato", P)
P <- gsub("plantain \\(cooking banana\\)", "banana", P)
P <- gsub("eucalyptus|^other$|other tree|other cereal|other fruit|other fodder crop", "unknown", P)
P <- gsub("arrow root", "bermudagrass", P)
P <- gsub("sugar cane", "sugarcane", P)
P <- gsub("cow pea", "cowpea", P)
P <- gsub("sweet potato", "sweetpotato", P)
P <- gsub("kales", "kale", P)
P <- gsub("unknown vegetable", "vegetable", P)
P <- gsub("green gram \\(moong bean\\)", "mung bean", P)
d$crop <- P

###  fixing season 
d$season <- gsub("Short rain \\(season 2\\)", "short rains", d$season)
d$season <- gsub("Long rain \\(season 1\\)", "long rains", d$season)
d$season <- gsub("Both Seasons", "wet", d$season)

d$country <- "Kenya"
d$currency = "KES"
d$on_farm <- FALSE
d$is_survey <- TRUE
d$geo_from_source <- FALSE
d$yield_part <- "none"
d$yield_moisture <- as.numeric(NA)
d$yield_isfresh <- TRUE
d$irrigated <- NA
d$trial_id <- paste(d$location, d$hhid, sep="-")


carobiner::write_files(path, meta, d)
}

