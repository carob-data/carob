# R script for "carob"
# license: GPL (>=3)

## ISSUES
# list processing issues here so that an editor can look at them


carob_script <- function(path) {

"
Dataset for: Eliciting farmers’ demand for quality and nutritionally enhanced sweetpotato planting material in Rwanda

Sweetpotato (Ipomoea batatas L.) is an important staple and food security crop in Rwanda. It is widely consumed by many households, mostly in boiled form . Per capita sweetpotato production in Rwanda is among the highest in SSA. Sweetpotato, especially the biofortified orange-fleshed varieties with proven efficacy , can play a greater role on the immediate and long-term food and nutrition security in Rwanda. However, improving the productivity of sweetpotato is hindered by several factors, most importantly, the access to quality planting material. The existing material, mainly sourced informally from farmers, is recycled, hence oftentimes heavily infected with yield-degenerating virus diseases and pests (especially weevils). To date, however, projects and NGOs are the major buyers of quality vines from Descentralized Vine Multipliers while the majority of farmers obtain their sweetpotato planting material from own sources or neighboring farmers. This leads to the following research questions: what premium price are farmers willing to pay for quality vines and what are the drivers of the demand for quality vines? Are farmers willing to pay a premium for the high beta carotene nutritious biofortified varieties? The database present the data of the a willingness to pay (WTP) approach using auctions. Based on this design, each participant had a total of 11 bids (3 bids in Round 1 and 2 bids in the subsequent 4 rounds). In addition to estimating the average WTP for quality seed and a biofortified sweetpotato (i.e., OFSP) variety, econometric models will be estimated to examine the factors that affect WTP for quality seed and the biofortified sweetpotato variety.
"

## when done, remove all the default comments, such as this one, from the script
## only keep the comments you added that are specific to this dataset

	uri <- "doi:10.21223/MHLYQY"
	group <- "survey"
	ff  <- carobiner::get_data(uri, path, group)


	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		# include the data provider and/or all institutes listed as authors (if any)
		data_organization = "CIP",
		publication = "",
		project = NA,
		# if available report the experimental or survey design
		design = "unitOfAnalysis: Sweetpotato producing households; universe: Sweetpotato households in Rwanda; timeMethod: Cross-sectional; dataCollector: International Potato Center; samplingProcedure: The study used multistage cluster sampling procedure to randomly select sweetpotato growers in each purposively village and district.  Power calculation was conducted using the Optimal Design software beta version 3.01 to identify appropriate sample size for the study.  The power analysis is based on primary on willingness to pay (WTP) for quality seed as the outcome variable. This outcome variable is hypothesized to be affected by information related to quality, proxied in this study by seed source and nutritional benefit of OFSP, and the observed field performance. The total sample size is determined by the average treatment effect on primary outcome variables due to treatments. Since sweetpotato traded through informal market and particularly OFSP is a new variety in the market, we used expert opinion to obtain price ranges. Thus, the price of the sweetpotato seed per (20-30 cm) cutting in the rural market ranges from 5-10 Rwanda Francs(Rwf)[1] with an average price of 7.5 Rwf per cutting with a standard deviation (SD) of 1.5 for a normal distribution. However, farmers normally buy seed in kilograms (kg) for average price of 200 Rwf per kg which contains 20-25 cuttings.  We assume a 10-12% price increase[2] price per cutting) (effect size-ES) in the response of matched pairs among treatments that difference is normally distributed with an SD of 1.5 Rwf. But we double SD to be maximum expected SD as used by Oparinde et al (2016) i.e., 2.9 Rwf. Based on this, we estimate minimum detectable effect size (MDES) to be 0.2565 for 10% average treatment effect size due to information effect, 0.2821 for 11% average treatment effect size due to nutritional value of OFSP and 0.3078 for 12% average treatment effect size due to visual observation from the demo lots. Based on literature, expert’s opinion and farmers’ views, the power calculations were conducted. Using 5% significance level and a power of 0.8, we estimate that a minimum sample size of 667 participants is required to detect the minimum effect size on the primary outcome variables. This minimum sample size is increased by 7more samples to cater for potential dropouts and non-usable responses resulting in a final total sample size of 684.; collectionMode: This study used the 2nd price auction, with bidders informed a priori that: i) there would five rounds of auctions, ii) they would use their own money to purchase the product if they “won”, and iii) a binding round in which there would be purchases will be randomly selected at the end of four rounds. The bidders were informed that “winners” will be issued with a coupon to be redeemed at the time of planting for the amount of vines auctioned (which will be a bundles of 200 cuttings, each 30cm long). In each round, following the presentation of the products and the treatment information in each round, the participants were asked to make individual bids for the product (with 0 bids allowed, to cater for, among others, the fact that farmers often obtain seed for free from neighbors). Bids were collected a sealed envelope bearing the participant’s ID and seal it. The experimenter then collected the sealed bids and display them in descending order. At the end of the bidding rounds (i.e., Round 5), a binding round was randomly selected from among the five. The highest[1] bidders in that binding round would get to purchase the products[2] but pays the second highest price/bid (not his/her bid) using own money (i.e., no endowment). Non-truthful bidding due to strategic behavior was avoided by the experimenter giving “cheap talk” that emphasizes the importance of truthfulness and need not to consider what other participants are likely to bid. Participants who “win” in the auction would buy and take home the vines they have won the same day. This is because the study auctions were conducted at the time when the farmers are actually doing planting (which mostly occurs in November and December).; researchInstrument: Structured household survey questionnaire",
		
		# data_type can be e.g. "on-farm experiment", "survey", "compilation"
		data_type = NA,
		
		# treatment_vars has semi-colon separated variable names that represent the
		# treatments if the data is from an experiment. E.g. "N_fertilizer;P_fertilizer;K_fertilizer"
		treatment_vars = "",
		
		# response variables of interest such as yield, fwy_residue, disease incidence, etc. Do not include variables
		# that describe management for all treatments or other observations that were not related to the aim of 
		# the trial (e.g. the presence of a disease).
		response_vars = "", 

		# notes for the end-user
		notes = "",

		carob_contributor = "Your Name",
		carob_date = "2026-10-30",
		# The percentage of relevant variables that have been standardized (between 0 and 100%) 
		carob_completion = 0,	
		# The number of hours spent creating this script
		carob_effort = -1
	)
	

	f1 <- ff[basename(ff) == "biding_sheet_1.xls"]
	f2 <- ff[basename(ff) == "biding_sheet_2.xls"]
	f3 <- ff[basename(ff) == "biding_sheet_3.xls"]
	f4 <- ff[basename(ff) == "biding_sheet_4.xls"]
	f5 <- ff[basename(ff) == "monitoring.xls"]

	r1a <- carobiner::read.excel(f1, sheet="Data")
	r1b <- carobiner::read.excel(f1, sheet="Dictionary")
	r2a <- carobiner::read.excel(f2, sheet="Data")
	r2b <- carobiner::read.excel(f2, sheet="Dictionary")
	r3a <- carobiner::read.excel(f3, sheet="Data")
	r3b <- carobiner::read.excel(f3, sheet="Dictionary")
	r4a <- carobiner::read.excel(f4, sheet="Data")
	r4b <- carobiner::read.excel(f4, sheet="Dictionary")
	r5a <- carobiner::read.excel(f5, sheet="Data")
	r5b <- carobiner::read.excel(f5, sheet="Dictionary")

## select the variables of interest and assign them to the correct name

	d1a <- data.frame(
		hhid = r1a[["farmer_id"]],
		adm2 = carobiner::fix_name(r1a[["district_name"]], "title")
	)
##r1a: "demo_id", "vill_name", "cell_name", "sector_name", "date", "p1", "p2", "p3"


	d1b <- data.frame()
##r1b: "...1", "Name", "Label"


	d2a <- data.frame(
		hhid = r2a[["farmer_id"]],
		adm2 = carobiner::fix_name(r2a[["district_name"]], "title")
	)
##r2a: "demo_id", "vill_name", "cell_name", "sector_name", "date", "p1", "p2"


	d2b <- data.frame()
##r2b: "...1", "Name", "Label"


	d3a <- data.frame(
		hhid = r3a[["farmer_id"]],
		adm2 = carobiner::fix_name(r3a[["district_name"]], "title")
	)
##r3a: "demo_id", "vill_name", "cell_name", "sector_name", "date", "p1", "p2"


	d3b <- data.frame()
##r3b: "...1", "Name", "Label"


	d4a <- data.frame(
		hhid = r4a[["farmer_id"]],
		adm2 = carobiner::fix_name(r4a[["district_name"]], "title")
	)
##r4a: "demo_id", "vill_name", "cell_name", "sector_name", "date", "p1", "p2"


	d4b <- data.frame()
##r4b: "...1", "Name", "Label"


	d5a <- data.frame(
		hhid = r5a[["farmer_id"]],
		adm2 = carobiner::fix_name(r5a[["district_name"]], "title")
	)
##r5a: "sector_name", "cell_name", "vill_name", "m09_08a", "m09_08b", "m09_08c", "m09_08d", "m09_09", "m09_10", "m09_11a", "m09_11b", "m09_12_1", "m09_12_2", "m09_12_3", "m09_12_4", "m09_13_1", "m09_13_2", "m09_13_3", "m09_13_4", "m09_13a_1", "m09_13a_2", "m09_13a_3", "m09_13a_4", "m09_13b_1", "m09_13b_2", "m09_13b_3", "m09_13b_4", "m09_14a_1", "m09_14a_2", "m09_14a_3", "m09_14a_4", "m09_14b_1", "m09_14b_2", "m09_14b_3", "m09_14b_4", "m09_15a_1", "m09_15a_2", "m09_15a_3", "m09_15a_4", "m09_15b_1", "m09_15b_2", "m09_15b_3", "m09_15b_4", "m09_16_1", "m09_16_2", "m09_16_3", "m09_16_4", "m09_17_1", "m09_17_2", "m09_17_3", "m09_17_4", "m09_18_1", "m09_18_2", "m09_18_3", "m09_18_4", "m09_19_1", "m09_19_2", "m09_19_3", "m09_19_4", "m09_20_1", "m09_20_2", "m09_20_3", "m09_20_4", "m09_21_1", "m09_21_2", "m09_21_3", "m09_21_4", "m09_22_1", "m09_22_2", "m09_22_3", "m09_22_4", "m09_23_1", "m09_23_2", "m09_23_3", "m09_23_4", "m09_24_1", "m09_24_2", "m09_24_3", "m09_24_4", "m09_25_1", "m09_25_2", "m09_25_3", "m09_25_4", "m09_26_1", "m09_26_2", "m09_26_3", "m09_26_4", "m09_29_1", "m09_29_2", "m09_29_3", "m09_29_4", "m09_30_1", "m09_30_2", "m09_30_3", "m09_30_4", "m09_31_1", "m09_31_2", "m09_31_3", "m09_31_4", "m09_32_1", "m09_32_2", "m09_32_3", "m09_32_4", "m09_33_1", "m09_33_2", "m09_33_3", "m09_33_4", "m09_34_1", "m09_34_2", "m09_34_3", "m09_34_4", "m09_35_1", "m09_35_2", "m09_35_3", "m09_35_4", "m09_35b_1", "m09_35b_2", "m09_35b_3", "m09_35b_4", "m09_36a_1", "m09_36a_2", "m09_36a_3", "m09_36a_4", "m09_36b_1", "m09_36b_2", "m09_36b_3", "m09_36b_4"


	d5b <- data.frame()
##r5b: "...1", "Name", "Label", "...4"


## separate individual trials. For example trials in different locations or years. 
## do _not_ separate by treatments within a trial. For a survey, each row gets a unique trial_id
	d$trial_id <- as.character(as.integer(as.factor( ____ )))
	
## about the data (TRUE/FALSE)
	d$on_farm <- 
	d$is_survey <- 
	d$irrigated <-
	
## crop rotation. If available, add all crops, including "d$crop". Use an underscore for intercrops 
    d$crop_rotation <- "crop1;crop2;crop3_crop4"
	
## each site must have corresponding longitude and latitude
## if the raw data do not provide them you can estimate them from the location/adm data 
## see carobiner::geocode
	d$longitude <- 
	d$latitude <- 
# are the coordinates from the source (data/publication) or estimated by you?	
	d$geo_from_source <- TRUE/FALSE


## time can be year ("2023", four characters), year-month ("2023-07", 7 characters) or date ("2023-07-21", 10 characters).
## if dates come as character values, you can use as.character(as.Date()) for dates to assure the correct format.
	d$planting_date <- as.character(as.Date(   ))
	d$harvest_date  <- as.character(as.Date(    ))

### Fertilizers 
## note that we use P and K, not P2O5 and K2O
## P <- P2O5 / 2.29
## K <- K2O / 1.2051
   d$P_fertilizer <- 
   d$K_fertilizer <-
   d$N_fertilizer <- 
   d$S_fertilizer <- 
   d$lime <- 
## normalize names 
   d$fertlizer_type <- 

## for legumes   
   d$inoculated <- TRUE or FALSE
   d$inoculant <- "name of inoculant"
   
### in general, add comments to your script if computations are
### based on information gleaned from metadata, a publication, 
### or when they are not immediately obvious for other reasons

### Yield

	yield <- r$yield_tonha * 1000
	#what plant part does yield refer to?
	d$yield_part <- "tubers"
	d$yield_moisture <- r$moisture * 100

#NOTE: yield is the _fresh weight_ production (kg/ha) of the "yield_part 
# Also record fresh and/or dry weight production of other organs (or "residue" or "total")
# if the data allow for that 

	d$fwy_storage <- r$yield_tonha * 1000
	d$dmy_storage <- (1-r$moisture) * r$yield_tonha * 1000
	d$dmy_totat <- r$dry_biomass
	
# all scripts must end like this
	carobiner::write_files(path, meta, d)
}

## now test your function in a _clean_ R environment (no packages loaded, no other objects available)
# carob_script(path=_____)

