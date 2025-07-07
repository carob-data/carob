# R script for "carob"


carob_script <- function(path) {

"Conventional rice–wheat (RW) rotation in the Indo-Gangetic Plains (IGP)  of South Asia is tillage, water, energy, and capital intensive. Coupled with these, crop residue burning contributes significantly to greenhouse gas (GHG) emission and environmental pollution. So, to evaluate the GHG mitigation potential of various climate-smart agricultural practices (CSAPs), an on-farm research trial was conducted during 2014–2017 in Karnal, India. Six management scenarios (portfolios of practices), namely, Sc1—business as usual (BAU)/conventional tillage (CT) without residue, Sc2—CT with residue, Sc3—reduced tillage (RT)with residue + recommended dose of fertilizer (RDF), —RT/zero tillage (ZT) with residue + RDF, Sc5—ZT with residue + RDF + GreenSeeker + Tensiometer, and Sc6—Sc5 + nutrient-expert tool, were included. The global warming potential (GWP) of the RW system under CSAPs (Sc4, Sc5, and Sc6) and the improved BAU (Sc2 and Sc3) were 33–40% and 4–26% lower than BAU (7653 kg CO2 eq./ha/year), respectively. This reflects that CSAPs have the potential to mitigate GWP by ~387 metric tons (Mt) CO2 eq./year from the 13.5 Mha RW system of South Asia. 
Lower GWP under CSAPs resulted in 36–44% lower emission intensity (383 kg CO2 eq./Mg/year) compared to BAU (642 kg CO2 eq./Mg/year). Meanwhile, the N-factor productivity and eco-efficiency of the RW system under CSAPs were 32–57% and 70–105% higher than BAU, respectively, which reflects that CSAPs are more economically and environmentally sustainable than BAU. The wheat yield obtained under various CSAPs was 0.62 Mg/ha and 0.84 Mg/ha higher than BAU during normal and bad years (extreme weather events), respectively. Thus, it is evident that CSAPs can cope better with climatic extremes than BAU. Therefore, a portfolio of CSAPs should be promoted in RWbelts for more adaptation and climate change mitigation. (2021-12-14)
"
	uri <- "hdl:11529/10548753"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)
	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		project=NA,
		publication= NA,
		data_organization = "HAU;ICAR;CIMMYT",
		data_type="is_experiment", 
		carob_contributor="Shumirai Manzvera",
		carob_date="2023-12-12"
	)

	f <- ff[basename(ff) == "Agriculture_11-0-1269_Kakraliya et al 2021.xlsx"]
	
	get_data <- function(r) {
		d <- data.frame(
				fwy_residue = r$`straw yield (t/ha)` * 1000,
				treatment = as.character(r$Scenario),
				rep = as.integer(r$Replication),
				grain_N = r$`N uptake grain`,
				residue_N = r$`N uptake straw`,
				N_fertilizer = r$Nitrogen,
				control_yield = r$`grain yield in control plot`,
				irrigation = r$`irrigation (mm)`,
				rain = r$`Rainfall`    
		)
		d$yield = r[[grep("grain yield$", colnames(r))]] * 1000
		d
	}
	

## rice
	r1 <- carobiner::read.excel(f,skip=1, n_max=18)
	d1 <- get_data(r1)
	d1$crop <- "rice"

## wheat

	r2 <- carobiner::read.excel(f, skip = 22, n_max=19) 
	colnames(r2)[3] <- "Nitrogen"
	d2 <- get_data(r2)
	d2$crop <- "wheat"
  
	d <- rbind(d1 ,d2)

	i <- match(d$treatment, 1:6)
	d$P_fertilizer <- (c(58, 58, 60, 60, 60, 39)/2.29)[i]
	d$K_fertilizer <- (c(0, 0, 60, 60, 60, 70)/1.2051)[i]

	d$country <- "India"
	d$location <- "Karnal"
	d$longitude <- 76.990547
	d$latitude <- 29.685629
	
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$irrigated <- TRUE
	d$yield_part <- "grain"
	d$irrigated <- TRUE

message("need to figure out what the 'control yield' refers to") 

	carobiner::write_files(meta, d, path=path)
}
