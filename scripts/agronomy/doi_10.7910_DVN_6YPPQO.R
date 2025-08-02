# R script for "carob"
# license: GPL (>=3)

## ISSUES


carob_script <- function(path) {

"
Africa RISING Tanzania- Maize Intensification Using Fertilizer

Using appropriate fertilizer recommendations and effective fertilizer materials is important to meet nutrient requirements of maize and sustain soil fertility. Unlike other agroecological zones, no fertilizer recommendations have been developed for the semi-arid zone in Tanzania, undermining the effort to target technologies in the specific biophysical conditions in which smallholder farmers operate. To fill this gap field experiments were carried out to develop fertilizer application rates for nitrogen (N) and phosphorus (P) and to identify the effective P source for this zone at Molet and Njoro Villages. Randomized complete block design (RCBD) with three replications was adopted in this study. Triple Super Phosphate (TSP) fertilizer was used to test various application rates: 0, 7.5, 15, 30, 45 and 60 kg P/ha. For P-source trial, Minjingu Mazao, Minjingu hyperphosphate, and TSP were tested at 0 and 30 kg P/ha for each fertilizer material. For the N-fertilizer trial, treatments included control (no fertilizer), 20, 40, 60, 80 and 120 kg N/ha and it was conducted at Njoro alone. In 2015 about 292 farmers (Female 134 and Male 158) in 3 villages demonstrated the fertilizer technology on their baby plots using the application rates developed on the mother sites.  About the project  Project title: Africa RISING   Project abstract  The aim of the Africa RISING project in Kongwa and Kiteto Districts, Tanzania is to provide a scientific basis for sustainably intensifying agricultural production in semi-arid areas of central Tanzania. The project activities are falls under 4 thematic areas that address three critical elements of sustainable intensification (SI), i.e. genetic, ecological and socio-economic intensification technologies. The scope of activities being implemented include: packaging of new legume and cereal varieties with over 120% yield advantage, packaging and validation of integrated productivity enhancing technologies for cereals, legumes, legume trees and soil health technologies, food safety primarily to reduce aflatoxin contamination and integration of livestock into the cropping systems. The innovation platform is used to set R4D priority in the action sites. In the 2013-2014 season, we reached out to about 1217 farmers Kongwa and Kiteto districts. In 2014 we plan to reach out to about 1500 new farmers. The project team is comprised of national partners (e.g. ARI-Hombolo, District Agricultural Officers, SUA and UDOM) and CG Partners (CIMMYT and ICRAF) under the leadership of ICRISAT. Project website:  http://africa-rising.net  Project start date: 05/1/2012   Project end date : 09/30/2016
"

	uri <- "doi:10.7910/DVN/6YPPQO"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=0,
		data_organization = "ICRAF;SUA",
		publication = "NA",
		project = "Africa RISING",
		carob_date = "2025-07-30",
		design = "RCBD",
		data_type = "experiment",
		treatment_vars = "N_fertilizer;P_fertilizer",
		response_vars = "yield", 
		carob_contributor = "Cedric Ngakou",
		completion = 100,	
		notes =NA
	)
	

	f1 <- ff[basename(ff) == "001_babyplotsonfertilizer_Mlali.csv"]
	f2 <- ff[basename(ff) == "001_babyplotsonfertilizer_Moleti.csv"]
	fft <- ff[grepl("Trials", basename(ff))]
	
	r1 <- read.csv(f1)
	r2 <- read.csv(f2)
	

	### Process 
	
	## location: Mlali 
	d1 <- data.frame(
		plot_id = r1$Plot.No,
		location = r1$Village,
		treatment = r1$Treatment_Fertilizer.type,
		variety = gsub(" ", "", r1$Treatment_Seed.Variety),
		yield = r1$Grain.Yield.t.ha.*1000,
		planting_date= as.character(r1$Growing.Season),
		trial_id= "1"
	)
	 
	## location:  Moleti 
	d2 <- data.frame(
	   plot_id = r2$Plot.No,
	   location = r2$Village,
	   treatment = r2$Treatement_Fertilizer.type,
	   variety = gsub(" " , "" , r2$Treatment_Maize.variety),
	   yield = r2$X.t.ha.*1000,
	   planting_date= as.character(r2$Growing.Season),
	   trial_id= "2"
	)
 d <- rbind(d1, d2)
 
 ### Adding fertilizer
 
 d$P_fertilizer <- ifelse(grepl("Yara Milla|Yara Mila", d$treatment), 30, 
                   ifelse(grepl("Minjingu mazao", d$treatment), 30, 0))
 d$N_fertilizer <- ifelse(grepl("Yara Milla|Yara Mila", d$treatment), 0, 
                          ifelse(grepl("Minjingu mazao", d$treatment), 60, 0))
 d$K_fertilizer <- 0 
 

 proc <- function(f){
     
     r <- read.csv(f)
     if (c("Site.1") %in% names(r)) {r$Site <- r$Site.1}
     names(r) <- gsub("Fertilizer.Treatment", "Fertilizer", names(r))
     names(r) <- gsub("Plot.No.", "Plot.No", names(r))
     names(r) <- gsub("Blk.Replicate", "Blk", names(r))
     names(r) <- gsub("GrYtha..t.ha.", "GrYtha.t.ha", names(r))
     names(r) <- gsub("^Treatment$", "N_fertilizer", names(r))
     if (is.null(r$Plot.No))  r$Plot.No <- NA
     if (is.null(r$N_fertilizer))  r$N_fertilizer <- 0 ## N fertilizer
     if (is.null(r$Fertilizer))  r$Fertilizer <- NA 
     if (is.null(r$Treatment..kg.P.ha.))  r$Treatment..kg.P.ha. <- 0 ## P fertilizer 
     
    data.frame(
        rep= as.integer(gsub("Blk", "", r$Blk)),
        plot_id= r$Plot.No,
        location= r$Community,
        site= r$Site,
        crop= substr(r$Crop, 1, 5),
        planting_date= as.character(r$Growing.Season),
        trial_id= gsub(" ", "", r$Trial),
        treatment= gsub(" ", "", r$Fertilizer),
        intercrops= ifelse(grepl("INTERCROPPING", r$Trial), "pigeon pea", "none"),
        P_fertilizer= as.numeric(gsub("kgPha", "", r$Treatment..kg.P.ha.)),
        N_fertilizer=as.numeric(gsub("KgN|kgN", "", r$N_fertilizer)),
        K_fertilizer= 0,
        yield= as.numeric(gsub("kgPha", "", r$GrYtha.t.ha))*1000
     )
     
 }
 
 d3 <- lapply(fft, proc)
 d3 <- do.call(carobiner::bindr, d3)
  
 d <- carobiner::bindr(d, d3) 
 i <- which(d$N_fertilizer!=0 | d$P_fertilizer!=0)
 d$fertilizer_used <- FALSE
 d$fertilizer_used[i] <- TRUE
 d$country <- "Tanzania"
 d$yield_part <- "grain"
 d$on_farm <- TRUE
 d$is_survey <- FALSE
 d$geo_from_source <- FALSE
 d$irrigated <- NA
 d$crop <- "maize" ## 
 
 ### Adding geo coordinate
 geo <- data.frame(
    location= c("Moleti", "Mlali", "Njoro", "Mollet"),
    longitude= c(36.8131, 36.7501, 36.666, 35.686),
    latitude= c(-6.1766, -6.2816, -5.498, -7.767)
 ) 
 
 d <- merge(d, geo, by="location", all.x = TRUE)	
 
carobiner::write_files(path, meta, d)

}


