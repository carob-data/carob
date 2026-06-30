# R script for "carob"
# license: GPL (>=3)

## ISSUES



carob_script <- function(path) {

"
Sixty years of crop diversification with perennials improves yields more than no-tillage in Ohio grain cropping systems

Context Diversifying crops and reducing tillage can enhance the environmental health of agroecosystems, consequently improving crop performance over time. We report data from the Triplett-Van Doren No-Tillage and Crop Rotation Experiment, one of the longest-running replicated agricultural experiments in the United States, to evaluate the impact of no-tillage and crop rotation on corn and soybean yields over six decades.   Objective To assess the long-term impact of crop rotation on crop performance in Ohio agricultural systems, and how these rotations interact with tillage practices.   Methods This study was conducted at two sites with contrasting soil characteristics: Wooster (well-drained silt-loam) and Hoytville (poorly drained clay-loam). The experiment was a two-way full factorial design with three levels of tillage (Moldboard, Chisel, and No-Tillage) and three crop rotations (Continuous-Corn, a two-year Corn-Soybean rotation, and a three-year Corn-Forage-Forage rotation).   Results Crop rotation was the main driver of long-term crop performance, with the most favorable responses observed when forage crops were included in the cropping system at both sites. Over 60 years of field data showed corn yield increases ranging from 12 % to 21 % when transitioning from Continuous-Corn to Corn-Forage-Forage rotation in Wooster and Hoytville. Crop responses to tillage varied by soil type. In the well-drained silt-loam soil of Wooster, no-tillage led to immediate positive responses in all crop rotation systems. However, in the poorly drained clay-loam soil of Hoytville, significant initial yield penalties of up to 20 % were observed for No-Tillage Continuous-Corn. These initial yield reductions were mitigated when soybean or forage crops were included in the system. Corn yields under No-Tillage increased between 6 % and 16 % in Wooster, with no significant change for Hoytville. Soybean yields under no-tillage increased by up to 15 % in Wooster and 4 % in Hoytville compared to Moldboard systems. Crop responses varied under extreme precipitation. In Wooster, corn rotated with forage, outperformed continuous corn in dry conditions, especially under no-tillage. In Hoytville, corn-forage rotations yielded highest under wet conditions, regardless of tillage.   Conclusions Based on data spanning over 60 years of crop rotation and no-tillage practices in Ohio, rotations including perennial crops consistently showed significantly increased yields compared to monoculture corn and corn rotated with soybean, regardless of the tillage system. Furthermore, yield reductions associated with the transition to no-tillage were mitigated when forage crops were integrated into the cropping system.   Implications By evaluating long-term trends, we found that no-tillage can be viable even in clay soils under temperate climates when perennial crops are included in the rotation system. Our results demonstrate that long-term crop yields can significantly benefit from the implementation of both practices adopted together in cropping systems.
"

"
Materials and Methods 2.1 Sites description The Triplett Van-Doren No-Tillage and Crop Rotation Experiment was established in 1962 in Wooster, Ohio, USA (40° 45′ N, 81° 54′ W) and 1963 in Hoytville, Ohio, USA (41°13'N 83°45'W). These sites include two contrasting soil types; at Wooster a silt loam (well-drained, fine-loamy, mixed, mesic Typic Fragiudalfs) (USDA-SCS, 1973), and at Hoytville, a clay loam (poorly drained, fine, illitic, mesic Mollic Ochraqualfs) (USDA-SCS, 1984). Both sites are located in a humid continental climate. Crop season (April to October) cumulative precipitation, and average, maximum, and minimum atmospheric temperature from 1962 to 2023 are shown in Figure 1a and 1b for Wooster and Hoytville, respectively. 2.2 Experimental design and treatments The experiment consisted of a two-way full factorial with three levels of tillage intensity and three crop rotations deployed in a randomized complete block design with three replicates. The levels of tillage were: 1) No-Tillage; 2) Chisel; and 3) Moldboard. The No-Tillage treatment consisted of zero soil disturbance with all the residues from previous crop seasons left on the field. The Chisel treatment used a paraplow (1962-1983) and a chisel plow (1984-present) to till the soil while leaving at least 30% of the residues of the previous crop on the soil surface. The Moldboard treatment used a moldboard plow to invert the soil from 0-20 cm depth, with the majority of the crop residues buried in the soil profile. The Chisel and Moldboard treatments were performed in the spring at the Wooster site, and in the fall for the Hoytville site. Crop rotations were: 1) Continuous-Corn; 2) a two-year rotation of Corn-Soybean; and 3) a three-year rotation of Corn-Forage-Forage, with forage crops consisting of oats (Avena sativa L.) as a nurse crop seeded with a grass-legume mix including species such as orchard grass (Dactylis glomerata), tall fescue (Festuca arundinacea), alfalfa (Medicago sativa), red clover (Trifolium pratense), and clover (Trifolium repens L.). Corn, soybean, and oat crop residues remained in the field post-harvest, whereas alfalfa, clover, and red clover were harvested 2-3 times annually for hay after the establishment year. The first years (1962-1963) of the experiment included numerous compromised stands as equipment and field operation were being refined. As such, these years were excluded from the analysis. The size of the plots was 22.3 x 4.3 m in Wooster and 30.5 x 6.4 m in Hoytville. Field operations not involving tillage or crop rotation were maintained consistently, so that fertility, pest, and weed management were similar across treatments. Historical agronomic management practices are part of The Ohio State University public resource and described in detail by Dick (2013). Crop varieties varied over the long-term experiment as genetic material was improved, but always represented widely available varieties that were typically planted by farmers in the region at the time. A list of the varieties used in the long-term experiment is provided for Wooster (Supplemental Table S1) and for Hoytville, Ohio (Supplemental Table S2). All phases of each crop rotation (corn, soybean, forage year 1, and forage year 2) and tillage treatments were present each year, allowing comparisons of corn yield under all the crop rotations and tillage treatments, and soybean yield under all tillage treatments. 2.3 Field crop yields  Corn and soybean were harvested from the center of each plot at physiological maturity with a small plot grain combine, and final yields were adjusted to moisture content of 15.5% for corn and 13.0% for soybean. The harvested plot area was used to determine annual yield per unit area (Mg ha−1).
"
   

	uri <- "doi:10.5061/dryad.1zcrjdg4t"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=6, minor=NA,
		data_organization = "OHSU; OSU; COLSU",
		publication = "doi:10.1016/j.fcr.2025.109993",
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "land_prep_method;crop_rotation",
		response_vars = "yield", 
		notes = NA,
		carob_contributor = "Cedric Ngakou",
		carob_date = "2026-06-30",
		carob_completion = 100,	
		carob_effort = 2
	)
	
  ff <- ff[grepl("Corn|Soybean", basename(ff))]
	

	proc <- function(f){
	  
	  r <- read.csv(f, na= c(".", ""))
	  names(r) <- gsub("Yield.Mg.ha.|Yield..Mg.ha", "yield", names(r))
	  data.frame(
	    date = as.character(r$Year),
	    planting_date = as.Date(r$Plant.Date, "%m/%d/%y"),
	    harvest_date = as.Date(r$Harvest.Date, "%m/%d/%y"),
	    location = r$Site,
	    crop = ifelse(grepl("Corn", r$Crop), "maize", "soybean"),
	    plot_id = as.character(r$Plot),
	    rep = r$Repetition,
	    crop_rotation = ifelse(grepl("CS", r$Rotation), "maize;soybean",
	                    ifelse(grepl("CFF", r$Rotation), "maize;forage crop;forage crop", "none")),
	    land_prep_method = r$Tillage,
	    yield = r$yield* 1000,
	    trial_id = gsub("deCamargoSantosetal_|.csv", "", basename(f)),
	    country = "United States",
	    on_farm = TRUE, 
	    is_survey = FALSE, 
	    yield_part = "grain", 
	    yield_moisture = NA_real_, 
	    irrigated = NA,
	    yield_isfresh = NA
	  )
	}
	
	d <- lapply(ff, proc)
	d <- do.call(rbind, d)
  
	i <- format(d$planting_date, "%Y") > "2025" & !is.na(d$planting_date)
	d$planting_date[i] <- d$planting_date[i] - 36525
	d$planting_date <- as.character(d$planting_date)
	i <- format(d$harvest_date, "%Y") > "2025" & !is.na(d$harvest_date)
	d$harvest_date[i] <- d$harvest_date[i] - 36525
	d$harvest_date <- as.character(d$harvest_date)
	### Fixing land_prep
	P <- carobiner::fix_name(d$land_prep_method)
	P <- gsub("CT", "conventional", P)
	P <- gsub("MP", "ploughing", P)
	P <- gsub("NT", "none", P)
	d$land_prep_method <- P
	
	### Adding lon and lat coordinate  
	i <- grepl("Wooster", d$location) # From publication
	d$longitude[i] <- -81.9
	d$latitude[i] <- 40.75 
	
	i <- grepl("Hoytville", d$location) # From publication
	d$longitude[i] <- -83.75
	d$latitude[i] <- 41.21667
	d$geo_from_source <- TRUE 
	
	i <- grepl("Northwest", d$location)
	d$longitude[i] <- -84.729
	d$latitude[i] <- 41.692
	d$geo_from_source[i] <- FALSE
	
	d$harvest_date[which((as.Date(d$harvest_date)-as.Date(d$planting_date))>366)] <- NA
	d$harvest_date[which((as.Date(d$harvest_date)-as.Date(d$planting_date))< 1)] <- NA
	
	d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
	
	
	carobiner::write_files(path, meta, d)
}


