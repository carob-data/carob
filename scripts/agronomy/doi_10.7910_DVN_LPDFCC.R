# R script for "carob"


#NOTE 
#There is a variable "Trt.pattern" that is not used but seems important. Perhaps comparing that to what is in "cereal_legume_intercropping_protocol.pdf" can help understand what it means. 
#The pdf has more info that is not captured. For example, plating date appears to be a treatment. The spatial arrangement is really important in intercropping and should be captured. 

carob_script <- function(path) {
   
"This dataset is from the research study that aims to diversify the production environment to de-risk production in these semi-arid ecologies. In this study, two approaches were used , i.e. (i) testing suitability of legume x legume and legume by cereal production systems; (ii) modeling the multiple cropping systems using APSIM crop simulation model to assess changes in resource base, resource use efficiencies, productivity and profitability of the different cropping systems. The data included here are from the testing of suitability of legume legume x legume and legume by cereal production systems in Kongwa, Kiteto and Iringa districts of Tanzania and consists of overall biomass and seed weight along with variety name and intercropping"
   
   uri <- "doi:10.7910/DVN/LPDFCC"
   group <- "agronomy"
   
   ff  <- carobiner::get_data(uri, path, group)
   
   meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0, 
      data_organization = "ICRISAT", 
      publication =NA, 
      project = NA, 
      data_type = "experiment", 
      response_vars = "yield",
      treatment_vars = "intercrops;variety", 
      carob_contributor = "Cedric Ngakou", 
      carob_date = "2024-07-02"
   )
   
   ff <- ff[grep("\\.csv$", basename(ff))]
   
   process <- function(f){
      r <- read.csv(f)
      names(r) <- gsub("Ground.nut.pod.wt..kg.|Maize.Cob.wt..kg.|Pearl.millet.pod.wt..kg.|Sorghum.pod.wt..kg.", 
						"yld", names(r))
      names(r) <- gsub("Pigeon.pea.100.seed.weight..grams.|Pigeon.pea.100.kernll.weight..kg.", "ppseedw", names(r))
      names(r) <- gsub("Ground.nut..100.kernel.weight..gram.", "seedw", names(r))
      if (is.null(r$seedw)) r$seedw <- as.numeric(NA)

      d1 <- data.frame(
        adm1= r$District,
        location= r$Village,
        variety= r$Treatment,
        plot_area= r$Area,
        yield=r$Pigeon.pea.pod.wt..kg.,
		seed_weight=r$ppseedw,
        crop="pigeon pea",
        intercrops = substr(basename(f),start = 12,stop =16) 
      ) 
         
      d2 <- data.frame(
         adm1= r$District,
         location= r$Village,
         variety= r$Treatment,
         plot_area= r$Area,
         yield=r$yld, 
         seed_weight=r$seedw,
         crop=basename(f),
         intercrops= "pigeon pea"
      ) 
      
      rbind(d1, d2) |> na.omit()
   }
   
   d <- lapply(ff, process)
   d <- do.call(rbind, d)
   d$yield <- gsub(",", ".", d$yield)
   d$yield <- (as.numeric(d$yield)/d$plot_area)*10000 ## kg/ha
   ### remove all rows with no yield data
   d <- unique(d[!is.na(d$yield),]) 

	d$seed_weight <- as.numeric(gsub(",", ".", d$seed_weight))
   
   ### Add variables
   d$country <- "Tanzania"
   d$yield_part <- "pod"
   d$yield_part[d$crop=="maize"] <- "grain"
   d$on_farm <- TRUE
   d$is_survey <- FALSE
   d$irrigated <- NA
   d$inoculated <- FALSE
   d$trial_id <- "1"
   
   ### Fixing crop name 
   d$crop[grep("maize",d$crop)] <- "maize"
   d$crop[grep("sorghum",d$crop)] <- "sorghum"
   d$crop[grep("groundNut",d$crop)] <- "groundnut"
   d$crop[grep("pearlMillet",d$crop)] <- "pearl millet"
   
   ### Fixing intercrop
   d$intercrops[grep("groun",d$intercrops)] <- "groundnut"
   d$intercrops[grep("pearl",d$intercrops)] <- "pearl millet"
   d$intercrops[grep("sorgh",d$intercrops)] <- "sorghum"
   
   ### adding longitude and latitude 
   geo <- data.frame(location=c("Mlali", "Njoro", "Kiperesa", "Manyusi", "Laikala", "Moleti", "Igula"),
                     latitude=c(-6.9578539, -5.255432, -5.2439856, -5.5775449, -6.1920415, -6.1777796, -7.427173),
                     longitude=c(37.5400781, 36.4554004, 36.2927092, 36.2865828, 36.6081316, 36.813072, 35.8786794))
   
   d$geo_from_source <- FALSE
   d <- merge(d,geo, by="location",all.x = TRUE)
   
   d$planting_date <- "2019"
   
   d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
   
	d$yield_moisture <- as.numeric(NA) #needs to be checked

   carobiner::write_files(path, meta, d)
   
}


