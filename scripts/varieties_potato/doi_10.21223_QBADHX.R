# R script for "carob"


carob_script <- function(path) {
   
"During the 2020-2021 season, 8 potato clones selected in the previous 2019-2020 season were evaluated, previously selected for their high levels of resistance to late blight and their excellent quality for french fries tested under high and low-temperature conditions (important conditions for the content of reducing sugars such as glucose and fructose, which cause the dark color in frying) were used together with two control varieties Canchan and Única, widely adopted by farmers and final consumers. The experiments were planted using tuber seeds from in vitro (basic) plants, in two locations in central Peru using the statistical design of randomized complete blocks with three replications of 150 plants each. The fertilization rate was 200-220-180 NPK per hectare, using as sources ammonium nitrate 33% N; di-ammonium phosphate 46% P2O5, 18% N; and potassium sulfate 50% K2O. Pest and disease control was carried out in a timely and adequate manner. In all experiments, late blight control was carried out on Canchan and Unica varieties planted as susceptible controls. Clone selection was planned to be carried out using the Participatory Varietal Selection (PVS) methodology, at flowering, harvest, and post-harvest stages. At harvest, the number and weight of marketable and unmarketable tubers per plot were recorded, then the tuber yield per hectare in t/ha was calculated, tuber samples were taken to determine the dry matter content using the hydrometer method and the dry weight/fresh weight, The tubers were also stored at room temperature (15-16oC) for frying after three months to see if they maintain their frying quality. The frying quality of the potato chips was evaluated based on the frying color, using the scale in grades from 1 to 5, developed by the Potato chip- Snack Food Association (www.sfa.org), the color grade of the selected clones should be 1 or 2. Three clones were selected as potential varieties with resistance to late blight, quality for french fries and/or baked. These clones were selected based on their high yield, good quality for frying, low content of reducing sugars, high content of dry matter, and information from the PVS methodology."
   
   uri <- "doi:10.21223/QBADHX"
   group <- "varieties_potato" 
   ff  <- carobiner::get_data(uri, path, group)
   
   meta <- carobiner::get_metadata(uri, path, group, major=1, minor=2, 
      data_organization ="CIP", 
      publication ="doi:10.1007/s11540-024-09697-1", 
      project =NA, 
      data_type = "experiment",
      response_vars = "yield;yield_marketable",
      treatment_vars = "variety", 
      carob_contributor = "Cedric Ngakou", 
      carob_effort = NA,
      carob_date = "2024-09-08"
   )
   
   ff <- ff[grepl("xlsx", basename(ff))]
   
   
    process <- function(f){
      
      # Read files
      r1 <- carobiner::read.excel(f, sheet = "Fieldbook") 
      r2 <- carobiner::read.excel(f, sheet = "Minimal") 
      rr <- data.frame(rbind(r2$Value))
      colnames(rr) <- r2$Factor
      # Processing data 
      data.frame(
         rep=  as.integer(r1$REP),
         variety= r1$INSTN,
         yield= r1$TTYNA*1000,
         yield_marketable = r1$MTYNA*1000,
         trial_id= ifelse(grepl("HCO", basename(f)), "1", "2"),
         longitude = as.numeric(rr$Longitude),
         latitude= as.numeric(rr$Latitude),
         elevation= as.numeric(rr$Elevation),
         planting_date= as.character(as.Date(rr$Begin_date, "%d/%m/%Y")),
         harvest_date= as.character(as.Date(rr$End_date, "%d/%m/%Y")),
         country= rr$Country,
         adm1= rr$Admin1,
         adm2= rr$Admin2,
         adm3= rr$Admin3,
         geo_from_source= TRUE
      )
   }
   
   d <- lapply(ff, process)  
   d <- do.call(rbind, d)
   
   d$crop <- "potato"
   d$irrigated <- NA
   d$is_survey <- FALSE
   d$inoculated <- FALSE
   d$on_farm <- TRUE
   d$yield_part <- "tubers"
   
   # From data description
   d$N_fertilizer <- 200
   d$P_fertilizer <- 220/2.29
   d$K_fertilizer <- 180/1.2051
   
   d$plot_area <- 40.5
   d$plant_density <- 37037.037
   d$row_spacing <- 90
   d$plant_spacing <- 30
   
   
   carobiner::write_files (path, meta, d)
}


