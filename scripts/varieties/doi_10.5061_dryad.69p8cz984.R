# R script for "carob"
# license: GPL (>=3)

carob_script <- function(path) {
   
"Local adaptation and genotype by environment (G×E) interactions affect the expression of phenotypes in crop species. An investigation on the interplay between adaptation and G×E on sorghum heterosis phenotypes is lacking. To address this question, a set of 112 diverse grain sorghum hybrids and their 22 inbred parents of local and exotic origins, representing the primary female and male heterotic groups, were tested in five contrasting sorghum growing environments across two years in Queensland, Australia. Plant height, days to flowering, seed yield, grain weight, and grain number were measured and used in the estimation of heterosis. Mid parent heterosis for yield ranged from -25 to 217% and was highly influenced by grain number. In contrast to observations in maize the magnitude of heterosis for yield was not associated with site mean. Striking differences were observed in heterosis in hybrids from locally developed inbred parents compared with hybrids that were developed from exotic inbred parents developed in other countries. Heterosis in the latter combination was higher on average across all the test environments for the majority of traits. We hypothesise maladaptive phenotypic plasticity in the exotic parents contributed to the observed differences in heterosis estimates. These data confirm that heterosis estimates in sorghum must be obtained and interpreted in relevant genetic and environmental contexts. Breeders in developing countries with low sorghum hybrid seed uptake will find these insights useful when selecting hybrids for broader adaptation, improving efficiency of their breeding programs."
   
   uri <- "doi:10.5061/dryad.69p8cz984"
   group <- "varieties"
   ff <- carobiner::get_data(uri, path, group)
   
   meta <- carobiner::get_metadata(uri, path, group, major=5, minor=NA,
        data_organization = "UQL", # The University of Queensland
        publication="doi.org/10.1002/csc2.21160", 
        project=NA, 
        data_type= "experiment", 
        treatment_vars= "variety; variety_type", 
        response_vars = "yield", 
        carob_contributor= "Cedric Ngakou", 
        carob_date="2025-06-24",
        completion=100,
        notes=NA
   )
   
   
   f <- ff[basename(ff) == "Otwani_et_al__raw_data_.xlsx"]
   sheets <- c("Hermitage", "Biloela", "Dalby", "DalbyBox", "Springsure") 
   d <- lapply(sheets, function(i){
      r <- carobiner::read.excel(f, sheet = i, fix_names = TRUE)
      names(r) <- gsub("REP", "Rep", names(r))
      names(r) <- gsub("DTF", "DFL", names(r))
      names(r) <- gsub("Plot.yield.Kg.Ha|Yield.Kg.ha", "Yield.kg.ha", names(r))
      if(is.null(r$X1000.seed.weight)) r$X1000.seed.weight <- NA
      if(is.null(r$Rep)) r$Rep <- NA
      data.frame(
         planting_date= as.character(r$Year),
         location= r$Site,
         rep= as.integer(r$Rep),
         variety= r$Genotype,
         variety_type= tolower(r$Hybrid),
         plant_height= r$HGT,
         flowering_days= r$DFL, 
         yield= r$Yield.kg.ha, 
         seed_weight= r$X1000.seed.weight,
         trial_id= paste0(r$Site, "_", r$Year)
      )
   })
   
   d <- do.call(rbind, d)
   
   ## longitude/latitude
   geo <- data.frame(
      location= c("Hermitage", "Biloela", "Dalby", "Dalby Box", "Springsure"),
      latitude= c(-42.2496, -24.4022, -27.1795, -27.1732, -24.1154),
      longitude= c(146.8747, 150.5121, 151.2585, 151.2661, 148.0852)
   )
   d <- merge(d, geo, by="location", all.x = TRUE)
   
   d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
   d$row_spacing= 75
   d$crop= "sorghum"
   d$country= "Australia"
   d$adm1= "Queensland"
   d$geo_from_source= FALSE
   d$yield_part= "grain"
   d$on_farm = TRUE
   d$is_survey= FALSE
   d$irrigated= NA

   ## removing 3 rows with negative yield value 
   d <- d[which(d$yield != -1250), ]
   
   carobiner::write_files(path, meta, d)
}


