# R script for "carob"
# license: GPL (>=3)

carob_script <- function(path) {
   
"
Sustainable landscape, soil and crop management practices enhance biodiversity and yield in conventional cereal systems

1. Input-driven, modern agriculture is commonly associated with large-scale threats to biodiversity, the disruption of ecosystem services and long-term risks to food security and human health. A switch to more sustainable yet highly productive farming practices seems unavoidable. However, an integrative evaluation of targeted management schemes at field and landscape scales is currently lacking. Furthermore, the often-disproportionate influence of soil conditions and agrochemicals on yields may mask the benefits of biodiversity-driven ecosystem services.

2. Here, we used a real-world ecosystem approach to identify sustainable management practices for enhanced functional biodiversity and yield on 28 temperate wheat fields. Using path analysis, we assessed direct and indirect links between soil, crop and landscape management with natural enemies and pests, as well as follow-on effects on yield quantity and quality. A paired-field design with a crossed insecticide-fertilizer experiment allowed us to control for the relative influence of soil characteristics and agrochemical inputs.

3. We demonstrate that biodiversity-enhancing management options such as reduced tillage, crop rotation diversity and small field size can enhance natural enemies without relying on agrochemical inputs. Similarly, we show that in this system controlling pests and weeds by agrochemical means is less relevant than expected for final crop productivity.

4. Synthesis and applications: Our study highlights soil, crop and landscape management practices that can enhance beneficial biodiversity while reducing agrochemical usage and negative environmental impacts of conventional agriculture. The diversification of cropping systems and conservation tillage are practical measures most farmers can implement without productivity losses. Combining local measures with improved landscape management may also strengthen the sustainability and resilience of cropping systems in light of future global change.
"
   
   uri <- "doi:10.5061/dryad.bk3j9kd9m"
   group <- "agronomy"
   ff <- carobiner::get_data(uri, path, group)
   
   meta <- carobiner::get_metadata(uri, path, group, major=4, minor=NA,
          #"University of Würzburg; Leibniz University Hannover",
         data_organization = "JMU; LUH",
         publication="doi:10.1111/1365-2664.13821", 
         project=NA, 
         data_type= "experiment", 
         treatment_vars= "N_fertilizer; variety; land_prep_method", 
         response_vars = "yield; fwy_total", 
         carob_contributor= "Cedric Ngakou", 
         carob_date="2025-07-03",
         completion=100,
         design = "Field data was collected in 2014 during repeated sampling campaigns. Additional data was acquired from soil maps, GIS data bases and farm management surveys. Please refer to main paper for details.",
         notes=NA
   )
   
   f <- ff[basename(ff) == "Data_Redlich_et_al.xlsx"]
   
   r <- carobiner::read.excel(f, sheet="Data")
   
   d <- data.frame(
      trial_id= r$Field,
      plot_id= r$Plot,
      yield= r$Yield*1000, ## kg/ha
      fwy_total= r$Biomass_plant*1000, ## kg/ha 
      soil_SOC= r$SOC_soil,
      soil_type= r$Topsoil_type,
      soil_pH= r$Soil_pH,
      insecticide_used= ifelse(grepl("Yes", r$Insecticide), TRUE, FALSE),
      insecticide_product= "beta-cyfluthrin",
      land_prep_method= ifelse(grepl("plough", r$Soil_prep), "ploughing", 
                         ifelse(grepl("harrow", r$Soil_prep),"tillage", "tillage")),
      planting_date= as.character(as.Date(r$Sowing_date, origin = "1899-12-31")),
      plot_area= 12*14/10000, ## ha 
      seed_rate= r$Seeding_rate,
      previous_crop= gsub("winter_|_", "", tolower(r$Previous_crop)),
      variety= r$Cultivar,
      N_fertilizer= r$Field_Nfertilization,
      P_fertilizer= 0 ,
      K_fertilizer= 0 ,
      pest_species= "rust",
      pest_number= as.integer(r$Rust_sum),
      crop= "wheat",
      country= "Germany",
      location="Würzburg",
      longitude= 9.95 ,
      latitude= 49.78, ## from paper
      geo_from_source= TRUE,
      on_farm= TRUE,
      is_survey= FALSE ,
      yield_part= "grain",
      irrigated= NA
      
   )
   
   d$previous_crop <- gsub("sugarbeet", "sugar beet", d$previous_crop)
   d$previous_crop <- gsub("osr", "rapeseed", d$previous_crop)
   
	d$yield_moisture <- as.numeric(NA) #needs to be checked

   carobiner::write_files(path, meta, d)
}

