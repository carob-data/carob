# R script for "carob"

carob_script <- function(path) {
  
"Increasing organic matter/carbon contents of soils is one option from a basket of strategies being proposed to offset climate change inducing greenhouse gas (GHG) emissions, under the auspices of the Paris-COP 4 per mille initiative. One of the complimentary practices to sequester carbon in soils on decadal timescales is amending it with biochar, a carbon rich byproduct of biomass gasification. In sub-Saharan Africa (SSA) there is widespread and close interplay of agrarian based economies and the use of biomass for fuel, which makes that the co-benefits of biochar production for agriculture and energy supply are explicitly different from the rest of the world. To date, the quantities of residues available from staple crops for biochar production, and their potential for carbon sequestration in farming systems of SSA have not been comprehensively investigated. Herein we assessed the productivity and usage of biomass waste from: maize, sorghum, rice, millet and groundnut crops; specifically quantifying straw, shanks, chaff and shells, based on measurements from multiple farmer fields and census/surveys in eastern Uganda. Moreover, allometric models, using grain productivity, plant height and density, as input variables, were tested. These models enable rapid and low-cost assessment of the potential availability of feedstocks at both site-specific, farmer field and/or regional scales. Ultimately we modelled the carbon balance in soils of major cropping systems receiving a ‘circular’ amendment of biochar from residues, and up-scaled this for basic scenario analysis. This interdisciplinary approach has wielded a framework for country-wide assessment of the biophysical potential of crop biomass wastes for soil C sequestration through scaling of biochar technologies, and to determine its co-benefits for agriculture and energy production. In doing so, we identified engineering synergies that could substantially contribute to a number of the Sustainable Development Goals."
  

# NOTES  
#This study assessed the productivity of 4 crops maize, sorghum, rice, millet, and groundnut crops, This script is only for maize, the other crops where published under different doi's: millet(doi:10.25502/edk6-ac73/d), groundnuts(doi:10.25502/eerp-3f45/d),rice(doi:10.25502/cne2-h823/d),sorghum(doi:10.25502/fbgw-1m42/d)  

  
  uri <- "doi:10.25502/z5dp-be17/d"
  group <- "survey"
  
  ff  <- carobiner::get_data(uri, path, group)
  
  meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
     data_organization = "IITA",
     publication = "doi.org/10.1002/eap.1984",
     project = NA,
     data_type = "survey",
     treatment_vars = "none",
     response_vars = "yield",  
     carob_completion = 100,
     carob_contributor = "Mitchelle Njukuya",
     carob_date = "2026-06-08",
     carob_effort = NA,
     notes = NA, 
     design = NA
  )
  
  f <- ff[basename(ff) == "maize_biomass_sampling.csv"]
  r <- read.csv(f)
  
  d <- data.frame(
    country = "Uganda",
    crop= "maize", 
    field_id = r$Plot_ID,
    field_size = r$Plot_area_ha,
    plant_density = r$Density_plant_m.2 * 10000,
    plant_height = r$Avg_Hgt_in_quadrant_m * 100,
    yield = r$Yield_grain_dry_ton_ha.1 * 1000,
    yield_part = "grain",
    dmy_residue = r$Yield_straw_dry_ton_ha.1 * 1000,
    location = "Lake Kyoga Basin"
  )
 
  # field size is only reported once  
  u <- na.omit(d[, c("field_id", "field_size")])
  i <- match(d$field_id, u$field_id)
  d$field_size <- u$field_size[i]
 
  d$trial_id <- d$plot_ID
  
  d$on_farm <- TRUE
    d$is_survey <- TRUE
    d$geo_from_source <- FALSE
    d$longitude <- 33.9333
    d$latitude <- 0.9167
    d$elevation <- 1065
  
  d$yield_moisture <- as.numeric(NA)
  d$yield_isfresh <- TRUE # assumption for survey data

	d$planting_date <- as.character(NA)
	d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- NA_real_
	d$irrigated <- NA
  
  carobiner::write_files(path, meta, d)
}


