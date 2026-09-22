carob_script <- function(path) {
  
  "
Dataset for: Parental value for tuber yield in potato clones with resistance to late blight and heat tolerance, under high temperatures conditions (SET 2)

The International Potato Center (CIP) has developed a population named LBHT, obtaining new clones that are more tolerant to high temperatures, resistant to late blight, virus and early maturity, and they are available for use by developing countries for the selection of varieties or as parents in their breeding programs. The potato is highly heterozygozus crop, the most important economical characters are governed by additive and non-aditive genes. Therefore, the parental value of a clone cannot be assessed from phenotypic value alone. It is very important to know its combining ability as an indicator of parental value. Combining ability analysis is a method very useful for choosing better parents, and in the formulation of a crossing plan for a plant breeding program. During 2012 and 2013, 6 advanced clones of LBHT population, were crossed using line x tester mating design, where the clones were used as female parents (lines), and as male parents (testers). The varieties, Katahdin (Solanum tuberosum spp tuberosum), Huagalina (Solanum tuberosum spp andigena) and clone CIP302533.31 were used in this experiment. Furthermore, during 2013 - 2014, 18 progenies were evaluated under field conditions in three locations in Peru: La Molina, San Ramon and Majes. All experiments were conducted using statistical design of randomized complete block with three replications of 40 plants per plot. At harvest, the number and weight of marketable and non- marketable tubers were recorded. Then, the average weight of tubers, weight of marketable and total tubers per hectare in tons were calculated. The analysis of variance for line by tester mating desing and determination of the effects of general combining ability (GCA) for these characters were performed using statistical software, SAS V. 9.4 (SAS Institute Inc., Cary, NC, USA).
"
  
  uri <- "doi:10.21223/P3/KZWGT8"
  group <- "varieties_potato"
  
  ff <- carobiner::get_data(uri, path, group)
  
  meta <- carobiner::get_metadata(uri, path, group, major = 2, minor = 0,
    data_organization = "CIP",
    publication = NA,
    project = NA,
    design = "RCBD",
    data_type = "experiment",
    treatment_vars = "variety",
    response_vars = "yield;yield_marketable",
    notes = NA,
    carob_contributor = "Maryam Yahya",
    carob_date = "2026-09-22",
    carob_completion = 100,
    carob_effort = 2
  )
  
  ## Source files (processed)
  f1 <- ff[basename(ff) == "01_PTYield072013_CIPSRM_exp3_processed.xlsx"]
  f2 <- ff[basename(ff) == "02_PTYield112013_CIPHQ_exp3_processed.xlsx"]
  f3 <- ff[basename(ff) == "03_PTYield122013_MAJ_exp3_processed.xlsx"]
  
  r1 <- carobiner::read.excel(f1)
  r2 <- carobiner::read.excel(f2)
  r3 <- carobiner::read.excel(f3)
  
  ## Add location
  r1$location <- "San Ramon"
  r2$location <- "La Molina"
  r3$location <- "Majes"
  
  ## Combine
  r <- carobiner::bindr(r1, r2, r3)
  
  ## Create final data.frame
  d <- data.frame(
    trial_id = paste0("KZWGT8_", r$location),
    plot_id = as.character(r$plot),
    rep = as.integer(r$rep),
    variety = r$variety,
    location = r$location,
    country = r$country,
    adm1 = r$admin1,
    adm2 = r$admin2,
    adm3 = r$admin3,
    elevation = as.numeric(r$elevation),
    latitude = as.numeric(r$latitude),
    longitude = as.numeric(r$longitude),
    geo_from_source = TRUE,
    planting_date = as.character(as.Date(r$planting_date)),
    harvest_date = as.character(as.Date(r$harvest_date)),
    maturity_date = as.character(as.Date(r$maturity_date)),
    crop = "potato",
    on_farm = TRUE,
    is_survey = FALSE,
    irrigated = r$irrigated,
    soil_texture = tolower(r$soil_texture),
    yield_part = "tubers",
    yield = r$yield_fresh * 1000,
    yield_marketable = r$mtyna * 1000,
    yield_moisture = NA,
    yield_isfresh = TRUE,
    N_fertilizer = r$n_fertilizer,
    P_fertilizer = r$p_fertilizer,
    K_fertilizer = r$k_fertilizer
  )
  
  carobiner::write_files(path, meta, d)
}