
carob_script <- function(path) {
  
  "
  Dataset for: Parental value for tuber yield in potato clones with resistance to late blight and heat tolerance, under high temperatures conditions (SET 3)

  The International Potato Center (CIP) has developed a population named LBHT, obtaining new clones that are more tolerant to high temperatures, resistant to late blight, virus and early maturity, and they are available for use by developing countries for the selection of varieties or as parents in their breeding programs. The potato is highly heterozygozus crop, the most important economical characters are governed by additive and non-aditive genes. Therefore, the parental value of a clone cannot be assessed from phenotypic value alone. It is very important to know its combining ability as an indicator of parental value. Combining ability analysis is a method very useful for choosing better parents, and in the formulation of a crossing plan for a plant breeding program. During 2013 and 2014, 9 advanced clones of LBHT population, were crossed using line x tester mating design, where the clones were used as female parents (lines) and as male parents (testers). The varieties, Katahdin (Solanum tuberosum spp tuberosum), Huagalina (Solanum tuberosum spp andigena) and clone CIP398098.204 were used. Therefore, during 2015 - 2016, 27 progenies were evaluated under field conditions in two locations in Peru: La Molina and Majes. All experiments were conducted using statistical design of randomized complete block with three replications of 40 plants per plot. At harvest, the number and weight of marketable and non- marketable tubers were recorded. Then, the average weight of tubers, weight of marketable and total tubers per hectare in tons were calculated The analysis of variance for line by tester mating desing and determination of the effects of general combining ability (GCA) for these characters were performed using statistical software, SAS V. 9.4 (SAS Institute Inc., Cary, NC, USA).
  "
  
  uri <- "doi:10.21223/P3/2WCUAE"
  group <- "varieties_potato"
  
  ff <- carobiner::get_data(uri, path, group)
  
  meta <- carobiner::get_metadata(
    uri, path, group,
    major = 2, minor = 0,
    data_organization = "CIP",
    publication = NA,
    project = NA,
    design = "RCBD",
    data_type = "experiment",
    treatment_vars = "variety",
    response_vars = "yield",
    notes = NA,
    carob_contributor = "Maryam Yahya",
    carob_date = "2026-09-21",
    carob_completion = 80,
    carob_effort = 3.5
  )
  
  ## Source files
  f1 <- ff[basename(ff) == "01_PTYield122015_CIPHQ_exp1_processed.xlsx"]
  f2 <- ff[basename(ff) == "02_PTYield122015_MAJ_exp1_processed.xlsx"]
  
  ## Read source data
  r1 <- carobiner::read.excel(f1)
  r2 <- carobiner::read.excel(f2)
  
  ## Add location
  r1$location <- "La Molina"
  r2$location <- "Majes"
  
  ## Combine
  r <- carobiner::bindr(r1, r2)
  
  ## Coordinates estimated from Google Maps
  coords <- data.frame(
    location = c("La Molina", "Majes"),
    latitude = c(-12.076289, -16.49306),
    longitude = c(-76.948417, -72.18889)
  )
  
  ## Create final data.frame
  d <- data.frame(
    trial_id = r$location,
    plot_id = as.character(r$plot),
    rep = as.integer(r$rep),
    variety = r$variety,
    location = r$location,
    country = "Peru",
    adm1 = r$admin1,
    adm2 = r$admin2,
    adm3 = r$admin3,
    crop = "potato",
    on_farm = TRUE,
    is_survey = FALSE,
    irrigated = TRUE,
    soil_texture = tolower(r$soil_texture),
    elevation = as.numeric(r$elevation),
    yield_part = "tubers",
    yield = r$yield_fresh * 1000,
    yield_marketable = r$mtyna * 1000,
    yield_moisture = NA,
    yield_isfresh = TRUE,
    geo_from_source = FALSE,
    planting_date = "2015",
    harvest_date = "2016",
    N_fertilizer = r$n_fertilizer,
    P_fertilizer = r$p_fertilizer,
    K_fertilizer = r$k_fertilizer
  )
  
  ## Merge coordinates
  d <- merge(d, coords, by = "location", all.x = TRUE)
  
  ## Remove rows with missing yield
  d <- d[!is.na(d$yield), ]
  
  ## Write CAROB files
  carobiner::write_files(path, meta, d)
}