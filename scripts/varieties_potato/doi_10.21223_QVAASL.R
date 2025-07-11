# R script for "carob"


carob_script <- function(path) {
  
"Ten potato clones of population B, groups B3C1 and B3C2, previously selected for their high levels of resistance to late blight and their excellent quality for french fries tested under high and low-temperature conditions (important conditions for the content of reducing sugars such as glucose and fructose, which cause the dark color in frying) were used together with two control varieties Canchan and Única, widely adopted by farmers and final consumers. The experiments were planted using tuber seeds from in vitro (basic) plants, in 2 locations in the north of Peru using the statistical design of randomized complete blocks with three replications of 150 plants each. The fertilization rate was 200-220-180 NPK per hectare, using as sources ammonium nitrate 33% N; di-ammonium phosphate 46% P2O5, 18% N; and potassium sulfate 50% K2O. Pest and disease control was carried out in a timely and adequate manner. In all experiments, late blight control was carried out on Canchan and Unica varieties planted as susceptible controls. Clone selection was planned to be carried out using the Participatory Varietal Selection (PVS) methodology, at flowering, harvest, and post-harvest stages. At harvest, the number and weight of marketable and unmarketable tubers per plot were recorded, then the tuber yield per hectare in t/ha was calculated, tuber samples were taken to determine the dry matter content using the hydrometer method and the dry weight/fresh weight, The tubers were also stored at room temperature (15-16oC) for frying after three months to see if they maintain their frying quality. The frying quality of the potato chips was evaluated based on the frying color, using the scale in grades from 1 to 5, developed by the Potato chip- \"Snack Food Association\" (www.sfa.org), the color grade of the selected clones should be 1 or 2. Once the organoleptic evaluations were concluded, eight clones were selected, which will be planted in new experiments in the 2020-2021 season to meet the requirements of the Peruvian law to release and register a new potato variety. The clones were selected based on their high yield, good frying quality, low reducing sugar content, high dry matter content and PVS methodology information. Due to the COVI-19 pandemic, the PVS methodology could not be carried out as planned."
  
  uri <- "doi:10.21223/QVAASL"
  group = "varieties_potato"
  ff  <- carobiner::get_data(uri, path, group)
  
  meta <- carobiner::get_metadata(uri, path, group, major=1, minor=3,
      data_organization = "CIP",
      publication = NA,
      project = NA,
      data_type = "experiment",
      treatment_vars = "variety",
      response_vars = "yield;yield_marketable", 
      carob_contributor = "Henry Juarez",
      carob_date = "2024-09-13",
      notes = NA
  )
  
  process <- carobiner::get_function("process_cip_lbvars", path, group)
  
  f <- ff[grep("_installation", basename(ff))]
  d <- lapply(f, process)
  d <- do.call(rbind, d)
  
  carobiner::write_files(path = path, metadata = meta, wide=d)
  
}

