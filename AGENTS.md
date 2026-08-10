# Instructions for an AI agent: writing Carob data-processing scripts

This document tells an AI coding agent how to turn a published dataset into a Carob processing script, following the conventions used in this repository. Read it fully before starting.

**Also read these two official Carob pages first:**
- Guidelines: <https://carob-data.org/contribute/guidelines.html> — the authoritative rules ("Before you start", Scripts, Standardization, R coding style).
- Example script: <https://carob-data.org/contribute/example.html> — a full worked example (doi:10.21421/D2/STACVA) walking through metadata, reading a sheet, and building `d`.

This document supplements those pages with practical, agent-specific detail. If they ever disagree, follow the official guidelines.

**Before writing, also read a few existing scripts in `scripts/<group>/`** — especially the same group you are targeting — to see the conventions applied to real datasets (e.g. recent files in `scripts/varieties/` such as `doi_10.7910_DVN_SMGA6L.R`, or any script in `scripts/agronomy/` or `scripts/survey/`). They are the best templates for structure, naming, units, and how metadata and geo/housekeeping variables are set.

Do NOT guess silently. If you get stuck on *what* a value should be (not *how* to code it), leave a clear`# comment` in the code, and add a line to the `## NOTE:` or `## ISSUES` block at the top of the scripts. It is better not to process a variable (and leave a comment about it) than to introduce errors.

It is of fundamental importance that no critical information is lost to analyze the data as intended. Resist simplifying the data to the point where relevant information is lost

**You stay in charge.** Using AI to help is fine, but you are responsible for the result — do not blindly trust generated code, mappings, or `## NOTES`/`## ISSUES`. Verify every claim against the actual data and the publication before you leave it in the script. A wrong note or a plausible-but-incorrect mapping is worse than an honest "I could not determine this" (in `## ISSUES`).

---

## 1. What a Carob script is

Each script is a single R file that defines one function:

```r
carob_script <- function(path) {
    ...
    carobiner::write_files(path, meta, d)
}
```

It downloads one dataset, reshapes it into a **tidy `data.frame` `d`** whose column names and units follow the **terminag** vocabulary, attaches **metadata** (`meta`), and writes standardized output with `carobiner::write_files(path, meta, d)`. There may be an additional data.frame with "long" records for example to record multiple observations in time for each experimental plot.

- One dataset = one script = (ideally) one pull request.
- The file name is the dataset id: `yuri::simpleURI(uri)` with slashes replaced by underscores, e.g. `doi:10.7910/DVN/SMGA6L` → `doi_10.7910_DVN_SMGA6L.R`.
- The script lives in `scripts/<group>/` where `<group>` is the thematic group (`agronomy`, `varieties`, `survey`, `soil_samples`, `pest_disease`, ...).

Use `scripts/_template.R` as the canonical skeleton and any recent file in `scripts/varieties/` (e.g. `doi_10.7910_DVN_SMGA6L.R`) as a concrete example.

`path` passed to `carob_script()` is the carob repo working dir, e.g. `"C:/github/carob/carob"`. Downloads land in `data/raw/<group>/<dataset_id>/`.

---

## 2. vocabulary

- The **vocabulary** (which variable names and which categorical values are allowed) lives in **terminag**.

### Where to find the terminag files

Do **not** assume terminag is a git clone sitting next to the `carob` repo. That is only true in a maintainer's development setup; it is **not** the general case.

`carobiner` reads the vocabulary through the **`vocal`** package. By default the vocabulary source is the GitHub repo, addressed as the string **`"github:controvoc/terminag"`** (see `carobiner::carob_vocabulary()`). `vocal` downloads a **snapshot** of that repo (via the GitHub API) and stores it in a per-user cache, then refreshes it when the upstream `sha` changes. So on a typical machine the terminag files are here, not in a project folder:

```
<rappdirs::user_data_dir()>/.vocal/controvoc/terminag/
    variables/variables_<group>.csv   # variable names, types, valid_min/valid_max, vocabulary key
    values/values_<name>.csv          # accepted categorical values (crops, countries, ...)
    sha.txt                           # cached upstream commit id
```

`rappdirs::user_data_dir()` is platform-specific, e.g.:
- Windows: `C:\Users\<you>\AppData\Local\.vocal\controvoc\terminag`
- macOS:   `~/Library/Application Support/.vocal/controvoc/terminag`
- Linux:   `~/.local/share/.vocal/controvoc/terminag`

To locate/inspect them portably, resolve the path in R rather than hard-coding it:

```r
file.path(rappdirs::user_data_dir(), ".vocal", "controvoc", "terminag")
```

**For an agent, reading the cached CSVs directly is usually the most efficient** way to inspect the vocabulary (with your normal file-read/grep tools) — no R session needed. Just make sure you read the *active cache* at the path above, not a random clone. The `vocal` API reads the very same files, so use it only when you are already in R (e.g. to double-check the resolved values):

```r
vocal::accepted_variables(include = "varieties")  # allowed variable names (optionally by group)
vocal::accepted_values("crop")                    # allowed values for a categorical variable
carobiner::carob_vocabulary()                     # the current vocabulary source string
```

Whichever you use, be aware the cache is only refreshed by `vocal` (on the GitHub `sha`) — if in doubt about freshness, compare against GitHub. You can also browse the same files on GitHub: <https://github.com/controvoc/terminag>. The carob-side pointer to the chosen vocabulary is cached in `<rappdirs::user_data_dir()>/.carob/voc`.

- **Which variables are *required*** (and which may not be `NA`) is **not** in the vocabulary; it lives in the R package "carobiner" `terms/required_variables.csv`. This is what `carobiner::write_files()` checks.

---

## 3. Workflow

0. **Study a few finished scripts** in `scripts/<group>/` (ideally the target group) to learn the expected structure and conventions before you begin.
1. **bootstrap** with `carobiner::draft(uri, path, group)`. This downloads the data and writes a starter into `scripts/_draft/<group>/`.
2. **Inspect the raw data**: list `ff`, read each relevant sheet/file, and look at column names, codebooks, and the dataset's description/abstract. Codebooks (often extra `.csv`/`.xlsx`/`.pdf`) tell you units and category codes. 
3. **Associated publication** If no associated publication (paper) is reported in the metadata, do a search to see if there is an probably match. Note that you found it in a comment. Do a careful crosscheck to see if this is indeed reporting the data at hand.
4. **Read the associated publication's Methods section** (Section 8) for location, management, design, and unit information not in the data files. Note if you cannot do that, for example because the publication is behind a paywall. 
5. **Map columns** to terminag names and correct units (Section 5–6).
6. **Fill metadata** (Section 4).
7. **Build `d`**, then set the required "housekeeping" variables (trial_id, geo, on_farm/is_survey, yield_part, ...).
8. **Test** in a clean session and resolve every `write_files()` message (Section 9).
9. **Place** the finished file in `scripts/<group>/` (Section 10).

---

## 4. Metadata: `carobiner::get_metadata(...)`

```r
meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
    data_organization = "PURDUE",     # data provider and/or author institutes
    publication = NA,                 # paper DOI if any (add matching RIS in references/); use NA, never ""
    project = NA,
    design = NA,                      # experimental/survey design if known
    data_type = "on-farm experiment", # "experiment", "on-farm experiment", "survey", "compilation", ...
    treatment_vars = "variety",       # ";"-separated variables that ARE the treatments
    response_vars = "yield;plant_height;maturity_days", # ";"-separated measured responses
    carob_contributor = "Author Name",
    carob_LLM = "LLM & version",
    carob_date = "2026-07-17",        # date first written (YYYY-MM-DD)
    carob_completion = 80,            # % of relevant variables standardized (0-100)
    carob_effort = 0.1               # hours spent
)
```

Rules and gotchas:

- Prefer `NA` (not `""`) for absent `publication`, `project`, `design`, etc. An empty string triggers warnings from `write_files()`.
- `treatment_vars` must be actual column names present in `d`, and each must have
  >1 non-missing value (there must be variation). `response_vars` are the measured
  outcomes of interest — **not** management variables applied to all plots.
- Do **not** confuse treatments and responses. A treatment is something the experimenters *chose/applied* and varied (variety, N rate, orientation, spacing); a response is something they *measured* as an outcome (yield, plant_height, disease). A management level applied to every plot (e.g. a blanket fertilizer dose) is neither.
- `data_type` "survey" (and the `survey`/`soil_samples` groups) relax some crop/agronomy requirements — see the required-variables logic below.
- **Get `data_type`, `group`, and `design` right.** Do not label observational/survey data as an "experiment", and do not invent a `design` (e.g. "RCBD") that the source does not state — leave `design = NA` if unknown. Put the dataset in the correct group (a variety trial is `varieties`, not `agronomy`).
- Copy the dataset **title and abstract** verbatim into the quoted string near the top of the function (see the template) so reviewers have context.
- Do _not_ guess things if they are not reported in the metadata 
- Do *not* fill in the name of the "carob_contributor". `carob_contributor` must be a **real person** (the human responsible for the script) — never an AI/LLM, a placeholder, or a GitHub handle. Leave it for that person to fill in; record the model you used under `carob_LLM` instead.
- Use _your_ model name and version as "carob_LLM"
- Estimate carob_completion (% of variables in the raw data that have been processed) 
- Estimate carob_effort based on your actual time spent (typically a fraction of an hour)

---

## 5. Building `d`: the tidy data.frame


### Reading

- Read each **input** file into **`r`**. If a script reads more than one file (or sheet), name them **`r1`, `r2`, ...** (or descriptive names like `rA`, `rD`). 
- Do not change the values in `r` 
- When missing value "flags" are used, consider `read.csv(f, na.strings="nd")` and `carobiner::read.excel(f, na=  )` to automatically transform them. 
- Consider argument `skip` to skip empty rows
- Consider `carobiner::read.excel.hdr(f, )` for excel files that have multiple header rows.


### Naming and style convention

- Transform each into an **output** `data.frame` named **`d`**, or **`d1`, `d2`, ...** when there are several, then combine as needed.
- Do the mapping **as directly as possible inside the `data.frame(...)` call** — one line per variable, using a plain column reference or a simple in-line transformation (e.g. a unit multiplication, `tolower()`, `as.numeric()`):

  ```r
  d <- data.frame(
      crop = r$Crop,
      longitude = r$lon,
      yield = r$yield_tha * 1000       # t/ha -> kg/ha
  )
  ```

- **Avoid** re-opening `d` afterwards to assign columns that could have been mapped directly. That is, do **not** write later:

  ```r
  d$country <- r$Country   # avoid: this belongs inside the data.frame() above
  ```

  Keep the direct `r$... -> d$...` mappings together in the `data.frame()` call. Reserve later `d$<name> <- ...` assignments for values that genuinely cannot go there: constants set for the whole table, "housekeeping" variables, values derived from several columns or from another `data.frame`, or post-hoc cleanup.

Assign source columns to terminag names, coercing types and units as you go:

```r
f <- ff[basename(ff) == "B lines observation nursery at Mieso 2014.xlsx"]
r <- carobiner::read.excel(f, na="NA")   # or read.csv(f) / haven::read_dta(f)

d <- data.frame(
    crop = "sorghum",
    variety = r$Genotype,
    treatment = r$Genotype,
    yield = r$`YieldKg/Ha`,                 # kg/ha
    plant_height = r$PHTMean,
    maturity_days = r$DTM,
    plant_density = 10000 * as.numeric(r$StandAtHarv) / as.numeric(r$PlotArea),
    country = "Ethiopia",
    location = r$Site
)
```

Then set the "housekeeping" variables that most scripts need:

```r
## one trial = one location x season (NOT one per treatment/replicate).
## For a survey, each row/household gets a unique trial_id.
d$trial_id <- as.character(as.integer(as.factor(paste(d$location, year))))

d$on_farm  <- TRUE   # or FALSE / NA
d$is_survey <- FALSE
d$irrigated <- NA

## geography (see Section 7)
d$longitude <- 40.5638
d$latitude  <- 9.1779
d$geo_from_source <- FALSE            # TRUE if coords came from the data/paper

## crop production
d$yield_part <- "grain"               # what the yield refers to
d$yield_moisture <- as.numeric(NA)    # % moisture if known
```

General rules:

- **variables** all variables should be processed unless they are redundant (used to compute a variable of interest, or derived thereof) or cannot be interpreted. Write a comment for each variable that is not processed.
- **treatment variables** it is imperative that all treatment variables are included as individual variables and that they are interpretable. It is _not_ sufficient to only have it as part of a treatment code (in variable "treatment"). This holds **even when terminag has no matching variable name**: give the treatment its own clear new variable name (ending in an underscore, see "New variable names" below) rather than burying it only in `treatment`. Such a new variable triggers a warning and is dropped from the written output, but that is fine — naming it explicitly is what lets a maintainer add the term to terminag (a separate process). A missing vocabulary term is never a reason to hide a treatment inside the `treatment` code.
- **Variable (Column) names** should match a variable name from terminag. 
- **New variable names** where there is not matching name in terminag; propose an appropriate new variable name, that ends in an underscore (e.g. `annual_income_`). List these new variables at the top of the script under ## NEW VARIABLES and describe what they represent, and what their unit is (do not add unit to the variable name). New variables cause a warning and are dropped but that is _not_ a concern. Do not change terminag, that is a separate process.
- **Categorical values, and units must match terminag.** Check `variables_*.csv` (names, `valid_min`/`valid_max`) and `values_*.csv` (accepted category values, e.g. crop names, country names).
- **Coerce explicitly.** `read.excel`/`read.csv` may read a column as character; wrap numeric math in `as.numeric(...)` (e.g. density calculations) and integers in `as.integer(...)`. This avoids "bad datatype" warnings.
- **Normalize names**: `carobiner::fix_name(x, "title")` for admin/location names; `trimws()` to remove stray whitespace (untrimmed values are flagged).
- **`crop`** and other controlled values must be lowercase accepted terms (`tolower(...)` where appropriate). Intercrops use an underscore: `"maize_bean"`. In the rare case of multiple crop_rotations, these can be separated with a `#`
- **Intercrops need care.** In an intercropping trial the `crop` is not simply e.g. `"cassava"`; it is `"cassava"` (sole) vs `"cassava_sweetpotato"` (intercropped), etc. When yield is reported per component crop, split an intercrop plot into one row per crop. Let the **treatment name** guide you (a sole-crop treatment is a sole crop even if another column looks contradictory — note the discrepancy in `## ISSUES`).
- **Measured data only.** Include observed/measured values, not values that were *simulated* or *modeled*. If a column is a model output (e.g. simulated emissions), drop it and note why. For measurements, record what a value represents (an instantaneous reading vs an average, and over what period) in a `#` comment.
- **Keep it simple and readable.** Do not read files you do not use, do not create `d1`/`d2` when a single `d` suffices, and remove unused objects and commented-out code before submitting. Prefer the simplest computation that is correct (reviewers repeatedly ask for this, e.g. for fertilizer math).
- **Do not use packages** Do not use other R packages than carobiner
- **Use `pkg::fun()`, never `library()`/`require()`.** 
- **One operation per line** where at all reasonable (building a `data.frame(...)` is the exception). Avoid both excessive line-splitting and cramming unrelated steps onto one line.

### Repeated measurements and extra record types → long format

- If a variable is **measured several times** (over time: weekly shoot counts, plant height at 6/8/12 weeks, repeated fertilizer applications, ...), do **not** pick the last value or drop the time information. Keep every observation in a **long** table passed as `carobiner::write_files(path, meta, d, long=dlong)`.
- The long table needs a key back to the wide records (`record_id`, `plot_id`, `trial_id`, or `hhid`) **and** a variable that makes each long row unique — a time variable (`date`, or `DAP`/`DAE` = days after planting/emergence) or an application/measurement `order`/`rep`. Without one you cannot reshape the long data back to wide.
- A partly-wide long table (one row per record × time/order, with `N_fertilizer`, `P_fertilizer`, ... as its own columns) is usually easier to read than a pure `variable`/`value` long table; either is acceptable.
- **Weather** data go in their own table via `write_files(..., wth = weather)`. Do **not** aggregate weather into the wide records.
- **Do not aggregate** (e.g. to one row per plot/household) to reduce `NA`s or simplify the shape. Aggregation discards repeated observations (e.g. LAI or biomass measured on several dates, or per-subplot yields); keep them, in the long table if needed.
- Do not force several record types into one wide row when that loses data (see Section 9).

---

## 6. Units and common conversions

- **Yield**: kg/ha, as **fresh weight** of `yield_part`. Convert t/ha → kg/ha (`* 1000`). Set `yield_moisture` (%) when known; if all yields are dry or moisture is unknown, consider `yield_isfresh`.
- **Yield from production and area**: if `yield` is not reported directly but production and the plot/field **area** are, compute `yield = production / area` (kg/ha).
- **Area**: hectares. 1 acre = 0.4046863 ha.
- **Fertilizer**: report `P_fertilizer` and `K_fertilizer` as the weight of the **elements P and K**, *not* the weight of the oxides P2O5 and K2O. Convert with `P = P2O5 / 2.29` and `K = K2O / 1.2051`. Likewise report elemental **N** (and `S_fertilizer`, `lime`) in kg/ha. Compute nutrient amounts from product rate × nutrient fraction (e.g. urea 46% N); nutrient percentages for named products are in the terminag `values_fertilizer_type` table (<https://controvoc.github.io/terminag/table.html?f=values%2Fvalues_fertilizer_type.csv>).
- **A control / "no fertilizer" treatment is `0`, not `NA`.** This is the single most common review correction. `0` is a real, measured value (no nutrient applied); `NA` means "unknown". The same holds for any "none"/"nil"/"control" level of a quantitative treatment.
- **Keep fertilizer math simple.** Compute each application as `rate × nutrient_fraction`. When a nutrient is applied **several times** to the same plot, sum the applications *or* keep them as long records (Section 5) — do **not** multiply a single nutrient fraction by a grand total, which double counts.
- **Dates**: character strings, one of `"2023"` (year), `"2023-07"` (year-month), or `"2023-07-21"` (full date). Use `as.character(as.Date(...))` for full dates. Note that dates from excel are often read as integers and need to be reconstructed as.Date("1899-12-30"). If dates are character values they may also be a mix of "dd/mm/yyyy", "mm-dd-year" and other cases. If dates are ambiguous (e.g. a single date of "6-11-2022" can be June 6 or November 11, write a comment about that 
- **Prices**: include `currency` whenever `crop_price` is present (a price without a currency is flagged).
- **no "per plot" or "per plant" values**: counts and weights that are reported by plot or plant shoud normally be normalized to a per-ha basis using the plot's known area or the plant_density. Always record the relevant *_density field (plant_density, or the organ-specific density if the raw data gives it directly) alongside it so the per-plant/per-plot figure remains recoverable by dividing the two densities back out

Add a short `#` comment whenever a computation relies on the codebook, the paper, or a non-obvious assumption (e.g. basket→kg conversions, nutrient fractions); include a URL when you cite an external report or webpage.

---

## 7. Georeferencing

Every distinct site needs `longitude`/`latitude`.

- If the data/publication provide coordinates, use them and set `d$geo_from_source <- TRUE`.
- If the coordinates do not match the country (per a message by `write_files`) check if longitude and latitude are flipped or miss a mius sign
- If records to not have coordinates, they need to be **georeferenced at the most detailed location level** If there is no site or location variable, follow the Georeferencing page (<https://carob-data.org/contribute/georeference.html>): first normalize `adm*` names to match GADM. When merging consider higher levels as well as lower level names may not be be duplicated. Paste the resulting values into the script as a hard-coded lookup and merge it into `d`.
In other cases - where there is a "location" or "site" variable, write a note that the script author should manually georeference with Google Maps
- **`adm1`–`adm4` are official administrative units only** (as in GADM). Many place names — including villages, farms, research stations, and markets — are **not** administrative units. Put those in `location` (or `site`), not `adm4`, and georeference them from coordinates or by geocoding. Only assign a name to `adm4` when you have **verified** it is a real 4th-level admin unit; when unsure, use `location`.
- **Match names to GADM.** Reconcile `adm*` spellings to the current GADM names (the development GADM is often a better match); fix spelling variants explicitly. A blind `merge(..., all.x=TRUE)` that leaves many unmatched is not acceptable.
- When you estimate from an admin unit, also set `d$geo_uncertainty` (and `d$geo_source` when relevant) to document the estimate.
- Fill `adm1`/`adm2`/`adm3` (title-cased) and `location`/`site` when available; use `location` before `site` (a `site` column is not allowed without `location`).
- **`geo_from_source`** is `TRUE` only when for records where the coordinates come from the data or the publication; if you estimated them from a place name or admin unit it is `FALSE`.
- **Document estimated coordinates.** When you estimate `longitude`/`latitude`, add a `#` comment stating the basis (which place name / admin level) and how `geo_uncertainty` was derived.
- Do **not** overwrite existing/known georeferences without a clear, documented reason.
**Do not put live geo lookups in the script.** Never call `carobiner::geo_adm()`, `carobiner::adm_pointRadius()`, `carobiner::geocode()`, `geodata::gadm()`, or similar **inside `carob_script()`**. Use these helpers *interactively while writing the script* to derive coordinates, then **hard-code the output** — typically a small lookup `data.frame` of `adm*` → `longitude`/`latitude`/`geo_uncertainty` that you `merge` into `d`. `carobiner::dfput()` is handy for turning a looked-up table into script code. (The same rule applies to any external/online lookup: derive offline, hard-code the result.)

---

## 8. Filling gaps from the associated publication

Repository files rarely contain everything. When the dataset links to a paper (a DOI in the metadata, a `publication`, or a citation in the dataset description), read its **Methods / Materials and Methods** (and often the study-area and supplementary sections) to recover information that is missing from, or only coded in, the data files. Treat the publication as an authoritative source and cite it in a `#` comment wherever you use it.

Common things the Methods section supplies:

- **Location / geography**: study site names, region/district, and often explicit **coordinates** or a map. Use these to set `location`/`adm*` and `longitude`/`latitude`. If the paper gives the coordinates, set `d$geo_from_source <- TRUE`; if you only get a place name and must estimate, keep `FALSE` and record `geo_uncertainty`/`geo_source` (Section 7).
- **Management**: planting/harvest **dates** or seasons, **fertilizer** rates and products (convert to elemental N/P/K, Section 6), **irrigation**, plant spacing / **plant_density**, tillage, variety details, and whether trials were `on_farm`. These are frequently described once in prose and applied to all plots, so they won't appear as columns in the data.
- **Design and treatments**: the experimental/survey **design** (→ `design`), replication, and what the **treatments** and measured **responses** actually were (→ `treatment_vars` / `response_vars`).
- **Units and definitions**: what a yield figure refers to (`yield_part`), whether it is fresh or dry (`yield_moisture` / `yield_isfresh`), moisture basis, plot sizes, and the meaning of coded columns in a codebook.

Guidance:

- Add a RIS file for the paper in the `references/` folder (matching the `publication` DOI) as noted in Section 4.
- When a value is stated in the paper but not the data, hard-code it with a comment, e.g. `d$N_fertilizer <- 120   # Methods: 120 kg N/ha as urea, all plots`.
- If the value applies to some rows only, be careful to assign it to the right subset rather than the whole table.
- If the paper and data disagree, prefer the data for measured values, note the discrepancy in `## ISSUES`, and use the paper for context/management.
- If there is no accessible paper, or the Methods do not resolve a gap, leave the variable `NA` and record the open question in `## ISSUES`.

## 9. Testing and interpreting `write_files()`

Run in a **clean** R session (no stray objects), e.g.:

```r
devtools::load_all("<root>/carobiner")
carob_script <- NULL; source("scripts/<group>/<dataset_id>.R")
carob_script(path = "<root>/carob/carob")
```

`carobiner::write_files(path, meta, d)` prints messages you must resolve:

- **`missing variables` / `missing metadata`**: a required variable/metadata field is absent. Add it (see `carobiner/inst/terms/required_variables.csv`). Some are conditional on the group (e.g. `crop`, `yield`, `N/P/K_fertilizer`, `irrigated` are not required for `survey`/`soil_samples`).
- **`unknown variables`**: a column name is not in the vocabulary. Rename it to a terminag name, or, if it is legitimately non-standard, **keep it** with an underscore name (Section 5). Do **not** delete or omit a meaningful variable just because it is not in terminag — naming it is what lets it be added later.
- **`out of bounds`**: a numeric value is outside `valid_min`/`valid_max`. Consider fixing the units or the value. DO NOT set outlier values to NA. Add a comment about it, suggesting next steps, but leave the action to the script author.
- **`bad datatype`**: coerce the column (`as.numeric`, `as.integer`, `as.character`).
- **`NA detected`**: a variable that may not be `NA` (per `required_variables.csv`, `NAok=no`) contains `NA`. Provide values or reconsider the mapping.
- **`empty character values` / `untrimmed characters`**: clean strings with `trimws()` and replace `""` with a real value or `NA`.
- **`invalid terms`**: a categorical value is not in the accepted `values_*.csv` list. If it is only a spelling/case variant of an accepted term, map it. But if the value is meaningful and simply **not yet** in the vocabulary, keep it (the warning is fine) — do **not** replace it with `"none"` or `"unknown"`. Combine multiple values with `;` (e.g. a fertilizer `"urea & ammonium nitrate"` → `"urea; AN"`). Use `"unknown"` only when the value is genuinely unknowable, and `"none"`/`"mixture"` only when that is literally the case.

Keep iterating until the only remaining output is the contributor line / `TRUE`.

### Never suppress or work around warnings

**R warnings are generally not allowed in a finished script.** A clean run should emit no R warnings at all. The only tolerable exceptions are warnings you genuinely cannot prevent — e.g. some `readxl`/`read.excel` warnings when opening a malformed `.xlsx` — and even those must be noted in a `#` comment. Warnings from `write_files()` and from R itself are signals, not noise. For each one there are only two acceptable outcomes:

1. **Truly fix it** — you identified a concrete problem and solved it (corrected a unit, coerced a type, mapped a value to an accepted term, fixed a name, etc.).
2. **Leave it visible** — if it reflects a genuine limitation of the source data that you cannot resolve, let the warning stand so a reviewer can inspect it, and add a `#` comment (and/or a `## ISSUES` note) explaining **why** it remains.

Do **not**:

- use `suppressWarnings()`, `suppressMessages()`, `options(warn=-1)`, `try()`/ `tryCatch()` swallowing, or similar, to hide a warning;
- filter to "numeric-looking" values (e.g. `grepl("^[0-9.]+$", x)` before `as.numeric()`) — this is just as bad as `suppressWarnings()`; find and fix/map the offending values instead;
- filter/drop rows, coerce blindly, or tweak values **just to silence** a message without understanding it;
- drop records that have `NA` in a required variable (e.g. `yield`) only to silence the message — the `NA` flags something to check, not to delete; keep the records unless they truly hold nothing else of interest;
- delete or comment out a variable only to make a warning disappear.
- set outliers to NA

**Coercion warnings are never allowed.** `NAs introduced by coercion` from `as.numeric()`/`as.integer()`/`as.logical()` means the input held text those functions could not parse. Fix it in one of exactly two ways:

1. **Set the missing-value flag when reading the file**, so the offending strings become `NA` before any coercion: `read.csv(f, na.strings=c("nd","-",""))` or `carobiner::read.excel(f, na=...)`. Use this when the offenders are just missing-value markers.
2. **Repair the offending character values first, then coerce.** Use this when the values carry information that must be cleaned: e.g. month names (`"December"`, and the misspelled `"Desember"`) mapped to numbers; numbers with embedded units (`"0.5KM"`, `"2.75KM"`, `"1200 m"`) parsed out — and watch for **inconsistent units within one column** (e.g. distance vs travel time).

Never coerce first and let it warn (and never wrap it in `suppressWarnings()`). The same applies to `as.character(as.numeric(x))` chains that warn.

The goal is that every remaining warning is either intentional and explained, or gone because the underlying problem was actually fixed — never merely hidden.

Do **not** force data into a one-row-per-unit shape when that loses information. If a dataset has several record types (e.g. a household survey with per-variety records *and* a separate "largest plot" block), capture each in its own `data.frame` with interpretable, standardized names rather than deduplicating away real observations. Decide how to write extra tables (e.g. as "long" records) separately.

---


## 10. Where scripts go, and PR conventions

- Work-in-progress from `draft()`: `scripts/_draft/<group>/`.
- Hard/auto-rejected drafts: `scripts/_AI/_rejected/` (a review queue; skipped by the build).
- **Finished** script: `scripts/<group>/<dataset_id>.R`.
- Confirmed "do not process": `scripts/_rejected/`.
- The build (`make_carob()` / `process_carob()`) **skips** `_draft`, `_AI`, `_pending`, and `_rejected`. Only files under a real `scripts/<group>/` folder are compiled.
- **Move the finished file out of `_draft/`** into `scripts/<group>/` when you submit — a script left in `_draft/` is not compiled.
- **One dataset = one file = one PR.** Do not bundle two datasets/scripts in one PR. Use a branch named after the script file.
- **Address review feedback by pushing to the *same* PR/branch** — do not open a new PR for fixes, and do not add an unrelated new file to an open PR.

### Commit and PR messages

- **Commit message** follows the pattern `<new|edited> <group> script <uri>`. Use `new` for a script you are adding, and `edited` for a change to an existing script. Examples:
  - `new agronomy script doi:10.25502/20180814/1514/HJ`
  - `edited varieties script doi:10.7910/DVN/SMGA6L`
- **PR message** is for the reviewer — make it **very succinct**. It should:
  - list, in one line each, the `## NOTES`/`## ISSUES` the reviewer needs to consider (assumptions, unit conversions, anything moved to `_pending`/`_rejected` and *why*, open questions);
  - list the messages printed by `write_files()` that remain (e.g. `unknown variables: stake_diameter_`, `out of bounds: ...`, `datespan: ...`), so the reviewer knows what to expect and why each is acceptable.
- Do **not** submit an auto-generated / boilerplate message (e.g. "Added metadata"). No message should be identical across datasets.

---

## 11. Group-specific notes

### `varieties`
- Core treatment is `variety` (also set `treatment = variety`). Include `variety_pedigree` when given.
- Set `variety_type` — often derivable from the dataset description (e.g. "advanced drought tolerant hybrid"). Read the abstract.
- Identify and flag **check** varieties when the data/description indicate them (e.g. a `Check` column, or named hybrid/OPV/parent checks). Follow the pattern in existing `scripts/varieties/` files.
- Density variables: `plant_density`, `spike_density` = `10000 * count / plot_area`  (per ha); coerce `count` and `plot_area` with `as.numeric` first.

### `survey`
- A **survey** observes the state of the world *without applying treatments* (vs an **experiment**, which has treatments and responses). This covers field/weed/soil surveys, not only *household* surveys — do not assume "survey" means "household".
- `data_type = "survey"`, `d$is_survey <- TRUE`. Each surveyed unit (e.g. household) gets a unique `hh_id`. `crop`, `yield`, and management vars are not required.
- Multi-module surveys often need to create several `data.frame`s (Section 9) that can be merged.

### `soil_samples`
- Soil samples collected in the field with no treatment applied are a **survey**: set `data_type = "survey"` and `d$is_survey <- TRUE`.
- `crop`/`management` requirements are relaxed; focus on soil variables and `sample_id`. Still set `on_farm` and geography.
- Soil nutrients are in ppm; a value given in kg/ha can be approximated as ppm by dividing by ~1.1 (note the assumption in a `#` comment).

### `pest_disease`
- Record that a trial concerned a stress with `stress` (e.g. `stress = "pests;disease"`). Capture incidence/severity/score responses with their terminag names.

---

## 12. Reject / pending

- scripts for datasets that are not suitable should be moved to the `_rejected` folder
- reasons for rejection include: data are not readable, or were collected in a greenhouse or lab rather than in the field (greenhouse experiments for variety traits are OK)
- reasons to recommend rejection or moving to the `_pending` folder include: reported treatment variables are not included in the data; surveys without quantitative georeferences of location; no location data and no planting dates. If moved to `_pending` the dataset authors should be contacted for help.
- when you move a dataset to `_pending` or `_rejected`, say so — and why — in the PR message.


## 13. Pre-submission checklist

- [ ] File named `<dataset_id>.R` and placed in `scripts/<group>/`.
- [ ] Title + abstract copied into the script; 
- [ ] `## NOTES` describe peculiarities --- make these succinct!
- [ ] `## ISSUES` notes any caveats that need attention --- make these succinct!
- [ ] `## NOTES`/`## ISSUES` are correct — verified against the data and paper, not blindly trusted.
- [ ] `uri`, `group`, `get_data`, `get_metadata`, `write_files` all present.
- [ ] Metadata: real `data_organization`, `data_type`, `treatment_vars`, `response_vars`, `carob_LLM`, `carob_date`, `carob_completion`, `carob_effort`; `NA` (not `""`) for absent fields; treatments and responses not swapped.
- [ ] Associated publication's Methods checked for location/management/design/units; values taken from it are commented and (if a paper exists) a RIS added to `references/`.
- [ ] All column names and categorical values match terminag; units correct (kg/ha yields, elemental N/P/K, ha areas, proper date formats).
- [ ] Meaningful non-standard variables/values kept (named, not omitted or mapped to `"none"`/`"unknown"`); multiple values joined with `;`.
- [ ] No `library()`/`require()` (use `pkg::`); numbers-with-units parsed before `as.numeric()`; one operation per line where reasonable.
- [ ] Every treatment variable is an explicit, interpretable column (not only inside `treatment`); a control / "no input" level is `0`, not `NA`.
- [ ] Repeated/time-series measurements kept in a long table (not reduced to one value or aggregated away); weather passed via `wth=`; only measured (not simulated) values included.
- [ ] `trial_id`, `on_farm`, `is_survey`, `longitude`, `latitude`, `geo_from_source` (TRUE only if coords from data/paper), `yield_part` set appropriately; estimated coordinates documented.
- [ ] Non-admin places (villages, farms, stations) are in `location`/`site`, not `adm*`; admin names matched to GADM.
- [ ] No live geo/online lookups in the script — helpers (`geo_adm`, `gadm`, `geocode`, `adm_pointRadius`) used only to *derive* values, which are then hard-coded.
- [ ] `carob_script(path)` runs clean in a fresh session with no unresolved `write_files()` messages and no R warnings (incl. `NAs introduced by coercion`).
- [ ] No `suppressWarnings()`/`suppressMessages()`/`options(warn=-1)` and no "numeric-looking" filters used; every remaining warning is either fixed or left with a `#` comment explaining why.
- [ ] No records dropped merely to silence a warning; no information silently dropped to fit a single table.
- [ ] Finished file moved out of `_draft/`; one dataset per PR.
- [ ] Commit message `<new|edited> <group> script <uri>`; PR message very succinctly lists the `## NOTES`/`## ISSUES` and the remaining `write_files()` messages.

---

## Related references

- **Official contributor guidelines (read this): <https://carob-data.org/contribute/guidelines.html>**
- **Official example script walkthrough (read this): <https://carob-data.org/contribute/example.html>**
- `scripts/_template.R` — canonical skeleton (note the template typo: `fertlizer_type` should be `fertilizer_type`).
- terminag: <https://github.com/controvoc/terminag> (variables and accepted values).
- Carob contribute docs: <https://carob-data.org/contribute/index.html> (see the Example, Guidelines, and Georeferencing pages).
