library(chromote)
library(rvest)
library(dplyr)
library(purrr)
library(stringr)
library(progress)

# Making a readable webpage to scrape
get_chromote <- function() {
  if (!exists("b", inherits = TRUE) || is.null(b) || !b$is_active()) {
    message("Respawning Chromote session...")
    assign("b", ChromoteSession$new(), envir = .GlobalEnv)
  }
  b
}

# Function to browse through event numbers
check_event_js <- function(id) {
  url <- paste0("https://ifsc.results.info/event/", id)
  tryCatch({
    b <- get_chromote()
    b$Page$navigate(url)
    Sys.sleep(5)
    
    html_content <- b$Runtime$evaluate(
      "document.documentElement.outerHTML"
    )$result$value
    
    page <- read_html(html_content)
    
    title <- page %>%
      html_element("div.font-weight-bold.h5") %>%
      html_text(trim = TRUE)
    
    tibble(event_id = id, title = title)
  }, error = function(e) {
    message(paste("Failed ID:", id))
    tibble(event_id = id, title = NA_character_)
  })
}

# Selecting event ids 500 onwards as result recording is consistent from approx event number 500
candidate_ids <- 500:1800
checkpoint_file <- "events_checkpoint.rds"

# Creating a saving progress bar for collecting relevant event ids

if (file.exists(checkpoint_file)) {
  results_list <- readRDS(checkpoint_file)
  start_index <- sum(!sapply(results_list, is.null)) + 1
  message("Resuming from ID index: ", start_index)
} else {
  results_list <- vector("list", length(candidate_ids))
  start_index <- 1
}

remaining <- seq.int(start_index, length(candidate_ids))

# Scraping all event names

if (length(remaining) == 0) {
  message("Nothing left to scrape.")
} else {
  
  pb <- progress_bar$new(
    format = " Scraping [:bar] :current/:total (:percent) ETA: :eta",
    total = length(remaining),
    width = 60
  )
  
  for (i in remaining) {
    id <- candidate_ids[i]
    results_list[[i]] <- check_event_js(id)
    pb$tick()
    
    if (i %% 50 == 0) {
      saveRDS(results_list, checkpoint_file)
    }
  }
}

saveRDS(results_list, checkpoint_file)

events <- results_list %>% bind_rows()

# Filtering all events for only World Cups and Championships (Non-Youth, Non-Para)

keywords <- c("IFSC World", "IFSC Climbing", "World Climbing Series")

events_filtered <- events %>%
  filter(
    (
      (
        str_detect(title, regex("\\bIFSC\\b", ignore_case = TRUE)) &
          str_detect(title, regex("world", ignore_case = TRUE))
      ) |
        str_detect(
          title,
          regex("World Climbing Series", ignore_case = TRUE)
        )
    ) &
      !str_detect(title, regex("\\byouth\\b", ignore_case = TRUE)) &
      !str_detect(title, regex("para", ignore_case = TRUE))
  )

valid_event_ids <- events_filtered$event_id
saveRDS(valid_event_ids, "valid_event_ids.rds")

# Obtaining all competition round ids for each event

get_cr_ids <- function(event_id) {
  
  b <- get_chromote()
  url <- paste0("https://ifsc.results.info/event/", event_id)
  
  b$Page$navigate(url)
  Sys.sleep(3)
  
  # ---- CLICK BOULDER TAB ----
  b$Runtime$evaluate(
    expression = "
    const tab = Array.from(document.querySelectorAll('.tab-item'))
      .find(el => el.textContent.trim().toLowerCase() === 'boulder');
    if (tab) {
      tab.click();
      true;
    } else {
      false;
    }
  ",
    returnByValue = TRUE
  )
  
  Sys.sleep(2)  # wait for Vue render
  
  # ---- SCRAPE UPDATED DOM ----
  html_content <- b$Runtime$evaluate(
    'document.documentElement.outerHTML'
  )$result$value
  
  page <- read_html(html_content)
  
  cr_links <- page %>%
    html_elements('a') %>%
    html_attr('href') %>%
    .[str_detect(., '/cr/')]
  
  str_extract(cr_links, '(?<=/cr/)\\d+') %>%
    as.integer() %>%
    unique()
}

# Event Dates (Final Date for Simplicity)
get_event_end_date <- function(event_id) {
  
  b <- get_chromote()
  url <- paste0("https://ifsc.results.info/event/", event_id)
  
  b$Page$navigate(url)
  Sys.sleep(3)
  
  res <- b$Runtime$evaluate(
    expression = "
      (document.body.innerText.match(/\\d{1,2}-[A-Za-z]{3}-\\d{4}/g) || []).slice(-1)[0]
    ",
    returnByValue = TRUE
  )
  
  end_date_str <- res$result$value
  
  if (is.null(end_date_str) || is.na(end_date_str)) return(NA)
  
  as.Date(end_date_str, format = "%d-%b-%Y")
}

# Filter for only Bouldering Competitions

is_boulder_cr <- function(page) {
  # Get all dcat-row elements
  rows <- page %>% html_elements("div.dcat-row") 
  
  # Find the row that contains 'Boulder' (case-insensitive)
  discipline_row <- rows %>% 
    keep(~ str_detect(html_text(.x, trim = TRUE), regex("boulder", ignore_case = TRUE))) %>% 
    .[[1]]  # Take the first match
  
  # If no match, return FALSE
  !is.null(discipline_row)
}

# Collecting all urls for relevant boulder competitions

read_html_js <- function(url, wait = 3) {
  
  b <- get_chromote()
  
  tryCatch({
    
    b$Page$navigate(url)
    Sys.sleep(wait)
    
    html <- b$Runtime$evaluate(
      "document.documentElement.outerHTML"
    )$result$value
    
    read_html(html)
    
  }, error = function(e) {
    
    message("Chromote died, respawning and retrying...")
    assign("b", ChromoteSession$new(), envir = .GlobalEnv)
    b <- get_chromote()
    
    b$Page$navigate(url)
    Sys.sleep(wait)
    
    html <- b$Runtime$evaluate(
      "document.documentElement.outerHTML"
    )$result$value
    
    read_html(html)
  })
}

# Function to obtaining data for each boulder competition

scrape_ifsc_boulder <- function(url) {
  page <- read_html_js(url)
  
  discipline_text <- page %>% 
    html_elements("div.dcat-row") %>% 
    keep(~ str_detect(html_text(.x, trim = TRUE), regex("boulder", ignore_case = TRUE))) %>% 
    html_text(trim = TRUE) %>% 
    .[[1]]
  
  if (is.null(discipline_text)) stop("Not a boulder CR")
  
  # Extract gender from that element
  gender <- case_when(
    str_detect(discipline_text, "Men") ~ "M",
    str_detect(discipline_text, "Women") ~ "W",
    TRUE ~ NA_character_
  )
  
  # Extract round
  round_text <- page %>%
    html_element("div.round-name") %>%
    html_text(trim = TRUE)
  
  round <- case_when(
    str_detect(round_text, regex("^qual", ignore_case = TRUE)) ~ "Q",
    str_detect(round_text, regex("^semi", ignore_case = TRUE)) ~ "S",
    str_detect(round_text, regex("^final", ignore_case = TRUE)) ~ "F",
    TRUE ~ NA_character_
  )
  
  rows <- html_elements(page, "table.table tr.r-row")
  details <- html_elements(page, "table.table tr[class*='boulder-asc-detail']")
  
  if (length(rows) == 0 || length(details) == 0) stop("No boulder rows found")
  
  n <- min(length(rows), length(details))
  
  map_df(seq_len(n), function(i) {
    row <- rows[[i]]
    detail <- details[[i]]
    
    # Athlete info
    rank <- html_text(html_element(row, "td.rank"), trim = TRUE)
    
    athlete_node <- html_element(row, "a.r-name")
    athlete <- html_text(athlete_node, trim = TRUE)
    athlete_id <- html_attr(athlete_node, "href") %>%
      str_extract("\\d+") %>%
      as.integer()
    
    country <- html_text(html_element(row, ".r-name-sub span:nth-child(2)"), trim = TRUE)
    
    # Extract boulder details
    boulder_cells <- html_elements(detail, ".asc-cell-container")
    
    # Initialize vectors for 5 boulders
    zone_attempts <- rep(NA_integer_, 5)
    top_attempts <- rep(NA_integer_, 5)
    
    for (cell in boulder_cells) {
      boulder_num <- html_text(html_element(cell, ".asc-route-name"), trim = TRUE) %>% as.integer()
      if (boulder_num > 5) next  # ignore >5
      zone <- html_text(html_element(cell, ".zone.zoned"), trim = TRUE) %>% as.integer()
      top <- html_text(html_element(cell, ".top.topped"), trim = TRUE) %>% as.integer()
      zone_attempts[boulder_num] <- zone
      top_attempts[boulder_num] <- top
    }
    
    tib <- tibble(
      rank = rank,
      athlete = athlete,
      athlete_id = athlete_id,
      gender = gender,
      round = round,
      country = country,
      b1_zone = zone_attempts[1], b1_top = top_attempts[1],
      b2_zone = zone_attempts[2], b2_top = top_attempts[2],
      b3_zone = zone_attempts[3], b3_top = top_attempts[3],
      b4_zone = zone_attempts[4], b4_top = top_attempts[4],
      b5_zone = zone_attempts[5], b5_top = top_attempts[5]
    )
    
    if (round %in% c("S", "F")) {
      tib <- tib %>% select(-b5_zone, -b5_top)
    }
    
    tib
  })
}

# Function to obtain athlete info
scrape_athlete_profile <- function(athlete_id) {
  
  url <- paste0("https://ifsc.results.info/athlete/", athlete_id)
  page <- read_html_js(url, wait = 2)
  
  info_text <- page %>%
    html_element("div.athlete-info.left-side") %>%
    html_elements("div") %>%
    html_text(trim = TRUE)
  
  age <- info_text %>%
    keep(~ str_detect(.x, "^Age")) %>%
    str_extract("\\d+") %>%
    as.integer()
  
  height <- info_text %>%
    keep(~ str_detect(.x, "^Height")) %>%
    str_extract("\\d+") %>%
    as.integer()
  
  scrape_year <- as.integer(format(Sys.Date(), "%Y"))
  
  birth_year <- if (!is.na(age)) scrape_year - age else NA_integer_
  
  tibble(
    athlete_id = athlete_id,
    age_at_scrape = age,
    height_cm = height,
    birth_year = birth_year,
    scrape_year = scrape_year
  )
}

# Obtaining Event Dates
event_dates <- tibble(
  event_id = integer(),
  event_date = as.Date(character())
)

# Results checkpoint 
results_file <- "boulder_results_checkpoint.rds" 

if (file.exists(results_file)) { 
  all_results <- readRDS(results_file) 
  done_event_ids <- unique(all_results$event_id) 
} else { 
    all_results <- tibble() 
    done_event_ids <- integer() 
    } 

# Athlete checkpoint 
athlete_file <- "athlete_metadata.rds" 
if (file.exists(athlete_file)) { 
  athlete_meta <- readRDS(athlete_file) 
} else { athlete_meta <- tibble( 
    athlete_id = integer(), 
    height_cm = integer(), 
    birth_year = integer() 
) 
}

# Athlete End of Year Rankings
scrape_athlete_boulder_rankings <- function(athlete_id) {
  
  b <- get_chromote()
  url <- paste0("https://ifsc.results.info/athlete/", athlete_id)
  
  b$Page$navigate(url)
  Sys.sleep(3)  # initial load
  
  # ---- CLICK RANKINGS TAB ----
  b$Runtime$evaluate(
    expression = "
      const tab = Array.from(document.querySelectorAll('.nav-item'))
        .find(el => el.textContent.trim() === 'Rankings');
      if (tab) tab.click();
    "
  )
  
  Sys.sleep(2)  # wait for Vue render
  
  # ---- SCRAPE RANKINGS DOM ----
  res <- b$Runtime$evaluate(
    returnByValue = TRUE,
    expression = "
      Array.from(document.querySelectorAll('.result-basic-line'))
        .map(line => {
          
          const discipline = line.querySelector('.discipline')?.textContent.trim();
          if (!discipline || discipline.toLowerCase() !== 'boulder') return null;
          
          const eventName = line.querySelector('.event-name')?.textContent
          ?.toLowerCase();
        if (!eventName || !eventName.includes('world')) return null;
          
          const rank = line.querySelector('.rank')?.textContent.trim();
          const year = line.querySelector('.date')?.textContent.trim();
          
          return {
            discipline,
            rank: rank ? parseInt(rank) : null,
            year: year ? parseInt(year.match(/\\d{4}/)) : null
          };
        })
        .filter(x => x && x.rank && x.year)
    "
  )
  
  data <- res$result$value
  if (length(data) == 0) return(tibble())
  
  tibble::tibble(
    athlete_id = athlete_id,
    year = purrr::map_int(data, "year"),
    boulder_world_rank = purrr::map_int(data, "rank")
  )
}

scrape_athlete_boulder_rankings <- function(athlete_id) {
  
  b <- get_chromote()
  url <- paste0("https://ifsc.results.info/athlete/", athlete_id)
  
  b$Page$navigate(url)
  Sys.sleep(5)  # wait for page load
  
  # Click the Rankings tab
  b$Runtime$evaluate(
    expression = "
      const tab = Array.from(document.querySelectorAll('.nav-item'))
        .find(el => el.textContent.trim() === 'Rankings');
      if (tab) tab.click();
    "
  )
  
  Sys.sleep(5)  # wait for Vue render
  
  # Scrape only boulder + world events
  res <- b$Runtime$evaluate(
    returnByValue = TRUE,
    expression = "
      Array.from(document.querySelectorAll('div.event-info'))
        .map(ev => {
          const discipline = ev.querySelector('.discipline')?.textContent.trim();
          const eventName = ev.querySelector('.event-name')?.textContent.trim();
          const yearText = eventName?.match(/\\d{4}/);
          const year = yearText ? parseInt(yearText[0]) : null;
          const rankText = ev.querySelector('.rank')?.textContent.trim();
          const rank = rankText ? parseInt(rankText) : null;
          
          if (!discipline || discipline.toLowerCase() !== 'boulder') return null;
          if (!eventName || !eventName.toLowerCase().includes('world')) return null;
          if (!rank || !year) return null;
          
          return { discipline, eventName, year, rank };
        })
        .filter(x => x !== null)
    "
  )
  
  data <- res$result$value
  if (length(data) == 0) return(tibble())
  
  tibble(
    athlete_id = athlete_id,
    year = purrr::map_int(data, "year"),
    boulder_world_rank = purrr::map_int(data, "rank"),
    event_name = purrr::map_chr(data, "eventName")
  )
}

scrape_athlete_boulder_rankings <- function(athlete_id) {
  b <- get_chromote()
  url <- paste0("https://ifsc.results.info/athlete/", athlete_id)
  
  b$Page$navigate(url)
  Sys.sleep(3)
  
  # Click Rankings tab
  b$Runtime$evaluate("
      const tab = Array.from(document.querySelectorAll('.nav-item'))
        .find(el => el.textContent.trim() === 'Rankings');
      if (tab) tab.click();
  ")
  
  # Wait until at least 1 event-info element exists (max 15s)
  max_wait <- 15
  wait_time <- 0
  repeat {
    res <- b$Runtime$evaluate("document.querySelectorAll('div.event-info').length", returnByValue = TRUE)
    if (res$result$value > 0 || wait_time >= max_wait) break
    Sys.sleep(1)
    wait_time <- wait_time + 1
  }
  
  # Read updated HTML
  html <- b$Runtime$evaluate("document.documentElement.outerHTML")$result$value
  page <- read_html(html)
  
  events <- page %>% html_elements("div.event-info")
  
  if (length(events) == 0) return(tibble())
  
  map_df(events, function(ev) {
    discipline <- ev %>% html_element(".discipline") %>% html_text(trim = TRUE)
    event_name <- ev %>% html_element(".event-name") %>% html_text(trim = TRUE)
    
    # Only boulder + world events
    if (!str_detect(discipline, regex("boulder", ignore_case = TRUE))) return(NULL)
    if (!str_detect(event_name, regex("world", ignore_case = TRUE))) return(NULL)
    
    year <- str_extract(event_name, "\\d{4}") %>% as.integer()
    
    # Extract rank text — often in sibling div/span, fallback to NA if missing
    rank_text <- ev %>% html_element(xpath = ".//following-sibling::div[contains(@class,'rank')]") %>% html_text(trim = TRUE)
    rank <- if (!is.na(rank_text)) as.integer(str_extract(rank_text, "\\d+")) else NA_integer_
    
    tibble(
      athlete_id = athlete_id,
      year = year,
      boulder_world_rank = rank,
      event_name = event_name
    )
  })
}

# =========================
# Progress bar (events)
# =========================
pb <- progress_bar$new(
  format = " Scraping events [:bar] :current/:total (:percent) ETA: :eta",
  total = length(valid_event_ids),
  width = 60
)

# =========================
# MAIN LOOP
# =========================

for (event_id in valid_event_ids) {
  
  if (event_id %in% done_event_ids) {
    pb$tick()
    next
  }
  
  message("Processing event ", event_id)
  
  if (!event_id %in% event_dates$event_id) {
    event_dates <- bind_rows(
      event_dates,
      tibble(
        event_id = event_id,
        event_date = get_event_end_date(event_id)
      )
    )
  }
  
  cr_ids <- tryCatch(
    get_cr_ids(event_id),
    error = function(e) integer(0)
  )
  
  if (length(cr_ids) == 0) {
    pb$tick()
    next
  }
  
  cr_results <- map_df(cr_ids, function(cr_id) {
    
    cr_url <- paste0(
      "https://ifsc.results.info/event/",
      event_id,
      "/cr/",
      cr_id
    )
    
    tryCatch({
      
      scrape_ifsc_boulder(cr_url) %>%
        mutate(
          event_id = event_id,
          cr_id = cr_id,
          cr_url = cr_url,
          event_date = event_dates$event_date[
            match(event_id, event_dates$event_id)
          ]
        )
      
    }, error = function(e) {
      message("  Skipped non-boulder CR ", cr_id)
      NULL
    })
  })
  
  if (nrow(cr_results) > 0) {
    all_results <- bind_rows(all_results, cr_results)
    saveRDS(all_results, results_file)
  }
  
  pb$tick()
}

# =========================
# DONE
# =========================
message("Finished scraping BOULDER competitions only.")

all_results <- all_results |>
  filter(!is.na(rank) & rank != "")

missing_athletes <- setdiff(
  unique(all_results$athlete_id),
  athlete_meta$athlete_id
)

if (length(missing_athletes) > 0) {
  
  message("Scraping ", length(missing_athletes), " athlete profiles")
  
  pb_ath <- progress_bar$new(
    format = " Athletes [:bar] :current/:total (:percent)",
    total = length(missing_athletes),
    width = 60
  )
  
  for (aid in missing_athletes) {
    athlete_meta <- bind_rows(
      athlete_meta,
      tryCatch(
        scrape_athlete_profile(aid),
        error = function(e)
          tibble(athlete_id = aid, height_cm = NA, birth_year = NA)
      )
    )
    
    saveRDS(athlete_meta, athlete_file)
    pb_ath$tick()
  }
}

boulder_rankings_file <- "athlete_boulder_rankings.rds"

if (file.exists(boulder_rankings_file)) {
  athlete_boulder_rankings <- readRDS(boulder_rankings_file)
} else {
  athlete_boulder_rankings <- tibble(
    athlete_id = integer(),
    year = integer(),
    boulder_world_rank = integer()
  )
}

all_athletes <- unique(all_results$athlete_id)

# ---- STEP 0: last competition year per athlete ----
last_comp_year <- all_results %>%
  group_by(athlete_id) %>%
  summarise(
    last_comp_year = max(event_year, na.rm = TRUE),
    .groups = "drop"
  )

# ---- STEP 1: latest ranking year per athlete ----
latest_rank_year <- athlete_boulder_rankings %>%
  group_by(athlete_id) %>%
  summarise(latest_rank_year = max(year, na.rm = TRUE), .groups = "drop")

athlete_status <- full_join(
  last_comp_year,
  latest_rank_year,
  by = "athlete_id"
)

# ---- STEP 2: decide who actually needs scraping ----
current_year <- as.integer(format(Sys.Date(), "%Y"))

athletes_to_update <- athlete_status %>%
  filter(
    is.na(latest_rank_year) |                    # new athlete
      (
        last_comp_year >= current_year - 1 &       # recently active
          latest_rank_year < current_year
      )
  ) %>%
  pull(athlete_id)

pb_rank <- progress_bar$new(
  format = " Boulder rankings [:bar] :current/:total (:percent) ETA: :eta",
  total = length(athletes_to_update),
  width = 60
)

for (aid in athletes_to_update) {
  
  new_data <- tryCatch(
    scrape_athlete_boulder_rankings(aid),
    error = function(e) tibble()
  )
  
  if (nrow(new_data) > 0) {
    athlete_boulder_rankings <- athlete_boulder_rankings %>%
      bind_rows(new_data) %>%
      distinct(athlete_id, year, .keep_all = TRUE)
    
    saveRDS(athlete_boulder_rankings, boulder_rankings_file)
  }
  
  pb_rank$tick()
}

all_results <- all_results %>%
  select(
    -matches("\\.x$|\\.y$|_meta|_rank$")
  )

all_results <- all_results %>%
  left_join(athlete_meta, by = "athlete_id", suffix = c("", "_meta")) %>%
  left_join(
    athlete_boulder_rankings %>% rename(ranking_year = year),
    by = c("athlete_id", "ranking_year"),
    suffix = c("", "_rank")
  ) %>%
  left_join(
    events_filtered %>% select(event_id, event_title = title),
    by = "event_id"
  )

# ---- Columns that might be duplicated ----
dup_cols <- c(
  "b1_zone","b1_top","b2_zone","b2_top",
  "b3_zone", "b3_top", "b4_zone", "b4_top",
  "b5_zone", "b5_top", "height_cm", "birth_year", "age_at_scrape", "scrape_year",
  "age_at_event", "event_year", "ranking_year",
  "boulder_world_rank", "event_title"
)

# ---- Resolve duplicates safely ----
for (v in dup_cols) {
  x <- paste0(v, ".x")
  y <- paste0(v, ".y")
  rank <- paste0(v, "_rank")
  meta <- paste0(v, "_meta")
  
  all_results[[v]] <- dplyr::coalesce(
    all_results[[v]],
    all_results[[x]],
    all_results[[y]],
    all_results[[meta]],
    all_results[[rank]]
  )
  
  all_results <- all_results %>%
    select(-any_of(c(x, y, meta, rank)))
  
  if (!v %in% names(all_results)) all_results[[v]] <- NA
}

# ---- Compute derived columns ----
all_results <- all_results %>%
  mutate(
    age_at_event = if_else(
      !is.na(birth_year) & !is.na(event_date),
      as.integer(format(event_date, "%Y")) - birth_year,
      NA_integer_
    ),
    event_year   = as.integer(format(event_date, "%Y")),
    ranking_year = event_year - 1
  )

library(openxlsx)
write.xlsx(all_results, "ifsc_boulder_results.xlsx")

# Check for 6 competition rounds
events_to_fix <- all_results %>%
  summarise(
    n_cr = n_distinct(cr_id),
    .by = event_id
  ) %>%
  filter(n_cr < 6) %>%
  pull(event_id)

# Adding single missed events
add_event_to_results <- function(event_id) {
  
  message("Processing event ", event_id, " ...")
  
  # ---- Event date ----
  event_date <- get_event_end_date(event_id)
  event_year <- as.integer(format(event_date, "%Y"))
  ranking_year <- event_year - 1
  
  # ---- All CR ids ----
  all_cr_ids <- get_cr_ids(event_id)
  
  # If starting from scratch, scrape everything
  if (!exists("all_results", inherits = TRUE) || nrow(all_results) == 0) {
    cr_ids <- all_cr_ids
  } else {
    # Otherwise, scrape only missing CRs
    existing_cr_ids <- all_results %>%
      filter(event_id == !!event_id) %>%
      pull(cr_id) %>%
      unique()
    
    cr_ids <- setdiff(all_cr_ids, existing_cr_ids)
  }
  
  if (length(cr_ids) == 0) {
    message("No CRs to scrape for event ", event_id)
    return(NULL)
  }
  
  # ---- Scrape all CRs ----
  cr_results <- map_df(cr_ids, function(cr_id) {
    cr_url <- paste0("https://ifsc.results.info/event/", event_id, "/cr/", cr_id)
    tryCatch({
      scrape_ifsc_boulder(cr_url) %>%
        mutate(
          event_id = event_id,
          cr_id = cr_id,
          cr_url = cr_url,
          event_date = event_date,
          event_year = event_year,
          ranking_year = ranking_year
        )
    }, error = function(e) {
      message("  Skipped CR ", cr_id, ": ", e$message)
      NULL
    })
  })
  
  if (nrow(cr_results) == 0) {
    message("No valid CR results for event ", event_id)
    return(NULL)
  }
  
  # ---- Merge athlete metadata ----
  cr_results <- cr_results %>%
    left_join(athlete_meta, by = "athlete_id", suffix = c("", "_meta")) %>%
    left_join(
      athlete_boulder_rankings %>% rename(ranking_year = year),
      by = c("athlete_id", "ranking_year"),
      suffix = c("", "_rank")
    ) %>%
    mutate(
      age_at_event = if_else(!is.na(birth_year) & !is.na(event_date),
                             event_year - birth_year,
                             NA_integer_)
    )
  
  # Event title
  cr_results <- cr_results %>%
    mutate(event_id = as.integer(event_id)) %>%
    left_join(
      events_filtered %>% 
        mutate(event_id = as.integer(event_id)) %>% 
        select(event_id, event_title = title),
      by = "event_id"
    )
  
  # ---- Append to main results ----
  assign(
    "all_results",
    bind_rows(all_results, cr_results),
    envir = .GlobalEnv
  )
  
  # ---- Save checkpoint ----
  saveRDS(all_results, results_file)
  
  message("Event ", event_id, " successfully added.")
  return(cr_results)
}

# Removing repeated rows
all_results <- all_results %>%
  distinct(event_id, cr_id, athlete_id, .keep_all = TRUE)

# For future fix,
# Events - CR 
# 992 - 4312, 
# 1187 - 6164, 6165, 
# 1190 - 6454, 6455, 6456, 6457, 
# 1192 - 6176, 6500, 6501, 6502, 6503 
# still missing data
# Look into adjusting code for individual

# For 1187, 1190, 1192,
# Error due to boulders being mislabelled with M/F
# Alternative scraper defined for these below

scrape_ifsc_boulder_alt <- function(url) {
  page <- read_html_js(url)
  
  discipline_text <- page %>% 
    html_elements("div.dcat-row") %>% 
    keep(~ str_detect(html_text(.x, trim = TRUE), regex("boulder", ignore_case = TRUE))) %>% 
    html_text(trim = TRUE) %>% 
    .[[1]]
  
  if (is.null(discipline_text)) stop("Not a boulder CR")
  
  # Extract gender from that element
  gender <- case_when(
    str_detect(discipline_text, "Men") ~ "M",
    str_detect(discipline_text, "Women") ~ "W",
    TRUE ~ NA_character_
  )
  
  # Extract round
  round_text <- page %>%
    html_element("div.round-name") %>%
    html_text(trim = TRUE)
  
  round <- case_when(
    str_detect(round_text, regex("^qual", ignore_case = TRUE)) ~ "Q",
    str_detect(round_text, regex("^semi", ignore_case = TRUE)) ~ "S",
    str_detect(round_text, regex("^final", ignore_case = TRUE)) ~ "F",
    TRUE ~ NA_character_
  )
  
  rows <- html_elements(page, "table.table tr.r-row")
  details <- html_elements(page, "table.table tr[class*='boulder-asc-detail']")
  
  if (length(rows) == 0 || length(details) == 0) stop("No boulder rows found")
  
  n <- min(length(rows), length(details))
  
  map_df(seq_len(n), function(i) {
    row <- rows[[i]]
    detail <- details[[i]]
    
    # Athlete info
    rank <- html_text(html_element(row, "td.rank"), trim = TRUE)
    
    athlete_node <- html_element(row, "a.r-name")
    athlete <- html_text(athlete_node, trim = TRUE)
    athlete_id <- html_attr(athlete_node, "href") %>%
      str_extract("\\d+") %>%
      as.integer()
    
    country <- html_text(html_element(row, ".r-name-sub span:nth-child(2)"), trim = TRUE)
    
    # Extract boulder details
    boulder_cells <- html_elements(detail, ".asc-cell-container")
    
    # Initialize vectors for 5 boulders
    zone_attempts <- rep(NA_integer_, 5)
    top_attempts <- rep(NA_integer_, 5)
    
    for (cell in boulder_cells) {
      boulder_num <- html_text(html_element(cell, ".asc-route-name"), trim = TRUE) %>%
        str_extract("\\d+") %>%  # extract numeric part only
        as.integer()
      
      if (boulder_num > 5) next  # ignore >5
      zone <- html_text(html_element(cell, ".zone.zoned"), trim = TRUE) %>% as.integer()
      top <- html_text(html_element(cell, ".top.topped"), trim = TRUE) %>% as.integer()
      zone_attempts[boulder_num] <- zone
      top_attempts[boulder_num] <- top
    }
    
    tib <- tibble(
      rank = rank,
      athlete = athlete,
      athlete_id = athlete_id,
      gender = gender,
      round = round,
      country = country,
      b1_zone = zone_attempts[1], b1_top = top_attempts[1],
      b2_zone = zone_attempts[2], b2_top = top_attempts[2],
      b3_zone = zone_attempts[3], b3_top = top_attempts[3],
      b4_zone = zone_attempts[4], b4_top = top_attempts[4],
      b5_zone = zone_attempts[5], b5_top = top_attempts[5]
    )
    
    if (round %in% c("S", "F")) {
      tib <- tib %>% select(-b5_zone, -b5_top)
    }
    
    tib
  })
}

add_event_to_results_alt <- function(event_id) {
  
  message("Processing event ", event_id, " ...")
  
  # ---- Event date ----
  event_date <- get_event_end_date(event_id)
  event_year <- as.integer(format(event_date, "%Y"))
  ranking_year <- event_year - 1
  
  # ---- All CR ids ----
  all_cr_ids <- get_cr_ids(event_id)
  
  # If starting from scratch, scrape everything
  if (!exists("all_results", inherits = TRUE) || nrow(all_results) == 0) {
    cr_ids <- all_cr_ids
  } else {
    # Otherwise, scrape only missing CRs
    existing_cr_ids <- all_results %>%
      filter(event_id == !!event_id) %>%
      pull(cr_id) %>%
      unique()
    
    cr_ids <- setdiff(all_cr_ids, existing_cr_ids)
  }
  
  if (length(cr_ids) == 0) {
    message("No CRs to scrape for event ", event_id)
    return(NULL)
  }
  
  # ---- Scrape all CRs ----
  cr_results <- map_df(cr_ids, function(cr_id) {
    cr_url <- paste0("https://ifsc.results.info/event/", event_id, "/cr/", cr_id)
    tryCatch({
      scrape_ifsc_boulder_alt(cr_url) %>%
        mutate(
          event_id = event_id,
          cr_id = cr_id,
          cr_url = cr_url,
          event_date = event_date,
          event_year = event_year,
          ranking_year = ranking_year
        )
    }, error = function(e) {
      message("  Skipped CR ", cr_id, ": ", e$message)
      NULL
    })
  })
  
  if (nrow(cr_results) == 0) {
    message("No valid CR results for event ", event_id)
    return(NULL)
  }
  
  # ---- Merge athlete metadata ----
  cr_results <- cr_results %>%
    left_join(athlete_meta, by = "athlete_id", suffix = c("", "_meta")) %>%
    left_join(
      athlete_boulder_rankings %>% rename(ranking_year = year),
      by = c("athlete_id", "ranking_year"),
      suffix = c("", "_rank")
    ) %>%
    mutate(
      age_at_event = if_else(!is.na(birth_year) & !is.na(event_date),
                             event_year - birth_year,
                             NA_integer_)
    )
  
  # Event title
  cr_results <- cr_results %>%
    mutate(event_id = as.integer(event_id)) %>%
    left_join(
      events_filtered %>% 
        mutate(event_id = as.integer(event_id)) %>% 
        select(event_id, event_title = title),
      by = "event_id"
    )
  
  # ---- Append to main results ----
  assign(
    "all_results",
    bind_rows(all_results, cr_results),
    envir = .GlobalEnv
  )
  
  # ---- Save checkpoint ----
  saveRDS(all_results, results_file)
  
  message("Event ", event_id, " successfully added.")
  return(cr_results)
}

# 992 specific
scrape_992_cr4312 <- function(url = "https://ifsc.results.info/event/992/cr/4312") {
  
  page <- read_html_js(url)
  
  # Extract round (qual/semi/final)
  round_text <- page %>%
    html_element("div.round-name") %>%
    html_text(trim = TRUE)
  
  round <- case_when(
    str_detect(round_text, regex("^qual", ignore_case = TRUE)) ~ "Q",
    str_detect(round_text, regex("^semi", ignore_case = TRUE)) ~ "S",
    str_detect(round_text, regex("^final", ignore_case = TRUE)) ~ "F",
    TRUE ~ NA_character_
  )
  
  # Extract athlete rows and boulder details
  rows <- html_elements(page, "table.table tr.r-row")
  details <- html_elements(page, "table.table tr[class*='boulder-asc-detail']")
  
  if (length(rows) == 0) {
    stop("No athlete rows found on CR 4312")
  }
  
  # If there are no boulder details, fill NA
  if (length(details) == 0) {
    message("No boulder details found, filling NAs")
    details <- rep(list(NULL), length(rows))
  }
  
  n <- min(length(rows), length(details))
  
  map_df(seq_len(n), function(i) {
    
    row <- rows[[i]]
    detail <- details[[i]]
    
    # Athlete info
    rank <- html_text(html_element(row, "td.rank"), trim = TRUE)
    athlete_node <- html_element(row, "a.r-name")
    athlete <- html_text(athlete_node, trim = TRUE)
    athlete_id <- html_attr(athlete_node, "href") %>% str_extract("\\d+") %>% as.integer()
    country <- html_text(html_element(row, ".r-name-sub span:nth-child(2)"), trim = TRUE)
    
    # Initialize boulder attempts
    zone_attempts <- rep(NA_integer_, 5)
    top_attempts <- rep(NA_integer_, 5)
    
    if (!is.null(detail)) {
      boulder_cells <- html_elements(detail, ".asc-cell-container")
      
      for (cell in boulder_cells) {
        boulder_num <- html_text(html_element(cell, ".asc-route-name"), trim = TRUE) %>% as.integer()
        if (is.na(boulder_num) || boulder_num > 5) next
        
        zone <- html_text(html_element(cell, ".zone.zoned"), trim = TRUE) %>% as.integer()
        top  <- html_text(html_element(cell, ".top.topped"), trim = TRUE) %>% as.integer()
        zone_attempts[boulder_num] <- zone
        top_attempts[boulder_num] <- top
      }
    }
    
    tibble(
      rank = rank,
      athlete = athlete,
      athlete_id = athlete_id,
      gender = "M",
      round = round,
      country = country,
      b1_zone = zone_attempts[1], b1_top = top_attempts[1],
      b2_zone = zone_attempts[2], b2_top = top_attempts[2],
      b3_zone = zone_attempts[3], b3_top = top_attempts[3],
      b4_zone = zone_attempts[4], b4_top = top_attempts[4],
      b5_zone = zone_attempts[5], b5_top = top_attempts[5]
    )
    
  })
}

# Example usage
cr4312_results <- scrape_992_cr4312()


library(openxlsx)
write.xlsx(all_results, "ifsc_boulder_results.xlsx")