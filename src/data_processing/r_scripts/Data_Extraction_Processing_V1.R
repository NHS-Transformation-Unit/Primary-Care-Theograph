
# Theograph synthetic data preparation

required_packages <- c("dplyr", "here", "readxl", "tibble")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0L) {
  stop(
    "Install the following packages before processing Theograph data: ",
    paste(missing_packages, collapse = ", "),
    call. = FALSE
  )
}

find_current_source_file <- function() {
  source_files <- vapply(
    sys.frames(),
    function(frame) {
      if (is.null(frame$ofile)) NA_character_ else as.character(frame$ofile)
    },
    character(1)
  )
  source_files <- source_files[!is.na(source_files) & nzchar(source_files)]
  if (length(source_files) == 0L) return(NA_character_)
  normalizePath(source_files[[length(source_files)]], winslash = "/", mustWork = FALSE)
}

configured_project_root <- Sys.getenv("THEOGRAPH_PROJECT_ROOT", unset = "")
processing_source_file <- find_current_source_file()

if (nzchar(configured_project_root)) {
  Theograph_Project_Root <- normalizePath(
    configured_project_root, winslash = "/", mustWork = FALSE
  )
} else if (!is.na(processing_source_file)) {
  Theograph_Project_Root <- normalizePath(
    file.path(dirname(processing_source_file), "..", ".."),
    winslash = "/",
    mustWork = FALSE
  )
} else {
  Theograph_Project_Root <- normalizePath(
    here::here(), winslash = "/", mustWork = FALSE
  )
}

`%||%` <- function(x, fallback) {
  if (is.null(x) || length(x) == 0L || is.na(x[[1]]) || !nzchar(x[[1]])) {
    fallback
  } else {
    x
  }
}

resolve_theograph_input <- function() {
  configured_path <- Sys.getenv("THEOGRAPH_DATA_PATH", unset = "")

  data_directories <- unique(c(
    file.path(Theograph_Project_Root, "data", "raw_extracts"),
    file.path(Theograph_Project_Root, "data", "processed_extracts"),
    here::here("data", "raw_extracts"),
    here::here("data", "processed_extracts")
  ))
  exact_candidates <- file.path(
    data_directories, "Theograph_Synthetic_Patient_Dataset.xlsx"
  )
  versioned_candidates <- unlist(lapply(
    data_directories,
    list.files,
    pattern = "^Theograph_Synthetic_Patient_Dataset( ?\\([0-9]+\\))?\\.xlsx$",
    full.names = TRUE
  ), use.names = FALSE)

  candidates <- unique(c(
    configured_path,
    exact_candidates,
    versioned_candidates
  ))
  candidates <- candidates[nzchar(candidates)]
  existing <- candidates[file.exists(candidates)]

  if (length(existing) == 0L) {
    stop(
      paste0(
        "The Theograph workbook could not be found. Put it at ",
        "data/raw_extracts/Theograph_Synthetic_Patient_Dataset.xlsx ",
        "or set THEOGRAPH_DATA_PATH."
      ),
      call. = FALSE
    )
  }

  normalizePath(existing[[1]], winslash = "/", mustWork = TRUE)
}

as_theograph_date <- function(x, field_name) {
  if (inherits(x, "Date")) {
    return(x)
  }

  if (inherits(x, "POSIXt")) {
    return(as.Date(x))
  }

  if (is.numeric(x)) {
    return(as.Date(x, origin = "1899-12-30"))
  }

  parsed <- suppressWarnings(as.Date(as.character(x)))
  if (any(is.na(parsed) & !is.na(x))) {
    stop("Could not parse every value in ", field_name, " as a date.", call. = FALSE)
  }
  parsed
}

normalise_text <- function(x) {
  out <- trimws(as.character(x))
  out[is.na(x)] <- NA_character_
  out
}

assert_columns <- function(data, expected, sheet_name) {
  missing <- setdiff(expected, names(data))
  extra <- setdiff(names(data), expected)

  if (length(missing) > 0L || length(extra) > 0L) {
    details <- c(
      if (length(missing) > 0L) paste("missing:", paste(missing, collapse = ", ")),
      if (length(extra) > 0L) paste("unexpected:", paste(extra, collapse = ", "))
    )
    stop(
      "The ", sheet_name, " sheet does not match the minimum schema (",
      paste(details, collapse = "; "), ").",
      call. = FALSE
    )
  }

  invisible(TRUE)
}

assert_required_values <- function(data, fields, sheet_name) {
  for (field in fields) {
    field_values <- data[[field]]
    missing <- is.na(field_values)
    if (is.character(field_values)) {
      missing <- missing | !nzchar(trimws(field_values))
    }
    if (any(missing)) {
      stop(
        "The ", sheet_name, " sheet has blank values in required field ",
        field, ".",
        call. = FALSE
      )
    }
  }
  invisible(TRUE)
}

validate_theograph_data <- function(patient_profile, events, clinical_notes) {
  assert_required_values(
    patient_profile,
    c(
      "Patient_ID", "Patient_Name", "Patient_DOB", "Patient_Gender",
      "Height_m", "Weight_kg", "Profile_Focus", "BMI"
    ),
    "Patient Profile"
  )
  assert_required_values(
    events,
    c(
      "Patient_ID", "Event_Date", "Event_Type", "Event_Name",
      "Linked_Condition", "Display_Series", "Event_Detail",
      "Display_Order", "Show_On_Timeline"
    ),
    "Events"
  )
  assert_required_values(
    clinical_notes,
    c("Patient_ID", "Note_Date", "Note_Type", "Note_Text"),
    "Clinical Notes"
  )

  if (anyDuplicated(patient_profile$Patient_ID)) {
    stop("Patient_ID must be unique in Patient Profile.", call. = FALSE)
  }

  known_ids <- patient_profile$Patient_ID
  orphan_events <- setdiff(unique(events$Patient_ID), known_ids)
  orphan_notes <- setdiff(unique(clinical_notes$Patient_ID), known_ids)
  if (length(orphan_events) > 0L || length(orphan_notes) > 0L) {
    stop("Every event and clinical note must join to Patient Profile.", call. = FALSE)
  }

  allowed_types <- c("Condition", "Biomarker", "Medicine", "Risk Score")
  unknown_types <- setdiff(unique(events$Event_Type), allowed_types)
  if (length(unknown_types) > 0L) {
    stop(
      "Unsupported Event_Type values: ", paste(unknown_types, collapse = ", "),
      call. = FALSE
    )
  }

  allowed_flags <- c("Yes", "No")
  unknown_flags <- setdiff(unique(events$Show_On_Timeline), allowed_flags)
  if (length(unknown_flags) > 0L) {
    stop("Show_On_Timeline must contain only Yes or No.", call. = FALSE)
  }

  value_without_units <- !is.na(events$Value) &
    (is.na(events$Units) | !nzchar(events$Units))
  if (any(value_without_units)) {
    stop("Every non-missing event Value must have Units.", call. = FALSE)
  }

  qrisk_patients <- events |>
    dplyr::filter(.data$Event_Type == "Risk Score", grepl("QRisk2", .data$Event_Name, ignore.case = TRUE)) |>
    dplyr::distinct(.data$Patient_ID) |>
    dplyr::pull(.data$Patient_ID)
  if (length(setdiff(known_ids, qrisk_patients)) > 0L) {
    stop("Every patient must have at least one QRisk2 event.", call. = FALSE)
  }

  invisible(TRUE)
}

process_theograph_data <- function(input_path = resolve_theograph_input()) {
  expected_sheets <- c(
    "Patient Profile", "Events", "Clinical Notes", "Data Dictionary"
  )
  available_sheets <- readxl::excel_sheets(input_path)

  if (!setequal(expected_sheets, available_sheets)) {
    stop(
      "The source workbook must contain exactly these sheets: ",
      paste(expected_sheets, collapse = ", "),
      call. = FALSE
    )
  }

  patient_profile <- readxl::read_excel(input_path, sheet = "Patient Profile")
  events <- readxl::read_excel(input_path, sheet = "Events")
  clinical_notes <- readxl::read_excel(input_path, sheet = "Clinical Notes")
  data_dictionary <- readxl::read_excel(input_path, sheet = "Data Dictionary")

  assert_columns(
    patient_profile,
    c(
      "Patient_ID", "Patient_Name", "Patient_DOB", "Patient_Gender",
      "Height_m", "Weight_kg", "Profile_Focus", "BMI"
    ),
    "Patient Profile"
  )
  assert_columns(
    events,
    c(
      "Patient_ID", "Event_Date", "Event_Type", "Event_Name",
      "Linked_Condition", "Display_Series", "Value", "Units",
      "Event_Detail", "Display_Order", "Show_On_Timeline"
    ),
    "Events"
  )
  assert_columns(
    clinical_notes,
    c("Patient_ID", "Note_Date", "Note_Type", "Note_Text"),
    "Clinical Notes"
  )
  assert_columns(
    data_dictionary,
    c("Sheet", "Field", "Data_Type", "Required", "Description"),
    "Data Dictionary"
  )

  patient_profile <- patient_profile |>
    dplyr::mutate(
      dplyr::across(dplyr::where(is.character), normalise_text),
      Patient_DOB = as_theograph_date(.data$Patient_DOB, "Patient_DOB"),
      Height_m = as.numeric(.data$Height_m),
      Weight_kg = as.numeric(.data$Weight_kg),
      BMI = round(.data$Weight_kg / (.data$Height_m^2), 1)
    ) |>
    dplyr::arrange(.data$Patient_ID)

  events <- events |>
    dplyr::mutate(
      dplyr::across(dplyr::where(is.character), normalise_text),
      Event_Date = as_theograph_date(.data$Event_Date, "Event_Date"),
      Value = suppressWarnings(as.numeric(.data$Value)),
      Display_Order = as.integer(.data$Display_Order),
      Show_On_Timeline = ifelse(
        tolower(.data$Show_On_Timeline) == "yes", "Yes", "No"
      )
    ) |>
    dplyr::arrange(
      .data$Patient_ID, .data$Display_Order, .data$Event_Date,
      .data$Event_Type, .data$Event_Name
    )

  clinical_notes <- clinical_notes |>
    dplyr::mutate(
      dplyr::across(dplyr::where(is.character), normalise_text),
      Note_Date = as_theograph_date(.data$Note_Date, "Note_Date")
    ) |>
    dplyr::arrange(.data$Patient_ID, dplyr::desc(.data$Note_Date))

  data_dictionary <- data_dictionary |>
    dplyr::mutate(dplyr::across(dplyr::where(is.character), normalise_text))

  validate_theograph_data(patient_profile, events, clinical_notes)

  timeline_events <- events |>
    dplyr::filter(.data$Show_On_Timeline == "Yes")

  series_catalogue <- timeline_events |>
    dplyr::group_by(.data$Patient_ID, .data$Display_Series) |>
    dplyr::summarise(
      Event_Type = dplyr::first(.data$Event_Type),
      Linked_Condition = dplyr::first(.data$Linked_Condition),
      Display_Order = min(.data$Display_Order, na.rm = TRUE),
      Event_Count = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::arrange(.data$Patient_ID, .data$Display_Order, .data$Display_Series)

  qrisk2_events <- events |>
    dplyr::filter(
      .data$Event_Type == "Risk Score",
      grepl("QRisk2", .data$Event_Name, ignore.case = TRUE)
    ) |>
    dplyr::arrange(.data$Patient_ID, .data$Event_Date)

  structure(
    list(
      patient_profile = patient_profile,
      events = events,
      timeline_events = timeline_events,
      series_catalogue = series_catalogue,
      qrisk2_events = qrisk2_events,
      clinical_notes = clinical_notes,
      data_dictionary = data_dictionary,
      source_path = input_path,
      processed_at = Sys.time()
    ),
    class = c("theograph_data", "list")
  )
}

write_theograph_rds <- function(data, output_path = NULL) {
  configured_output_path <- Sys.getenv("THEOGRAPH_PROCESSED_PATH", unset = "")
  default_output_path <- if (nzchar(configured_output_path)) {
    configured_output_path
  } else {
    file.path(
      Theograph_Project_Root, "data", "processed_extracts",
      "Theograph_Processed_Data.rds"
    )
  }
  output_path <- output_path %||% default_output_path

  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  saveRDS(data, output_path)
  normalizePath(output_path, winslash = "/", mustWork = TRUE)
}

Theograph_Data <- process_theograph_data()
Theograph_Processed_Path <- write_theograph_rds(Theograph_Data)

message(
  "Prepared ", nrow(Theograph_Data$patient_profile), " patients, ",
  nrow(Theograph_Data$events), " events, ",
  nrow(Theograph_Data$clinical_notes), " clinical notes and ",
  nrow(Theograph_Data$series_catalogue),
  " patient-specific timeline series."
)
