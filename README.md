Discover-check-monitor-OA tool
For Pure and OpenAlex Data Integration

This R script facilitates the integration of data between OpenAlex and Pure, enabling librarians, researchers, and administrators to validate, enhance, and monitor publication metadata by comparing records from both systems. It performs the following operations:

    Retrieves DOIs from OpenAlex, filtering by a user defined institutional ROR ID and date range.
    Queries DOIs from OpenAlex.
    Retrieves publication metadata and organizational details from the Pure API.
    Merges and updates the retrieved data into a consolidated dataset, stored in a user defined working directory.
    Outputs a merged CSV file for further analysis/data visualisation.

Table of Contents

    Features
    Requirements
    Setup
    Usage
    Outputs
    Troubleshooting
    Future Improvements

Features

    Dynamic Input Handling:
        GUI prompts for entering API URL and API key.
    DOI Lookup:
        Iterates over a list of DOIs and retrieves associated metadata from Pure.
    Publication Metadata Extraction:
        Captures key publication dates, embargo details, license types, and file versions.
    Organization Hierarchy Parsing:
        Traverses organizational structures (e.g., Schools, Faculties) within Pure to assign detailed metadata.
    Data Merging:
        Consolidates new data with existing records, ensuring no duplicates and prioritizing non-missing values.
    Error Handling:
        Safeguards against missing or malformed API responses.

Requirements
Software

    R: Version 4.0 or later
    R Packages:
        httr
        jsonlite
        tcltk
        dplyr (optional for cleaner data manipulation)
        xml2 (optional for Soton version with web scraping)
        stringr (optional for Soton version with web scraping)

Data

    Input Files:
        A master data file (master_open_alex_pure.csv) for storing cumulative results.
        To initiate the master, rename a copy of your first output from this script master_open_alex_pure.csv. Subsequent outputs will be appended and records will be deduplicated in case of overlap between datasets.

Setup

    Clone this repository or download the script.
    Ensure the required R packages are installed:

    install.packages(c("httr", "jsonlite", "tcltk"))

    Prepare input files:
               master_open_alex_pure.csv: A pre-existing master data file or an empty CSV with DOI as the primary column (see above).

Usage

    Run the Script:
        Execute the script in RStudio or your preferred R environment:

        source("path_to_script.R")

    Set wd:
        The script will prompt you to select the folder you would like to be your working directory in a file browser window.

    Input to inform OpenAlex API query:
        The script will prompt you to enter:
            eMail, to be added to OpenAlex's polite list.
            The date range being studied, from date1 to date2.
            The institutional ROR ID. The script will print the name of the research organisation to confirm it was entered correctly.
    Input Pure API Credentials:
        The script will prompt you to enter:
            API URL path. You must use this format: "https://pure.<HEI>.ac.uk/ws/api/".
            API Key.
    Processing:
        The script will:
            Retrieve metadata for each DOI.
            Parse publication, funder, and organization details.
            Merge the data into a master dataset.
    Output:
        Review the updated data in the generated output files.

Outputs
Generated Files

    Results File:
        Filename: <YYYY-MM-DD>_from_<start_date>_to_<end_date>_open_alex_pure.csv (<YYYY-MM-DD> = system date)
        Contains results for the current execution, including:
            Publication details (e.g., title, authors, venue, publication dates, embargo periods, license types).
            UK funder details, for funders with open access policy requirements.
            Organization hierarchy (e.g., Schools, Faculties).
            Open access details (e.g. OpenAlex recorded OA status, colour, license, repository, Pure recorded accepted manuscript (P.AM), embargo periods, and licenses).
            For the Soton version, the ePrints URI is also recorded through web scraping.

    Updated Master File:
        Filename: master_open_alex_pure.csv
        Consolidates all previous results with the latest data.

Troubleshooting
Common Issues

    Error: Invalid API Response:
        Ensure the API URL and key are correct.
        Verify that your network allows access to the Pure API. Access might be restricted to particular IP ranges and it might be necessary to use a VPN while running the script.

    Missing or Empty Data Columns:
        Check the structure of the API response; malformed or missing fields can lead to NA values.

    Performance Issues:
        For large datasets, reduce the batch API calls to improve efficiency. The script combines a master output, so users need to judge whether it is more efficient to run more smaller batches (i.e. reducing the date range) or fewer larger batches (i.e. increasing the date range). For UK research intensives, date ranges of 3-months are recommended to align with the REF exercise and keep datasets manageable.

    Duplicate Records in Output:
        The script ensures DOIs are unique in input and master files.

Logging

    Logging is limited during loops. If you prefer to log loops to capture unexpected issues, use print() statements or logging functions e.g.

    print(paste("Processing DOI:", DOI))

Future Improvements

    Check or improve:
        How is the script handling rows with NA in the DOI field? Is it retaining all unique rows, or only one?
        Fuzzy match on the title to a Pure record where there is no DOI recorded in Pure. Perhaps use ePrints search if Pure API searching is not possible?
        Extract the AM file deposit date from the PURE API.        
    Post Processing:
        Implement conditional logging to indicate if outputs have met funder OA requirements, REF requirements etc. Also, log actions to be taken in the repository.
    Enhanced Error Logging:
        Integrate a logging library to capture detailed error messages.
    Script management:
        Improve the script to manage its functions more efficiently.
