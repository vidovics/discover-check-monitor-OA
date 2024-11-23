#Discover-Check-Monitor OA 0.9.1.
#This is a beta version. The version schema is major.minor.patch e.g. 1.0.9.

#Open all the packages and dependencies to run the script
{
library(httr)
library(xml2)
library(jsonlite)
library(tcltk)
library(stringr)
library(dplyr)
}

#Set the working directory. This script will save, backup and merge files.
{
wd<-choose.dir()
setwd(wd)
print(paste0("You set ",wd," as your working directory"))
}

#Set date and institutional parameters.

#Start date
{
get_date1 <- function() {
  tt <- tktoplevel()
  tkwm.title(tt, "Date range start date")
  label <- tklabel(tt, text = "Enter the start date for your date range 'yyyy-mm-dd'")
  tkpack(label, padx = 10, pady = 5)
  date1_var <- tclVar()
  entry <- tkentry(tt, textvariable = date1_var, width = 50)
  tkpack(entry, padx = 10, pady = 5)
  on_ok <- function() {
    tkdestroy(tt)
  }
  ok_button <- tkbutton(tt, text = "OK", command = on_ok)
  tkpack(ok_button, pady = 10)
  tkwait.window(tt)
  return(tclvalue(date1_var))
}
# Get start date for the api using the pop-up box
date1 <- get_date1()
}

#End date
{
get_date2 <- function() {
  tt <- tktoplevel()
  tkwm.title(tt, "Date range end date")
  label <- tklabel(tt, text = "Enter the end date for your date range 'yyyy-mm-dd'")
  tkpack(label, padx = 10, pady = 5)
  date2_var <- tclVar()
  entry <- tkentry(tt, textvariable = date2_var, width = 50)
  tkpack(entry, padx = 10, pady = 5)
  on_ok <- function() {
    tkdestroy(tt)
  }
  ok_button <- tkbutton(tt, text = "OK", command = on_ok)
  tkpack(ok_button, pady = 10)
  tkwait.window(tt)
  return(tclvalue(date2_var))
}
# Get the end date for the api using the pop-up box
date2 <- get_date2()
}

#Get the ROR ID for the institution being studied. For the University of Southampton use:
#https://ror.org/01ryk1543
{
get_ror <- function() {
  tt <- tktoplevel()
  tkwm.title(tt, "ROR ID")
  label <- tklabel(tt, text = "Enter your institution's ROR ID
                   e.g. University of Southampton: 
                   https://ror.org/01ryk1543")
  tkpack(label, padx = 10, pady = 5)
  ror_var <- tclVar()
  entry <- tkentry(tt, textvariable = ror_var, width = 50)
  tkpack(entry, padx = 10, pady = 5)
  on_ok <- function() {
    tkdestroy(tt)
  }
  ok_button <- tkbutton(tt, text = "OK", command = on_ok)
  tkpack(ok_button, pady = 10)
  tkwait.window(tt)
  return(tclvalue(ror_var))
}
# Get ROR for the api using the pop-up box
ror <- get_ror()
institutionapi<-(GET(paste0("https://api.openalex.org/institutions/",ror)))
institution_content <- content(institutionapi, "text", encoding = "UTF-8")

# Parse the JSON content
institution_JSON <- fromJSON(institution_content)  
local_affil<-institution_JSON[["display_name"]]
print(paste("You have chosen the institution:", local_affil))
}

#This is redundant but potentially useful for future development
# Define the pattern to match quoted strings starting with 10.
doi_pattern <- '"10\\.[^"]+"'  # Matches "10." followed by any characters until the next quote


# Function to create a pop-up input box for the email to get into the polite pool. Comment out for common pool.
{
get_email <- function() {
  tt <- tktoplevel()
  tkwm.title(tt, "Enter your email to join the polite pool")
  label <- tklabel(tt, text = "Email:")
  tkpack(label, padx = 10, pady = 5)
  email_var <- tclVar()
  entry <- tkentry(tt, textvariable = email_var, width = 50)
  tkpack(entry, padx = 10, pady = 5)
  on_ok <- function() {
    tkdestroy(tt)
  }
  ok_button <- tkbutton(tt, text = "OK", command = on_ok)
  tkpack(ok_button, pady = 10)
  tkwait.window(tt)
  return(tclvalue(email_var))
}

# Get email for the api using the pop-up box
email <- get_email()
}

#GET an initial response from the Open Alex api for setting parameters.
{
  api_endpoint <- paste0("https://api.openalex.org/works?filter=from_publication_date:",date1,",to_publication_date:",date2,",institutions.ror:",ror)
  response <- GET(api_endpoint, add_headers(paste0("mailto=",email)))
}

{
  #Read the first page of an api call to establish the number of pages
   if (status_code(response) == 200) {
    # Extract the content as text
    content <- content(response, "text", encoding = "UTF-8")
    
    # Parse the JSON content
    json_data <- fromJSON(content)  
   }
#Determine the number of pages to loop through
  if (round(json_data[["meta"]][["count"]]/25)<(json_data[["meta"]][["count"]]/25)){
    pages<-round(json_data[["meta"]][["count"]]/25)+1
  }else{
    pages<-round(json_data[["meta"]][["count"]]/25)
  }
#setup dataframe for a full API call
  publication_data <- data.frame(
    Authors = character(),
    Title = character(),
    Publisher = character(),
    Sourcetitle = character(),
    Type = character(),
    DOI = character(),
    Hyperlink.DOI = character(),
    Correspondingaffil = character(),
    CollaboratingHEIs = character(),
    Pubdate = character(),
    Is.OA = character(),
    OA.colour = character(),
    License = character(),
    Any.repository = character(),
    Repositorylist = character(),
    Datasets = character(),
    SDG = character(),
    Funder = character()

)
} 

  #loop through the API query to return pages
  for (i in 1:pages){
    api_endpoint <- paste0("https://api.openalex.org/works?filter=from_publication_date:",date1,",to_publication_date:",date2,",institutions.ror:",ror,"&page=",i)
    response <- GET(api_endpoint, add_headers(paste0("mailto=",email)))
    
    if (status_code(response) == 200) {
      # Extract the content as text
      content <- content(response, "text", encoding = "UTF-8")
      
      # Parse the JSON content
      json_data <- fromJSON(content)  
    }
    
    #Collect relevant fields from Open Alex API binding them to the dataframe
    
for (n in 1:(sum(sapply(json_data[["results"]][["id"]],length)))){
  
  
            Authors <- paste(c(json_data[["results"]][["authorships"]][[n]][["raw_author_name"]]), collapse = ", ")
            Title<-json_data[["results"]][["title"]][[n]]
            Sourcetitle<- if (is.list(json_data[["results"]][["locations"]][[n]][["source"]])) {paste(c(json_data[["results"]][["locations"]][[n]][["source"]][["display_name"]][[1]]))}else{NA}
            doctype<-json_data[["results"]][["type_crossref"]][[n]]
            publisher<-json_data[["results"]][["primary_location"]][["source"]][["host_organization_name"]][[n]]
            Hyperlink.DOI<-json_data[["results"]][["doi"]][[n]]
            DOI<-gsub("^.*?(10\\.)", "10.", Hyperlink.DOI)
            Correspondingaffil<- if (length(json_data[["results"]][["corresponding_institution_ids"]][[n]])==0){paste0(NA)}else{paste0(json_data[["results"]][["corresponding_institution_ids"]][[n]])}
            CollaboratingHEIs<-json_data[["results"]][["institutions_distinct_count"]][[n]]
            publication.date<-json_data[["results"]][["publication_date"]][[n]]
            Is.OA<-json_data[["results"]][["best_oa_location"]][["is_oa"]][[n]]
            OA.colour<-paste(c(json_data[["results"]][["open_access"]][["oa_status"]][[n]]))
            license<-paste(c(json_data[["results"]][["best_oa_location"]][["license"]][[n]]))
            Any.repository<-json_data[["results"]][["open_access"]][["any_repository_has_fulltext"]][[n]]
            repositorylist<-if (is.list(json_data[["results"]][["locations"]][[n]][["source"]])) {paste0(json_data[["results"]][["locations"]][[n]][["source"]][["display_name"]][-1],collapse = "; ")}else{NA}
            Datasets<-if (length(json_data[["results"]][["datasets"]][[n]])==0){NA}else{paste(json_data[["results"]][["datasets"]][[n]], collapse = "; ")}
            SDG<- if (is.null(json_data[["results"]][["sustainable_development_goals"]][[n]][["display_name"]])){NA}else{paste(c(json_data[["results"]][["sustainable_development_goals"]][[n]][["display_name"]]),collapse = "; ")}
            Funder<-paste(c(json_data[["results"]][["grants"]][[n]][["funder_display_name"]]), collapse = "; ")
            
            
   publication_data <- rbind(publication_data,data.frame(Authors=Authors,Title=Title,Sourcetitle=Sourcetitle,Publisher=publisher,Type=doctype,DOI=DOI,Hyperlink.DOI=Hyperlink.DOI,Correspondingaffil=Correspondingaffil,CollaboratingHEIs=CollaboratingHEIs,Pubdate=publication.date,Is.OA=Is.OA,OA.colour=OA.colour,License=license,Any.repository=Any.repository,Repositorylist=repositorylist,Datasets=Datasets,SDG=SDG,Funder=Funder,stringsAsFactors = FALSE))
}
  }


#To retrieve affiliation names, an api GET request has to be created

  publication_data$Correspondingaffil <- lapply(publication_data$Correspondingaffil, function(x) {
   # Make the API call
    response <- GET(paste0("https://api.openalex.org/institutions/", x), add_headers(mailto = email))
    

    # Check if the response is successful
    if (status_code(response) == 200) {
      # Get and print raw content for clarity
      raw_content <- content(response, "text", encoding = "UTF-8")

      
      # Parse JSON content
      inst_data <- tryCatch(
        fromJSON(raw_content),
        error = function(e) {
          print(paste("JSON parsing failed for ID:", x))
          print(e)
          return("JSON parsing failed for ID:", x)
        }
      )
      
      # If JSON parsing was successful
      if (!is.null(inst_data)) {
        
        # Extract 'display_name' and 'country'
        display_name <- inst_data[["display_name"]]
        country <- inst_data[["geo"]][["country"]]
        
        # If either value is missing, return NA
        if (is.null(display_name) || is.null(country)) {
          return(x)
        } else {
          result <- paste0(display_name, ", ", country)
          return(result)
        }
      } else {
        return(x)  # Return NA if JSON parsing failed
      }
      
    } else {
      return(x)  # Return NA if request failed
    }
  })
  
  publication_data_combined <- publication_data %>%
    group_by(DOI) %>%  # Group by the DOI or other unique identifier for each publication
    summarize(
      Authors = first(Authors),
      Title = first(Title),
      Sourcetitle = first(Sourcetitle),
      Publisher = first(Publisher),
      Type = first(Type),
      Hyperlink.DOI = first(Hyperlink.DOI),
      Correspondingaffil = paste(unique(Correspondingaffil), collapse = "; "),
      CollaboratingHEIs = first(CollaboratingHEIs),
      Pubdate = first(Pubdate),
      Is.OA = first(Is.OA),
      OA.colour = first(OA.colour),
      License = first(License),
      Any.repository = first(Any.repository),
      Repositorylist = paste(unique(Repositorylist), collapse = "; "),
      Datasets = paste(unique(Datasets), collapse = "; "),
      SDG = first(SDG),
      Funder = first(Funder),
      .groups = 'drop'  # Ensure it un-groups the data at the end
    )


  
  {
#Determine if there is a corresponding author at the local institution
  publication_data_combined$Local.corresponding<-NA

    # Check for multiple corresponding author affiliations in each row
  publication_data_combined$Local.corresponding <- ifelse(
    sapply(publication_data_combined$Correspondingaffil, function(localaffil) {
      any(trimws(unlist(strsplit(localaffil, "[;,]"))) %in% local_affil)
    }),
    "Yes",
    "No"
  )
  publication_data_combined<-publication_data_combined %>% relocate(Local.corresponding, .after = Correspondingaffil)
    }

{  

  #Convert "Bronze" to indeterminate OA
  publication_data_combined$OA.colour[publication_data_combined$OA.colour == "bronze"] <- "indeterminate"
  
    #Dataset identified yes/no
  publication_data_combined$DatasetStatus <- ifelse(
    is.na(publication_data_combined$Datasets) | publication_data_combined$Datasets == "NA",
    "Not identified",
    "Yes"
  )
  publication_data_combined<-publication_data_combined %>% relocate(DatasetStatus, .before = Datasets)
  
}
  
#Funder Yes/No processing
{  #UKRI
    publication_data_combined$UKRI<-NA

  ukri_funders <- c(
    "UK Research and Innovation",
    "Engineering and Physical Sciences Research Council",
    "Natural Environment Research Council",
    "Medical Research Council",
    "Economic and Social Research Council",
    "Arts and Humanities Research Council",
    "Biotechnology and Biological Sciences Research Council",
    "Science and Technology Facilities Council"
  )
  
  # Check for multiple funders in each row
  publication_data_combined$UKRI <- ifelse(
    sapply(publication_data_combined$Funder, function(funder) {
      any(trimws(unlist(strsplit(funder, ";"))) %in% ukri_funders)
    }),
    "Yes",
    "No"
  )
  }
  
{  #Wellcome Trust
  publication_data_combined$Wellcome<-NA
  WT_name_var <- c(
    "Wellcome Trust",
    "Wellcome"
  )
  
  # Check for multiple funders in each row
  publication_data_combined$Wellcome <- ifelse(
    sapply(publication_data_combined$Funder, function(funder) {
      any(trimws(unlist(strsplit(funder, ";"))) %in% WT_name_var)
    }),
    "Yes",
    "No"
  )
  }
  
{  #NIHR
  publication_data_combined$NIHR<-NA
  NIHR_name_var <- c(
    "National Institute for Health Research Southampton Biomedical Research Centre",
    "National Institute for Health and Care Research"
  )
  
  # Check for multiple funders in each row
  publication_data_combined$NIHR <- ifelse(
    sapply(publication_data_combined$Funder, function(funder) {
      any(trimws(unlist(strsplit(funder, ";"))) %in% NIHR_name_var)
    }),
    "Yes",
    "No"
  )
  }
  
{  #CRUK
  publication_data_combined$CRUK<-NA
  CRUK_name_var <- c(
    "Cancer Research UK"
    )
  
  # Check for multiple funders in each row
  publication_data_combined$CRUK <- ifelse(
    sapply(publication_data_combined$Funder, function(funder) {
      any(trimws(unlist(strsplit(funder, ";"))) %in% CRUK_name_var)
    }),
    "Yes",
    "No"
  )
  }
  
  {  #BHF
    publication_data_combined$BHF<-NA
    BHF_name_var <- c(
      "British Heart Foundation"
    )
    
    # Check for multiple funders in each row
    publication_data_combined$BHF <- ifelse(
      sapply(publication_data_combined$Funder, function(funder) {
        any(trimws(unlist(strsplit(funder, ";"))) %in% BHF_name_var)
      }),
      "Yes",
      "No"
    )
  }
  ##############
{
  target_dois_df <- data.frame(
         target_doi = publication_data_combined$DOI
    )
  
  #prepare the data frame for adding content from Pure
  target_dois_df$target_doi<-tolower(target_dois_df$target_doi)
  target_dois_df$P.uuid<-NA
  target_dois_df$P.accepted<-NA
  target_dois_df$P.Epub<-NA
  target_dois_df$P.pub<-NA
  target_dois_df$P.Access<-NA
  target_dois_df$P.AM<-NA
  target_dois_df$P.AMembargo<-NA
  target_dois_df$P.AMlicense<-NA
  target_dois_df$P.VoR<-NA
  target_dois_df$P.VoRlicense<-NA
  target_dois_df$P.embargo<-NA
  target_dois_df$P.type<-NA
  target_dois_df$P.org<-NA
  target_dois_df$P.schools<-NA
  target_dois_df$P.faculties<-NA
  target_dois_df$record_number<-NA
  target_dois_df<-target_dois_df[!is.na(target_dois_df$target_doi), ]
  row.names(target_dois_df)<-NULL
}
##################################
  #Pure matching
  #Example request body below
#  {
#    "size": 10,
#    "offset": 0,
#    "orderings": [
#      "created"
#    ],
#    "searchString": "10.1371/journal.pone.0145713",
#    "orderBy": "ascending"
#  }
  #Add values for the API endpoint
  url <- "https://pure.soton.ac.uk/ws/api/research-outputs/search"
  {
    api_key <- function() {
      tt <- tktoplevel()
      tkwm.title(tt, "Your personal Pure API key")
      label <- tklabel(tt, text = "API key:")
      tkpack(label, padx = 10, pady = 5)
      api_key_var <- tclVar()
      entry <- tkentry(tt, textvariable = api_key_var, width = 50)
      tkpack(entry, padx = 10, pady = 5)
      on_ok <- function() {
        tkdestroy(tt)
      }
      ok_button <- tkbutton(tt, text = "OK", command = on_ok)
      tkpack(ok_button, pady = 10)
      tkwait.window(tt)
      return(tclvalue(api_key_var))
    }
    # Get the end date for the api using the pop-up box
    api_key <- api_key()
  }

  #Loop through the target_dois_df searching Pure
  for (L in 1:length(target_dois_df$target_doi)){
  #Define the DOI to be searched
  DOI<-target_dois_df$target_doi[L]
    # Define the body as a list and convert it to JSON
  body <- list(
    size = 100,
    offset = 0,
    orderings = list("created"),
    searchString = DOI,
    orderBy = "ascending"
  )
  body <- toJSON(body, auto_unbox = TRUE, pretty = TRUE)
  
  # Perform the POST request
  response <- POST(
    url = url,
    body = body,
    encode = "json",
    add_headers(
      "Content-Type" = "application/json",
      "accept" = "application/json",
      "api-key" = api_key
    )
  )
  content <- content(response, "text", encoding = "UTF-8")
  
  # Parse the JSON content
  json_data <- fromJSON(content)
#Stop the loop if the json returned has no results
    if (response[["status_code"]] != 200 || json_data[["count"]]==0){} else {
  for (r in 1:json_data[["count"]]){
  #Extract the DOIs from a list of records in cases where the search recovers more than one record
  for (t in 1:length(json_data[["items"]][["electronicVersions"]][[r]][["doi"]])){
    
    dois <- json_data[["items"]][["electronicVersions"]][[r]][["doi"]][[t]]
  
  
 
  
  #END
  if (length(dois) == 0 || is.null(dois) || all(is.na(dois))){} else{ 
  if (tolower(dois) == tolower(target_dois_df$target_doi[L])){
  #match DOIs from the list against the table of DOIs from Open Alex and record the Json index number
  #dois<-tolower(dois)
    
      target_dois_df$record_number[L] <- r #rbind(match(target_dois_df$target_doi[L], dois))

    dates<-as.data.frame(json_data[["items"]][["publicationStatuses"]][[target_dois_df$record_number[L]]][["publicationDate"]])
    row.names(dates)<-json_data[["items"]][["publicationStatuses"]][[target_dois_df$record_number[L]]][["publicationStatus"]][["term"]][["en_GB"]]
    if (any(row.names(dates) == "Accepted/In press",na.rm = TRUE)) {target_dois_df$P.accepted[L]<-gsub("-NA","",paste0(dates["Accepted/In press",1],"-",dates["Accepted/In press",2],"-",dates["Accepted/In press",3]))}
    if (any(row.names(dates) == "E-pub ahead of print",na.rm = TRUE)) {target_dois_df$P.Epub[L]<-gsub("-NA","",paste0(dates["E-pub ahead of print",1],"-",dates["E-pub ahead of print",2],"-",dates["E-pub ahead of print",3]))}
    if (any(row.names(dates) == "Published",na.rm = TRUE)) {target_dois_df$P.pub[L]<-gsub("-NA","",paste0(dates["Published",1],"-",dates["Published",2],"-",dates["Published",3]))
    }
    
    filesdf<-data.frame("version" = character(),
                        "file.license" = character(),
                        "file.embargo" = character(),
                        stringsAsFactors = FALSE)
    for (z in 1:length(json_data[["items"]][["electronicVersions"]][[target_dois_df$record_number[L]]][["versionType"]][["term"]][["en_GB"]])){
      version<-if (is.null(json_data[["items"]][["electronicVersions"]][[target_dois_df$record_number[L]]][["versionType"]][["term"]][["en_GB"]][[z]])){
        NA
      }else{
        json_data[["items"]][["electronicVersions"]][[target_dois_df$record_number[L]]][["versionType"]][["term"]][["en_GB"]][[z]]
      }
      file_license<- if (is.null(json_data[["items"]][["electronicVersions"]][[target_dois_df$record_number[L]]][["licenseType"]][["term"]][["en_GB"]][[z]])){
        NA
      }else{
        json_data[["items"]][["electronicVersions"]][[target_dois_df$record_number[L]]][["licenseType"]][["term"]][["en_GB"]][[z]]
      }
      
      file_embargo<- if (is.null(json_data[["items"]][["electronicVersions"]][[target_dois_df$record_number[L]]][["embargoPeriod"]][["endDate"]][[z]])){
        NA
      }else{
        json_data[["items"]][["electronicVersions"]][[target_dois_df$record_number[L]]][["embargoPeriod"]][["endDate"]][[z]]
      }
    
      filesdf <- rbind(filesdf, data.frame(
        version = version,
        file.license = file_license,
        file.embargo = file_embargo,
        stringsAsFactors = FALSE
      ))
    }
      
    if (any(filesdf$version == "Accepted manuscript",na.rm = TRUE)){
      AMrow<-which(filesdf$version == "Accepted manuscript")
      target_dois_df$P.AM[L]<-"Yes"
      target_dois_df$P.AMembargo[L]<-filesdf$file.embargo[AMrow]
      target_dois_df$P.AMlicense[L]<-filesdf$file.license[AMrow]
    }else{
      target_dois_df$P.AM[L]<-"No"
    }
    
    if (any(filesdf$version == "Version of Record",na.rm = TRUE)){
      VoRrow<-which(filesdf$version == "Version of Record")
      target_dois_df$P.VoR[L]<-"Yes"
      target_dois_df$P.VoRlicense[L]<-filesdf$file.license[VoRrow]
    }else{
      target_dois_df$P.VoR[L]<-"No"
    }
    ########
    Orgstore <- data.frame("Orgs" = character(),
                           "Schools" = character(),
                           "Faculties" = character(),
                           stringsAsFactors = FALSE)
    
    # Function to fetch organization details and return its name and parent UUID
    fetch_org_details <- function(org_uuid, api_key) {
      org_url <- paste0("https://pure.soton.ac.uk/ws/api/organizations/", org_uuid)
      
      org_json <- fromJSON(
        content(
          GET(org_url,
              add_headers("accept" = "application/json", "api-key" = api_key)),
          "text", encoding = "UTF-8"
        )
      )
      
      if (is.null(org_json[["name"]][["en_GB"]])) return(NULL)
      
      list(
        name = org_json[["name"]][["en_GB"]],
        parent_uuid = org_json[["parents"]][["uuid"]],
        type = org_json[["type"]][["term"]][["en_GB"]]
      )
    }
    
    # Loop through organizations
    for (z in 1:length(json_data[["items"]][["organizations"]][[target_dois_df$record_number[L]]][["uuid"]])) {
      org_uuid <- json_data[["items"]][["organizations"]][[target_dois_df$record_number[L]]][["uuid"]][[z]]
      
      # Fetch initial organization details
      initial_org_details <- fetch_org_details(org_uuid, api_key)
      
      if (is.null(initial_org_details)) next
      
      # Initialize variables
      initial_org_name <- initial_org_details$name
      school_name <- NULL
      faculty_name <- NULL
      
      # Traverse up the hierarchy
      while (!is.null(org_uuid)) {
        org_details <- fetch_org_details(org_uuid, api_key)
        
        if (is.null(org_details)) break
        
        org_type <- org_details$type
        org_uuid <- org_details$parent_uuid # Move to parent org for the next iteration
        
        if (org_type == "School" && is.null(school_name)) {
          school_name <- org_details$name
        } else if (org_type == "Faculty" && is.null(faculty_name)) {
          faculty_name <- org_details$name
        }
      }
      
      # Add results to the dataframe
      Orgstore <- rbind(Orgstore, data.frame(
        Orgs = initial_org_name, # Use the initial organization's name here
        Schools = ifelse(is.null(school_name), NA, school_name),
        Faculties = ifelse(is.null(faculty_name), NA, faculty_name),
        stringsAsFactors = FALSE
      ))
    }
    # Add orgs to the target data frame
    target_dois_df$P.org[L] <- gsub(", ", "; ", toString(unique(Orgstore$Orgs)))
    target_dois_df$P.schools[L] <- gsub(", ", "; ", toString(unique(Orgstore$Schools)))
    target_dois_df$P.faculties[L] <- gsub(", ", "; ", toString(unique(Orgstore$Faculties)))
    
    # finalise content to be added to the target data frame
    target_dois_df$P.uuid[L]<- json_data[["items"]][["uuid"]][[target_dois_df$record_number[L]]]
    target_dois_df$P.Access[L]<-json_data[["items"]][["electronicVersions"]][[target_dois_df$record_number[L]]][["accessType"]][["term"]][["en_GB"]][[1]]
    target_dois_df$P.type[L]<-json_data[["items"]][["type"]][["term"]][["en_GB"]][[target_dois_df$record_number[L]]]
    
    
    if (is.null(json_data[["items"]][["electronicVersions"]][[target_dois_df$record_number[L]]][["embargoPeriod"]][["endDate"]]
)){}else{
    target_dois_df$P.embargo[L]<-paste0(c(json_data[["items"]][["electronicVersions"]][[target_dois_df$record_number[L]]][["embargoPeriod"]]), collapse = "; ")
}
    
    } else{}}
 
  #Close the for loop searching through target_dois_df
  }
  }
  }
  }
  
  
  #Merge and save data frames
  names(target_dois_df)[1] <- "DOI"
  merged_df <- merge(publication_data_combined, target_dois_df, by = "DOI", all = TRUE)
  merged_df$record_number<-NULL
  merged_df$searchdate<-paste(Sys.Date())
  
  write.csv(merged_df,file = paste0(Sys.Date(),"_from_",date1,"_to_",date2,"_open_alex_pure.csv"))
  oldmaster<-read.csv("master_open_alex_pure.csv")
  
  mastermerged <- merge(oldmaster, merged_df, by = "DOI", all = TRUE, suffixes = c("_df1", "_df2"))
  
  # Function to update missing or empty values
  update_column <- function(col_df1, col_df2) {
    ifelse(!is.na(col_df1) & col_df1 != "", col_df1, col_df2)
  }
  
  # Apply the update function to each column (excluding the ID column)
  newmerged <- data.frame(DOI = mastermerged$DOI)  # Start with ID column
  cols <- setdiff(names(mastermerged), "DOI")
  
  for (col in unique(gsub("(_df1|_df2)$", "", cols))) {
    col_df1 <- mastermerged[[paste0(col, "_df1")]]
    col_df2 <- mastermerged[[paste0(col, "_df2")]]
    
    # Ensure inputs to `update_column` are valid
    if (is.null(col_df1) || is.null(col_df2)) {
      warning(paste("Skipping column", col, "due to missing data"))
      newmerged[[col]] <- NA  # Assign NA if column data is missing
      next
    }
    
    updated_column <- update_column(col_df1, col_df2)
    
    # Ensure the returned column matches the row count of newmerged
    if (length(updated_column) != nrow(newmerged)) {
      stop(paste("Column", col, "has mismatched length:", length(updated_column)))
    }
    
    newmerged[[col]] <- updated_column
  }
  
  newmerged$X<-NULL
  
  for (a in 1:nrow(newmerged)){
  if (newmerged$ePrints.ID[a] == "" | is.na(newmerged$ePrints.ID[a])){
    if (is.na(newmerged$P.uuid[a]) | is.null(newmerged$P.uuid[a]) | newmerged$P.uuid[a]==""){
    }else{
    URLs <- as.character(paste0(
      "https://eprints.soton.ac.uk/cgi/search/archive/advanced?",
      "screen=Search&dataset=archive&_action_search=Search&documents_merge=ALL&documents=&eprintid=",
      "&title_merge=ALL&title=&contributors_name_merge=ALL&contributors_name=&abstract_merge=ALL&abstract=",
      "&date=&keywords_merge=ALL&keywords=&divisions_merge=ANY&refereed=EITHER",
      "&publication%2Fseries_name_merge=ALL&publication%2Fseries_name=",
      "&documents.date_embargo=&lastmod=&pure_uuid=", newmerged$P.uuid[a],
      "&contributors_id=&satisfyall=ALL&order=contributors_name%2F-date%2Ftitle"
    ))
    if (URLs == "") {  # Check for value
    } else {
      # Use httr::GET with SSL verification disabled
      response <- GET(URLs, config(ssl_verifypeer = FALSE))
      
      if (http_error(response)) {  # Check for HTTP errors
        print('Error accessing URL!')
      } else {
        pg <- read_xml(content(response, as = "text", encoding = "UTF-8"))  # Scrape the URL as XML
        pg2 <- read_html(content(response, as = "text", encoding = "UTF-8"))  # Scrape the URL as HTML
        
        results <- pg %>% html_nodes(".ep_search_result")  # Isolate the eprints search results
        if (length(results) == 0) {
          newmerged$ePrints.ID[a] <- print('No Record Found!')
        } else {
          links <- html_nodes(pg2, ".ep_search_result a")
          newmerged$ePrints.ID[a] <- unlist(lapply(xml_attrs(links), function(x) data.frame(as.list(x), stringsAsFactors = FALSE)))
            }
          }
        }
      }
    }
  }
  write.csv(newmerged,file = "master_open_alex_pure.csv")
  
  
  # More dataset processing?
  
  #wishlist: RRS, *different script is_retracted
  
