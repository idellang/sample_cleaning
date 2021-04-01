``` r
library(tidyverse)
library(lubridate)
library(arsenal)
library(data.table)
library(tidyr)
library(broom)
library(knitr)
```

# Explore the data

Load datasets

``` r
june5 = read_csv('Exam List June 5.csv', na = c('','-','NA'))
june6 = read_csv('Exam List June 6.csv', na = c('','-','NA'))
june7 = read_csv('Exam List June 7.csv', na = c('','-','NA'))
```

Check column names

``` r
colnames(june5)
```

    ##  [1] "casenumber"                       "age"                             
    ##  [3] "sex"                              "nationality"                     
    ##  [5] "healthstatus"                     "currentregion"                   
    ##  [7] "currentprovince"                  "currentmunicipalitycity"         
    ##  [9] "regionpsgc"                       "provincepsgc"                    
    ## [11] "municipalitycitypsgc"             "dru"                             
    ## [13] "region_dru"                       "otherreportingunit"              
    ## [15] "confirminglab"                    "comorbidity"                     
    ## [17] "specifycomorbidity"               "fever"                           
    ## [19] "cough"                            "cold"                            
    ## [21] "sorethroat"                       "difficultyofbreathing"           
    ## [23] "diarrhea"                         "none"                            
    ## [25] "othersignssymptomsspecify"        "onset_date"                      
    ## [27] "admitted_date"                    "specimen_date"                   
    ## [29] "datespecimenreceivedbylaboratory" "result_date"                     
    ## [31] "discharged_date"                  "recovered_date"                  
    ## [33] "died_date"                        "quarantined"

June 5 has no columns drucity and druprovince

``` r
colnames(june6)[!(colnames(june6) %in% colnames(june5))]
```

    ## [1] "drucity"     "druprovince"

Add these columns and set to NA

``` r
june5$drucity = NA
june5$druprovince = NA
```

``` r
glimpse(june7)
```

    ## Rows: 22,903
    ## Columns: 36
    ## $ casenumber                       <chr> "PH1062", "PH1511", "PH1512", "PH1...
    ## $ age                              <dbl> 65, 52, 35, 62, 39, 30, 59, 34, 60...
    ## $ sex                              <chr> "Female", "Male", "Male", "Male", ...
    ## $ nationality                      <chr> "Philippine, Filipino", "Philippin...
    ## $ healthstatus                     <chr> "Mild", "Recovered", "Recovered", ...
    ## $ currentregion                    <chr> "CaLaBaRZon", "Davao", "Davao", "D...
    ## $ currentprovince                  <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA...
    ## $ currentmunicipalitycity          <chr> NA, "City of panabo", "City of tag...
    ## $ regionpsgc                       <dbl> 4, 11, 11, 11, 11, 13, 13, 13, 4, ...
    ## $ provincepsgc                     <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA...
    ## $ municipalitycitypsgc             <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA...
    ## $ dru                              <chr> "SAN LAZARO HOSPITAL", "DAVAO REGI...
    ## $ region_dru                       <dbl> 13, 11, 11, 11, 11, 13, 13, 13, 13...
    ## $ drucity                          <chr> "Quezon city", "City of digos (cap...
    ## $ druprovince                      <chr> "NCR, Second District (not a provi...
    ## $ otherreportingunit               <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA...
    ## $ confirminglab                    <chr> "RESEARCH INSTITUTE FOR TROPICAL M...
    ## $ comorbidity                      <chr> "Yes", NA, NA, NA, NA, NA, NA, NA,...
    ## $ specifycomorbidity               <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA...
    ## $ fever                            <chr> NA, "yes", NA, NA, NA, "yes", NA, ...
    ## $ cough                            <chr> "yes", "yes", "yes", "yes", "yes",...
    ## $ cold                             <chr> "yes", NA, "yes", "yes", NA, "yes"...
    ## $ sorethroat                       <chr> "yes", NA, NA, "yes", NA, "yes", "...
    ## $ difficultyofbreathing            <chr> "yes", NA, NA, NA, NA, NA, "yes", ...
    ## $ diarrhea                         <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA...
    ## $ none                             <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA...
    ## $ othersignssymptomsspecify        <chr> NA, "BODY MALAISE", "COUGH AND COL...
    ## $ onset_date                       <chr> "3/17/2020", "3/4/2020", NA, NA, N...
    ## $ admitted_date                    <chr> "3/24/2020", "3/21/2020", "3/23/20...
    ## $ specimen_date                    <chr> "3/24/2020", "3/20/2020", "3/23/20...
    ## $ datespecimenreceivedbylaboratory <chr> NA, NA, "3/23/2020", "3/20/2020", ...
    ## $ result_date                      <chr> "3/27/2020", NA, "3/24/2020", "3/2...
    ## $ discharged_date                  <chr> NA, "4/8/2020", "4/21/2020", "4/3/...
    ## $ recovered_date                   <chr> NA, "4/8/2020", "4/21/2020", "4/3/...
    ## $ died_date                        <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA...
    ## $ quarantined                      <chr> "No", "Yes", "Yes", "Yes", NA, "Ye...

Im assuming 1 case number per patient. Check if this is true

``` r
june5 %>%
        group_by(casenumber) %>%
        count() %>%
        filter(n > 1, !is.na(casenumber))
```

    ## # A tibble: 46 x 2
    ## # Groups:   casenumber [46]
    ##    casenumber     n
    ##    <chr>      <int>
    ##  1 PH10463        2
    ##  2 PH10519        2
    ##  3 PH10574        2
    ##  4 PH11142        2
    ##  5 PH11622        2
    ##  6 PH11954        2
    ##  7 PH11972        2
    ##  8 PH12116        2
    ##  9 PH12182        2
    ## 10 PH12184        2
    ## # ... with 36 more rows

``` r
june6 %>%
        group_by(casenumber) %>%
        count() %>%
        filter(n > 1, !is.na(casenumber))
```

    ## # A tibble: 46 x 2
    ## # Groups:   casenumber [46]
    ##    casenumber     n
    ##    <chr>      <int>
    ##  1 PH10463        2
    ##  2 PH10519        2
    ##  3 PH10574        2
    ##  4 PH11142        2
    ##  5 PH11622        2
    ##  6 PH11954        2
    ##  7 PH11972        2
    ##  8 PH12116        2
    ##  9 PH12182        2
    ## 10 PH12184        2
    ## # ... with 36 more rows

Same multiple entries on June 7

Check this casenumbers

``` r
multiple_cases = june5 %>%
        group_by(casenumber) %>%
        count() %>%
        filter(n > 1, !is.na(casenumber)) %>%
        pull(casenumber)

june5 %>%
        filter(casenumber %in% multiple_cases) %>%
        arrange(casenumber) %>%
        head(15) %>%
        kable()
```

| casenumber | age | sex    | nationality          | healthstatus | currentregion           | currentprovince                                      | currentmunicipalitycity       | regionpsgc | provincepsgc | municipalitycitypsgc | dru                                           | region_dru | otherreportingunit      | confirminglab                            | comorbidity | specifycomorbidity     | fever | cough | cold | sorethroat | difficultyofbreathing | diarrhea | none | othersignssymptomsspecify                | onset_date | admitted_date | specimen_date | datespecimenreceivedbylaboratory | result_date | discharged_date | recovered_date | died_date | quarantined | drucity | druprovince |
|:-----------|----:|:-------|:---------------------|:-------------|:------------------------|:-----------------------------------------------------|:------------------------------|-----------:|-------------:|---------------------:|:----------------------------------------------|-----------:|:------------------------|:-----------------------------------------|:------------|:-----------------------|:------|:------|:-----|:-----------|:----------------------|:---------|:-----|:-----------------------------------------|:-----------|:--------------|:--------------|:---------------------------------|:------------|:----------------|:---------------|:----------|:------------|:--------|:------------|
| PH10463    |  65 | Female | Philippine, Filipino | Recovered    | Northern Mindanao       | Misamis Oriental                                     | Cagayan de oro city (capital) |         10 |         1043 |               104305 | OTHER REPORTING UNIT                          |         NA | CHO-CAGAYAN DE ORO CITY | SOUTHERN PHILIPPINES MEDICAL CENTER      | NA          | NA                     | NA    | yes   | NA   | NA         | NA                    | NA       | NA   | NA                                       | NA         | NA            | 4/30/2020     | 5/2/2020                         | 5/4/2020    | NA              | 5/21/2020      | NA        | Yes         | NA      | NA          |
| PH10463    |  65 | Female | Philippine, Filipino | Recovered    | Northern Mindanao       | Misamis Oriental                                     | Cagayan de oro city (capital) |         10 |         1043 |               104305 | NA                                            |         NA | NA                      | NA                                       | NA          | NA                     | NA    | yes   | NA   | NA         | NA                    | NA       | NA   | NA                                       | 4/30/2020  | NA            | 4/30/2020     | 5/5/2020                         | 5/4/2020    | NA              | 5/21/2020      | NA        | Yes         | NA      | NA          |
| PH10519    |  21 | Female | 0                    | NA           | National Capital Region | NA                                                   | NA                            |         13 |           NA |                   NA | CAPITOL MEDICAL CENTER, INC.                  |         13 | NA                      | CHINESE GENERAL HOSPITAL                 | NA          | NA                     | yes   | NA    | NA   | NA         | NA                    | yes      | NA   | NA                                       | NA         | 5/5/2020      | 5/6/2020      | 5/6/2020                         | 5/7/2020    | 5/10/2020       | NA             | NA        | NA          | NA      | NA          |
| PH10519    |  21 | Male   | Philippine, Filipino | NA           | National Capital Region | NCR, Fourth District (not a province)                | City of las piÃ±as            |         13 |         1376 |                   NA | NA                                            |         NA | NA                      | RESEARCH INSTITUTE FOR TROPICAL MEDICINE | NA          | NA                     | NA    | NA    | NA   | NA         | NA                    | NA       | NA   | NA                                       | NA         | 5/4/2020      | 5/6/2020      | 5/6/2020                         | NA          | 5/7/2020        | NA             | NA        | Yes         | NA      | NA          |
| PH10574    |  52 | Female | Philippine, Filipino | NA           | National Capital Region | NA                                                   | NA                            |         13 |           NA |                   NA | OTHER REPORTING UNIT                          |         NA | PASIG CESU              | THE MEDICAL CITY - ORTIGAS               | NA          | NA                     | NA    | NA    | NA   | NA         | NA                    | NA       | NA   | NA                                       | NA         | NA            | 5/1/2020      | 5/4/2020                         | 5/6/2020    | NA              | NA             | NA        | NA          | NA      | NA          |
| PH10574    |  52 | Female | Philippine, Filipino | NA           | National Capital Region | NCR, Second District (not a province)                | City of pasig                 |         13 |         1374 |               137403 | PASIG CITY CHILDREN’S HOSPITAL - CHILD’S HOPE |         NA | NA                      | THE MEDICAL CITY - ORTIGAS               | No          | NA                     | NA    | NA    | NA   | yes        | NA                    | NA       | NA   | NONE                                     | 5/7/2020   | NA            | 5/20/2020     | 5/21/2020                        | 5/23/2020   | NA              | NA             | NA        | NA          | NA      | NA          |
| PH11142    |  27 | Male   | Philippine, Filipino | Recovered    | CAR                     | Benguet                                              | La trinidad (capital)         |         14 |         1411 |               141110 | BAGUIO GENERAL HOSPITAL AND MEDICAL CENTER    |         14 | NA                      | BAGUIO GENERAL HOSPITAL MEDICAL CENTER   | No          | NA                     | NA    | yes   | NA   | NA         | NA                    | NA       | NA   | HEADACHE                                 | 4/26/2020  | 5/2/2020      | 5/3/2020      | 5/3/2020                         | 5/11/2020   | NA              | NA             | NA        | Yes         | NA      | NA          |
| PH11142    |  26 | Male   | Philippine, Filipino | Recovered    | CAR                     | Benguet                                              | Baguio city                   |         14 |         1411 |               141102 | NA                                            |         NA | NA                      | NA                                       | No          | NA                     | NA    | yes   | NA   | NA         | NA                    | NA       | NA   | NA                                       | NA         | NA            | 5/3/2020      | 5/3/2020                         | 5/7/2020    | NA              | 5/28/2020      | NA        | NA          | NA      | NA          |
| PH11622    |  46 | Female | Philippine, Filipino | NA           | National Capital Region | NCR, Fourth District (not a province)                | NA                            |         13 |         1376 |                   NA | OTHER REPORTING UNIT                          |         NA | BUCOR                   | RESEARCH INSTITUTE FOR TROPICAL MEDICINE | NA          | NA                     | NA    | NA    | NA   | NA         | NA                    | NA       | NA   | NA                                       | NA         | NA            | 5/7/2020      | NA                               | 5/14/2020   | NA              | NA             | NA        | NA          | NA      | NA          |
| PH11622    |  46 | Female | Philippine, Filipino | NA           | NA                      | NA                                                   | NA                            |         NA |           NA |                   NA | UNIVERSITY OF SANTO TOMAS HOSPITAL            |         13 | NA                      | NA                                       | NA          | NA                     | NA    | yes   | NA   | NA         | NA                    | NA       | NA   | NA                                       | 5/15/2020  | NA            | 5/21/2020     | NA                               | 5/23/2020   | NA              | NA             | NA        | NA          | NA      | NA          |
| PH11954    |  46 | Male   | Philippine, Filipino | NA           | National Capital Region | NCR, Fourth District (not a province)                | City of paraÃ±aque            |         13 |         1376 |                   NA | OTHER REPORTING UNIT                          |         NA | PHILIPPINE RED CROSS    | PHILIPPINE RED CROSS - PLMC              | NA          | NA                     | NA    | NA    | NA   | NA         | NA                    | NA       | NA   | NA                                       | NA         | NA            | 5/5/2020      | NA                               | NA          | NA              | NA             | NA        | NA          | NA      | NA          |
| PH11954    |  46 | Male   | Philippine, Filipino | NA           | National Capital Region | NCR, Fourth District (not a province)                | City of paraÃ±aque            |         13 |         1376 |                   NA | NA                                            |         NA | NA                      | PHILIPPINES RED CROSS                    | NA          | NA                     | NA    | NA    | NA   | NA         | NA                    | NA       | yes  | NA                                       | NA         | NA            | 5/5/2020      | 5/5/2020                         | 5/5/2020    | NA              | NA             | NA        | Yes         | NA      | NA          |
| PH11972    |  71 | Male   | Philippine, Filipino | NA           | National Capital Region | NCR, City of Manila, First District (not a province) | San nicolas                   |         13 |         1339 |               133904 | metropolitan medical center                   |         13 | NA                      | CHINESE GENERAL HOSPITAL                 | NA          | NA                     | NA    | NA    | NA   | NA         | NA                    | NA       | NA   | general body weakness , loss of appetite | 5/3/2020   | 5/10/2020     | 5/23/2020     | 5/23/2020                        | 5/23/2020   | NA              | NA             | NA        | NA          | NA      | NA          |
| PH11972    |  72 | Male   | Philippine, Filipino | NA           | National Capital Region | NA                                                   | NA                            |         13 |           NA |                   NA | METROPOLITAN MEDICAL CENTER                   |         13 | NA                      | UP NATIONAL INSTITUTES OF HEALTH         | NA          | ISCHEMIC HEART DISEASE | NA    | yes   | NA   | NA         | NA                    | NA       | NA   | general body weakness and poor appetite  | 5/8/2020   | 5/10/2020     | NA            | NA                               | NA          | NA              | NA             | NA        | NA          | NA      | NA          |
| PH12116    |  56 | Male   | Philippine, Filipino | NA           | National Capital Region | NCR, Fourth District (not a province)                | City of parañaque             |         13 |         1376 |                   NA | SUN VALLEY HEALTH CENTER                      |         13 | NA                      | RESEARCH INSTITUTE FOR TROPICAL MEDICINE | NA          | NA                     | NA    | NA    | NA   | NA         | NA                    | NA       | NA   | NA                                       | NA         | NA            | 5/12/2020     | NA                               | 5/15/2020   | NA              | NA             | NA        | Yes         | NA      | NA          |

Focusing only on the demographics

-   PH10519 is both female and male,
-   PH11142 changed age
-   PH12184 is both chinese and filipino
-   PH2315 is both age 53 and 23

Try to check these casenumbers in june 7 data

``` r
june7 %>%
        filter(casenumber %in% multiple_cases) %>%
        arrange(casenumber) %>%
        distinct(casenumber, sex, age, nationality, .keep_all = F) %>%
        group_by(casenumber) %>%
        add_count() %>%
        filter(n > 1) %>%
        head(15) %>%
        kable()
```

| casenumber | age | sex    | nationality          |   n |
|:-----------|----:|:-------|:---------------------|----:|
| PH10519    |  21 | Female | 0                    |   2 |
| PH10519    |  21 | Male   | Philippine, Filipino |   2 |
| PH11142    |  27 | Male   | Philippine, Filipino |   2 |
| PH11142    |  26 | Male   | Philippine, Filipino |   2 |
| PH11972    |  71 | Male   | Philippine, Filipino |   2 |
| PH11972    |  72 | Male   | Philippine, Filipino |   2 |
| PH12184    |  46 | Male   | Philippine, Filipino |   2 |
| PH12184    |  46 | Male   | Chinese              |   2 |
| PH12512    |  69 | Male   | 0                    |   2 |
| PH12512    |  64 | Male   | Philippine, Filipino |   2 |
| PH12619    |  28 | Male   | 0                    |   2 |
| PH12619    |  28 | Male   | Philippine, Filipino |   2 |
| PH12799    |  26 | Male   | Philippine, Filipino |   2 |
| PH12799    |  25 | Male   | 0                    |   2 |
| PH13795    |  81 | Female | Philippine, Filipino |   2 |

It seems that this was not updated in the june 7 data and looking at the
demographics alone, there are a lot of inconsistencies.

Check the completeness of dates columns and the dates columns

``` r
june5 %>%
        select(ends_with('_date')) %>%
        map_dbl(function(x) sum(is.na(x)/ length(x)))
```

    ##      onset_date   admitted_date   specimen_date     result_date discharged_date 
    ##       0.6146603       0.6445553       0.1463208       0.2420319       0.8879996 
    ##  recovered_date       died_date 
    ##       0.8767007       0.9559814

Looks like the specimen date has the least missing data

``` r
june5 %>%
        select(contains('date')) %>%
        arrange(mdy(specimen_date)) %>%
        head(15) %>%
        kable()
```

| onset_date | admitted_date | specimen_date | datespecimenreceivedbylaboratory | result_date | discharged_date | recovered_date | died_date |
|:-----------|:--------------|:--------------|:---------------------------------|:------------|:----------------|:---------------|:----------|
| NA         | NA            | 1/1/1900      | NA                               | NA          | NA              | NA             | NA        |
| NA         | NA            | 2/3/1957      | 5/29/2020                        | 5/30/2020   | NA              | NA             | NA        |
| NA         | NA            | 12/25/1970    | NA                               | 4/21/2020   | NA              | NA             | NA        |
| NA         | NA            | 4/7/1971      | NA                               | 5/1/2020    | NA              | NA             | NA        |
| NA         | NA            | 8/14/1982     | 5/18/2020                        | 5/19/2020   | NA              | NA             | NA        |
| NA         | NA            | 8/8/1986      | NA                               | 5/21/2020   | NA              | NA             | NA        |
| NA         | NA            | 5/26/2002     | 5/27/2020                        | 5/28/2020   | NA              | NA             | NA        |
| NA         | NA            | 5/14/2010     | 5/16/2020                        | 5/20/2020   | NA              | NA             | NA        |
| NA         | NA            | 1/1/2011      | NA                               | 5/17/2020   | NA              | NA             | NA        |
| NA         | NA            | 12/31/2019    | 5/21/2020                        | 5/21/2020   | NA              | NA             | NA        |
| NA         | 4/24/2020     | 1/1/2020      | NA                               | 4/27/2020   | NA              | NA             | NA        |
| NA         | 4/29/2020     | 1/1/2020      | NA                               | 5/20/2020   | NA              | NA             | NA        |
| NA         | 4/27/2020     | 1/1/2020      | 4/4/2020                         | 4/6/2020    | NA              | NA             | NA        |
| NA         | NA            | 1/1/2020      | NA                               | 5/5/2020    | NA              | NA             | NA        |
| NA         | NA            | 1/1/2020      | NA                               | 5/20/2020   | NA              | NA             | NA        |

There are specimen dates that are way before 2020. There are also a lot
of 1/1/2020 specimen date which I assume are entry errors. Im trying to
look for ways to complete missing case numbers.

Check for june5 cases that were removed

``` r
june5 %>%
        filter(!(casenumber %in% june6$casenumber)) %>%
        kable()
```

| casenumber | age | sex    | nationality          | healthstatus | currentregion           | currentprovince                       | currentmunicipalitycity | regionpsgc | provincepsgc | municipalitycitypsgc | dru                            | region_dru | otherreportingunit | confirminglab                            | comorbidity | specifycomorbidity          | fever | cough | cold | sorethroat | difficultyofbreathing | diarrhea | none | othersignssymptomsspecify | onset_date | admitted_date | specimen_date | datespecimenreceivedbylaboratory | result_date | discharged_date | recovered_date | died_date | quarantined | drucity | druprovince |
|:-----------|----:|:-------|:---------------------|:-------------|:------------------------|:--------------------------------------|:------------------------|-----------:|-------------:|---------------------:|:-------------------------------|-----------:|:-------------------|:-----------------------------------------|:------------|:----------------------------|:------|:------|:-----|:-----------|:----------------------|:---------|:-----|:--------------------------|:-----------|:--------------|:--------------|:---------------------------------|:------------|:----------------|:---------------|:----------|:------------|:--------|:------------|
| PH162      |  72 | Male   | Philippine, Filipino | Recovered    | National Capital Region | NCR, Second District (not a province) | Quezon city             |         13 |         1374 |               137404 | DILIMAN DOCTORS HOSPITAL, INC. |         13 | NA                 | RESEARCH INSTITUTE FOR TROPICAL MEDICINE | NA          | BENIGN PROSTATIC HYPERLASIA | yes   | yes   | yes  | NA         | NA                    | NA       | NA   | LOSS OF APPETITE          | 3/11/2020  | 3/11/2020     | 3/12/2020     | NA                               | 3/16/2020   | 4/5/2020        | 4/5/2020       | NA        | NA          | NA      | NA          |
| PH706      |  74 | Male   | Philippine, Filipino | Recovered    | National Capital Region | NCR, Second District (not a province) | Quezon city             |         13 |         1374 |               137404 | DILIMAN DOCTORS HOSPITAL, INC. |         13 | NA                 | RESEARCH INSTITUTE FOR TROPICAL MEDICINE | Yes         | NA                          | yes   | yes   | yes  | yes        | NA                    | NA       | NA   | NA                        | 3/12/2020  | 3/16/2020     | 3/16/2020     | NA                               | 3/25/2020   | NA              | NA             | NA        | NA          | NA      | NA          |
| PH7540     |  40 | Female | Philippine, Filipino | Mild         | National Capital Region | NCR, Second District (not a province) | City of pasig           |         13 |         1374 |               137403 | PASIG CITY GENERAL HOSPITAL    |         13 | NA                 | RESEARCH INSTITUTE FOR TROPICAL MEDICINE | NA          | NA                          | NA    | NA    | NA   | NA         | NA                    | NA       | yes  | NA                        | 4/13/2020  | 4/16/2020     | 4/16/2020     | NA                               | 4/25/2020   | NA              | NA             | NA        | NA          | NA      | NA          |

``` r
june5 %>%
        filter(!(casenumber %in% june7$casenumber)) %>%
        kable()
```

| casenumber | age | sex    | nationality          | healthstatus | currentregion           | currentprovince                                      | currentmunicipalitycity | regionpsgc | provincepsgc | municipalitycitypsgc | dru                                          | region_dru | otherreportingunit | confirminglab                            | comorbidity | specifycomorbidity          | fever | cough | cold | sorethroat | difficultyofbreathing | diarrhea | none | othersignssymptomsspecify | onset_date | admitted_date | specimen_date | datespecimenreceivedbylaboratory | result_date | discharged_date | recovered_date | died_date | quarantined | drucity | druprovince |
|:-----------|----:|:-------|:---------------------|:-------------|:------------------------|:-----------------------------------------------------|:------------------------|-----------:|-------------:|---------------------:|:---------------------------------------------|-----------:|:-------------------|:-----------------------------------------|:------------|:----------------------------|:------|:------|:-----|:-----------|:----------------------|:---------|:-----|:--------------------------|:-----------|:--------------|:--------------|:---------------------------------|:------------|:----------------|:---------------|:----------|:------------|:--------|:------------|
| PH162      |  72 | Male   | Philippine, Filipino | Recovered    | National Capital Region | NCR, Second District (not a province)                | Quezon city             |         13 |         1374 |               137404 | DILIMAN DOCTORS HOSPITAL, INC.               |         13 | NA                 | RESEARCH INSTITUTE FOR TROPICAL MEDICINE | NA          | BENIGN PROSTATIC HYPERLASIA | yes   | yes   | yes  | NA         | NA                    | NA       | NA   | LOSS OF APPETITE          | 3/11/2020  | 3/11/2020     | 3/12/2020     | NA                               | 3/16/2020   | 4/5/2020        | 4/5/2020       | NA        | NA          | NA      | NA          |
| PH706      |  74 | Male   | Philippine, Filipino | Recovered    | National Capital Region | NCR, Second District (not a province)                | Quezon city             |         13 |         1374 |               137404 | DILIMAN DOCTORS HOSPITAL, INC.               |         13 | NA                 | RESEARCH INSTITUTE FOR TROPICAL MEDICINE | Yes         | NA                          | yes   | yes   | yes  | yes        | NA                    | NA       | NA   | NA                        | 3/12/2020  | 3/16/2020     | 3/16/2020     | NA                               | 3/25/2020   | NA              | NA             | NA        | NA          | NA      | NA          |
| PH7540     |  40 | Female | Philippine, Filipino | Mild         | National Capital Region | NCR, Second District (not a province)                | City of pasig           |         13 |         1374 |               137403 | PASIG CITY GENERAL HOSPITAL                  |         13 | NA                 | RESEARCH INSTITUTE FOR TROPICAL MEDICINE | NA          | NA                          | NA    | NA    | NA   | NA         | NA                    | NA       | yes  | NA                        | 4/13/2020  | 4/16/2020     | 4/16/2020     | NA                               | 4/25/2020   | NA              | NA             | NA        | NA          | NA      | NA          |
| PH8806     |  32 | Female | Philippine, Filipino | Asymptomatic | National Capital Region | NCR, City of Manila, First District (not a province) | Tondo i / ii            |         13 |         1339 |               133901 | GAT ANDRES BONIFACIO MEMORIAL MEDICAL CENTER |         13 | NA                 | UP NATIONAL INSTITUTES OF HEALTH         | NA          | NA                          | yes   | yes   | NA   | yes        | NA                    | NA       | NA   | NA                        | 4/14/2020  | 5/1/2020      | 4/17/2020     | NA                               | 4/28/2020   | NA              | NA             | NA        | NA          | NA      | NA          |

There were 3 cases that were in june 5 but not in june 6 and 4 cases
that are in june 5 but not in june 7. If they are just updating. This
shouldnt be removed.

Check if there are some changes in some column values from June 5 to
June 7.

Im interested whether there are cases that changed province and
hospitals. This information is very important on whether to impose GCQ
or ECQ on certain locations

``` r
june7 %>%
        select(casenumber, currentregion) %>%
        left_join(june5 %>% select(casenumber, currentregion), by = 'casenumber') %>%
        distinct() %>%
        group_by(casenumber) %>%
        filter(currentregion.x != currentregion.y, !is.na(casenumber))
```

    ## # A tibble: 29 x 3
    ## # Groups:   casenumber [23]
    ##    casenumber currentregion.x         currentregion.y        
    ##    <chr>      <chr>                   <chr>                  
    ##  1 PH14183    CaLaBaRZon              National Capital Region
    ##  2 PH4546     Bicol                   National Capital Region
    ##  3 PH2663     Central Luzon           Repatriate             
    ##  4 PH2392     CaLaBaRZon              National Capital Region
    ##  5 PH3680     CaLaBaRZon              National Capital Region
    ##  6 PH5637     National Capital Region CaLaBaRZon             
    ##  7 PH13086    National Capital Region Zamboanga Peninsula    
    ##  8 PH4546     National Capital Region Bicol                  
    ##  9 PH8302     National Capital Region Ilocos                 
    ## 10 PH2108     Central Luzon           National Capital Region
    ## # ... with 19 more rows

There are few cases where the patient changed current region

I am also interested whether there are some patients that were declared
dead on june 5 but then has different condition on june 6 or 7

``` r
june5 %>%
        select(casenumber, healthstatus) %>%
        filter(healthstatus == 'Died') %>%
        left_join(june7 %>% select(casenumber, healthstatus), by = c('casenumber' = 'casenumber')) %>%
        filter(!is.na(casenumber)) %>%
        filter(healthstatus.x != healthstatus.y)
```

    ## # A tibble: 0 x 3
    ## # ... with 3 variables: casenumber <chr>, healthstatus.x <chr>,
    ## #   healthstatus.y <chr>

At least there are no dead patients in June 5 that were alive in June 7
data

How about if some patients changed their gender

``` r
june7 %>%
        select(casenumber, sex) %>%
        left_join(june5 %>% select(casenumber, sex), by = 'casenumber') %>%
        distinct() %>%
        group_by(casenumber) %>%
        filter(sex.x != sex.y, !is.na(casenumber)) %>%
        rename(sexjune5 = sex.y, sexjune7 = sex.x)
```

    ## # A tibble: 9 x 3
    ## # Groups:   casenumber [7]
    ##   casenumber sexjune7 sexjune5
    ##   <chr>      <chr>    <chr>   
    ## 1 PH5953     Female   Male    
    ## 2 PH16088    Male     Female  
    ## 3 PH13004    Female   Male    
    ## 4 PH10519    Female   Male    
    ## 5 PH5953     Male     Female  
    ## 6 PH10519    Male     Female  
    ## 7 PH14063    Female   Male    
    ## 8 PH16449    Female   Male    
    ## 9 PH16489    Female   Male

Sanity check

``` r
changed_gender = june7 %>%
        select(casenumber, sex) %>%
        left_join(june5 %>% select(casenumber, sex), by = 'casenumber') %>%
        distinct() %>%
        group_by(casenumber) %>%
        filter(sex.x != sex.y, !is.na(casenumber)) %>%
        rename(sexjune5 = sex.y, sexjune7 = sex.x) %>%
        pull(casenumber)

june5 %>%
        filter(casenumber %in% changed_gender) %>%
        arrange(casenumber) %>%
        select(casenumber, age, sex, nationality)
```

    ## # A tibble: 9 x 4
    ##   casenumber   age sex    nationality         
    ##   <chr>      <dbl> <chr>  <chr>               
    ## 1 PH10519       21 Female 0                   
    ## 2 PH10519       21 Male   Philippine, Filipino
    ## 3 PH13004       27 Male   Philippine, Filipino
    ## 4 PH14063       18 Male   Philippine, Filipino
    ## 5 PH16088       22 Female Philippine, Filipino
    ## 6 PH16449       35 Male   Philippine, Filipino
    ## 7 PH16489       11 Male   Philippine, Filipino
    ## 8 PH5953        34 Female Philippine, Filipino
    ## 9 PH5953        35 Male   Philippine, Filipino

``` r
june7 %>%
        filter(casenumber %in% changed_gender) %>%
        arrange(casenumber) %>%
        select(casenumber, age, sex, nationality)
```

    ## # A tibble: 9 x 4
    ##   casenumber   age sex    nationality         
    ##   <chr>      <dbl> <chr>  <chr>               
    ## 1 PH10519       21 Female 0                   
    ## 2 PH10519       21 Male   Philippine, Filipino
    ## 3 PH13004       27 Female Philippine, Filipino
    ## 4 PH14063       18 Female Philippine, Filipino
    ## 5 PH16088       18 Male   Philippine, Filipino
    ## 6 PH16449       35 Female Philippine, Filipino
    ## 7 PH16489       11 Female Philippine, Filipino
    ## 8 PH5953        34 Female Philippine, Filipino
    ## 9 PH5953        35 Male   Philippine, Filipino

Either multiple entry or just changed gender.

Compare if some missing values are filled on June 7 data. My assumption
is June 7 has the most complete data

``` r
june6_missing = aggregate(. ~ casenumber, data = june6, function(x) sum(is.na(x)), na.action = na.pass)

june6_missing = june6_missing %>%
        gather(-casenumber, key = 'col', value = 'count_missing') %>%
        group_by(casenumber) %>%
        summarise(missing_values = sum(count_missing))
```

``` r
june7_missing = aggregate(. ~ casenumber, data = june7, function(x) sum(is.na(x)), na.action = na.pass)


june7_missing = june7_missing %>%
        gather(-casenumber, key = 'col', value = 'count_missing') %>%
        group_by(casenumber) %>%
        summarise(missing_values = sum(count_missing))
```

Rename columns

``` r
june6_missing = june6_missing %>%
        rename_with(function(x) paste0('June6_', x))

june7_missing = june7_missing %>%
        rename_with(function(x) paste0('June7_', x))
```

``` r
june7_missing %>%
        inner_join(june6_missing, by = c('June7_casenumber' = 'June6_casenumber')) %>%
        filter(June7_missing_values != June6_missing_values)
```

    ## # A tibble: 458 x 3
    ##    June7_casenumber June7_missing_values June6_missing_values
    ##    <chr>                           <int>                <int>
    ##  1 PH10072                            17                   18
    ##  2 PH10077                            20                   21
    ##  3 PH10091                            17                   18
    ##  4 PH10094                            21                   18
    ##  5 PH10113                            19                   20
    ##  6 PH10201                            17                   21
    ##  7 PH10228                            10                   14
    ##  8 PH10271                            19                   20
    ##  9 PH10346                            12                   15
    ## 10 PH10377                            16                   17
    ## # ... with 448 more rows

It seems that some missing value in june6 were now filled. Can assume
that June7 is more updated

Check if there are entries in June7 that has more missing values

``` r
more_missing = june7_missing %>%
        inner_join(june6_missing, by = c('June7_casenumber' = 'June6_casenumber')) %>%
        filter(June7_missing_values > June6_missing_values)

june7 %>%
        filter(casenumber %in% more_missing$June7_casenumber) %>%
        select(casenumber,datespecimenreceivedbylaboratory)
```

    ## # A tibble: 14 x 2
    ##    casenumber datespecimenreceivedbylaboratory
    ##    <chr>      <chr>                           
    ##  1 PH9537     <NA>                            
    ##  2 PH15062    <NA>                            
    ##  3 PH16200    5/16/2020                       
    ##  4 PH15120    <NA>                            
    ##  5 PH15822    <NA>                            
    ##  6 PH16516    <NA>                            
    ##  7 PH14608    <NA>                            
    ##  8 PH15654    <NA>                            
    ##  9 PH4954     <NA>                            
    ## 10 PH17070    <NA>                            
    ## 11 PH1320     <NA>                            
    ## 12 PH5237     5/22/2020                       
    ## 13 PH6923     <NA>                            
    ## 14 PH10094    4/30/2020

``` r
june6 %>%
        filter(casenumber %in% more_missing$June7_casenumber) %>%
        select(casenumber,datespecimenreceivedbylaboratory)
```

    ## # A tibble: 14 x 2
    ##    casenumber datespecimenreceivedbylaboratory
    ##    <chr>      <chr>                           
    ##  1 PH9537     0001-01-01                      
    ##  2 PH16200    5/16/2020                       
    ##  3 PH15062    0001-01-01                      
    ##  4 PH16516    0020-05-23                      
    ##  5 PH15120    0001-01-01                      
    ##  6 PH15822    0001-05-14                      
    ##  7 PH14608    0001-01-01                      
    ##  8 PH15654    0001-05-07                      
    ##  9 PH4954     0001-01-01                      
    ## 10 PH17070    0001-01-01                      
    ## 11 PH1320     0001-01-01                      
    ## 12 PH5237     4/22/2020                       
    ## 13 PH6923     4/20/2020                       
    ## 14 PH10094    4/30/2020

Okay, some entries for the datespecimenreceivedbylaboratory were changed
from unknown date format to NA

Check entries in June 7 that were not in june5

``` r
june7 %>%
        anti_join(june5, by = c('casenumber' = 'casenumber')) %>%
        summarise(earliest_date = min(mdy(result_date), na.rm = T), 
                  latest_date = max(mdy(result_date), na.rm = T))
```

    ## # A tibble: 1 x 2
    ##   earliest_date latest_date
    ##   <date>        <date>     
    ## 1 2020-02-15    2020-06-05

``` r
june5 %>%
        filter(year(mdy(result_date)) == '2020') %>%
        summarise(latest_date = max((mdy(result_date)), na.rm = T))
```

    ## # A tibble: 1 x 1
    ##   latest_date
    ##   <date>     
    ## 1 2020-12-05

Okay that’s weird. There’s a December 2020 case in June5 data

``` r
june5 %>%
        filter(mdy(result_date) == '2020-12-05') %>%
        kable()
```

| casenumber | age | sex    | nationality          | healthstatus | currentregion | currentprovince | currentmunicipalitycity | regionpsgc | provincepsgc | municipalitycitypsgc | dru            | region_dru | otherreportingunit | confirminglab         | comorbidity | specifycomorbidity | fever | cough | cold | sorethroat | difficultyofbreathing | diarrhea | none | othersignssymptomsspecify | onset_date | admitted_date | specimen_date | datespecimenreceivedbylaboratory | result_date | discharged_date | recovered_date | died_date | quarantined | drucity | druprovince |
|:-----------|----:|:-------|:---------------------|:-------------|:--------------|:----------------|:------------------------|-----------:|-------------:|---------------------:|:---------------|-----------:|:-------------------|:----------------------|:------------|:-------------------|:------|:------|:-----|:-----------|:----------------------|:---------|:-----|:--------------------------|:-----------|:--------------|:--------------|:---------------------------------|:------------|:----------------|:---------------|:----------|:------------|:--------|:------------|
| PH16632    |  23 | Female | Philippine, Filipino | NA           | NA            | NA              | NA                      |         NA |           NA |                   NA | PCG/PRC        |         NA | NA                 | PHILIPPINES RED CROSS | NA          | NA                 | NA    | NA    | NA   | NA         | NA                    | NA       | NA   | NA                        | NA         | NA            | NA            | NA                               | 12/5/2020   | NA              | NA             | NA        | NA          | NA      | NA          |
| PH16633    |  30 | Male   | Philippine, Filipino | NA           | NA            | NA              | NA                      |         NA |           NA |                   NA | SOGO MONUMENTO |         NA | NA                 | PHILIPPINES RED CROSS | NA          | NA                 | NA    | NA    | NA   | NA         | NA                    | NA       | NA   | NA                        | NA         | NA            | NA            | NA                               | 12/5/2020   | NA              | NA             | NA        | Yes         | NA      | NA          |
| PH16634    |  28 | Male   | Philippine, Filipino | NA           | NA            | NA              | NA                      |         NA |           NA |                   NA | SOGO MONUMENTO |         NA | NA                 | PHILIPPINES RED CROSS | NA          | NA                 | NA    | NA    | NA   | NA         | NA                    | NA       | NA   | NA                        | NA         | NA            | 11/5/2020     | NA                               | 12/5/2020   | NA              | NA             | NA        | NA          | NA      | NA          |

This is probably May 12 2020

There are also inconsistencies in the date format. Some are Month day
year and some are day month year. These inconsistencies is problematic
for time-series and time related analysis

I am comparing the dates of June 5 and June 7 data to check whether the
June7 data were new entries. Maybe just check the case number

New entries for June 7

``` r
june7 %>%
        anti_join(june5, by = c('casenumber' = 'casenumber')) %>%
        arrange(desc(parse_number(casenumber))) %>%
        select(casenumber, age, sex, nationality, healthstatus)
```

    ## # A tibble: 443 x 5
    ##    casenumber   age sex    nationality          healthstatus
    ##    <chr>      <dbl> <chr>  <chr>                <chr>       
    ##  1 PH20215       47 Male   Philippine, Filipino <NA>        
    ##  2 PH20072       14 Male   Philippine, Filipino <NA>        
    ##  3 PH20061       44 Female Philippine, Filipino <NA>        
    ##  4 PH20042       46 Female Philippine, Filipino <NA>        
    ##  5 PH19748       30 Male   Philippine, Filipino <NA>        
    ##  6 PH19746       59 Male   Philippine, Filipino Mild        
    ##  7 PH19745       10 Female Philippine, Filipino <NA>        
    ##  8 PH19744       14 Male   Philippine, Filipino <NA>        
    ##  9 PH19743        4 Female Philippine, Filipino <NA>        
    ## 10 PH19742       39 Male   Philippine, Filipino <NA>        
    ## # ... with 433 more rows

There are 443 case numbers that are new on June 7

``` r
june5 %>%
        arrange(desc(parse_number(casenumber))) %>%
        select(casenumber, age, sex, nationality, healthstatus) %>%
        head(15)
```

    ## # A tibble: 15 x 5
    ##    casenumber   age sex    nationality          healthstatus
    ##    <chr>      <dbl> <chr>  <chr>                <chr>       
    ##  1 PH204263      30 Female Philippine, Filipino <NA>        
    ##  2 PH204262      29 Female Philippine, Filipino <NA>        
    ##  3 PH19681       59 Female Philippine, Filipino Mild        
    ##  4 PH18588       34 Female Philippine, Filipino <NA>        
    ##  5 PH17883       41 Female Philippine, Filipino <NA>        
    ##  6 PH17224       37 Male   Philippine, Filipino <NA>        
    ##  7 PH17223       82 Male   Philippine, Filipino Mild        
    ##  8 PH17222       35 Female Philippine, Filipino Recovered   
    ##  9 PH17220       55 Male   Philippine, Filipino <NA>        
    ## 10 PH17218       58 Female Philippine, Filipino <NA>        
    ## 11 PH17217       55 Male   Philippine, Filipino <NA>        
    ## 12 PH17216       29 Male   Philippine, Filipino <NA>        
    ## 13 PH17215       29 Female Philippine, Filipino <NA>        
    ## 14 PH17214       27 Female Philippine, Filipino <NA>        
    ## 15 PH17213       33 Male   Philippine, Filipino <NA>

Im not sure how these new entries were gathered since the case number
and dates or overlapping.

I saw one entry without a number in case number

``` r
june7 %>%
        filter(!is.na(casenumber)) %>%
        filter(!(str_detect(casenumber,'[:digit:]'))) %>%
        kable()
```

| casenumber | age | sex  | nationality          | healthstatus | currentregion | currentprovince | currentmunicipalitycity         | regionpsgc | provincepsgc | municipalitycitypsgc | dru                          | region_dru | drucity   | druprovince    | otherreportingunit | confirminglab | comorbidity | specifycomorbidity | fever | cough | cold | sorethroat | difficultyofbreathing | diarrhea | none | othersignssymptomsspecify | onset_date | admitted_date | specimen_date | datespecimenreceivedbylaboratory | result_date | discharged_date | recovered_date | died_date | quarantined |
|:-----------|----:|:-----|:---------------------|:-------------|:--------------|:----------------|:--------------------------------|-----------:|-------------:|---------------------:|:-----------------------------|-----------:|:----------|:---------------|:-------------------|:--------------|:------------|:-------------------|:------|:------|:-----|:-----------|:----------------------|:---------|:-----|:--------------------------|:-----------|:--------------|:--------------|:---------------------------------|:------------|:----------------|:---------------|:----------|:------------|
| PH         |  21 | Male | Philippine, Filipino | Mild         | Soccsksargen  | South Cotabato  | General santos city (dadiangas) |         12 |         1263 |               126303 | DR. JORGE P. ROYECA HOSPITAL |         12 | Esperanza | Sultan Kudarat | NA                 | Others        | No          | NA                 | NA    | NA    | yes  | NA         | NA                    | NA       | NA   | LOSS OF APPETITE          | 5/30/2020  | 6/2/2020      | 6/2/2020      | 6/4/2020                         | 6/5/2020    | NA              | NA             | NA        | NA          |

# Combine the data

There are a lot of inconsistencies in the data. Maybe just assume that
the June 7 is the most updated data. The goal now is to update entries
from June 5 and June 6 using June 7 data. Also, make sure that the
missing entries in June 7 that are in June 5 and 6 are also included.
Some missing values in June 6 were updated in the June 7. ASide from
missing values, some values also changed in the June 7 entry. We will
prioritize the values of June7. I think it is also safer not to remove
multiple entries as one of those entries is possibly the right one.
Again, the assumption here is that the June 7 data is the most accurate
data. Then clean the data after.

``` r
june7 %>%
        filter(is.na(casenumber)) %>%
        nrow(.)
```

    ## [1] 5274

\~5.3k rows without data

``` r
#common values to all
common_to_all = june7 %>%
        semi_join(june6) %>%
        semi_join(june5)

#in June 7 but not in june 5 and 6
new_values_june7 = june7 %>%
        anti_join(june6) %>%
        anti_join(june5)


#with case number not in june 6 and 7
cases_in_june5_notin_june67 = june5 %>%
        anti_join(june6, by = c('casenumber')) %>%
        anti_join(june7, by = c('casenumber'))

#with case number in june6 not in june 7
cases_injune6_notin_june7 = june6 %>%
        anti_join(june7, by = c('casenumber'))

# bind all dataframe
june7_initial = bind_rows(common_to_all, new_values_june7, cases_in_june5_notin_june67, cases_injune6_notin_june7)

# get the remaining entries from june7
june7_combined = june7 %>%
        anti_join(june7_initial) %>%
        rbind(june7_initial)
```

The challenge now is how to get those without case number in June 5 and
June6 but not recorded in June 7

``` r
june6 %>%
        anti_join(june7_combined) %>%
         map_dbl(function(x) sum(is.na(x)/ length(x))) %>%
        as.data.frame() %>%
        rename(percent_missing = '.') %>%
        arrange(percent_missing)
```

    ##                                  percent_missing
    ## age                                   0.00000000
    ## sex                                   0.00000000
    ## nationality                           0.00000000
    ## confirminglab                         0.06040892
    ## specimen_date                         0.14869888
    ## dru                                   0.20167286
    ## currentregion                         0.23327138
    ## regionpsgc                            0.25000000
    ## result_date                           0.25000000
    ## currentprovince                       0.28903346
    ## provincepsgc                          0.28996283
    ## currentmunicipalitycity               0.32806691
    ## municipalitycitypsgc                  0.36895911
    ## datespecimenreceivedbylaboratory      0.44702602
    ## casenumber                            0.48048327
    ## region_dru                            0.54275093
    ## drucity                               0.54275093
    ## druprovince                           0.54275093
    ## healthstatus                          0.66635688
    ## onset_date                            0.83736059
    ## admitted_date                         0.84572491
    ## quarantined                           0.89405204
    ## comorbidity                           0.89869888
    ## cough                                 0.90334572
    ## othersignssymptomsspecify             0.92936803
    ## fever                                 0.93215613
    ## recovered_date                        0.94888476
    ## otherreportingunit                    0.95353160
    ## difficultyofbreathing                 0.95817844
    ## none                                  0.95910781
    ## cold                                  0.96003717
    ## sorethroat                            0.96189591
    ## specifycomorbidity                    0.96375465
    ## discharged_date                       0.97118959
    ## diarrhea                              0.99256506
    ## died_date                             0.99256506

``` r
june5 %>%
        anti_join(june7_combined, by = c('casenumber')) %>%
        nrow(.)
```

    ## [1] 0

``` r
june6 %>%
        anti_join(june7_combined, by = c('casenumber')) %>%
        nrow(.)
```

    ## [1] 0

Looking at case number alone, all data from June 5 and June6 were
already at the june7 combined data

``` r
june6 %>%
        anti_join(june7_combined) %>%
        filter(is.na(casenumber)) %>%
        head(10) %>%
        kable()
```

| casenumber | age | sex    | nationality          | healthstatus | currentregion | currentprovince | currentmunicipalitycity | regionpsgc | provincepsgc | municipalitycitypsgc | dru                          | region_dru | drucity     | druprovince                           | otherreportingunit | confirminglab               | comorbidity | specifycomorbidity | fever | cough | cold | sorethroat | difficultyofbreathing | diarrhea | none | othersignssymptomsspecify | onset_date | admitted_date | specimen_date | datespecimenreceivedbylaboratory | result_date | discharged_date | recovered_date | died_date | quarantined |
|:-----------|----:|:-------|:---------------------|:-------------|:--------------|:----------------|:------------------------|-----------:|-------------:|---------------------:|:-----------------------------|-----------:|:------------|:--------------------------------------|:-------------------|:----------------------------|:------------|:-------------------|:------|:------|:-----|:-----------|:----------------------|:---------|:-----|:--------------------------|:-----------|:--------------|:--------------|:---------------------------------|:------------|:----------------|:---------------|:----------|:------------|
| NA         |  47 | Male   | Philippine, Filipino | NA           | Unknown       | NA              | NA                      |         NA |           NA |                   NA | SAN LAZARO HOSPITAL          |         13 | Quezon city | NCR, Second District (not a province) | NA                 | SAN LAZARO HOSPITAL         | NA          | NA                 | NA    | yes   | NA   | NA         | yes                   | NA       | NA   | NA                        | 5/2/2020   | 5/8/2020      | 5/8/2020      | 5/9/2020                         | 5/12/2020   | NA              | NA             | NA        | NA          |
| NA         |  44 | Female | Philippine, Filipino | NA           | Unknown       | NA              | NA                      |         NA |           NA |                   NA | SAN LAZARO HOSPITAL          |         13 | Quezon city | NCR, Second District (not a province) | NA                 | SAN LAZARO HOSPITAL         | NA          | NA                 | NA    | NA    | NA   | yes        | NA                    | NA       | NA   | NA                        | 5/6/2020   | NA            | 5/9/2020      | 5/9/2020                         | 5/12/2020   | NA              | NA             | NA        | NA          |
| NA         |  30 | Male   | Philippine, Filipino | NA           | NA            | NA              | NA                      |         NA |           NA |                   NA | PCG                          |         NA | NA          | NA                                    | NA                 | PHILIPPINES RED CROSS       | NA          | NA                 | NA    | NA    | NA   | NA         | NA                    | NA       | NA   | NA                        | NA         | NA            | 4/5/2020      | NA                               | NA          | NA              | NA             | NA        | Yes         |
| NA         |  41 | Female | Philippine, Filipino | NA           | NA            | NA              | NA                      |         NA |           NA |                   NA | PHILIPPINE COAST GUARD - OFW |         NA | NA          | NA                                    | NA                 | PHILIPPINES RED CROSS       | NA          | NA                 | NA    | NA    | NA   | NA         | NA                    | NA       | NA   | NA                        | NA         | NA            | 5/4/2020      | NA                               | NA          | NA              | NA             | NA        | Yes         |
| NA         |  33 | Male   | Philippine, Filipino | NA           | NA            | NA              | NA                      |         NA |           NA |                   NA | QUARANTINE FACILITY          |         NA | NA          | NA                                    | NA                 | PHILIPPINE RED CROSS - PLMC | NA          | NA                 | NA    | NA    | NA   | NA         | NA                    | NA       | NA   | NA                        | NA         | NA            | 5/16/2020     | 5/18/2020                        | 5/18/2020   | NA              | NA             | NA        | NA          |
| NA         |  57 | Male   | Philippine, Filipino | NA           | NA            | NA              | NA                      |         NA |           NA |                   NA | PCG                          |         NA | NA          | NA                                    | NA                 | NA                          | NA          | NA                 | NA    | NA    | NA   | NA         | NA                    | NA       | NA   | NA                        | NA         | NA            | NA            | NA                               | NA          | NA              | NA             | NA        | NA          |
| NA         |  31 | Male   | Philippine, Filipino | NA           | NA            | NA              | NA                      |         NA |           NA |                   NA | PCG OFW                      |         NA | NA          | NA                                    | NA                 | PHILIPPINES RED CROSS       | NA          | NA                 | NA    | NA    | NA   | NA         | NA                    | NA       | NA   | NA                        | NA         | NA            | 5/8/2020      | 5/17/2020                        | 5/18/2020   | NA              | NA             | NA        | NA          |
| NA         |  36 | Female | Philippine, Filipino | NA           | NA            | NA              | NA                      |         NA |           NA |                   NA | ONE TAGAYTAY                 |         NA | NA          | NA                                    | NA                 | PHILIPPINES RED CROSS       | NA          | NA                 | NA    | NA    | NA   | NA         | NA                    | NA       | NA   | NA                        | 5/28/2020  | NA            | 5/4/2020      | 5/21/2020                        | NA          | NA              | NA             | NA        | NA          |
| NA         |  26 | Male   | Philippine, Filipino | NA           | NA            | NA              | NA                      |         NA |           NA |                   NA | PCG                          |         NA | NA          | NA                                    | NA                 | PHILIPPINES RED CROSS       | NA          | NA                 | NA    | NA    | NA   | NA         | NA                    | NA       | NA   | NA                        | NA         | NA            | 4/5/2020      | 4/5/2020                         | NA          | NA              | NA             | NA        | Yes         |
| NA         |  29 | Male   | Philippine, Filipino | NA           | NA            | NA              | NA                      |         NA |           NA |                   NA | PHILIPPINE COAST GUARD - OFW |         NA | NA          | NA                                    | NA                 | PHILIPPINES RED CROSS       | NA          | NA                 | NA    | NA    | NA   | NA         | NA                    | NA       | NA   | NA                        | NA         | NA            | 4/5/2020      | NA                               | NA          | NA              | NA             | NA        | Yes         |

However there are cases like this where there is no case number to
anchor the search. It may be possible that these patients were already
recorded and updated in the June7 Combined data

Just assume a new patient based on this columns if casenumber is missing
- age - sex - nationality - confirming_lab - result_date

June5 assumed patients not in june7 combined

``` r
#30 patients
assumed_june5_patients = june5 %>%
        filter(is.na(casenumber)) %>%
        anti_join(june7_combined, by = c('age','sex','nationality','confirminglab','result_date'))

#18 patients
assumed_june6_patients = june6 %>%
        filter(is.na(casenumber)) %>%
        anti_join(june7_combined, by = c('age','sex','nationality','confirminglab','result_date'))
```

The code above will identify possible patients that were not included in
the june7 combined base on the age, sex, nationality, confirming lab,
and result data

``` r
all_entries = bind_rows(june7_combined, assumed_june5_patients, assumed_june6_patients)
all_entries = all_entries %>%
        distinct()
```

Check

``` r
all_entries %>%
        anti_join(june7_combined) %>%
         map_dbl(function(x) sum(is.na(x)/ length(x))) %>%
        as.data.frame() %>%
        rename(percent_missing = '.') %>%
        arrange(percent_missing)
```

    ##                                  percent_missing
    ## age                                   0.00000000
    ## sex                                   0.00000000
    ## nationality                           0.00000000
    ## confirminglab                         0.08571429
    ## specimen_date                         0.14285714
    ## result_date                           0.25714286
    ## dru                                   0.28571429
    ## datespecimenreceivedbylaboratory      0.34285714
    ## currentregion                         0.48571429
    ## regionpsgc                            0.48571429
    ## currentprovince                       0.51428571
    ## provincepsgc                          0.51428571
    ## currentmunicipalitycity               0.60000000
    ## municipalitycitypsgc                  0.60000000
    ## region_dru                            0.74285714
    ## onset_date                            0.82857143
    ## admitted_date                         0.85714286
    ## healthstatus                          0.88571429
    ## comorbidity                           0.88571429
    ## fever                                 0.88571429
    ## othersignssymptomsspecify             0.88571429
    ## drucity                               0.91428571
    ## druprovince                           0.91428571
    ## cough                                 0.91428571
    ## difficultyofbreathing                 0.91428571
    ## quarantined                           0.91428571
    ## none                                  0.94285714
    ## discharged_date                       0.97142857
    ## casenumber                            1.00000000
    ## otherreportingunit                    1.00000000
    ## specifycomorbidity                    1.00000000
    ## cold                                  1.00000000
    ## sorethroat                            1.00000000
    ## diarrhea                              1.00000000
    ## recovered_date                        1.00000000
    ## died_date                             1.00000000

Almost same percentage of missing data

# Clean some data

Try to clean some data.

Nationality

``` r
all_entries %>%
        count(nationality)
```

    ## # A tibble: 44 x 2
    ##    nationality        n
    ##  * <chr>          <int>
    ##  1 0               2274
    ##  2 American           8
    ##  3 AMERICAN SAMOA     3
    ##  4 AUSTRALIA          1
    ##  5 Australian         6
    ##  6 BANGLADESH         1
    ##  7 British, UK        1
    ##  8 CAMBODIA           1
    ##  9 Canadian           5
    ## 10 CHINA              4
    ## # ... with 34 more rows

Not sure if current location is a good indicator of nationality. Might
be a foreigner staying in PH

``` r
all_entries = all_entries %>%
        mutate(nationality = str_to_title(nationality)) %>%
        mutate(nationality = str_replace(nationality, '^Australia$','Australian'),
               nationality = str_replace(nationality, '^India$','Indian'),
               nationality = str_replace(nationality, 'United Kingdom','British, Uk'),
               nationality = str_replace(nationality, '^Filipino$','Philippine, Filipino'))

#if case number starts with PH
all_entries = all_entries %>%
        mutate(nationality = ifelse(str_detect(casenumber, 'PH'),'Philippine, Filipino', nationality )) 
```

No Typo on health status

``` r
all_entries %>%
        count(healthstatus)
```

    ## # A tibble: 7 x 2
    ##   healthstatus     n
    ## * <chr>        <int>
    ## 1 Asymptomatic  1312
    ## 2 Critical        24
    ## 3 Died           903
    ## 4 Mild          5629
    ## 5 Recovered     3740
    ## 6 Severe          76
    ## 7 <NA>         11025

``` r
all_entries %>%
        select(onset_date) %>%
        mutate(onset_date = mdy(onset_date)) %>%
        arrange(onset_date)
```

    ## # A tibble: 22,709 x 1
    ##    onset_date
    ##    <date>    
    ##  1 1900-01-01
    ##  2 1900-01-01
    ##  3 1900-01-01
    ##  4 1900-01-01
    ##  5 1900-01-01
    ##  6 1900-01-01
    ##  7 1900-01-01
    ##  8 1900-01-01
    ##  9 1900-01-01
    ## 10 1900-01-01
    ## # ... with 22,699 more rows

Im not sure if this is default or not.

``` r
all_entries %>%
        select(casenumber, age,specimen_date, onset_date, result_date) %>%
        mutate(specimen_date = mdy(specimen_date)) %>%
        arrange(specimen_date)
```

    ## # A tibble: 22,709 x 5
    ##    casenumber   age specimen_date onset_date result_date
    ##    <chr>      <dbl> <date>        <chr>      <chr>      
    ##  1 <NA>         120 1900-01-01    <NA>       <NA>       
    ##  2 <NA>          63 1957-02-03    <NA>       5/30/2020  
    ##  3 <NA>          56 1964-03-11    <NA>       6/4/2020   
    ##  4 <NA>          49 1970-12-25    <NA>       4/21/2020  
    ##  5 PH16869       49 1971-04-07    <NA>       5/1/2020   
    ##  6 <NA>          37 1982-08-14    <NA>       5/19/2020  
    ##  7 PH13465       33 1986-08-08    <NA>       5/21/2020  
    ##  8 PH16745       45 2002-05-26    <NA>       5/28/2020  
    ##  9 PH17498       23 2010-05-14    <NA>       5/20/2020  
    ## 10 PH15919       38 2011-01-01    <NA>       5/17/2020  
    ## # ... with 22,699 more rows

Some input their birthday on specimen date. I think result date is much
accurate.

Try to fix specimen date. If the year is below 2020 just change it to
result date.

``` r
all_entries = all_entries %>%
        mutate(specimen_date = ifelse(year(mdy(specimen_date)) < '2020', result_date, specimen_date))
```

``` r
all_entries %>%
        select(casenumber, result_date, datespecimenreceivedbylaboratory) %>%
        mutate(result_date = mdy(result_date)) %>%
        arrange((result_date))
```

    ## # A tibble: 22,709 x 3
    ##    casenumber result_date datespecimenreceivedbylaboratory
    ##    <chr>      <date>      <chr>                           
    ##  1 PH14137    1978-10-17  <NA>                            
    ##  2 PH7948     2002-04-28  4/27/2020                       
    ##  3 <NA>       2002-05-28  <NA>                            
    ##  4 PH14759    2018-05-21  <NA>                            
    ##  5 <NA>       2019-05-27  5/27/2020                       
    ##  6 <NA>       2019-05-27  <NA>                            
    ##  7 PH2389     2020-01-01  <NA>                            
    ##  8 PH7704     2020-01-01  <NA>                            
    ##  9 PH4870     2020-01-01  <NA>                            
    ## 10 PH7425     2020-01-01  <NA>                            
    ## # ... with 22,699 more rows

There are few typos in result date. Try to fix

-   Remove the 1970 entry
-   Change the last digit to 2020
-   If the date is greater than August, use day-month-year format
    instead

``` r
all_entries = all_entries %>%
        mutate(result_date = ifelse(mdy(result_date)< '2000-01-01',NA, result_date)) %>%
        mutate(result_date = str_replace(result_date, '/[:digit:]{4}', '/2020')) %>%
        mutate(result_date = if_else(mdy(result_date) < '2020-08-01', mdy(result_date), dmy(result_date)))
       
#return to original format
all_entries = all_entries %>%
        mutate(result_date = format(result_date, format = '%m/%d/%Y'))
```

There are other date columns and I would probably do the same checking
with the other date columns. The data wrangling will depend on how the
data is collected, what system are they using, are there default values,
etc

Check columns for symptoms

``` r
all_entries %>%
        select(comorbidity:none, -specifycomorbidity) %>%
        stack() %>%
        rename(answer= values, column = ind) %>%
        count(answer)
```

    ##   answer      n
    ## 1     No   1105
    ## 2    yes  16901
    ## 3    Yes   2687
    ## 4   <NA> 160979

Just some typo

Fix typo for those boolean answers

``` r
all_entries = all_entries %>%
        mutate_at(c('comorbidity','fever','cough','cold','sorethroat','difficultyofbreathing','diarrhea','none','quarantined'), str_to_title) 
```

############################### 

Create a function to combine and clean data

``` r
library(tidyverse)
library(lubridate)
library(arsenal)
library(data.table)
library(tidyr)
library(broom)

##load the data

june5 = read_csv('Exam List June 5.csv', na = c('','-','NA'))
june6 = read_csv('Exam List June 6.csv', na = c('','-','NA'))
june7 = read_csv('Exam List June 7.csv', na = c('','-','NA'))

combine_data = function(data1, data2, data3){
        data1$drucity = NA
        data1$druprovince = NA
        
        
                #common values to all
        common_to_all = data3 %>%
                semi_join(data2) %>%
                semi_join(data1)
        
        #in June 7 but not in june 5 and 6
        new_values_june7 = data3 %>%
                anti_join(data2) %>%
                anti_join(data1)
        
        
        #with case number not in june 6 and 7
        cases_in_june5_notin_june67 = data1 %>%
                anti_join(data2, by = c('casenumber')) %>%
                anti_join(data3, by = c('casenumber'))
        
        #with case number in june6 not in june 7
        cases_injune6_notin_june7 = data2 %>%
                anti_join(data3, by = c('casenumber'))
        
        # bind all dataframe
        june7_initial = bind_rows(common_to_all, new_values_june7, cases_in_june5_notin_june67, cases_injune6_notin_june7)
        
        # get the remaining entries from june7
        june7_combined = data3 %>%
                anti_join(june7_initial) %>%
                rbind(june7_initial)
        
        
        # For those without case number that might not be in the june7 combined
        
        #30 patients
        assumed_june5_patients = data1 %>%
                filter(is.na(casenumber)) %>%
                anti_join(june7_combined, by = c('age','sex','nationality','confirminglab','result_date'))
        
        #18 patients
        assumed_june6_patients = data2 %>%
                filter(is.na(casenumber)) %>%
                anti_join(june7_combined, by = c('age','sex','nationality','confirminglab','result_date'))
        
       all_entries = bind_rows(june7_combined, assumed_june5_patients, assumed_june6_patients)
        
       all_entries = all_entries %>%
                distinct() 
                        
       all_entries
}

combined = combine_data(june5, june6, june7)
```

``` r
clean_combined = function(combined){
        all_entries = all_entries %>%
        mutate(nationality = str_to_title(nationality)) %>%
        mutate(nationality = str_replace(nationality, '^Australia$','Australian'),
               nationality = str_replace(nationality, '^India$','Indian'),
               nationality = str_replace(nationality, 'United Kingdom','British, Uk'),
               nationality = str_replace(nationality, '^Filipino$','Philippine, Filipino'))

        #if case number starts with PH
        all_entries = all_entries %>%
                mutate(nationality = ifelse(str_detect(casenumber, 'PH'),'Philippine, Filipino', nationality )) 
        
        all_entries = all_entries %>%
        mutate(specimen_date = ifelse(year(mdy(specimen_date)) < '2020', result_date, specimen_date))
        
        all_entries = all_entries %>%
        mutate(result_date = ifelse(mdy(result_date)< '2000-01-01',NA, result_date)) %>%
        mutate(result_date = str_replace(result_date, '/[:digit:]{4}', '/2020')) %>%
        mutate(result_date = if_else(mdy(result_date) < '2020-08-01', mdy(result_date), dmy(result_date)))
       
        #return to original format
        all_entries = all_entries %>%
                mutate(result_date = format(result_date, format = '%m/%d/%Y'))
        
        #fix typos
        all_entries = all_entries %>%
        mutate_at(c('comorbidity','fever','cough','cold','sorethroat','difficultyofbreathing','diarrhea','none','quarantined'), str_to_title) 
        
        all_entries
}
combined = clean_combined(combined)
```

``` r
combined %>%
        write_csv('combined.csv')
```
