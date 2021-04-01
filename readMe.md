``` r
library(tidyverse)
library(lubridate)
library(arsenal)
library(data.table)
library(tidyr)
library(broom)
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
        arrange(casenumber)
```

    ## # A tibble: 92 x 36
    ##    casenumber   age sex   nationality healthstatus currentregion currentprovince
    ##    <chr>      <dbl> <chr> <chr>       <chr>        <chr>         <chr>          
    ##  1 PH10463       65 Fema~ Philippine~ Recovered    Northern Min~ Misamis Orient~
    ##  2 PH10463       65 Fema~ Philippine~ Recovered    Northern Min~ Misamis Orient~
    ##  3 PH10519       21 Fema~ 0           <NA>         National Cap~ <NA>           
    ##  4 PH10519       21 Male  Philippine~ <NA>         National Cap~ NCR, Fourth Di~
    ##  5 PH10574       52 Fema~ Philippine~ <NA>         National Cap~ <NA>           
    ##  6 PH10574       52 Fema~ Philippine~ <NA>         National Cap~ NCR, Second Di~
    ##  7 PH11142       27 Male  Philippine~ Recovered    CAR           Benguet        
    ##  8 PH11142       26 Male  Philippine~ Recovered    CAR           Benguet        
    ##  9 PH11622       46 Fema~ Philippine~ <NA>         National Cap~ NCR, Fourth Di~
    ## 10 PH11622       46 Fema~ Philippine~ <NA>         <NA>          <NA>           
    ## # ... with 82 more rows, and 29 more variables: currentmunicipalitycity <chr>,
    ## #   regionpsgc <dbl>, provincepsgc <dbl>, municipalitycitypsgc <dbl>,
    ## #   dru <chr>, region_dru <dbl>, otherreportingunit <chr>, confirminglab <chr>,
    ## #   comorbidity <chr>, specifycomorbidity <chr>, fever <chr>, cough <chr>,
    ## #   cold <chr>, sorethroat <chr>, difficultyofbreathing <chr>, diarrhea <chr>,
    ## #   none <chr>, othersignssymptomsspecify <chr>, onset_date <chr>,
    ## #   admitted_date <chr>, specimen_date <chr>,
    ## #   datespecimenreceivedbylaboratory <chr>, result_date <chr>,
    ## #   discharged_date <chr>, recovered_date <chr>, died_date <chr>,
    ## #   quarantined <chr>, drucity <lgl>, druprovince <lgl>

Focusing only on the demographics

PH10519 is both female and male, PH11142 changed age PH12184 is both
chinese and filipino PH2315 is both age 53 and 23

Try to check these casenumbers in june 7 data

``` r
june7 %>%
        filter(casenumber %in% multiple_cases) %>%
        arrange(casenumber) %>%
        distinct(casenumber, sex, age, nationality, .keep_all = F) %>%
        group_by(casenumber) %>%
        add_count() %>%
        filter(n > 1)
```

    ## # A tibble: 52 x 5
    ## # Groups:   casenumber [26]
    ##    casenumber   age sex    nationality              n
    ##    <chr>      <dbl> <chr>  <chr>                <int>
    ##  1 PH10519       21 Female 0                        2
    ##  2 PH10519       21 Male   Philippine, Filipino     2
    ##  3 PH11142       27 Male   Philippine, Filipino     2
    ##  4 PH11142       26 Male   Philippine, Filipino     2
    ##  5 PH11972       71 Male   Philippine, Filipino     2
    ##  6 PH11972       72 Male   Philippine, Filipino     2
    ##  7 PH12184       46 Male   Philippine, Filipino     2
    ##  8 PH12184       46 Male   Chinese                  2
    ##  9 PH12512       69 Male   0                        2
    ## 10 PH12512       64 Male   Philippine, Filipino     2
    ## # ... with 42 more rows

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
        arrange(mdy(specimen_date))
```

    ## # A tibble: 21,241 x 8
    ##    onset_date admitted_date specimen_date datespecimenrec~ result_date
    ##    <chr>      <chr>         <chr>         <chr>            <chr>      
    ##  1 <NA>       <NA>          1/1/1900      <NA>             <NA>       
    ##  2 <NA>       <NA>          2/3/1957      5/29/2020        5/30/2020  
    ##  3 <NA>       <NA>          12/25/1970    <NA>             4/21/2020  
    ##  4 <NA>       <NA>          4/7/1971      <NA>             5/1/2020   
    ##  5 <NA>       <NA>          8/14/1982     5/18/2020        5/19/2020  
    ##  6 <NA>       <NA>          8/8/1986      <NA>             5/21/2020  
    ##  7 <NA>       <NA>          5/26/2002     5/27/2020        5/28/2020  
    ##  8 <NA>       <NA>          5/14/2010     5/16/2020        5/20/2020  
    ##  9 <NA>       <NA>          1/1/2011      <NA>             5/17/2020  
    ## 10 <NA>       <NA>          12/31/2019    5/21/2020        5/21/2020  
    ## # ... with 21,231 more rows, and 3 more variables: discharged_date <chr>,
    ## #   recovered_date <chr>, died_date <chr>

There are specimen dates that are way before 2020. There are also a lot
of 1/1/2020 specimen date which I assume are entry errors. Im trying to
look for ways to complete missing case numbers.

Check for june5 cases that were removed

``` r
june5 %>%
        filter(!(casenumber %in% june6$casenumber))
```

    ## # A tibble: 3 x 36
    ##   casenumber   age sex   nationality healthstatus currentregion currentprovince
    ##   <chr>      <dbl> <chr> <chr>       <chr>        <chr>         <chr>          
    ## 1 PH162         72 Male  Philippine~ Recovered    National Cap~ NCR, Second Di~
    ## 2 PH706         74 Male  Philippine~ Recovered    National Cap~ NCR, Second Di~
    ## 3 PH7540        40 Fema~ Philippine~ Mild         National Cap~ NCR, Second Di~
    ## # ... with 29 more variables: currentmunicipalitycity <chr>, regionpsgc <dbl>,
    ## #   provincepsgc <dbl>, municipalitycitypsgc <dbl>, dru <chr>,
    ## #   region_dru <dbl>, otherreportingunit <chr>, confirminglab <chr>,
    ## #   comorbidity <chr>, specifycomorbidity <chr>, fever <chr>, cough <chr>,
    ## #   cold <chr>, sorethroat <chr>, difficultyofbreathing <chr>, diarrhea <chr>,
    ## #   none <chr>, othersignssymptomsspecify <chr>, onset_date <chr>,
    ## #   admitted_date <chr>, specimen_date <chr>,
    ## #   datespecimenreceivedbylaboratory <chr>, result_date <chr>,
    ## #   discharged_date <chr>, recovered_date <chr>, died_date <chr>,
    ## #   quarantined <chr>, drucity <lgl>, druprovince <lgl>

``` r
june5 %>%
        filter(!(casenumber %in% june7$casenumber))
```

    ## # A tibble: 4 x 36
    ##   casenumber   age sex   nationality healthstatus currentregion currentprovince
    ##   <chr>      <dbl> <chr> <chr>       <chr>        <chr>         <chr>          
    ## 1 PH162         72 Male  Philippine~ Recovered    National Cap~ NCR, Second Di~
    ## 2 PH706         74 Male  Philippine~ Recovered    National Cap~ NCR, Second Di~
    ## 3 PH7540        40 Fema~ Philippine~ Mild         National Cap~ NCR, Second Di~
    ## 4 PH8806        32 Fema~ Philippine~ Asymptomatic National Cap~ NCR, City of M~
    ## # ... with 29 more variables: currentmunicipalitycity <chr>, regionpsgc <dbl>,
    ## #   provincepsgc <dbl>, municipalitycitypsgc <dbl>, dru <chr>,
    ## #   region_dru <dbl>, otherreportingunit <chr>, confirminglab <chr>,
    ## #   comorbidity <chr>, specifycomorbidity <chr>, fever <chr>, cough <chr>,
    ## #   cold <chr>, sorethroat <chr>, difficultyofbreathing <chr>, diarrhea <chr>,
    ## #   none <chr>, othersignssymptomsspecify <chr>, onset_date <chr>,
    ## #   admitted_date <chr>, specimen_date <chr>,
    ## #   datespecimenreceivedbylaboratory <chr>, result_date <chr>,
    ## #   discharged_date <chr>, recovered_date <chr>, died_date <chr>,
    ## #   quarantined <chr>, drucity <lgl>, druprovince <lgl>

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
        arrange(casenumber)
```

    ## # A tibble: 9 x 36
    ##   casenumber   age sex   nationality healthstatus currentregion currentprovince
    ##   <chr>      <dbl> <chr> <chr>       <chr>        <chr>         <chr>          
    ## 1 PH10519       21 Fema~ 0           <NA>         National Cap~ <NA>           
    ## 2 PH10519       21 Male  Philippine~ <NA>         National Cap~ NCR, Fourth Di~
    ## 3 PH13004       27 Male  Philippine~ <NA>         National Cap~ NCR, Second Di~
    ## 4 PH14063       18 Male  Philippine~ Mild         CaLaBaRZon    Batangas       
    ## 5 PH16088       22 Fema~ Philippine~ <NA>         <NA>          <NA>           
    ## 6 PH16449       35 Male  Philippine~ <NA>         CaLaBaRZon    Batangas       
    ## 7 PH16489       11 Male  Philippine~ <NA>         CaLaBaRZon    Batangas       
    ## 8 PH5953        34 Fema~ Philippine~ Asymptomatic CaLaBaRZon    Cavite         
    ## 9 PH5953        35 Male  Philippine~ Asymptomatic CaLaBaRZon    Cavite         
    ## # ... with 29 more variables: currentmunicipalitycity <chr>, regionpsgc <dbl>,
    ## #   provincepsgc <dbl>, municipalitycitypsgc <dbl>, dru <chr>,
    ## #   region_dru <dbl>, otherreportingunit <chr>, confirminglab <chr>,
    ## #   comorbidity <chr>, specifycomorbidity <chr>, fever <chr>, cough <chr>,
    ## #   cold <chr>, sorethroat <chr>, difficultyofbreathing <chr>, diarrhea <chr>,
    ## #   none <chr>, othersignssymptomsspecify <chr>, onset_date <chr>,
    ## #   admitted_date <chr>, specimen_date <chr>,
    ## #   datespecimenreceivedbylaboratory <chr>, result_date <chr>,
    ## #   discharged_date <chr>, recovered_date <chr>, died_date <chr>,
    ## #   quarantined <chr>, drucity <lgl>, druprovince <lgl>

``` r
june7 %>%
        filter(casenumber %in% changed_gender) %>%
        arrange(casenumber)
```

    ## # A tibble: 9 x 36
    ##   casenumber   age sex   nationality healthstatus currentregion currentprovince
    ##   <chr>      <dbl> <chr> <chr>       <chr>        <chr>         <chr>          
    ## 1 PH10519       21 Fema~ 0           <NA>         National Cap~ <NA>           
    ## 2 PH10519       21 Male  Philippine~ <NA>         National Cap~ NCR, Fourth Di~
    ## 3 PH13004       27 Fema~ Philippine~ <NA>         National Cap~ NCR, City of M~
    ## 4 PH14063       18 Fema~ Philippine~ Mild         CaLaBaRZon    Batangas       
    ## 5 PH16088       18 Male  Philippine~ <NA>         Cagayan Vall~ Isabela        
    ## 6 PH16449       35 Fema~ Philippine~ <NA>         CaLaBaRZon    Batangas       
    ## 7 PH16489       11 Fema~ Philippine~ <NA>         CaLaBaRZon    Batangas       
    ## 8 PH5953        34 Fema~ Philippine~ Asymptomatic CaLaBaRZon    Cavite         
    ## 9 PH5953        35 Male  Philippine~ Asymptomatic CaLaBaRZon    Cavite         
    ## # ... with 29 more variables: currentmunicipalitycity <chr>, regionpsgc <dbl>,
    ## #   provincepsgc <dbl>, municipalitycitypsgc <dbl>, dru <chr>,
    ## #   region_dru <dbl>, drucity <chr>, druprovince <chr>,
    ## #   otherreportingunit <chr>, confirminglab <chr>, comorbidity <chr>,
    ## #   specifycomorbidity <chr>, fever <chr>, cough <chr>, cold <chr>,
    ## #   sorethroat <chr>, difficultyofbreathing <chr>, diarrhea <chr>, none <chr>,
    ## #   othersignssymptomsspecify <chr>, onset_date <chr>, admitted_date <chr>,
    ## #   specimen_date <chr>, datespecimenreceivedbylaboratory <chr>,
    ## #   result_date <chr>, discharged_date <chr>, recovered_date <chr>,
    ## #   died_date <chr>, quarantined <chr>

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

Check date entries in June 7 that were not in june5

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
        filter(mdy(result_date) == '2020-12-05')
```

    ## # A tibble: 3 x 36
    ##   casenumber   age sex   nationality healthstatus currentregion currentprovince
    ##   <chr>      <dbl> <chr> <chr>       <chr>        <chr>         <chr>          
    ## 1 PH16632       23 Fema~ Philippine~ <NA>         <NA>          <NA>           
    ## 2 PH16633       30 Male  Philippine~ <NA>         <NA>          <NA>           
    ## 3 PH16634       28 Male  Philippine~ <NA>         <NA>          <NA>           
    ## # ... with 29 more variables: currentmunicipalitycity <chr>, regionpsgc <dbl>,
    ## #   provincepsgc <dbl>, municipalitycitypsgc <dbl>, dru <chr>,
    ## #   region_dru <dbl>, otherreportingunit <chr>, confirminglab <chr>,
    ## #   comorbidity <chr>, specifycomorbidity <chr>, fever <chr>, cough <chr>,
    ## #   cold <chr>, sorethroat <chr>, difficultyofbreathing <chr>, diarrhea <chr>,
    ## #   none <chr>, othersignssymptomsspecify <chr>, onset_date <chr>,
    ## #   admitted_date <chr>, specimen_date <chr>,
    ## #   datespecimenreceivedbylaboratory <chr>, result_date <chr>,
    ## #   discharged_date <chr>, recovered_date <chr>, died_date <chr>,
    ## #   quarantined <chr>, drucity <lgl>, druprovince <lgl>

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
        arrange(desc(parse_number(casenumber)))
```

    ## # A tibble: 443 x 36
    ##    casenumber   age sex   nationality healthstatus currentregion currentprovince
    ##    <chr>      <dbl> <chr> <chr>       <chr>        <chr>         <chr>          
    ##  1 PH20215       47 Male  Philippine~ <NA>         National Cap~ NCR, Second Di~
    ##  2 PH20072       14 Male  Philippine~ <NA>         National Cap~ NCR, City of M~
    ##  3 PH20061       44 Fema~ Philippine~ <NA>         National Cap~ NCR, City of M~
    ##  4 PH20042       46 Fema~ Philippine~ <NA>         National Cap~ NCR, Third Dis~
    ##  5 PH19748       30 Male  Philippine~ <NA>         Central Visa~ Cebu           
    ##  6 PH19746       59 Male  Philippine~ Mild         National Cap~ NCR, Third Dis~
    ##  7 PH19745       10 Fema~ Philippine~ <NA>         Central Visa~ Cebu           
    ##  8 PH19744       14 Male  Philippine~ <NA>         Central Visa~ Cebu           
    ##  9 PH19743        4 Fema~ Philippine~ <NA>         Central Visa~ Cebu           
    ## 10 PH19742       39 Male  Philippine~ <NA>         Central Visa~ Cebu           
    ## # ... with 433 more rows, and 29 more variables: currentmunicipalitycity <chr>,
    ## #   regionpsgc <dbl>, provincepsgc <dbl>, municipalitycitypsgc <dbl>,
    ## #   dru <chr>, region_dru <dbl>, drucity <chr>, druprovince <chr>,
    ## #   otherreportingunit <chr>, confirminglab <chr>, comorbidity <chr>,
    ## #   specifycomorbidity <chr>, fever <chr>, cough <chr>, cold <chr>,
    ## #   sorethroat <chr>, difficultyofbreathing <chr>, diarrhea <chr>, none <chr>,
    ## #   othersignssymptomsspecify <chr>, onset_date <chr>, admitted_date <chr>,
    ## #   specimen_date <chr>, datespecimenreceivedbylaboratory <chr>,
    ## #   result_date <chr>, discharged_date <chr>, recovered_date <chr>,
    ## #   died_date <chr>, quarantined <chr>

There are 443 case numbers that are new on June 7

``` r
june5 %>%
        arrange(desc(parse_number(casenumber)))
```

    ## # A tibble: 21,241 x 36
    ##    casenumber   age sex   nationality healthstatus currentregion currentprovince
    ##    <chr>      <dbl> <chr> <chr>       <chr>        <chr>         <chr>          
    ##  1 PH204263      30 Fema~ Philippine~ <NA>         National Cap~ NCR, Fourth Di~
    ##  2 PH204262      29 Fema~ Philippine~ <NA>         National Cap~ NCR, City of M~
    ##  3 PH19681       59 Fema~ Philippine~ Mild         Northern Min~ Misamis Orient~
    ##  4 PH18588       34 Fema~ Philippine~ <NA>         National Cap~ NCR, Second Di~
    ##  5 PH17883       41 Fema~ Philippine~ <NA>         National Cap~ NCR, Second Di~
    ##  6 PH17224       37 Male  Philippine~ <NA>         Zamboanga Pe~ Zamboanga del ~
    ##  7 PH17223       82 Male  Philippine~ Mild         National Cap~ NCR, Fourth Di~
    ##  8 PH17222       35 Fema~ Philippine~ Recovered    National Cap~ NCR, Second Di~
    ##  9 PH17220       55 Male  Philippine~ <NA>         National Cap~ NCR, City of M~
    ## 10 PH17218       58 Fema~ Philippine~ <NA>         National Cap~ NCR, Second Di~
    ## # ... with 21,231 more rows, and 29 more variables:
    ## #   currentmunicipalitycity <chr>, regionpsgc <dbl>, provincepsgc <dbl>,
    ## #   municipalitycitypsgc <dbl>, dru <chr>, region_dru <dbl>,
    ## #   otherreportingunit <chr>, confirminglab <chr>, comorbidity <chr>,
    ## #   specifycomorbidity <chr>, fever <chr>, cough <chr>, cold <chr>,
    ## #   sorethroat <chr>, difficultyofbreathing <chr>, diarrhea <chr>, none <chr>,
    ## #   othersignssymptomsspecify <chr>, onset_date <chr>, admitted_date <chr>,
    ## #   specimen_date <chr>, datespecimenreceivedbylaboratory <chr>,
    ## #   result_date <chr>, discharged_date <chr>, recovered_date <chr>,
    ## #   died_date <chr>, quarantined <chr>, drucity <lgl>, druprovince <lgl>

Im not sure how these new entries were gathered since the case number
and dates or overlapping.

I saw one entry without a number in case number

``` r
june7 %>%
        filter(!is.na(casenumber)) %>%
        filter(!(str_detect(casenumber,'[:digit:]')))
```

    ## # A tibble: 1 x 36
    ##   casenumber   age sex   nationality healthstatus currentregion currentprovince
    ##   <chr>      <dbl> <chr> <chr>       <chr>        <chr>         <chr>          
    ## 1 PH            21 Male  Philippine~ Mild         Soccsksargen  South Cotabato 
    ## # ... with 29 more variables: currentmunicipalitycity <chr>, regionpsgc <dbl>,
    ## #   provincepsgc <dbl>, municipalitycitypsgc <dbl>, dru <chr>,
    ## #   region_dru <dbl>, drucity <chr>, druprovince <chr>,
    ## #   otherreportingunit <chr>, confirminglab <chr>, comorbidity <chr>,
    ## #   specifycomorbidity <chr>, fever <chr>, cough <chr>, cold <chr>,
    ## #   sorethroat <chr>, difficultyofbreathing <chr>, diarrhea <chr>, none <chr>,
    ## #   othersignssymptomsspecify <chr>, onset_date <chr>, admitted_date <chr>,
    ## #   specimen_date <chr>, datespecimenreceivedbylaboratory <chr>,
    ## #   result_date <chr>, discharged_date <chr>, recovered_date <chr>,
    ## #   died_date <chr>, quarantined <chr>

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
        filter(is.na(casenumber))
```

    ## # A tibble: 5,274 x 36
    ##    casenumber   age sex   nationality healthstatus currentregion currentprovince
    ##    <chr>      <dbl> <chr> <chr>       <chr>        <chr>         <chr>          
    ##  1 <NA>          46 Fema~ Philippine~ <NA>         Unknown       <NA>           
    ##  2 <NA>          40 Male  Philippine~ <NA>         <NA>          <NA>           
    ##  3 <NA>          39 Fema~ Philippine~ <NA>         <NA>          <NA>           
    ##  4 <NA>          29 Male  Philippine~ <NA>         <NA>          <NA>           
    ##  5 <NA>          58 Male  Philippine~ <NA>         <NA>          <NA>           
    ##  6 <NA>          34 Male  Philippine~ <NA>         <NA>          <NA>           
    ##  7 <NA>           4 Fema~ Philippine~ <NA>         <NA>          <NA>           
    ##  8 <NA>          43 Male  Philippine~ <NA>         <NA>          <NA>           
    ##  9 <NA>          42 Male  Philippine~ <NA>         <NA>          <NA>           
    ## 10 <NA>          61 Male  Philippine~ <NA>         <NA>          <NA>           
    ## # ... with 5,264 more rows, and 29 more variables:
    ## #   currentmunicipalitycity <chr>, regionpsgc <dbl>, provincepsgc <dbl>,
    ## #   municipalitycitypsgc <dbl>, dru <chr>, region_dru <dbl>, drucity <chr>,
    ## #   druprovince <chr>, otherreportingunit <chr>, confirminglab <chr>,
    ## #   comorbidity <chr>, specifycomorbidity <chr>, fever <chr>, cough <chr>,
    ## #   cold <chr>, sorethroat <chr>, difficultyofbreathing <chr>, diarrhea <chr>,
    ## #   none <chr>, othersignssymptomsspecify <chr>, onset_date <chr>,
    ## #   admitted_date <chr>, specimen_date <chr>,
    ## #   datespecimenreceivedbylaboratory <chr>, result_date <chr>,
    ## #   discharged_date <chr>, recovered_date <chr>, died_date <chr>,
    ## #   quarantined <chr>

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
        anti_join(june7_combined, by = c('casenumber'))
```

    ## # A tibble: 0 x 36
    ## # ... with 36 variables: casenumber <chr>, age <dbl>, sex <chr>,
    ## #   nationality <chr>, healthstatus <chr>, currentregion <chr>,
    ## #   currentprovince <chr>, currentmunicipalitycity <chr>, regionpsgc <dbl>,
    ## #   provincepsgc <dbl>, municipalitycitypsgc <dbl>, dru <chr>,
    ## #   region_dru <dbl>, otherreportingunit <chr>, confirminglab <chr>,
    ## #   comorbidity <chr>, specifycomorbidity <chr>, fever <chr>, cough <chr>,
    ## #   cold <chr>, sorethroat <chr>, difficultyofbreathing <chr>, diarrhea <chr>,
    ## #   none <chr>, othersignssymptomsspecify <chr>, onset_date <chr>,
    ## #   admitted_date <chr>, specimen_date <chr>,
    ## #   datespecimenreceivedbylaboratory <chr>, result_date <chr>,
    ## #   discharged_date <chr>, recovered_date <chr>, died_date <chr>,
    ## #   quarantined <chr>, drucity <lgl>, druprovince <lgl>

``` r
june6 %>%
        anti_join(june7_combined, by = c('casenumber'))
```

    ## # A tibble: 0 x 36
    ## # ... with 36 variables: casenumber <chr>, age <dbl>, sex <chr>,
    ## #   nationality <chr>, healthstatus <chr>, currentregion <chr>,
    ## #   currentprovince <chr>, currentmunicipalitycity <chr>, regionpsgc <dbl>,
    ## #   provincepsgc <dbl>, municipalitycitypsgc <dbl>, dru <chr>,
    ## #   region_dru <dbl>, drucity <chr>, druprovince <chr>,
    ## #   otherreportingunit <chr>, confirminglab <chr>, comorbidity <chr>,
    ## #   specifycomorbidity <chr>, fever <chr>, cough <chr>, cold <chr>,
    ## #   sorethroat <chr>, difficultyofbreathing <chr>, diarrhea <chr>, none <chr>,
    ## #   othersignssymptomsspecify <chr>, onset_date <chr>, admitted_date <chr>,
    ## #   specimen_date <chr>, datespecimenreceivedbylaboratory <chr>,
    ## #   result_date <chr>, discharged_date <chr>, recovered_date <chr>,
    ## #   died_date <chr>, quarantined <chr>

Looking at case number alone, all data from June 5 and June6 were
already at the june7 combined data

``` r
june6 %>%
        anti_join(june7_combined) %>%
        filter(is.na(casenumber))
```

    ## # A tibble: 517 x 36
    ##    casenumber   age sex   nationality healthstatus currentregion currentprovince
    ##    <chr>      <dbl> <chr> <chr>       <chr>        <chr>         <chr>          
    ##  1 <NA>          47 Male  Philippine~ <NA>         Unknown       <NA>           
    ##  2 <NA>          44 Fema~ Philippine~ <NA>         Unknown       <NA>           
    ##  3 <NA>          30 Male  Philippine~ <NA>         <NA>          <NA>           
    ##  4 <NA>          41 Fema~ Philippine~ <NA>         <NA>          <NA>           
    ##  5 <NA>          33 Male  Philippine~ <NA>         <NA>          <NA>           
    ##  6 <NA>          57 Male  Philippine~ <NA>         <NA>          <NA>           
    ##  7 <NA>          31 Male  Philippine~ <NA>         <NA>          <NA>           
    ##  8 <NA>          36 Fema~ Philippine~ <NA>         <NA>          <NA>           
    ##  9 <NA>          26 Male  Philippine~ <NA>         <NA>          <NA>           
    ## 10 <NA>          29 Male  Philippine~ <NA>         <NA>          <NA>           
    ## # ... with 507 more rows, and 29 more variables: currentmunicipalitycity <chr>,
    ## #   regionpsgc <dbl>, provincepsgc <dbl>, municipalitycitypsgc <dbl>,
    ## #   dru <chr>, region_dru <dbl>, drucity <chr>, druprovince <chr>,
    ## #   otherreportingunit <chr>, confirminglab <chr>, comorbidity <chr>,
    ## #   specifycomorbidity <chr>, fever <chr>, cough <chr>, cold <chr>,
    ## #   sorethroat <chr>, difficultyofbreathing <chr>, diarrhea <chr>, none <chr>,
    ## #   othersignssymptomsspecify <chr>, onset_date <chr>, admitted_date <chr>,
    ## #   specimen_date <chr>, datespecimenreceivedbylaboratory <chr>,
    ## #   result_date <chr>, discharged_date <chr>, recovered_date <chr>,
    ## #   died_date <chr>, quarantined <chr>

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

Remove the 1970 entry Change the last digit to 2020 If the date is
greater than August, use day-month-year format instead

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
