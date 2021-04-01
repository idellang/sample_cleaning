

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



combined %>%
        write_csv('combined.csv')


