#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(lubridate)
library(shiny)
library(gtsummary)
library(broom.mixed)
library(gt)
library(usmap)


polls <- read_csv("data/presidential-polls.csv", col_types = cols(
  .default = col_character(),
  question_id = col_double(),
  poll_id = col_double(),
  cycle = col_double(),
  pollster_id = col_double(),
  sponsor_ids = col_number(),
  pollster_rating_id = col_double(),
  sample_size = col_double(),
  seat_number = col_double(),
  seat_name = col_logical(),
  sponsor_candidate = col_logical(),
  internal = col_logical(),
  tracking = col_logical(),
  nationwide_batch = col_logical(),
  ranked_choice_reallocated = col_logical(),
  race_id = col_double(),
  candidate_id = col_double(),
  pct = col_double()
)) %>%
  select(end_date, state, poll_id, candidate_name, candidate_party, pct,
         fte_grade) %>%
  rename(date = end_date) %>%
  mutate(date = mdy(date))

# Above, I used library(lubridate) to match the dates up so that I could join
# the data and have the dates match

# Below is all the data used for the polling graphs that I include in the app.
# Each state's COVID numbers came from a separate dataset from the COVID
# Tracking Project.

covid_states <- read_csv("data/us-states-covid.csv", col_types = cols(
  date = col_date(format = ""),
  state = col_character(),
  fips = col_character(),
  cases = col_double(),
  deaths = col_double(),
  confirmed_cases = col_double(),
  confirmed_deaths = col_double(),
  probable_cases = col_double(),
  probable_deaths = col_double()
)) %>%
  select(date, state, cases, deaths)


covid_wisconsin <- read_csv("data/wisconsin-history.csv", col_types = cols(
  .default = col_double(),
  date = col_date(format = ""),
  state = col_character(),
  dataQualityGrade = col_character(),
  negativeTestsAntibody = col_logical(),
  negativeTestsPeopleAntibody = col_logical(),
  negativeTestsViral = col_logical(),
  onVentilatorCumulative = col_logical(),
  positiveTestsAntibody = col_logical(),
  positiveTestsAntigen = col_logical(),
  positiveTestsPeopleAntibody = col_logical(),
  positiveTestsPeopleAntigen = col_logical(),
  positiveTestsViral = col_logical(),
  totalTestsAntibody = col_logical(),
  totalTestsAntigen = col_logical(),
  totalTestsPeopleAntibody = col_logical(),
  totalTestsPeopleAntigen = col_logical()
)) %>%
  select(date, death, deathIncrease, hospitalized, hospitalizedCumulative,
         hospitalizedIncrease, positive, positiveIncrease) %>%
  mutate(date = ymd(date))

wisconsin_covid_polls <- polls %>%
  filter(state == "Wisconsin") %>%
  right_join(covid_wisconsin, by = "date")

covid_michigan <- read_csv("data/michigan-history.csv", col_types = cols(
  .default = col_double(),
  date = col_date(format = ""),
  state = col_character(),
  dataQualityGrade = col_character(),
  negativeTestsAntibody = col_logical(),
  negativeTestsPeopleAntibody = col_logical(),
  negativeTestsViral = col_logical(),
  onVentilatorCumulative = col_logical(),
  positiveTestsAntibody = col_logical(),
  positiveTestsAntigen = col_logical(),
  positiveTestsPeopleAntibody = col_logical(),
  positiveTestsPeopleAntigen = col_logical(),
  positiveTestsViral = col_logical(),
  totalTestsAntibody = col_logical(),
  totalTestsAntigen = col_logical(),
  totalTestsPeopleAntibody = col_logical(),
  totalTestsPeopleAntigen = col_logical()
)) %>%
  select(date, death, deathIncrease, hospitalized, hospitalizedCumulative,
         hospitalizedIncrease, positive, positiveIncrease) %>%
  mutate(date = ymd(date))

michigan_covid_polls <- polls %>%
  filter(state == "Michigan") %>%
  right_join(covid_michigan, by = "date")

covid_pennsylvania <- read_csv("data/pennsylvania-history.csv",
                               col_types = cols(
                                 .default = col_double(),
                                 date = col_date(format = ""),
                                 state = col_character(),
                                 dataQualityGrade = col_character(),
                                 negativeTestsAntibody = col_logical(),
                                 negativeTestsPeopleAntibody = col_logical(),
                                 negativeTestsViral = col_logical(),
                                 onVentilatorCumulative = col_logical(),
                                 positiveTestsAntibody = col_logical(),
                                 positiveTestsAntigen = col_logical(),
                                 positiveTestsPeopleAntibody = col_logical(),
                                 positiveTestsPeopleAntigen = col_logical(),
                                 positiveTestsViral = col_logical(),
                                 totalTestsAntibody = col_logical(),
                                 totalTestsAntigen = col_logical(),
                                 totalTestsPeopleAntibody = col_logical(),
                                 totalTestsPeopleAntigen = col_logical()
                               )) %>%
  select(date, death, deathIncrease, hospitalized, hospitalizedCumulative,
         hospitalizedIncrease, positive, positiveIncrease) %>%
  mutate(date = ymd(date))


# I individually joined each state's COVID numbers with the polling data with
# right_join

pennsylvania_covid_polls <- polls %>%
  filter(state == "Pennsylvania") %>%
  right_join(covid_pennsylvania, by = "date")

covid_arizona <- read_csv("data/arizona-history.csv", col_types = cols(
  .default = col_double(),
  date = col_date(format = ""),
  state = col_character(),
  dataQualityGrade = col_character(),
  negativeTestsAntibody = col_logical(),
  negativeTestsPeopleAntibody = col_logical(),
  negativeTestsViral = col_logical(),
  onVentilatorCumulative = col_logical(),
  positiveTestsAntibody = col_logical(),
  positiveTestsAntigen = col_logical(),
  positiveTestsPeopleAntibody = col_logical(),
  positiveTestsPeopleAntigen = col_logical(),
  positiveTestsViral = col_logical(),
  totalTestsAntibody = col_logical(),
  totalTestsAntigen = col_logical(),
  totalTestsPeopleAntibody = col_logical(),
  totalTestsPeopleAntigen = col_logical()
)) %>%
  select(date, death, deathIncrease, hospitalized, hospitalizedCumulative,
         hospitalizedIncrease, positive, positiveIncrease) %>%
  mutate(date = ymd(date))

arizona_covid_polls <- polls %>%
  filter(state == "Arizona") %>%
  right_join(covid_arizona, by = "date")

covid_georgia <- read_csv("data/georgia-history.csv", col_types = cols(
  .default = col_double(),
  date = col_date(format = ""),
  state = col_character(),
  dataQualityGrade = col_character(),
  negativeTestsAntibody = col_logical(),
  negativeTestsPeopleAntibody = col_logical(),
  negativeTestsViral = col_logical(),
  onVentilatorCumulative = col_logical(),
  positiveTestsAntibody = col_logical(),
  positiveTestsAntigen = col_logical(),
  positiveTestsPeopleAntibody = col_logical(),
  positiveTestsPeopleAntigen = col_logical(),
  positiveTestsViral = col_logical(),
  totalTestsAntibody = col_logical(),
  totalTestsAntigen = col_logical(),
  totalTestsPeopleAntibody = col_logical(),
  totalTestsPeopleAntigen = col_logical()
)) %>%
  select(date, death, deathIncrease, hospitalized, hospitalizedCumulative,
         hospitalizedIncrease, positive, positiveIncrease) %>%
  mutate(date = ymd(date))

georgia_covid_polls <- polls %>%
  filter(state == "Georgia") %>%
  right_join(covid_georgia, by = "date")


# I also did this with national data.

covid_national <- read_csv("data/national-history.csv", col_types = cols(
  .default = col_double(),
  date = col_date(format = ""),
  state = col_character(),
  dataQualityGrade = col_character(),
  negativeTestsAntibody = col_logical(),
  negativeTestsPeopleAntibody = col_logical(),
  negativeTestsViral = col_logical(),
  onVentilatorCumulative = col_logical(),
  positiveTestsAntibody = col_logical(),
  positiveTestsAntigen = col_logical(),
  positiveTestsPeopleAntibody = col_logical(),
  positiveTestsPeopleAntigen = col_logical(),
  positiveTestsViral = col_logical(),
  totalTestsAntibody = col_logical(),
  totalTestsAntigen = col_logical(),
  totalTestsPeopleAntibody = col_logical(),
  totalTestsPeopleAntigen = col_logical()
)) %>%
  select(date, death, deathIncrease, hospitalizedCumulative,
         hospitalizedIncrease, positive, positiveIncrease) %>%
  mutate(date = ymd(date))

national_covid_polls <- polls %>%
  right_join(covid_national, by = "date")

covid_county <- read_csv("data/us-counties-covid.csv", col_types = cols(
  date = col_date(format = ""),
  county = col_character(),
  state = col_character(),
  fips = col_double(),
  cases = col_double(),
  deaths = col_double(),
  confirmed_cases = col_double(),
  confirmed_deaths = col_double(),
  probable_cases = col_double(),
  probable_deaths = col_double()
))

# Now I start getting into the data cleaning for presidential results, which was
# the most time-consuming part of the project.

results <- read_csv("data/presidential-results.csv", col_types = cols(
  .default = col_double(),
  fips = col_double(),
  name = col_character(),
  absentee_method = col_character(),
  eevp_value = col_character(),
  eevp_display = col_character(),
  eevp_source = col_character(),
  absentee_count_progress = col_character(),
  absentee_outstanding = col_logical(),
  provisional_outstanding = col_logical(),
  provisional_count_progress = col_logical(),
  last_updated = col_datetime(format = ""),
  leader_margin_display = col_character(),
  leader_margin_name_display = col_character(),
  leader_party_id = col_character(),
  state = col_character(),
  retrieved_time = col_datetime(format = ""),
  results_kingr = col_logical(),
  results_absentee_kingr = col_logical(),
  results_segalj = col_logical(),
  results_absentee_segalj = col_logical()
  # ... with 36 more columns)
)) %>%
  rename(county = name) %>%
  select(fips, county, votes, absentee_votes, results_trumpd, results_bidenj,
         results_absentee_trumpd, results_absentee_bidenj, leader_party_id,
         leader_margin_display, margin2020, margin2016, votes2016, state) %>%
  rename(biden_votes = results_bidenj) %>%
  rename(trump_votes = results_trumpd)

# I renamed a few variables in the results dataset so they would be easier to
# understand

pop <- read_csv("data/PopulationEstimates.csv", col_types = cols(
  .default = col_number(),
  FIPStxt = col_double(),
  State = col_character(),
  Area_Name = col_character(),
  `Rural-urban_Continuum Code_2003` = col_double(),
  `Rural-urban_Continuum Code_2013` = col_double(),
  Urban_Influence_Code_2003 = col_double(),
  Urban_Influence_Code_2013 = col_double(),
  Economic_typology_2015 = col_double(),
  RESIDUAL_2010 = col_double(),
  RESIDUAL_2011 = col_double(),
  RESIDUAL_2016 = col_double(),
  RESIDUAL_2017 = col_double(),
  RESIDUAL_2018 = col_double(),
  RESIDUAL_2019 = col_double(),
  R_birth_2011 = col_double(),
  R_birth_2012 = col_double(),
  R_birth_2013 = col_double(),
  R_birth_2014 = col_double(),
  R_birth_2015 = col_double(),
  R_birth_2016 = col_double()
  # ... with 48 more columns
)) %>%
  rename(fips = FIPStxt) %>%
  rename(population = POP_ESTIMATE_2019) %>%
  rename(rucc = `Rural-urban_Continuum Code_2013`) %>%
  select(fips, population, rucc)

# RUCC was much easier than a column name with spaces. It is a measure by the
# USDA's ERS of how rural a given county is.

education <- read_csv("data/Education.csv", col_types = cols(
  .default = col_double(),
  State = col_character(),
  `Area name` = col_character(),
  `Less than a high school diploma, 1970` = col_number(),
  `High school diploma only, 1970` = col_number(),
  `Some college (1-3 years), 1970` = col_number(),
  `Four years of college or higher, 1970` = col_number(),
  `Less than a high school diploma, 1980` = col_number(),
  `High school diploma only, 1980` = col_number(),
  `Some college (1-3 years), 1980` = col_number(),
  `Four years of college or higher, 1980` = col_number(),
  `Less than a high school diploma, 1990` = col_number(),
  `High school diploma only, 1990` = col_number(),
  `Some college or associate's degree, 1990` = col_number(),
  `Bachelor's degree or higher, 1990` = col_number(),
  `Less than a high school diploma, 2000` = col_number(),
  `High school diploma only, 2000` = col_number(),
  `Some college or associate's degree, 2000` = col_number(),
  `Bachelor's degree or higher, 2000` = col_number(),
  `Less than a high school diploma, 2014-18` = col_number(),
  `High school diploma only, 2014-18` = col_number()
  # ... with 2 more columns
)) %>%
  rename(fips = `FIPS Code`) %>%
  rename(pct_degree = `Percent of adults with a bachelor's degree or higher, 2014-18`) %>%
  select(fips, pct_degree)

# I had to ensure that all of the fips columns were called "fips" so that they
# could be joined successfully.

poverty <- read_csv("data/PovertyEstimates.csv", col_types = cols(
  .default = col_double(),
  FIPStxt = col_double(),
  Stabr = col_character(),
  Area_name = col_character(),
  POVALL_2018 = col_number(),
  CI90LBAll_2018 = col_number(),
  CI90UBALL_2018 = col_number(),
  POV017_2018 = col_number(),
  CI90LB017_2018 = col_number(),
  CI90UB017_2018 = col_number(),
  POV517_2018 = col_number(),
  CI90LB517_2018 = col_number(),
  CI90UB517_2018 = col_number(),
  MEDHHINC_2018 = col_number(),
  CI90LBINC_2018 = col_number(),
  CI90UBINC_2018 = col_number(),
  POV04_2018 = col_number(),
  CI90LB04_2018 = col_number(),
  CI90UB04_2018 = col_number()
)) %>%
  rename(fips = FIPStxt)


unemployment <- read_csv("data/Unemployment.csv", col_types = cols(
  .default = col_number(),
  FIPStxt = col_double(),
  Stabr = col_character(),
  area_name = col_character(),
  Rural_urban_continuum_code_2013 = col_double(),
  Urban_influence_code_2013 = col_double(),
  Metro_2013 = col_double(),
  Unemployment_rate_2000 = col_double(),
  Unemployment_rate_2001 = col_double(),
  Unemployment_rate_2002 = col_double(),
  Unemployment_rate_2003 = col_double(),
  Unemployment_rate_2004 = col_double(),
  Unemployment_rate_2005 = col_double(),
  Unemployment_rate_2006 = col_double(),
  Unemployment_rate_2007 = col_double(),
  Unemployment_rate_2008 = col_double(),
  Unemployment_rate_2009 = col_double(),
  Unemployment_rate_2010 = col_double(),
  Unemployment_rate_2011 = col_double(),
  Unemployment_rate_2012 = col_double(),
  Unemployment_rate_2013 = col_double()
  # ... with 7 more columns
)) %>%
  rename(fips = FIPStxt) %>%
  mutate(unemployment_rate_chg_2000_2019 = Unemployment_rate_2019 - Unemployment_rate_2000) %>%
  select(unemployment_rate_chg_2000_2019, unemployment_rate_chg_2000_2019,
         Unemployment_rate_2000, fips, Median_Household_Income_2018,
         Med_HH_Income_Percent_of_State_Total_2018)

# I mutated in order to get the change in unemployment rate between 2000 and
# 2019.

results_2016 <- read_csv("data/tidy_data.csv", col_types = cols(
  .default = col_double(),
  fips = col_double(),
  name_16 = col_character(),
  votes16_jacobp = col_logical(),
  votes16_whitej = col_logical(),
  votes16_mooreheadm = col_logical(),
  votes16_none_of_these_candidates = col_logical(),
  votes16_duncanr = col_logical(),
  votes16_skewesp = col_logical(),
  votes16_giordanir = col_logical(),
  name_prev = col_character(),
  ST = col_character(),
  statecode_prev = col_character(),
  County = col_character(),
  State = col_character(),
  temp_bins = col_character(),
  lat_bins = col_character(),
  lon_bins = col_character(),
  precip_bins = col_character(),
  elevation_bins = col_character()
)) %>%
  select(fips, votes, votes16_trumpd, votes16_clintonh, rep16_frac, dem16_frac,
         dem08, rep08, dem08_frac, rep08_frac, dem12, rep12, dem12_frac,
         rep12_frac) %>%
  rename(trump_votes_16 = votes16_trumpd) %>%
  rename(clinton_votes_16 = votes16_clintonh) %>%
  rename(trump_pct_16 = rep16_frac) %>%
  rename(clinton_pct_16 = dem16_frac) %>%
  mutate(trump_pct_16 = trump_pct_16 * 100) %>%
  mutate(clinton_pct_16 = clinton_pct_16 * 100)

# Here, I brought in results from the 2016 election on the county level. These
# were shockingly hard to get. I mutated to get the Trump and Clinton percent
# columns out of decimal form and renamed several columns.

covid_results <- results %>%
  right_join(covid_county, by = "fips") %>%
  right_join(pop, by = "fips") %>%
  right_join(results_2016, by = "fips") %>%
  right_join(education, by = "fips") %>%
  right_join(unemployment, by = "fips") %>%
  mutate(cases_per_capita = cases / population) %>%
  mutate(biden_pct = (biden_votes / votes.x) * 100) %>%
  mutate(trump_pct = (trump_votes / votes.x) * 100) %>%
  mutate(diff = biden_pct - trump_pct) %>%
  mutate(trump_flip = case_when(leader_party_id == "republican" & margin2016 < 0 ~ TRUE, TRUE ~ FALSE)) %>%
  mutate(biden_flip = case_when(leader_party_id == "democrat" & margin2016 > 0 ~ TRUE, TRUE ~ FALSE)) %>%
  mutate(trump_pct_dif_16_20 = trump_pct - trump_pct_16) %>%
  mutate(biden_pct_dif_16_20 = biden_pct - clinton_pct_16) %>%
  mutate(trump_vote_dif_16_20 = trump_votes - trump_votes_16) %>%
  mutate(cases_per_10000 = cases / (population / 10000)) %>%
  mutate(deaths_per_100000 = deaths / (population / 100000))

# Here is the big join. I used right_join() to bring in all of the data that I
# loaded up above. I then created columns to indicate when a county is a flipped
# county, to show the candidates percentages, the difference in there vote share
# from '16 to '20, a cases per capita column, a cases per 10000 residents
# column, and a deaths per 100,000 residents column (along with a few others
# that didn't get used much).

map_tibble <- covid_results %>%
  mutate(cases1 = log(cases_per_10000 + 1))

# I created a tibble that I later plug into a map here. I used
# log(cases_per_10000 + 1) so differences could more easily be seen across
# counties.

# I then defined a UI for my app!

ui <- fluidPage(
  
# First, I created a page explaining the scope of the project that can welcome
# visitors.
  
  navbarPage(
    "The Presidential Politics of COVID-19",
    tabPanel("Introduction",
             titlePanel("Introduction"),
             p("Nothing shaped the 2020 election more than COVID-19. The arrival
             of the virus upended campaigning in its traditional sense, became
             the top issue for millions of voters, and provided shocks that
             changed the race."),
             p("This project will provide a framework for understanding the
             virus’ effect on the race. First, it shows how the virus’
             trajectory compared to the trajectory of the two candidates,
             Joseph R. Biden Jr. — the projected winner — and incumbent
             President Donald J. Trump. It will then explore how the race's 
               results compared to the virus' spread on a county-by-county
               basis."),
             p("The models presented in this project are predictive, and causal
             inferences should not be drawn from the data in the project.
             Voting data, via The New York Times, up to date as of November 24,
             2020. Coronavirus data is courtesy of The Atlantic's COVID
             Tracking Project. Population data included in the project are from
             the U.S. Department of Agriculture’s Economic Research Service."),
             
# Below, I print out the maps that I created.
             
             mainPanel(plotOutput("county_potus_results")),
             mainPanel(plotOutput("covid_map"))),

# My second tab is where most of my graphs are, and where most of the analysis
# is done.
    
    tabPanel("The Candidates vs. COVID",
             titlePanel("How The Virus Affected Electoral Performance"),
             
             # Sidebar with a slider input for number of bins 
             sidebarLayout(
               sidebarPanel(
                 
                 
# The second thing I did was use a select input to allow users to select the
# state they want to see. Differences across states are the most interesting
# part.
                 
               selectInput("state1",
                           "Select State:",
                           c("Alabama", "Alaska", 
                             "Arizona", 
                             "Arkansas", "California", "Colorado", 
                             "Connecticut", "Delaware", "District of Columbia",
                             "Florida", "Georgia", "Hawaii", "Idaho",
                             "Illinois", "Indiana", "Iowa", "Kansas",
                             "Kentucky", "Louisiana", "Maine", "Maryland",
                             "Massachusetts", "Michigan", "Minnesota", 
                             "Mississippi", "Missouri", "Montana", "Nebraska",
                             "Nevada", "New Hampshire", "New Jersey",
                             "New Mexico", "New York", "North Carolina",
                             "North Dakota", "Ohio",
                             "Oklahoma", "Oregon", "Pennsylvania",
                             "Rhode Island", "South Carolina",
                             "South Dakota", "Tennessee", "Texas", "Utah",
                             "Vermont",
                             "Virginia", "Washington", "West Virginia",
                             "Wisconsin", "Wyoming")),
               
# I first created a radio button that users can select the Y variable they are
# most interested in with. Unfortunately, the labels on the Y axes became
# slightly messed up and it removed the data labels from the graphs. But it is
# working! I thought it was most valuable to have users be able to select a
# variable so they could make comparisons for themselves.
               
               radioButtons("variable",
                            "Select Y Variable for National Graph:",
                            c("Change in Trump's Vote Share Since 2016",
                              "Change in the Democratic Vote Share Since 2016",
                              "Trump Vote Share 2020",
                              "Biden Vote Share 2020"))),
               
# Below is where I finally print out my plots!
               
               mainPanel(plotOutput("support_v_covid"),
                         p("On both the national and state levels, case rates
                           are not correlated with outcomes. Despite the
                           widespread view among pundits in the media that
                           Trump was badly damaged by the virus, data shows that
                           his margis did not suffer in areas hit harder by the
                           virus."),
                         plotOutput("support_v_covid_national"),
                         p("In counties where Joe Biden made the most signifigant
                           gains compared to Hillary Clinton's performance in 
                           the 2016 election, there appears to be more of a
                           weak positive correlation between Biden's gains and
                           the virus' spread."),
                         plotOutput("support_v_covid_topbiden"),
                         p("In counties that Biden flipped, there is little
                           correlation to the virus' spread."),
                         plotOutput("biden_flips"))
    )),
    
    tabPanel("COVID-Polling Correlation",
             titlePanel("COVID-Polling Correlation"),
             sidebarLayout(
               sidebarPanel(
                 selectInput("state",
                             "Select Swing State:",
                             c("Michigan", "Pennsylvania", "Arizona", "Georgia",
                               "Wisconsin"))),
               
               # Show a plot of the generated distribution
               
               mainPanel(plotOutput("distPlot"),
                         p("Similarly, on both the national and state levels,
                           there does not appear to be a correlation between
                           polling data and cumulative cases."),
                         plotOutput("national_polls_covid"))
             )
    ),
    
    tabPanel("Model",
             
# I then included my regression model, which is explained below.
             
             titlePanel("Trump Support is Positively Correlated with COVID-19
                        Case Rates"),
             p("The idea for this model came from the paper, 'The COVID-19 
             Pandemic and the 2020 U.S. Presidential Election', published by IZA
             Institute of Labor Economics. I sought to create a model that used 
             per capita COVID-19 rates to predict change Trump support from 
             2016, while controlling for relevant demographic variables. I felt
             that measuring change in Trump's support from 2016 would be more 
             telling than smiply looking at support for Trump in 2020,
               although the correlations are similar."),
             
# I used gt_output to include the model here.

             gt_output(outputId = "regression_model"),

             p("The model predicts that Trump's support will increase with an
               increase in COVID-19 cases. The model predicts that Trump will 
               see an average increase in support of 7.8% for every increase of
               1 case per capita. The 95% confidence interval falls between 2.6%
               and 13%. Additionally, the model clearly shows an 
               increase as the Rural-Urban Continuum Code increases. In other 
               words, the more rural a given county is per the ERS's metric,
               the more positive the change in Trump support is likely to be.
               Trump support is also slightly negatively correlated with
               education rates, according to the model.")
             
    ),
    tabPanel("About",
             titlePanel("About"),
             
# And, finally, here is my about page!
             
             h3("Goals & Data"),
             p("The aim of this project is to prive a framework for understanding
           how COVID-19 impacted the result of the 2020 election. The election 
           data used in the project comes from The New York Times. The COVID-19
               data comes from The New York Times, the COVID Tracking Project by
               The Atlantic, and John's Hopkins University. The population and
               demographic data used comes from the United States Department of
               Agriculture's Economic Research Service."),
             h3("About Me"),
             p("I am a sophomore at Harvard College studying Government. I can be
           reached at jaspergoodman@college.harvard.edu. You can find my code
           on my", a("GitHub account.", 
           href = "https://github.com/JasperGoodman")))

  )
)

# Below is my server, where the magic is made.

server <- function(input, output) {
  
# For the polling plots, I had to use if() functions and create them all
# separately because they all use different datasets. I suppose I could have
# joined them all into one large dataset, which probably would have been more
# efficient. But, hey, this worked.
  
  output$distPlot <- renderPlot({
    if(input$state == "Pennsylvania"){
      Out <- pennsylvania_covid_polls %>%
        
# I assigned all the plots to an object called Out, which I print at the end.
        
        filter(candidate_party == c("DEM", "REP")) %>%
        
# I wanted to look at only Biden and Trump so I filtered for c("DEM", "REP").
        
        ggplot(mapping = aes(x = positive, y = pct, color = candidate_party)) +
        geom_point() +
        geom_smooth() +
        scale_color_manual(values = c("blue", "red"),
                           labels = c("Biden", "Trump"),
                           name = "Candidate") +
        
# I used scale_color_manual to make the legend work, and used red and blue as
# the colors for each candidate, which has been consistent throughout.
        
        theme_minimal() +
        labs(title = "COVID's Correlation with Presidential Polling in Pennsylvania",
             x = "Cases",
             y = "Polling Percentage")
    }
    
    if(input$state=="Arizona"){
      
# I then did the same exact thing for all of the 5 swing states that flipped
# blue -- Michigan, Pennsylvania, Wisconsin, Arizona, and Georgia.
      
      Out <- arizona_covid_polls %>%
        filter(candidate_party == c("DEM", "REP")) %>%
        ggplot(mapping = aes(x = positive, y = pct, color = candidate_party)) +
        geom_point() +
        geom_smooth() +
        scale_color_manual(values = c("blue", "red"),
                           labels = c("Biden", "Trump"),
                           name = "Candidate") +
        theme_minimal() +
        labs(title = "COVID's Correlation with Presidential Polling in Arizona",
             x = "Cases",
             y = "Polling Percentage")
    }
    
    if(input$state=="Michigan"){
      Out <- michigan_covid_polls %>%
        filter(candidate_party == c("DEM", "REP")) %>%
        ggplot(mapping = aes(x = positive, y = pct, color = candidate_party)) +
        geom_point() +
        geom_smooth() +
        scale_color_manual(values = c("blue", "red"),
                           labels = c("Biden", "Trump"),
                           name = "Candidate") +
        theme_minimal() +
        labs(title = "COVID's Correlation with Presidential Polling in Michigan",
             x = "Cases",
             y = "Polling Percentage")
    }
    
    if(input$state=="Georgia"){
      Out <- georgia_covid_polls %>%
        filter(candidate_party == c("DEM", "REP")) %>%
        ggplot(mapping = aes(x = positive, y = pct, color = candidate_party)) +
        geom_point() +
        geom_smooth() +
        scale_color_manual(values = c("blue", "red"),
                           labels = c("Biden", "Trump"),
                           name = "Candidate") +
        theme_minimal() +
        labs(title = "COVID's Correlation with Presidential Polling in Georgia",
             x = "Cases",
             y = "Polling Percentage")
    }
    
    if(input$state=="Wisconsin"){
      Out <- wisconsin_covid_polls %>%
        filter(candidate_party == c("DEM", "REP")) %>%
        ggplot(mapping = aes(x = positive, y = pct, color = candidate_party)) +
        geom_point() +
        geom_smooth() +
        scale_color_manual(values = c("blue", "red"),
                           labels = c("Biden", "Trump"),
                           name = "Candidate") +
        theme_minimal() +
        labs(title = "COVID's Correlation with Presidential Polling in Wisconsin",
             x = "Cases",
             y = "Polling Percentage")
    }
    
# I printed out the object Out at the end to display the plot.
    
    Out
  })
  
# I then did the exact thing with national polling data.
  
  output$national_polls_covid <- renderPlot({
    national_covid_polls %>%
      filter(candidate_party == c("DEM", "REP")) %>%
      filter(fte_grade == c("B", "B+", "A-", "A", "A+")) %>%
      
# Because there was so much national polling data, I had to use only some of it.
# Luckily, FiveThirtyEight grades all polls. So I selected only the top-rated
# polls, per their grading.
      
      ggplot(mapping = aes(x = positive, y = pct, color = candidate_party)) +
      geom_point() +
      geom_smooth() +
      scale_color_manual(values = c("blue", "red"),
                         labels = c("Biden", "Trump"),
                         name = "Candidate") +
      theme_minimal() +
      labs(title = "COVID's Correlation with Presidential Polling in the US",
           x = "Cases",
           y = "Polling Percentage")
  })
  
  
  output$regression_model <- render_gt({
    
# This output is for my model. I used as_gt() to create the regression table.
    
    tbl_regression(fit, intercept = TRUE) %>% 
      as_gt() %>%
      tab_header(title = "Regression of Change in County-Level Trump Vote Share by COVID-19 Cases Per Capita, Rural-Urban Continuum Code, and Percentage of Residents with a College Degree")
    
  })
  
  output$county_potus_results <- renderPlot({
    
# This output is for the county-level election map that I made. I used
# plot_usmap.
    
    plot_usmap(regions = "counties", values = "leader_party_id",
               data = covid_results, 
               exclude = "AK") +
      
# I excluded Alaska because they don't have counties. I set regions equal to
# counties so it would be a county-level map. The values were set equal to
# leader_party_id, which is in the original results dataset and signifies who
# won a given county.
      
      scale_fill_manual(breaks = c("democrat", "republican"),
                        values = c("blue", "red"),
                        name = "Winner",
                        labels = c("Joseph R. Biden Jr.*", "Donald J. Trump")) +
      
# I used the same scale_fill_manual() as in the plots above (with a different
# name, of course) so that the colors would be consistent throughout.
      
      labs(title = "2020 United States Presidential Election Results by County",
           caption = "Source: The New York Times \n * Denotes Projected Winner")
  
  })
  
    output$covid_map <- renderPlot({
      
# I also used plot_usmap() to create a map of COVID cases rates, also on the
# county level. Way up at the top I used log(cases + 1) and assigned it to
# map_tibble, which is inputted here.
    
    plot_usmap(regions = "counties", values = "cases1", data = map_tibble, 
               exclude = "AK") +
      scale_fill_viridis_c(option = "C") +
      labs(title = "United States COVID-19 Cases Per 10,000 by County",
           caption = "Source: COVID Tracking Project") +
        
# I tried forever to use scale functions to fix this legend, but it didn't work.
        
      scale_color_manual(breaks = c(7, 5, 3),
                         labels = c("1500", "1000", "500"),
                         name = "COVID Cases Per 10,000 by County") +
        theme(legend.position = "none")
    
  })
  
  output$support_v_covid <- renderPlot({
                   
covid_results %>%
    filter(state.y == input$state1) %>%
      
# I had the graph filter to look at the state being selected in the sidebar
# panel.
      
    ggplot(mapping = aes(x = cases_per_10000,
                         y = trump_pct_dif_16_20,
                         size = population,
                         color = leader_party_id)) +
    scale_color_manual(breaks = c("democrat", "republican"),
                       values = c("blue", "red"),
                       name = "County Winner",
                       labels = c("Joseph R. Biden Jr.*", "Donald J. Trump")) +
    geom_jitter() +
    theme_classic() +
    labs(title = "COVID-19 Case Rates vs. Selected Variable in Selected State",
         x = "Cases Per 10,000 Residents by County",
         y = "Change in Trump Support from 2016") +
  geom_hline(yintercept = 0, col = "darkblue")
  
  })
  
  output$support_v_covid_national <- renderPlot({
    
# Here is where I create the main plots of my app. I use switch() to swap
# variables with the selection users make on the sidebar panel.
    
  data2 <- switch(input$variable,
                 "Change in Trump's Vote Share Since 2016" = covid_results$trump_pct_dif_16_20,
                 "Change in the Democratic Vote Share Since 2016" = covid_results$biden_pct_dif_16_20,
                 "Trump Vote Share 2020" = covid_results$trump_pct,
                 "Biden Vote Share 2020" = covid_results$biden_pct)
  
# Above, I set all the things I have on the radio button panel equal to their
# corresponding variable from the dataset.
  
  covid_results %>%
    ggplot(mapping = aes(x = cases_per_10000,
                         y = data2,
                         
# Here I set Y equal to data2, which is created by switch() up above.
                         
                         size = population,
                         color = leader_party_id)) +
    
# I set size equal to population and color equal to leader_party_id.
    
    scale_color_manual(breaks = c("democrat", "republican"),
                       values = c("blue", "red"),
                       name = "County Winner",
                       labels = c("Joseph R. Biden Jr.*", "Donald J. Trump")) +
    
# I again used the same scale_color_manual() to keep things consistent.
    
    geom_jitter() +
    theme_classic() +
    labs(title = "COVID-19 Case Rates vs. Selected Variable",
         x = "Cases Per 10,000 Residents by County",
         y = "Selected Variable")
  
# I was messing around with geom_hline() for a while, but it didn't do anything
# on the graph, which was disappointing.
    
    
  })
  
  output$support_v_covid_topbiden <- renderPlot({
    
# I created a plot to look at the counties where Democrats gained the most
# ground. This one is not interactive.
    
  covid_results %>%
      
# For some reason to look at counties where the Democrats gained a lot, I had to
# use desc() and not the other way around. I couldn't figure out why, but I
# checked to make sure the numbers were right and that's the way it needed to
# be.
      
    arrange(desc(biden_pct_dif_16_20)) %>%
    slice(1:80) %>%
      
# I sliced to look at the 80 counties where the Democrats gained the most vote
# share.
      
    ggplot(mapping = aes(x = cases_per_10000,
                         y = biden_pct_dif_16_20, size = population,
                         color = leader_party_id)) +
    scale_color_manual(breaks = c("democrat", "republican"),
                       values = c("blue", "red"),
                       name = "County Winner",
                       labels = c("Joseph R. Biden Jr.*", "Donald J. Trump")) +
    geom_jitter() +
    theme_classic() +
    labs(title = "COVID-19 Case Rates vs. Democratic Gain",
         subtitle = "Among the 80 Counties Where Democrats Gained the Largest Vote Share",
         x = "Cases Per 10,000 Residents by County",
         y = "Difference in Vote Share from 2016")
  
  })
  
  output$biden_flips <- renderPlot({
  
  covid_results %>%
    filter(biden_flip == TRUE) %>%
    ggplot(mapping = aes(x = cases_per_10000,
                         y = biden_pct_dif_16_20, size = population,
                         color = leader_party_id)) +
    scale_color_manual(breaks = c("democrat", "republican"),
                       values = c("blue", "red"),
                       name = "County Winner",
                       labels = c("Joseph R. Biden Jr.*", "Donald J. Trump")) +
    geom_jitter() +
    theme_classic() +
    labs(title = "COVID-19 Case Rates vs. Democratic Gain",
         subtitle = "Among Counties Flipped by Joe Biden",
         x = "Cases Per 10,000 Residents by County",
         y = "Difference in Vote Share from 2016")
  
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
