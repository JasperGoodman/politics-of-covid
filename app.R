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
library(gt)
library(usmap)

# This is just a normal object

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
# Make change to your dataset

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
  select(date, death, deathIncrease, hospitalized, hospitalizedCumulative, hospitalizedIncrease, positive, positiveIncrease) %>%
  mutate(date = ymd(date))

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
  select(fips, county, votes, absentee_votes, results_trumpd, results_bidenj, results_absentee_trumpd, results_absentee_bidenj, leader_party_id, leader_margin_display, margin2020, margin2016, votes2016, state) %>%
  rename(biden_votes = results_bidenj) %>%
  rename(trump_votes = results_trumpd)

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
  select(unemployment_rate_chg_2000_2019, unemployment_rate_chg_2000_2019, Unemployment_rate_2000, fips, Median_Household_Income_2018, Med_HH_Income_Percent_of_State_Total_2018)



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
  select(fips, votes, votes16_trumpd, votes16_clintonh, rep16_frac, dem16_frac, dem08, rep08, dem08_frac, rep08_frac, dem12, rep12, dem12_frac, rep12_frac) %>%
  rename(trump_votes_16 = votes16_trumpd) %>%
  rename(clinton_votes_16 = votes16_clintonh) %>%
  rename(trump_pct_16 = rep16_frac) %>%
  rename(clinton_pct_16 = dem16_frac) %>%
  mutate(trump_pct_16 = trump_pct_16 * 100) %>%
  mutate(clinton_pct_16 = clinton_pct_16 * 100)


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

regression <- tibble(tibble(Coefficient = 32.5,
                            Intercept = 2.5))

map_tibble1 <- covid_results %>%
  mutate(cases1 = log(cases + 1))

# Define UI for application that draws a histogram

ui <- fluidPage(
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
             President Donald J. Trump. It will then explore how the virus
             interrupted the race in a structural sense by looking at absentee
             voting rates and their electoral implications."),
             p("The models presented in this project are predictive, and causal
             inferences should not be drawn from the data in the project.
             Voting data, via The New York Times, up to date as of November 24,
             2020. Coronavirus data is courtesy of The Atlantic's COVID
             Tracking Project. Population data included in the project are from
             the U.S. Department of Agriculture’s Economic Research Service."),
             
             mainPanel(plotOutput("county_potus_results")),
             mainPanel(plotOutput("covid_map"))),
    
    tabPanel("Trump vs. COVID",
             titlePanel("How The Virus Affected Electoral Performance"),
             
             # Sidebar with a slider input for number of bins 
             sidebarLayout(
               sidebarPanel(
                 radioButtons("variable",
                             "Select Y Variable:",
                             c("Change in Trump's Vote Share Since 2016",
                               "Change in the Democratic Vote Share Since 2016",
                               "Trump Vote Share 2020",
                               "Biden Vote Share 2020")),
               selectInput("state1",
                           "Select State:",
                           c("Alabama", "Alaska", "American Samoa", 
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
                             "Wisconsin", "Wyoming"))),
               
               # Show a plot of the generated distribution
               
               mainPanel(plotOutput("support_v_covid"),
                         plotOutput("support_v_covid_national"))
    )),
    
    tabPanel("COVID-Polling Correlation",
             titlePanel("COVID-Polling Correlation"),
             
             
             
             # Sidebar with a slider input for number of bins 
             sidebarLayout(
               sidebarPanel(
                 selectInput("state",
                             "Select Swing State:",
                             c("Michigan", "Pennsylvania", "Arizona", "Georgia",
                               "Wisconsin"))),
               
               # Show a plot of the generated distribution
               
               mainPanel(plotOutput("distPlot"),
                         plotOutput("national_polls_covid"))
             )
    ),
    
    tabPanel("Absentee Voting",
             titlePanel("Biden Benefited from Absentee Votes"),
             p("Absentee votes counted in the days following November 3 delivered the 
  presidency to Joe Biden, pushing him well over 270 electoral votes with 
  wins in Wisconsin, Michigan, Pennsylvania, Arizona, and Georgia — all 
  states won by President Trump in 2016. But it wasn’t only in the swing 
  states that Biden benefitted from absentee votes. There is a positive 
  correlation between the proportion of absentee votes in a given U.S. county 
  and Biden’s result. This regression model predicts that for every increase 
  in the proportion of votes that were absentee in a given county, Biden’s
  vote share increased by 2.5 percent."),
             gt_output(outputId = "regression_model")
    ),
    tabPanel("About",
             titlePanel("About"),
             h3("Goals"),
             p("The aim of this project is to prive a framework for understanding
           how COVID-19 impacted the result of the 2020 election. In the days 
           before the project is officially due, I hope to accomplish several
           major changes. First off, I hope to create a model and graphics that
           incorporate results, creating a regression model that compares
           support for the given candidates and case rates and/or death rates.
           Additionally, I provide further graphics and data on the page
           looking at absentee voting. For instance, I plan to illustrate just
           how large the increase in absentee voting, and to analyze how
           mail-in voting affected turnout."),
             h3("About Me"),
             p("I am a sophomore at Harvard College studying Government. I can be
           reached at jaspergoodman@college.harvard.edu. You can find my code
           on my GitHub account."))
  )
)



# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    if(input$state == "Pennsylvania"){
      Out <- pennsylvania_covid_polls %>%
        filter(candidate_party == c("DEM", "REP")) %>%
        ggplot(mapping = aes(x = positive, y = pct, color = candidate_party)) +
        geom_point() +
        geom_smooth() +
        scale_color_manual(values = c("blue", "red"),
                           labels = c("Biden", "Trump"),
                           name = "Candidate") +
        theme_minimal() +
        labs(title = "COVID's Correlation with Presidential Polling in Pennsylvania",
             x = "Cases",
             y = "Polling Percentage")
    }
    
    if(input$state=="Arizona"){
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
    Out
  })
  
  output$national_polls_covid <- renderPlot({
    national_covid_polls %>%
      filter(candidate_party == c("DEM", "REP")) %>%
      filter(fte_grade == c("B", "B+", "A-", "A", "A+")) %>%
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
    gt(regression) %>%
      tab_header(title = "Linear Regression",              
                 subtitle = "The Effect of Absentee Voting on Joe Biden's County-Level Vote Share")
    
  })
  
  output$county_potus_results <- renderPlot({
    plot_usmap(regions = "counties",
               values = "leader_party_id",
               data = map_tibble, 
               exclude = "AK") +
      scale_fill_manual(breaks = c("democrat", "republican"),
                        values = c("blue", "red"),
                        name = "Winner",
                        labels = c("Joseph R. Biden Jr.*", "Donald J. Trump")) +
      labs(title = "2020 United States Presidential Election Results by County",
           caption = "Source: The New York Times \n * Denotes Projected Winner")
    
  })
  
  output$covid_map <- renderPlot({
    plot_usmap(regions = "counties", values = "cases1", data = map_tibble1, 
               exclude = "AK") +
      scale_fill_viridis_c(option = "C") +
      labs(title = "United States COVID-19 Cases by County",
           caption = "Source: COVID Tracking Project")
    
  })
  
  output$support_v_covid <- renderPlot({
    
    data <- switch(input$variable,
                   "Change in Trump's Vote Share Since 2016" = covid_results$trump_pct_dif_16_20,
                   "Change in the Democratic Vote Share Since 2016" = covid_results$biden_pct_dif_16_20,
                   "Trump Vote Share 2020" = covid_results$trump_pct,
                   "Biden Vote Share 2020" = covid_results$biden_pct)

                   
  covid_results %>%
    filter(state.y == input$state1) %>%
    ggplot(mapping = aes(x = cases_per_10000,
                         y = input$variable,
                         size = population,
                         color = leader_party_id)) +
    scale_color_manual(breaks = c("democrat", "republican"),
                       values = c("blue", "red"),
                       name = "County Winner",
                       labels = c("Joseph R. Biden Jr.*", "Donald J. Trump")) +
    geom_jitter() +
    theme_classic() +
    labs(title = "COVID-19 Case Rates vs. Selected Variable in Selected State",
         x = "Cases Per 1,000 Residents by County")
  
  })
  
  output$support_v_covid_national <- renderPlot({
    
  data2 <- switch(input$variable,
                 "Change in Trump's Vote Share Since 2016" = covid_results$trump_pct_dif_16_20,
                 "Change in the Democratic Vote Share Since 2016" = covid_results$biden_pct_dif_16_20,
                 "Trump Vote Share 2020" = covid_results$trump_pct,
                 "Biden Vote Share 2020" = covid_results$biden_pct)
  
  
  covid_results %>%
    ggplot(mapping = aes(x = cases_per_10000,
                         y = input$variable,
                         size = population,
                         color = leader_party_id)) +
    scale_color_manual(breaks = c("democrat", "republican"),
                       values = c("blue", "red"),
                       name = "County Winner",
                       labels = c("Joseph R. Biden Jr.*", "Donald J. Trump")) +
    geom_jitter() +
    theme_classic() +
    labs(title = "COVID-19 Case Rates vs. Selected Variable",
         x = "Cases Per 1,000 Residents by County")
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
