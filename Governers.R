library(robotstxt)
library(curl)
library(rvest)

paths_allowed(
  paths = c("https://en.wikipedia.org/wiki/List_of_Governors_of_Reserve_Bank_of_India")
)

# Since the o/p is TRUE we can go ahead with the extraction of data

rbi_guv <- read_html("https://en.wikipedia.org/wiki/List_of_Governors_of_Reserve_Bank_of_India")

rbi_guv

table <- rbi_guv %>%
  html_nodes("table") %>%
  html_table()
View(table)

# There are three tables in the web page 

rbi_guv %>%
  html_nodes("table") %>%
  html_table() %>%
  extract2(2) -> profile

profile %>%
  separate(`Term in office`, into = c("term", "days")) %>%
  select(Officeholder, term) %>%
  arrange(desc(as.numeric(term))) -> profile_1

profile %>%
  count(Background) ->background

profile %>%
  pull(Background) %>%
  fct_collapse(
    Bureaucrats = c("IAS officer", "ICS officer",
                    "Indian Administrative Service (IAS) officer",
                    "Indian Audit and Accounts Service officer",
                    "Indian Civil Service (ICS) officer"),
    `No Info` = c(""),
    `RBI Officer` = c("Career Reserve Bank of India officer")
  ) %>%
  fct_count() %>%
  rename(background = f, count = n) -> backgrounds  

backgrounds %>%
  ggplot() +
  geom_col(aes(background, count), fill = "blue") +
  xlab("Background") + ylab("Count") +
  ggtitle("Background of RBI Governors")
