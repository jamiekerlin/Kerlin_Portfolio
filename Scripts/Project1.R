library(tidyverse)
library(rvest)

### Save website ####
website <- read_html("https://www.amazon.com/gp/bestsellers/fashion/ref=zg-bs_fashion_dw_sml")

names <- c("rankings", "products", "stars", "number_reviews")
html_code <- c(".zg-bdg-text", "._cDEzb_p13n-sc-css-line-clamp-3_g3dy1", ".aok-align-top", "#gridItemRoot .a-size-small")

#generate a 4 column dataframe with specific column names
top30_20230306 <- data.frame(matrix(NA, nrow=30, ncol=4))
colnames(top30_20230306) <- html_code


for (i in html_code){
  html <- html_nodes(website, i)
  txt <- html_text(html)
  names <- c("rankings", "products", "stars", "number_reviews")

  top30_20230306[1:30, i] <- txt 
}

colnames(top30_20230306) <- names


### Clean dataframe ####
top30_20230306_clean <- top30_20230306 %>%
  mutate(rankings = as.numeric(gsub("#", "", rankings)),
         stars = as.numeric(gsub(" out of 5 stars", "", stars)),
         number_reviews = as.numeric(gsub(",", "", number_reviews)), 
         products = tolower(products),
         products = str_replace(products, " \\s*\\([^\\)]+\\)", "")) 


keywords <- c("sweatshirt","shirt", "tank", "blouse", "hoodie", "pant", "shoe", 
              "loafer", "clog", "slipper", "bra", "boxer", "shapewear", "underwear", 
              "socks", "luggage")
categories <- c("sweatshirt", "shirt", "shirt", "shirt", "sweatshirt", "pant", 
                "shoe", "shoe", "shoe", "shoe", "intimates", "intimates", "intimates",
                "intimates", "socks", "luggage")
lookup_table <- data.frame(keywords, categories)


categorize <- 
expand.grid(products = top30_20230306_clean$products, keywords=lookup_table$keywords) %>%  # create all combinations
  mutate_all(as.character) %>%                                          # update to character (if needed)
  mutate(v = map2_lgl(products, keywords, ~grepl(.y, .x))) %>%        # check if there's a match
  group_by(products) %>%                                              # for each new label
  summarise(keywords = ifelse(sum(v) > 0, keywords[v==TRUE], NA)) %>%   # get the keyword if there is one
  left_join(lookup_table, by="keywords") %>%                            # join categoris
  select(-keywords)  

top30_20230306_categorized <- top30_20230306_clean %>%
  left_join(categorize)


### Now do it again for older version of the site ####
### Save website ####
website_20230129 <- read_html("https://web.archive.org/web/20230129184158/https://www.amazon.com/gp/bestsellers/fashion/ref=zg-bs_fashion_dw_sml")

names <- c("rankings", "products", "stars", "number_reviews")
html_code <- c(".zg-bdg-text", "._cDEzb_p13n-sc-css-line-clamp-3_g3dy1", ".aok-align-top", "#gridItemRoot .a-size-small")

#generate a 4 column dataframe with specific column names
top30_20230129 <- data.frame(matrix(NA, nrow=30, ncol=4))
colnames(top30_20230129) <- html_code


for (i in html_code){
  html <- html_nodes(website_20230129, i)
  txt <- html_text(html)
  names <- c("rankings", "products", "stars", "number_reviews")
  
  top30_20230129[1:30, i] <- txt 
}

colnames(top30_20230129) <- names


### Clean dataframe ####
top30_20230129_clean <- top30_20230129 %>%
  mutate(rankings = as.numeric(gsub("#", "", rankings)),
         stars = as.numeric(gsub(" out of 5 stars", "", stars)),
         number_reviews = as.numeric(gsub(",", "", number_reviews)), 
         products = tolower(products),
         products = str_replace(products, " \\s*\\([^\\)]+\\)", ""))


keywords <- c("sweatshirt","shirt", "tank", "blouse", "hoodie", "pant", "shoe", 
              "loafer", "clog", "slipper", "bra", "boxer", "shapewear", "underwear", 
              "socks", "luggage", "jacket", "overalls", "beanie", "hat", "bodysuit")
categories <- c("jacket", "shirt", "shirt", "shirt", "jacket", "pant", 
                "shoe", "shoe", "shoe", "shoe", "intimates", "intimates", "intimates",
                "intimates", "socks", "luggage", "jacket", "pant", "hat", "hat", "shirt")
lookup_table <- data.frame(keywords, categories)


categorize <- 
  expand.grid(products = top30_20230129_clean$products, keywords=lookup_table$keywords) %>%  # create all combinations
  mutate_all(as.character) %>%                                          # update to character (if needed)
  mutate(v = map2_lgl(products, keywords, ~grepl(.y, .x))) %>%        # check if there's a match
  group_by(products) %>%                                              # for each new label
  summarise(keywords = ifelse(sum(v) > 0, keywords[v==TRUE], NA)) %>%   # get the keyword if there is one
  left_join(lookup_table, by="keywords") %>%                            # join categoris
  select(-keywords)  

top30_20230129_categorized <- top30_20230129_clean %>%
  left_join(categorize)


