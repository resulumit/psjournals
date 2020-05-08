library(rio)
library(tidyverse)
library(rvest)
library(magrittr)
library(viridis)

df.jlist <- import("remaining_journals.xlsx") %>%
  mutate(scholar_url = paste("https://scholar.google.ch/citations?hl=en&view_op=search_venues&vq=",
                              '\"',
                              str_replace_all(journal, " ", "+"),
                              '\"',
                              sep = "")) %>%
  select(journal, scholar_url)


datalist <- list()   

for(i in seq(nrow(df.jlist))) {
  
  test <- read_html(df.jlist$scholar_url[i]) %>%
    html_nodes("td.gsc_mvt_t") %>%
    html_text()
  
  if(length(test) > 0) {
  
  df.collect <-data.frame(journal <- df.jlist$journal[i],
                     
                   title <- read_html(df.jlist$scholar_url[i]) %>%
                     html_nodes("td.gsc_mvt_t") %>%
                     html_text(),
                   
                   h5_index <- read_html(df.jlist$scholar_url[i]) %>%
                     html_nodes("td:nth-child(3)") %>%
                     html_text(),
                   
                  h10_index <- read_html(df.jlist$scholar_url[i]) %>%
                     html_nodes("td:nth-child(4)") %>%
                     html_text())

                 datalist[[i]] <- df.collect
  }
}

df.scholar <- do.call(rbind, datalist) %>%
  rename(journal_original = 1, journal_scholar = 2, h5_index = 3, h10_index = 4) %>%
  mutate_at(c("h5_index", "h10_index"), as.character) %>%
  mutate_at(c("h5_index", "h10_index"), as.numeric)

export(df.scholar, "df.rem.scholar.csv")
###########################################################################
###########################################################################


df.scholar <- import("df.scholar.csv")

df <- df.scholar %>%
  mutate(match = case_when(as.character(journal_original) == 
                             as.character(journal_scholar) ~ 1, TRUE ~ 0)) %>%
  group_by(journal_original) %>%
  mutate(count = sum(match)) %>%
  filter(count == 0)


  
  
  
######################################
########################################


links <- read_html("https://ooir.org/journals.php?category=polisci&list=ps") %>%
  html_nodes("#journaltable a") %>%
  html_attr("href")

df <- data.frame(links, stringsAsFactors = FALSE)
df$links <- paste("https://ooir.org/", df$links, sep="")

datalist <- list()   

for(i in seq(nrow(df))) {
  
  df2 <- data.frame(journal <- read_html(df$links[i]) %>%
                     html_nodes("h1") %>%
                     html_text(),
                   url <- read_html(df$links[i]) %>%
                     html_nodes(".underline:nth-child(7)") %>%
                     html_attr("href"))
  
  datalist[[i]] <- df2
  
}

journal.urls <- do.call(rbind, datalist) %>%
  rename(ssci_journal = 1, website = 2) %>% 
  mutate_all(as.character) %>% 
  mutate(ssci_journal = str_trim(ssci_journal), 
         website = str_trim(website))

import("list_journals.xlsx", which = "ssci") %>% 
  left_join(., journal.urls, by = "ssci_journal") %>% 
  export("ssci_journals.xlsx")


###########################################################################
###########################################################################


df.ssci <- import("ssci_journals.xlsx") %>%
  mutate(gscholar_url = paste("https://scholar.google.ch/citations?hl=en&view_op=search_venues&vq=",
                             '\"',
                             gsub("&", "%26", str_replace_all(gscholar_journal, " ", "+")),
                             '\"',
                             sep = "")) %>%
  select(gscholar_journal, gscholar_url)


datalist <- list()   

for(i in seq(nrow(df.ssci))) {
  
  test <- read_html(df.ssci$gscholar_url[i]) %>%
    html_nodes("td.gsc_mvt_t") %>%
    html_text()
  
  if(length(test) > 0) {
    
    df.collect <-data.frame(journal <- df.ssci$gscholar_journal[i],
                            
                            title <- read_html(df.ssci$gscholar_url[i]) %>%
                              html_nodes("td.gsc_mvt_t") %>%
                              html_text(),
                            
                            h5_index <- read_html(df.ssci$gscholar_url[i]) %>%
                              html_nodes("td:nth-child(3)") %>%
                              html_text(),
                            
                            h10_index <- read_html(df.ssci$gscholar_url[i]) %>%
                              html_nodes("td:nth-child(4)") %>%
                              html_text())
    
    datalist[[i]] <- df.collect
  }
}

df.scholar <- do.call(rbind, datalist) %>%
  rename(gscholar_journal = 1, journal_scholar = 2, h5_index = 3, h10_index = 4) %>%
  mutate_at(c("h5_index", "h10_index"), as.character) %>%
  mutate_at(c("h5_index", "h10_index"), as.numeric) %>%
  filter(tolower(as.character(gscholar_journal)) == tolower(as.character(journal_scholar)))

df <- df.scholar %>%
  mutate(match = case_when(tolower(as.character(gscholar_journal)) == 
                             tolower(as.character(journal_scholar)) ~ 1, TRUE ~ 0)) %>%
  group_by(gscholar_journal) %>%
  mutate(count = sum(match)) %>%
  filter(count == 0)

df.found <- df.scholar %>% 
  select(gscholar_journal) %>% 
  distinct() %>% 
  mutate(yes = 1)


matches <- left_join(df.ssci, df.found, by= "gscholar_journal" )


import("ssci_journals.xlsx") %>%
  left_join(., df.scholar, by = "gscholar_journal") %>%
  export("done_journals.xlsx")


import("done_journals.xlsx") %>%
  select(link_journal, rank, h5_index, h10_index) %>%
  export("gscholar.Rda")


#######################################################################################################
#######################################################################################################

import("done_journals.xlsx") %>%
  gather(index, score, h5_index, h5_median) %>%
  ggplot(aes(y = rank, x = score, group = index, colour = index)) +
  geom_point(size=2) +
  geom_smooth(method=lm, aes(fill = index), fullrange = TRUE) +
  coord_cartesian(ylim = c(1, 176), xlim = c(3, 101)) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  ylab("SSCI Rank\n") + xlab("\nGoogle Scholar Metrics") +
  scale_color_viridis(discrete = TRUE, name = "", 
                      breaks = c("h5_index", "h5_median"),
                      labels = c("h5-index", "h5-median")) +
  scale_fill_viridis(discrete = TRUE, name = "", alpha = 0.2,
                     breaks = c("h5_index", "h5_median"),
                     labels = c("h5-index", "h5-median"))

ggsave("scatter.png", width=11.4, height=6.5)
  
