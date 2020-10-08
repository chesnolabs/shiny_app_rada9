amends_by_mps_WO <- read.csv("https://raw.githubusercontent.com/chesnolabs/shiny_app_rada9/master/amends_by_mps_WO.csv",
                             fileEncoding = "Windows-1251") %>%
  select(-X)



amends_by_mps <- read.csv("https://raw.githubusercontent.com/chesnolabs/shiny_app_rada9/master/amends_by_mps.csv", 
                          fileEncoding = "Windows-1251") %>%
  select(-X)



amendments_voting <- read.csv("https://raw.githubusercontent.com/chesnolabs/shiny_app_rada9/master/amendments_voting.csv", 
                          fileEncoding = "Windows-1251") %>%
  select(-X)


act_amends <- amendments_voting %>% 
  filter(for.>225)
  
  
  act_amends <- amendments_voting %>% 
  filter(for.>225)


get_factions_open <- function(){
  posts <- read_csv("https://data.rada.gov.ua/ogd/mps/skl9/mp-posts_ids.csv")
  posts_ids <- read_tsv("https://data.rada.gov.ua/ogd/mps/skl9/mp-posts_unit.txt", 
                        locale(encoding = "windows-1251"), col_names = F, col_types = NULL) %>% 
    rename(unit_id = X1, unit = X2)
  mps <- read_csv("https://data.rada.gov.ua/ogd/mps/skl9/mps09-data.csv")
  
  factions_full <- posts %>% 
    left_join(mps[, c("rada_id" ,"id", "full_name", "date_end", "region_name", "gender")], by = c("mp_id" = "id")) %>% 
    left_join(posts_ids) %>% 
    filter(unit_type %in% c("grp", "fra")) %>% 
    select(mp_id, full_name, unit)
  
  factions_df <-  mps %>% 
    #filter(is.na(resignation_text)) %>% 
    select(rada_id, id, full_name, region_name, date_end, gender) %>% 
    left_join(factions_full, by = c("id" = "mp_id", "full_name")) %>% 
    mutate(unit = ifelse(is.na(unit), "Позафракційні", unit)) %>% 
    rename(factions = unit, fullname = full_name) %>% 
    mutate(factions = recode(factions, 
                             `Фракція ПОЛІТИЧНОЇ ПАРТІЇ "СЛУГА НАРОДУ"` = "Слуга Народу",
                             `Фракція Політичної Партії "ГОЛОС" у Верховній Раді України дев'ятого скликання` = "ГОЛОС",
                             `Фракція ПОЛІТИЧНОЇ ПАРТІЇ "ЄВРОПЕЙСЬКА СОЛІДАРНІСТЬ"` = "ЄС",
                             `Фракція Політичної партії "ОПОЗИЦІЙНА ПЛАТФОРМА - ЗА ЖИТТЯ" у Верховній Раді України` = "ОПЗЖ",
                             `Фракція політичної партії Всеукраїнське об'єднання "Батьківщина" у Верховній Раді України дев'ятого скликання` = "Батьківщина",
                             `Група "Партія "За майбутнє"` = "За майбутнє",
                             `Група "ДОВІРА"`= "ДОВІРА"))%>%
    mutate(rada_id=as.character(rada_id), id=as.integer(id)) %>% 
    mutate(rada_id=recode(rada_id, 
                          "208"="438"))%>% # Можливо тимчасово, бо не встигли змінити айдішник Радіної
    mutate(date_end = ifelse(is.na(date_end), "", date_end))%>% # Replace NA with a blank
    mutate(region_name = ifelse(is.na(region_name), "", region_name)) # Replace NA with a blank
  
  return(factions_df)
}

factions_09 <- get_factions_open()



mps09 <- read.csv("https://data.rada.gov.ua/ogd/mps/skl9/mps09-data.csv", fileEncoding = "UTF-8")%>%
  filter(date_end=="")%>%
  select(id, full_name, region_name) %>%
  mutate(id=as.character(id))


out_amends <- function(){
  
  out_amends <- cSplit(amendments_voting, "results", sep="|", "long")%>%
    separate(results, c("mps_id", "faction", "vote_status"), ":")%>%
    mutate(faction = recode(faction,
                            `0` = "Позафракційні",
                            `1` = "Слуга Народу",
                            `2` = "ОПЗЖ",
                            `3` = "Батьківщина",
                            `4` = "ЄС",
                            `5` = "ГОЛОС",
                            `6` = "За майбутнє",
                            `7`="ДОВІРА",
                            `8` = "За майбутнє"
    ))%>%
    mutate(vote_status = recode(vote_status,
                                `0` = "Відсутній",
                                `1` = "За",
                                `2` = "Проти",
                                `3` = "Утримався",
                                `4` = "Не голосував",
                                `5` = "Присутній"))
  
  
  out_amends$mps_id[out_amends$mps_id ==  "208"] <- "438" # Changed surname in March 2020
  
  out_amends <- out_amends%>%
    mutate(mps_id=as.character(mps_id)) %>% 
    left_join(mps09, by=c("mps_id"="id"))%>%
    left_join(factions_09, by=c("mps_id"="rada_id"))%>%
    mutate(id_question=as.character(id_question))%>%
    filter(date_end=="")
  
  
}

out_amends <- out_amends()

