library(httr)
library(XML)
library(xlsx)
library(dplyr)
library(jsonlite)
myvk <- oauth_app("appfordata",
                  key = "5231387",
                  secret = "NmRYnpPuU5mzwppsQAPS")
vk <- oauth_endpoint(NULL,
      authorize = "https://oauth.vk.com/authorize",
      access = "https://oauth.vk.com/access_token"
)
vk_auth <- oauth2.0_token(vk, myvk, scope = "offline", cache = FALSE)
tmp <- strsplit(toString(names(vk_auth$credentials)), '"')
token <- tmp[[1]][4]
#################### USER SEARCH ####################
#АРГУМЕНТЫ ПОИСКА
## User search
us_se <- "https://api.vk.com/method/users.search?"
vers <- "&v=5.44"
## Сдвиг поиска (Всегда первый!)
offset <- "offset=50"
## Age from
age_from <- "&age_from=21"
## Age to
age_to <- "&age_to=29"
##FIELDS
      ## Personal
      pers <- "personal"
      ## Messaging
      mess <- "can_write_private_message"
      ## Occupation
      occu <- "occupation"
            ## CODE
            comma <- "%2C"
            fields <- paste("&fields=", pers, comma, mess, comma, occu, sep = "")
##Размер выдачи
counting <- "&count=1000"
##Группа поиска
group_id <- "&group_id=55284725"
##University MSU
uni <- "&university=2"
##Moscow 
moscow <- "&city=1" 
zapros <- paste(us_se, offset, age_from, age_to, counting, group_id, uni, moscow, fields, vers,
                "&access_token=", token, sep = "")
user_info <- fromJSON(zapros)
political <- rep("NULL", length(user_info$response$items$id))
df <- tbl_df(data.frame(1:length(user_info$response$items$id), user_info$response$items$id, 
                  political, user_info$response$items$first_name, user_info$response$items$last_name, 
                  user_info$response$items$can_write_private_message, user_info$response$items$occupation$type))
colnames(df) <- c("no", "id", "political", "First name", "Last name", "Message", "Occupation type")
df$political <- as.numeric(df$political)
for (i in 1:length(df$no)) {
      if (length(user_info$response$items$personal[[i]]$political) > 0) {
            df[i,"political"] <- user_info$response$items$personal[[i]]$political
      } else {df[i,"political"] <- NA}
}
df <- filter(df, !is.na(political) & Message == 1)

#################### WALL SEARCH ####################
wall_search <- function(extended=0, poisk = "путин") {
      ## Wall search
      wa_se <- "https://api.vk.com/method/wall.search?"
      ## Query
      query <- "&query="
      ## Extended
      ext_c <- "&extended="
      ext_v <- 0
      df$wall <- as.numeric(rep(NA, nrow(df)))
      while(anyNA(df$wall) == TRUE) {
            nas <- is.na(df$wall)
            nas_vec <- df$id[nas]
            for (k in 1:length(nas_vec)) {
                  zapros3 <- paste(wa_se, "&owner_id=", nas_vec[k], query, poisk, ext_c, ext_v, vers, "&access_token=", token, sep = "")
                  wall_info <- fromJSON(zapros3)
                  if(length(wall_info$response$count) > 0) {
                        df[which(df$id == nas_vec[k]), "wall"] <- wall_info$response$count
                  } else {df[k, "wall"] <- NA}
            }
      }
      message("Ready!")
}

df <- filter(df, wall > 0)
df <- mutate(df, link = paste("https://vk.com/id", df$id, sep = ""))
write.xlsx(df, "user_info.xlsx", sheetName = "last names", col.names = TRUE)
