library(httr)
library(XML)
library(xlsx)
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
#ÀÐÃÓÌÅÍÒÛ ÏÎÈÑÊÀ
##Constanta
vk_ad <- "https://api.vk.com/method/users.search?"
vers <- "&v=5.44"
##Ñäâèã ïîèñêà (Âñåãäà ïåðâûé!)
offset <- "offset=100"
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
##Ðàçìåð âûäà÷è
counting <- "&count=300"
##Ãðóïïà ïîèñêà
group_id <- "&group_id=29534144"
zapros <- paste(vk_ad, offset, counting, fields, group_id, vers,"&access_token=", token, sep = "")
user_info <- fromJSON(zapros)
df <- data.frame(no = 1:length(user_info$response$items$id))
political <- rep("NULL", length(df$no))
df <- cbind(df, user_info$response$items$id, political, user_info$response$items$first_name, user_info$response$items$last_name, user_info$response$items$can_write_private_message, user_info$response$items$occupation$type)
colnames(df) <- c("no", "id", "political", "First name", "Last name", "Message", "Occupation type")
df$political <- as.numeric(df$political)
for (i in 1:length(df$no)) {
      if (length(user_info$response$items$personal[[i]]$political) > 0) {
            df[i,"political"] <- user_info$response$items$personal[[i]]$political
      } else {df[i,"political"] <- NA}
}
write.xlsx(df, "user_info.xlsx", sheetName = "last names", col.names = TRUE)
