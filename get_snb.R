library(jsonlite)
library(tidyverse)

convert_date <- function(date_in) {
  if(grepl("-Q", date_in[1])) {
    q <- substr(date_in, nchar(date_in), nchar(date_in))
    q <- as.numeric(q) * 3 -1
    out <- paste0(substr(date_in, 1,5), q, "-15")
  } else if(nchar(date_in[1])==7) out <- paste0(date_in, "-15")
  else if(nchar(date_in[1])==4) out <- paste0(date_in, "-12-31")
  else out <- date_in
  
  as.Date(out)
}


# Good for testing
tablename <- "bahypoakredq"  # has same duplicate IDs
tablename <- "snboffzisa"  # has names spread over levels

get_snb <- function(tablename) {
  
  data_url <- paste0("https://data.snb.ch/api/cube/", tablename, "/data/csv/en")
  meta_url <- paste0("https://data.snb.ch/api/cube/", tablename, "/dimensions/en")
  
  # Download data file
  t <- tempfile()
  download.file(data_url, t, quiet=T)
  suppressMessages({
<<<<<<< HEAD
    data <- 
=======
    data <-
>>>>>>> 662035bd59d7b529cb2a12940bcfb19bc33c06f7
      read.delim(t, skip=3, sep=";") %>%
      #read_delim(t, skip=3, delim=";",  locale = locale(decimal_mark = ".")) %>% 
      #data.table::fread(t, skip=3) %>% tibble() %>%   # in case read_delim breaks
      unite("ticker", !contains(c("Date", "Value")), remove = F) %>% 
      mutate(Value = as.numeric(Value)) %>% 
      filter(!is.na(Value))
  })
  
  # Download meta data
  download.file(meta_url, t, quiet=T)
  meta <- fromJSON(t) 
  unlink(t)

  meta <- meta$dimensions  # we only need the dimensions here
  levels <- unlist(meta) %>% names() %>% str_count(., "dimensionItems") %>% max()
  
  # Determine the metadata ids from the colnames
  # to be used later
  dim_ids <- colnames(data)[-c(1,2,ncol(data))]
  dim_ids_list <- lapply(dim_ids, function(i) data %>% select(all_of(i)) %>% distinct())
  names(dim_ids_list) <- dim_ids 
  
  # Pull information from JSON to get metadata using unnest
  cont <- list()
  suppressMessages({
    cont[[1]] <- unnest(meta, cols = c("dimensionItems"), names_repair = "unique")
    if(levels >= 2) {
      for(i in seq(2,levels)) {
        res <- unnest(cont[[i-1]], cols = c("dimensionItems"), names_repair = "unique")
        cont[[i]] <- res
      }
    }
    
  })
  
  ids <- dim_ids_list %>% unlist() %>% unname()

  # Make a dataframe of all id combinations and create a combined ticker
  # not a nice thing: text1 is a text that will be eval-uated
  text1 <- 
    paste0("dim_ids_list$",  dim_ids) %>% 
    paste0(., "$", dim_ids, collapse=",")
  
  tickers <- eval(parse(text=paste0("expand.grid(", text1,")"))) %>% 
    unite(ticker, contains("Var"), remove=F) %>% 
    filter(ticker %in% unique(data$ticker))
  colnames(tickers)[2:ncol(tickers)] <- dim_ids 


  labels <- 
    bind_rows(cont) %>% 
    select(-contains("dimensionItems"), -name...2) %>% 
    #select(-contains("dimensionItems")) %>% 
    unite("label", contains("name"), sep=", ", na.rm=T) %>% 
    filter(if_any(contains("id"), ~ . %in% ids)) %>% 
    rename(dim_no = id...1) %>% 
    pivot_longer(contains("id"), names_to="var") %>% 
    filter(value %in% ids) %>% 
    select(-var)
  
  for(i in dim_ids) {
    tickers <- tickers %>% 
      left_join(labels %>% filter(dim_no == i), by=setNames("value", i))
  }
  
  tickers <- tickers %>% 
    unite("label", contains("label"), sep=", ") %>% 
    select(ticker, label)
  
  data %>% 
    left_join(tickers, by=c("ticker")) %>% 
    rename(date = Date, value = Value) %>% 
    select(date, value, label, ticker) %>% 
    mutate(date = convert_date(date)) %>% 
    mutate(label = gsub(" , ", ", ", label) %>% str_squish(.)) %>% 
    arrange(ticker, date) %>% 
    filter(!is.na(value))
}

tablename <- "snbgwdchfsgw"
tablename <- "snbimfra"
tablename <- "snbiprogq"
tablename <- "snboffzisa"
tablename <- "capcollch"
tablename <- "ausshawarm"
tablename <- "bahypoakredq"
tablename <- "plkoprinfla"
tablename <- "plimoinreg"
tablename <- "rendoblid"
#get_snb(tablename)

new <- get_snb(tablename)

ggplot(new) + 
  aes(x=date, y=value, group=ticker, color=label) + 
  geom_line() + 
  theme(legend.position = "bottom")
