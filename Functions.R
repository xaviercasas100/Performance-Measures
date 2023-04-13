# Libraries
library(tidyverse)

# Adjust dates

fix_date_00 <- function(x){
    x %>% 
        mdy_hm()
}

# This function is used to create the credential rate table
# for the adult population
x_cred_adult <-
function(wda_no) {
 cred_tbl <-  credentials %>% 
        mutate(`local_wda_no` = local_wda_no) %>% 
     # Filter by wda_no and adult population
    filter(`local_wda_no` == wda_no, Program == funds[[1]]) %>% 
     # Grouping by quarter then calculate num/den
    group_by(Quarter) %>% 
    summarise(Num=sum(`Texas Credential Numerator`),
              Den = n(),
              Percent = percent(Num/Den, accuracy = 0.1)) %>% 
    mutate(Target = case_when(wda_no == 5 ~ 68.10,
                                  wda_no == 14 ~ 65.40,
                                  wda_no == 18 ~ 84.60,
                                  wda_no == 19 ~ 68.70,
                                  wda_no == 20 ~ 76.10,
                                  wda_no == 22 ~ 65.60,
                                  wda_no == 23 ~ 79.40
                                  ),
           "% of Target" = percent((Num/Den)/(Target/100),accuracy = 0.01))
 # add table to kbl function
 cred_tbl %>% 
    kbl(caption = "Credential - Adult",
      align = "c",
      digits = 2) %>% 
  kable_styling(bootstrap_options = c("striped","hover","condensed"),
                full_width = TRUE)
}

x_cred_DW <-
function(wda_no) {
  credentials %>% 
        mutate(`local_wda_no` = local_wda_no) %>%
    filter(`local_wda_no` == wda_no, Program == funds[[2]]) %>% 
    group_by(Quarter) %>% 
    summarise(Num=sum(`Texas Credential Numerator`),
              Den = n(),
              Percent = percent(Num/Den, accuracy = 0.01)) %>% 
    mutate(Target = case_when(wda_no == 5 ~ 79.60,
                                  wda_no == 14 ~ 85.00,
                                  wda_no == 18 ~ 79.60,
                                  wda_no == 19 ~ 85.00,
                                  wda_no == 20 ~ 85.00,
                                  wda_no == 22 ~ 79.60,
                                  wda_no == 23 ~ 85.00),
           "% of Target" = percent((Num/Den)/(Target/100), accuracy = 0.01))%>% 
    kbl(caption = "Credential - DW",
      align = "c",
      digits = 2) %>% 
  kable_styling(bootstrap_options = c("striped","hover","condensed"),
                full_width = TRUE)
}

x_cred_youth <-
function(wda_no) {
  credentials %>% 
    mutate(`local_wda_no` = local_wda_no) %>%
    filter(`local_wda_no` == wda_no, Program == all_funds[[3]]) %>% 
    group_by(Quarter) %>% 
    summarise(Num=sum(`Texas Credential Numerator`),
              Den = n(),
              Percent = percent(Num/Den, accuracy = 0.01)) %>% 
    mutate(Target = case_when(wda_no == 5 ~ 67.90,
                                  wda_no == 14 ~ 0,
                                  wda_no == 18 ~ 62.30,
                                  wda_no == 19 ~ 0,
                                  wda_no == 20 ~ 56.70,
                                  wda_no == 22 ~ 48.20,
                                  wda_no == 23 ~ 62.30),
           "% of Target" = percent((Num/Den)/(Target/100),accuracy = 0.01))%>% 
    kbl(caption = "Credential - Youth",
      align = "c",
      digits = 2) %>% 
  kable_styling(bootstrap_options = c("striped","hover","condensed"),
                full_width = TRUE)
}
X_data_tbl <-
function(x,y){
  x %>%
    mutate(Program=as.factor(Program),
           `Measure  Name`=as.factor(`Measure  Name`),
           `Board ID`=as.factor(`Board ID`),
           `Numerator`=as.factor(`Numerator`)) %>% 
    select(`Measure  Name`,`Twist ID`,`Exit Date`,`Board ID`,Program,`Numerator`) %>% 
    datatable(rownames = FALSE,
              caption = paste("Table ",y),
              filter = 'top',
              colnames = c("Measure","TWIST","Exit Date","Board","Funding Source","Numerator"),
            extensions = 'Buttons',options=
            list(dom='Blfrtip',
                 autoWidth=TRUE,
                 buttons=c('pdf','excel')))
  }
X_EEQ2 <-
function(x,y){
  EEQ2 %>% 
    filter(`Board ID` == x,grepl(y,Program)) %>% 
  group_by(Quarter) %>% 
  summarise(Num=sum(as.numeric(`Numerator`)),
            Den=length(`Numerator`),
            Percent= percent(Num/Den, accuracy = 0.01),
            Target = mean(Target),
            "% of Target"= percent((Num/Den)/Target*100,accuracy = 0.01)) %>% 
  kbl(caption = paste("Employed Enrolled Quarter 2",y,sep = " - "),
      align = "c",
      digits = 2) %>% 
  kable_styling(bootstrap_options = c("striped","hover","condensed"),
                full_width = TRUE)
}
X_EEQ2_4 <-
function(x){
  EEQ2_Q4 %>% 
    filter(`Board ID`==x) %>%
        mutate(Target = 
                   case_when(
                       x == 23 ~83.40,
                       TRUE ~ 0)
                   ) %>% 
  group_by(Quarter) %>% 
  summarise(Num=sum(as.numeric(`Numerator`)),
            Den=length(`Numerator`),
            Percent= paste(round(Num/Den*100,2),"%"),
            Target = mean(Target)) %>% 
        mutate("% of Target" = percent((Num/Den)/(Target/100),
                                       accuracy = 0.01)) %>% 
  kbl(caption = "Employed Enrolled Quarter 2 - 4",
      align = "c",
      digits = 2) %>% 
  kable_styling(bootstrap_options = c("striped","hover","condensed"),
                full_width = TRUE)
}
X_EEQ4 <-
function(x,y){
  EEQ4 %>% 
  filter(`Board ID`==x,grepl(y,Program)) %>% 
  group_by(Quarter) %>% 
  summarise(Num=sum(as.numeric(`Numerator`)),
            Den=length(`Numerator`),
            Percent= percent(Num/Den, accuracy = 0.01),
            Target = mean(Target),
            "% of Target"= percent((Num/Den)/Target*100,accuracy = 0.01)) %>% 
  kbl(caption = paste("Employed Enrolled Quarter 4",y,sep = " - "),
      align = "c",
      digits = 2) %>% 
  kable_styling(bootstrap_options = c("striped","hover","condensed"),
                full_width = TRUE)
}
X_EQ2 <-
function(x,y){
  EQ2 %>% 
    filter(`Board ID` == x,grepl(y,Program)) %>% 
  group_by(Quarter) %>% 
  summarise(Num=sum(as.numeric(`Numerator`)),
            Den=length(`Numerator`),
            Percent= percent(Num/Den, accuracy = 0.01),
            Target = mean(Target),
            "% of Target"= percent((Num/Den)/Target*100, accuracy = 0.01)) %>% 
  kbl(caption = paste("Employed Quarter 2",y,sep = " - "),
      align = "c",
      digits = 2) %>% 
  kable_styling(bootstrap_options = c("striped","hover","condensed"),
                full_width = TRUE)
}
X_EQ4 <-
function(x,y){
EQ4 %>% 
  filter(`Board ID`==x,grepl(y,Program)) %>% 
  group_by(Quarter) %>% 
  summarise(Num=sum(as.numeric(`Numerator`)),
            Den=length(`Numerator`),
            Percent= percent(Num/Den , accuracy = 0.01),
            Target = mean(Target),
            "% of Target"= percent((Num/Den*100)/ Target, accuracy = 0.01)) %>% 
  kbl(caption = paste("Employed Quarter 4",y,sep = " - "),
      align = "c",
      digits = 2) %>% 
  kable_styling(bootstrap_options = c("striped","hover","condensed"),
                full_width = TRUE)
}
x_msg_adult <-
function(x){
MSG %>% 
  filter(WDA_NO == x, `Population Group Code` == 10) %>% 
  group_by(Quarter) %>% 
  summarise(Num = sum(`MSG Numerator`),
            Den = sum(`MSG Denominator`),
            Percent= percent(Num/Den,accuracy = 0.01)) %>% 
  mutate(Target = case_when(x == 5 ~ 64.40,
                            x == 14 ~ 76.80,
                            x == 18 ~ 61.60,
                            x == 19 ~ 61.60,
                            x == 20 ~ 61.60,
                            x == 22 ~ 61.60,
                            x == 23 ~ 76.80),
         "% of Target" = percent((Num/Den)/ (Target/100),accuracy = 0.01)) %>% 
    kbl(caption = "MSG - Adult",
        align = 'c',
        digits = 2) %>% 
    kable_styling(bootstrap_options = c('stripped','hover','condensed'),
                  full_width = TRUE)
}
x_msg_dw <-
function(x){
MSG %>% 
  filter(WDA_NO == x, `Population Group Code` == 13) %>% 
  group_by(Quarter) %>% 
  summarise(Num = sum(`MSG Numerator`),
            Den = sum(`MSG Denominator`),
            Percent= percent(Num/Den,accuracy = 0.01)) %>% 
  mutate(Target = case_when(x == 5 ~ 72.20,
                            x == 14 ~ 80.60,
                            x == 18 ~ 71.40,
                            x == 19 ~ 84.20,
                            x == 20 ~ 81.80,
                            x == 22 ~ 78.40,
                            x == 23 ~ 85.00),
         "% of Target" = percent((Num/Den)/ (Target/100),accuracy = 0.01)) %>% 
      kbl(caption = "MSG - DW",
        align = 'c',
        digits = 2) %>% 
    kable_styling(bootstrap_options = c('stripped','hover','condensed'),
                  full_width = TRUE)
  }
x_msg_youth <-
function(x){
MSG %>% 
  filter(WDA_NO == x, `Population Group Code` == 17) %>% 
  group_by(Quarter) %>% 
  summarise(Num = sum(`MSG Numerator`),
            Den = sum(`MSG Denominator`),
            Percent= percent(Num/Den,accuracy = 0.01)) %>% 
  mutate(Target = case_when(x == 5 ~ 51.70,
                            x == 14 ~ 0,
                            x == 18 ~ 63.20,
                            x == 19 ~ 50.90,
                            x == 20 ~ 59.30,
                            x == 22 ~ 81.30,
                            x == 23 ~ 71.00),
         "% of Target" = percent((Num/Den)/ (Target/100),accuracy = 0.01)) %>% 
          kbl(caption = "MSG - Youth",
        align = 'c',
        digits = 2) %>% 
    kable_styling(bootstrap_options = c('stripped','hover','condensed'),
                  full_width = TRUE)
}
X_Summary <-
function(x){
  
  a <- EQ2 %>% 
  filter(`Workforce Board Number` == x, Program == funds[[1]]) %>% 
  group_by(Measure = `Measure  Name`) %>% 
  summarise(Num=sum(as.numeric(`Numerator`)),
            Den=length(`Numerator`),
            Percent= percent(Num/Den,accuracy = 0.01),
            Target = mean(Target),
            Percent_of_Target = percent((Num/Den)/(Target/100), accuracy = 0.01)) %>% 
  mutate(Fund="Adult",.after= Measure)
  
c <- EQ4 %>% 
  filter(`Workforce Board Number` == x , Program == funds[[1]]) %>% 
  group_by(Measure=`Measure  Name`) %>% 
  summarise(Num=sum(as.numeric(`Numerator`)),
            Den=length(`Numerator`),
            Percent= percent(Num/Den, accuracy = 0.01),
            Target = mean(Target),
            Percent_of_Target = percent((Num/Den)/(Target/100), accuracy = 0.01)) %>% 
  mutate(Fund="Adult",.after= Measure)

j <- credentials %>% 
  filter(`Board Number` == x, Program == all_funds[[1]]) %>% 
  mutate(Target = case_when(`Board Number` == 5 ~ 68.10,
                            `Board Number` == 14 ~ 67.40,
                            `Board Number` == 18 ~ 84.60,
                            `Board Number` == 19 ~ 68.70,
                            `Board Number` == 20 ~ 72.30,
                            `Board Number` == 22 ~ 65.60,
                            `Board Number` == 23 ~ 79.40)) %>% 
  summarise(Num = sum(`Texas Credential Numerator`),
            Den = sum(`Credential Denominator`),
            Percent = percent(Num/Den,accuracy = 0.01),
            Target = mean(Target),
            Percent_of_Target = percent((Num/Den)/(Target/100), accuracy = 0.01) ) %>% 
    mutate(Measure = "Credentials",
           Fund = "Adult", .after = Measure) %>% 
  relocate(Measure,Fund)
# Adult MSG
m <- MSG %>% 
  filter(`Population Group Code` == 10, WDA_NO == x) %>% 
  mutate(Target = case_when(WDA_NO == 5 ~ 64.40,
                            WDA_NO == 14 ~ 76.80,
                            WDA_NO == 18 ~ 61.60,
                            WDA_NO == 19 ~ 61.60,
                            WDA_NO == 20 ~ 61.60,
                            WDA_NO == 22 ~ 61.60,
                            WDA_NO == 23 ~ 76.80)) %>% 
  summarise(Num = sum(`MSG Numerator`),
            Den = sum(`MSG Denominator`),
            Percent = percent(Num/Den,accuracy = 0.01),
            Target = mean(Target),
            Percent_of_Target = percent((Num/Den)/(Target/100), accuracy = 0.01)) %>% 
              mutate(Measure = "MSG",
                     Fund = "Adult", .before = Num) %>% 
  relocate(Measure,Fund)

Earnings_A <- MEQ2 %>%
    filter(`LWDA Number` == x) %>% 
    mutate(Numerator = as.numeric(Numerator)) %>% 
    filter(Program =="Title I Adult - Local") %>% 
    distinct(`Twist Id`,.keep_all = TRUE) %>% 
    summarise(Num = median(Numerator,digits = 0,na.rm = TRUE),
              Den = length(Numerator),
              Percent = "0",
              Target = mean(Target),
              Percent_of_Target = percent(Num/Target)) %>% 
    mutate(Measure = "Median Earnings",
           Fund = "Adult") %>% 
    relocate(Measure,Fund)

b <- EQ2 %>% 
  filter(`Board ID` == x, Program == funds[[2]]) %>% 
  group_by(Measure=`Measure  Name`) %>% 
  summarise(Num=sum(as.numeric(`Numerator`)),
            Den=length(`Numerator`),
            Percent= percent(Num/Den, accuracy = 0.01),
            Target = mean(Target),
            Percent_of_Target = percent((Num/Den)/(Target/100), accuracy = 0.01)) %>% 
  mutate(Fund="DW",.after=Measure)

d <- EQ4 %>% 
  filter(`Board ID` == x, Program == funds[[2]]) %>% 
  group_by(Measure=`Measure  Name`) %>% 
  summarise(Num=sum(as.numeric(`Numerator`)),
            Den=length(`Numerator`),
            Percent= percent(Num/Den, accuracy = 0.01),
            Target = mean(Target),
            Percent_of_Target = percent((Num/Den)/(Target/100), accuracy = 0.01)) %>% 
  mutate(Fund="DW",.after=Measure)

k <- credentials %>% 
  filter(`Board Number` == x, Program == all_funds[[2]]) %>% 
  mutate(Target = case_when(`Board Number` == 5 ~ 79.60,
                            `Board Number` == 14 ~ 85.30,
                            `Board Number` == 18 ~ 79.60,
                            `Board Number` == 19 ~ 92.50,
                            `Board Number` == 20 ~ 92.50,
                            `Board Number` == 22 ~ 79.60,
                            `Board Number` == 23 ~ 85.00)) %>% 
  summarise(Num = sum(`Texas Credential Numerator`),
            Den = n(),
            Percent = percent(Num/Den,accuracy = 0.01),
            Target = mean(Target),
            Percent_of_Target = percent((Num/Den)/(Target/100), accuracy = 0.01)) %>% 
    mutate(Measure = "Credentials",
           Fund = "DW", .after = Measure) %>% 
  relocate(Measure,Fund)

n <- MSG %>% 
  filter(`Population Group Code` == 13, WDA_NO == x) %>% 
    # population 13 = DW
  mutate(Target = case_when(WDA_NO == 5 ~ 72.20,
                            WDA_NO == 14 ~ 80.60,
                            WDA_NO == 18 ~ 71.40,
                            WDA_NO == 19 ~ 84.20,
                            WDA_NO == 20 ~ 81.80,
                            WDA_NO == 22 ~ 78.40,
                            WDA_NO == 23 ~ 85.00)) %>% 
  summarise(Num = sum(`MSG Numerator`),
            Den = sum(`MSG Denominator`),
            Percent = percent(Num/Den,accuracy = 0.01),
            Target = mean(Target),
            Percent_of_Target = percent((Num/Den)/(Target/100), accuracy = 0.01)) %>% 
              mutate(Measure = "MSG",
                     Fund = "DW", .before = Num) %>% 
  relocate(Measure,Fund)

Earnings_DW <- MEQ2 %>%
    filter(`LWDA Number` == x) %>%
    mutate(Numerator = as.numeric(Numerator)) %>% 
    filter(Program =="Title I DW - Local") %>% 
    distinct(`Twist Id`,.keep_all = TRUE) %>% 
    summarise(Num = median(Numerator, digits = 0,na.rm = TRUE),
              Den = length(Numerator),
              Percent = "0",
              Target = mean(Target),
              Percent_of_Target = percent(Num/Target)) %>% 
    mutate(Measure = "Median Earnings",
           Fund = "DW") %>% 
    relocate(Measure,Fund)

f <- EEQ2 %>% 
  filter(`Board ID` == x, Program == all_funds[[3]]) %>% 
  group_by(Measure=`Measure  Name`) %>% 
  summarise(Num=sum(as.numeric(`Numerator`)),
            Den=length(`Numerator`),
            Percent= percent(Num/Den, accuracy = 0.01),
            Target  = mean(Target),
            Percent_of_Target = percent((Num/Den)/(Target/100), accuracy = 0.01)) %>% 
  mutate(Fund="Youth",.after=Measure)

g <- EEQ4 %>% 
  filter(`Board ID` == x, Program == all_funds[[3]]) %>% 
  group_by(Measure=`Measure  Name`) %>% 
  summarise(Num=sum(as.numeric(`Numerator`)),
            Den=length(`Numerator`),
            Percent= percent(Num/Den, accuracy = 0.01),
            Target = mean(Target),
            Percent_of_Target = percent((Num/Den)/(Target/100), accuracy = 0.01)) %>% 
  mutate(Fund="Youth",.after=Measure)


l <- credentials %>% 
  filter(`Board Number` == x, Program == all_funds[[3]]) %>% 
  mutate(Target = case_when(`Board Number` == 5 ~ 67.90,
                            `Board Number` == 14 ~ 0,
                            `Board Number` == 18 ~ 62.30,
                            `Board Number` == 19 ~ 48.20,
                            `Board Number` == 20 ~ 0,
                            `Board Number` == 22 ~ 48.20,
                            `Board Number` == 23 ~ 57.50)) %>% 
  summarise(Num = sum(`Texas Credential Numerator`),
            Den = n(),
            Percent = percent(Num/Den,accuracy = 0.01),
            Target = mean(Target),
            Percent_of_Target = percent((Num/Den)/(Target/100), accuracy = 0.01)) %>% 
    mutate(Measure = "Credentials",
           Fund = "Youth", .after = Measure) %>% 
  relocate(Measure,Fund)


o <- MSG %>% 
  filter(`Population Group Code` == 17, WDA_NO == x) %>% 
    # population 17 = Youth
    mutate(Target = case_when(WDA_NO == 5 ~ 51.70,
                            WDA_NO == 14 ~ 0,
                            WDA_NO == 18 ~ 63.20,
                            WDA_NO == 19 ~ 50.90,
                            WDA_NO == 20 ~ 0,
                            WDA_NO == 22 ~ 81.30,
                            WDA_NO == 23 ~ 71.00)) %>% 
  summarise(Num = sum(`MSG Numerator`),
            Den = sum(`MSG Denominator`),
            Percent = percent(Num/Den,accuracy = 0.01),
            Target = mean(Target),
            Percent_of_Target = percent((Num/Den)/(Target/100), accuracy = 0.01)) %>% 
              mutate(Measure = "MSG",
                     Fund = "Youth", .before = Num) %>%
  relocate(Measure,Fund)

Earnings_Y <- MEQ2 %>%
    filter(`LWDA Number` == x) %>%
    mutate(Numerator = as.numeric(Numerator)) %>% 
    filter(Program =="Title I Youth - Local") %>% 
    distinct(`Twist Id`,.keep_all = TRUE) %>% 
    summarise(Num = median(Numerator, digits = 0,na.rm = TRUE),
              Den = length(Numerator),
              Percent = "0",
              Target = mean(Target),
              Percent_of_Target = percent(Num/Target)) %>% 
    mutate(Measure = "Median Earnings",
           Fund = "Youth") %>% 
    relocate(Measure,Fund)

all_measures <- bind_rows(a,c,j,m,Earnings_A,
                          b,d,k,n,Earnings_DW,
                          f,g,l,o,Earnings_Y)

all_measures <- all_measures %>% 
    # mutate(Num = Num %>% as.character()) %>% 
  mutate(Percent_of_Target = ifelse( is.na(Percent_of_Target) ,percent(0),Percent_of_Target)) %>% 
    mutate("+/-" = 
               if_else(Target < 101,round((Den*Target/100) - Num %>% 
               as.numeric(), digits = 0),
               Target - Num),
           Num = if_else(Num > 500, 
                         scales::dollar(Num),Num %>% as.character()),
        Target = if_else( Target < 100, 
                             scales::percent(Target/100,accuracy = 0.01),
                             scales::dollar(Target)))

all_measures %>% 
  kbl(caption = "Summary - Adult, Dislocated Workers, and Youth Measures",
      align = c('l','c','c','c','l'),
      col.names = c("Measure","Fund","Num","Den","Rate","Target","% of Target",
                    "+/-"),
      digits = 2) %>% 
  kable_styling(bootstrap_options = c("striped","hover","condensed")) %>%
  column_spec(column = 7 ,color = ifelse(parse_number(all_measures$Percent_of_Target) > 90,"black",
                                         ifelse(parse_number(all_measures$Percent_of_Target) > 0, "red","black"))) 
}


X_Summary_All <-
function(x){
  
  e <- EEQ2_All %>%
  filter(`Board ID` == x ) %>%
  group_by(Measure=`Measure  Name`) %>%
  summarise(Num=sum(as.numeric(`Numerator`)),
            Den=length(`Numerator`),
            Percent= percent(Num/Den, accuracy = 0.01),
            Target = mean(Target),
            Percent_of_Target = percent((Num/Den)/(Target/100), accuracy = 0.01)) %>%
  mutate(Fund="All",.after=Measure)
  
  
  h <- EEQ2_Q4 %>% 
  filter(`Board ID` == x) %>% 
  group_by(Measure=`Measure  Name`) %>% 
  summarise(Num=sum(as.numeric(`Numerator`)),
            Den=length(`Numerator`),
            Percent=percent(Num/Den, accuracy = 0.01),
            Target = mean(Target),
            Percent_of_Target = percent((Num/Den)/(Target/100), accuracy = 0.01)) %>% 
  mutate(Fund="All",.after=Measure) 
  
  i <- credentials %>% 
  filter(`Board Number` == x) %>% 
  mutate(Target = case_when(
      `Board Number` == 5 ~ 70.90,
      `Board Number` == 18 ~ 70.90,
      `Board Number` == 22 ~ 70.10,
      `Board Number` == 23 ~ 70.90,
      TRUE ~ 0)) %>% 
  summarise(Num = sum(`Texas Credential Numerator`),
            Den = n(),
            Percent = percent(Num/Den,accuracy = 0.01),
            Target = mean(Target),
            Percent_of_Target = percent((Num/Den)/(Target/100), accuracy = 0.01)) %>% 
    mutate(Measure = "Credentials",
           Fund = "All", .after = Measure) %>% 
  relocate(Measure,Fund) 
  
  all_measures <- bind_rows(e,h,i)

all_measures <- all_measures %>% 
  mutate(Percent_of_Target = ifelse(Percent_of_Target == "Inf",percent(0),Percent_of_Target)) %>% 
    mutate("+/-" = ((Den * (Target/100)) - Num) %>% round(),
           Target = (Target/100) %>% scales::percent(accuracy = 0.01))
  
all_measures <- all_measures %>% 
  mutate(Num = comma(Num),
         Den = comma(Den))

all_measures %>% 
  kbl(caption = "Summary - All Funds Measures",
      align = c('l','c','c','c','l'),
      col.names = c("Measure","Fund","Num","Den","Rate","Target","% of Target",
                    "+/-")) %>% 
  kable_styling(bootstrap_options = c("striped","hover","condensed")) %>%
  column_spec(column = 7 ,color = ifelse(parse_number(all_measures$Percent_of_Target) > 95,"black",
                                         ifelse(parse_number(all_measures$Percent_of_Target) > 0, "red","black")))

}

X_Summary_Qtr <-
function(x){
  
  a0 <-  EQ2 %>% 
  filter(Program == funds[[1]],`Board ID` == x) %>% 
  summarise(Num=sum(as.numeric(`Numerator`)),
            Den=length(`Numerator`),
            Percent= percent(Num/Den, accuracy = 0.01),
            Target = mean(Target),
            `Percent of Target` = percent((Num/Den*100)/Target,accuracy = 0.01)) %>%
    mutate(Measure = "") %>% 
  mutate_all(as.character)%>% 
  pivot_longer(cols = c(Num,Den,Percent,Target,`Percent of Target`),
               names_to = "Values",values_to = "YTD") %>%
  select( -c(1,2))
    
  a1 <- EQ2 %>% 
  filter(Program == funds[[1]],`Board ID` == x) %>% 
  group_by(Measure=`Measure  Name`,Quarter) %>% 
  summarise(Num=sum(as.numeric(`Numerator`)),
            Den=length(`Numerator`),
            Percent= Num/Den,
            Target = mean(Target),
            `Percent of Target` = percent(Percent/Target*100, accuracy = 0.01)) %>%
    mutate(Measure = "",
           Percent = percent(Percent, accuracy = 0.01)) %>% 
  mutate_all(as.character)%>% 
  pivot_longer(cols = c(Num,Den,Percent,Target,`Percent of Target`),names_to = "Values",values_to = "numbers") %>% 
  pivot_wider(names_from = "Quarter",values_from = "numbers") %>% 
    # mutate("Q4" = "NA") %>%
    cbind(a0)

  b0 <- EQ2 %>% 
  filter(Program == funds[[2]],`Board ID` == x) %>% 
  summarise(Num=sum(as.numeric(`Numerator`)),
            Den=length(`Numerator`),
            Percent= percent(Num/Den, accuracy = 0.01),
            Target = mean(Target),
            `Percent of Target` = percent((Num/Den*100)/Target)) %>%
    mutate(Measure = "") %>% 
  mutate_all(as.character)%>% 
  pivot_longer(cols = c(Num,Den,Percent,Target,`Percent of Target`),
               names_to = "Values",values_to = "YTD") %>%
  select( -c(1,2))
  
b1 <- EQ2 %>% 
  filter(Program == funds[[2]],`Board ID` == x) %>% 
  group_by(Measure=`Measure  Name`,Quarter) %>% 
  summarise(Num=sum(as.numeric(`Numerator`)),
            Den=length(`Numerator`),
            Percent= Num/Den,
            Target = mean(Target),
            `Percent of Target` = percent(Percent/Target*100,accuracy = 0.01)) %>% 
  mutate(Measure = "",
         Percent = percent(Percent,accuracy = 0.01)) %>% 
  mutate_all(as.character)%>% 
  pivot_longer(cols = c(Num,Den,Percent,Target,`Percent of Target`),names_to = "Values",values_to = "numbers") %>% 
  pivot_wider(names_from = "Quarter",values_from = "numbers") %>% 
  cbind(b0)

c0 <- EQ4 %>% 
  filter(Program == funds[[1]],`Board ID` == x) %>% 
  summarise(Num=sum(as.numeric(`Numerator`)),
            Den=length(`Numerator`),
            Percent= percent(Num/Den, accuracy = 0.01),
            Target = mean(Target),
            `Percent of Target` = percent((Num/Den*100)/Target, accuracy = 0.01)) %>%
    mutate(Measure = "") %>% 
  mutate_all(as.character)%>% 
  pivot_longer(cols = c(Num,Den,Percent,Target,`Percent of Target`),
               names_to = "Values",values_to = "YTD") %>%
  select( -c(1,2))

c1 <- EQ4 %>% 
  filter(Program == funds[[1]],`Board ID` == x) %>% 
  group_by(Measure=`Measure  Name`,Quarter) %>% 
  summarise(Num=sum(as.numeric(`Numerator`)),
            Den=length(`Numerator`),
            Percent= Num/Den,
            Target = mean(Target),
            `Percent of Target` = percent(Percent/Target*100, accuracy = 0.01)) %>% 
mutate(Measure = "",
       Percent = percent(Percent, accuracy = 0.01)) %>% 
  mutate_all(as.character)%>% 
  pivot_longer(cols = c(Num,Den,Percent,Target,`Percent of Target`),names_to = "Values",values_to = "numbers") %>% 
  pivot_wider(names_from = "Quarter",values_from = "numbers") %>% 
  cbind(c0)

d0 <- EQ4 %>% 
  filter(Program == funds[[2]], `Board ID` == x) %>% 
  summarise(Num=sum(as.numeric(`Numerator`)),
            Den=length(`Numerator`),
            Percent= percent(Num/Den, accuracy = 0.01),
            Target = mean(Target),
            `Percent of Target` = percent((Num/Den*100)/Target, accuracy = 0.01)) %>%
    mutate(Measure = "") %>% 
  mutate_all(as.character)%>% 
  pivot_longer(cols = c(Num,Den,Percent,Target,`Percent of Target`),
               names_to = "Values",values_to = "YTD") %>%
  select( -c(1,2))

d1 <- EQ4 %>% 
  filter(Program == funds[[2]], `Board ID` == x) %>% 
  group_by(Measure=`Measure  Name`,Quarter) %>% 
  summarise(Num=sum(as.numeric(`Numerator`)),
            Den=length(`Numerator`),
            Percent= Num/Den,
            Target = mean(Target),
            `Percent of Target` = percent(Percent/Target*100, accuracy = 0.01)) %>% 
mutate(Measure = "",
       Percent = percent(Percent, accuracy = 0.01)) %>% 
  mutate_all(as.character)%>% 
  pivot_longer(cols = c(Num,Den,Percent,Target,`Percent of Target`),names_to = "Values",values_to = "numbers") %>% 
  pivot_wider(names_from = "Quarter",values_from = "numbers") %>% 
  cbind(d0)


f0 <- EEQ2 %>% 
  filter(Program == all_funds[[3]],`Board ID` == x ) %>% 
  summarise(Num=sum(as.numeric(`Numerator`)),
            Den=length(`Numerator`),
            Percent= percent(Num/Den, accuracy = 0.01),
            Target = mean(Target),
            `Percent of Target` = percent((Num/Den*100)/Target, accuracy = 0.01)) %>%
    mutate(Measure = "") %>% 
  mutate_all(as.character)%>% 
  pivot_longer(cols = c(Num,Den,Percent,Target,`Percent of Target`),
               names_to = "Values",values_to = "YTD") %>%
  select( -c(1,2))

f1 <- EEQ2 %>% 
  filter(Program == "Title I Youth - Local", `Workforce Board Number` == x ) %>% 
  group_by(Measure=`Measure  Name`,Quarter) %>% 
  summarise(Num=sum(as.numeric(`Numerator`)),
            Den=length(`Numerator`),
            Percent= Num/Den,
            Target = mean(Target),
            `Percent of Target` = percent(Percent/Target*100, accuracy = 0.01)) %>% 
mutate(Measure = "",
       Percent = percent(Percent,accuracy = 0.01)) %>% 
  mutate_all(as.character)%>% 
  pivot_longer(cols = c(Num,Den,Percent,Target,`Percent of Target`),names_to = "Values",values_to = "numbers") %>% 
  pivot_wider(names_from = "Quarter",values_from = "numbers") %>% 
  cbind(f0)

g0 <- EEQ4 %>% 
  filter(Program == all_funds[[3]],`Board ID` == x) %>% 
  summarise(Num=sum(as.numeric(`Numerator`)),
            Den=length(`Numerator`),
            Percent= percent(Num/Den, accuracy = 0.01),
            Target = mean(Target),
            `Percent of Target` = percent((Num/Den*100)/Target, accuracy = 0.01)) %>%
    mutate(Measure = "") %>% 
  mutate_all(as.character)%>% 
  pivot_longer(cols = c(Num,Den,Percent,Target,`Percent of Target`),
               names_to = "Values",values_to = "YTD") %>%
  select( -c(1,2))

g1 <- EEQ4 %>% 
  filter(Program == all_funds[[3]],`Board ID` == x) %>% 
  group_by(Measure=`Measure  Name`,Quarter) %>% 
  summarise(Num=sum(as.numeric(`Numerator`)),
            Den=length(`Numerator`),
            Percent= Num/Den,
            Target = mean(Target),
            `Percent of Target` = percent(Percent/Target*100, accuracy = 0.01)) %>% 
 mutate(Measure = "",
        Percent = percent(Percent, accuracy = 0.01)) %>% 
  mutate_all(as.character)%>% 
  pivot_longer(cols = c(Num,Den,Percent,Target,`Percent of Target`),names_to = "Values",values_to = "numbers") %>% 
  pivot_wider(names_from = "Quarter",values_from = "numbers") %>% 
  cbind(g0)

h0 <- EEQ2_Q4 %>%
  filter(`Board ID` == x) %>%
  summarise(Num=sum(as.numeric(`Numerator`)),
            Den=length(`Numerator`),
            Percent= percent(Num/Den, accuracy = 0.01),
            Target = mean(Target),
            `Percent of Target` = percent((Num/Den*100)/Target, accuracy = 0.01)) %>%
    mutate(Measure = "") %>%
  mutate_all(as.character)%>%
  pivot_longer(cols = c(Num,Den,Percent,Target,`Percent of Target`),
               names_to = "Values",values_to = "YTD") %>%
  select( -c(1,2))

h1 <- EEQ2_Q4 %>%
  filter(`Board ID` == x) %>%
 group_by(Measure=`Measure  Name`,Quarter) %>%
  summarise(Num=sum(as.numeric(`Numerator`)),
            Den=length(`Numerator`),
            Percent= Num/Den,
            Target = mean(Target),
            `Percent of Target` = percent(Percent/Target*100, accuracy = 0.01)) %>%
 mutate(Measure = "",
        Percent = percent(Percent, accuracy = 0.01)) %>%
  mutate_all(as.character)%>%
  pivot_longer(cols = c(Num,Den,Percent,Target,`Percent of Target`),names_to = "Values",values_to = "numbers") %>%
  pivot_wider(names_from = "Quarter",values_from = "numbers") %>%
  cbind(h0)

i0 <- credentials %>% 
  filter(`Board Number` == x) %>%
  mutate(Target = 60,.keep_all=TRUE) %>% 
  summarise(Num=sum(`Texas Credential Numerator`),
            Den=n(),
            Percent= percent(Num/Den, accuracy = 0.01),
            Target = mean(Target),
            `Percent of Target` = percent((Num/Den)/(Target/100),accuracy = 0.01)) %>%
    mutate(Measure = "") %>% 
  mutate_all(as.character)%>% 
  pivot_longer(cols = c(Num,Den,Percent,Target,`Percent of Target`),
               names_to = "Values",values_to = "YTD") %>%
  select( -c(1,2))

i1 <- credentials %>% 
  filter( `Board Number` == x) %>%
  mutate(Target = 60,.keep_all=TRUE) %>% 
  group_by(Quarter) %>% 
  summarise(Measure = "",
            Num=sum(`Texas Credential Numerator`),
            Den = n(),
            Percent = Num/Den,
            Target = mean(Target),
            `Percent of Target` = percent((Num/Den)/(Target/100),accuracy = 0.01)) %>% 
  ungroup() %>% 
  mutate(Percent = percent(Percent, accuracy = 0.01)) %>% 
  mutate_all(as.character) %>% 
  pivot_longer(cols = c(Num,Den,Percent,Target,`Percent of Target`),names_to = "Values",values_to = "numbers") %>% 
  pivot_wider(names_from = Quarter,values_from = "numbers") %>% 
  cbind(i0)

j0 <- credentials %>% 
  filter(`Board Number` == x ,Program == funds[[1]]) %>%
  mutate(Target = case_when(`Board Number` == 5 ~ 68.10,
                            `Board Number` == 14 ~ 67.40,
                            `Board Number` == 18 ~ 84.60,
                            `Board Number` == 19 ~ 68.70,
                            `Board Number` == 20 ~ 72.30,
                            `Board Number` == 22 ~ 65.60,
                            `Board Number` == 23 ~ 79.40),
         .keep_all=TRUE) %>% 
  summarise(Measure = "",
            Num=sum(`Texas Credential Numerator`),
            Den = n(),
            Percent = percent(Num/Den, accuracy = 0.01),
            Target = mean(Target),
            `Percent of Target` = percent((Num/Den)/(Target/100),accuracy = 0.01)) %>% 
  ungroup() %>% 
  mutate_all(as.character) %>% 
  pivot_longer(cols = c(Num,Den,Percent,Target,`Percent of Target`),
               names_to = "Values",values_to = "YTD") %>% 
  select(-c(1,2))

j1 <- credentials %>% 
  filter(`Board Number` == x , Program == funds[[1]]) %>%
  mutate(Target = case_when(`local_wda_no` == 5 ~ 68.10,
                            `local_wda_no` == 14 ~ 67.40,
                            `local_wda_no` == 18 ~ 84.60,
                            `local_wda_no` == 19 ~ 68.70,
                            `local_wda_no` == 20 ~ 72.30,
                            `local_wda_no` == 22 ~ 65.60,
                            `local_wda_no` == 23 ~ 79.40),
         .keep_all=TRUE) %>% 
  group_by(Quarter) %>% 
  summarise(Measure = "",
            Num=sum(`Texas Credential Numerator`),
            Den = length(`Texas Credential Numerator`),
            Percent = Num/Den,
            Target = mean(Target),
            `Percent of Target` = percent((Num/Den)/(Target/100),accuracy = 0.01)) %>% 
  ungroup() %>%
  mutate(Percent=percent(Percent, accuracy = 0.01)) %>% 
  mutate_all(as.character) %>% 
  pivot_longer(cols = c(Num,Den,Percent,Target,`Percent of Target`),names_to = "Values",values_to = "numbers") %>% 
  pivot_wider(names_from = Quarter,values_from = "numbers") %>% 
  cbind(j0)

k0 <- credentials %>% 
  filter(`local_wda_no` == x ,Program == funds[[2]]) %>%
mutate(Target = case_when(`local_wda_no` == 5 ~ 79.60,
                            `local_wda_no` == 14 ~ 85.30,
                            `local_wda_no` == 18 ~ 79.60,
                            `local_wda_no` == 19 ~ 92.50,
                            `local_wda_no` == 20 ~ 92.50,
                            `local_wda_no` == 22 ~ 79.60,
                            `local_wda_no` == 23 ~ 92.50),
         .keep_all=TRUE) %>% 
  summarise(Measure = "",
            Num=sum(`Texas Credential Numerator`),
            Den = length(`Texas Credential Numerator`),
            Percent = percent(Num/Den, accuracy = 0.01),
            Target = mean(Target),
            `Percent of Target` = percent((Num/Den)/(Target/100),accuracy = 0.01)) %>% 
  ungroup() %>% 
  mutate_all(as.character) %>% 
  pivot_longer(cols = c(Num,Den,Percent,Target,`Percent of Target`),
               names_to = "Values",values_to = "YTD") %>% 
  select(-c(1,2))

k1 <- credentials %>% 
  filter(`local_wda_no` == x ,Program == funds[[2]]) %>%
mutate(Target = case_when(`local_wda_no` == 5 ~ 79.60,
                            `local_wda_no` == 14 ~ 85.30,
                            `local_wda_no` == 18 ~ 79.60,
                            `local_wda_no` == 19 ~ 92.50,
                            `local_wda_no` == 20 ~ 92.50,
                            `local_wda_no` == 22 ~ 79.60,
                            `local_wda_no` == 23 ~ 92.50),
         .keep_all=TRUE) %>% 
  group_by(Quarter) %>% 
  summarise(Measure = "",
            Num=sum(`Texas Credential Numerator`),
            Den = length(`Texas Credential Numerator`),
            Percent = round(Num/Den*100,digits = 2),
            Target = mean(Target),
            `Percent of Target` = percent((Num/Den)/(Target/100),accuracy = 0.01)) %>% 
  ungroup() %>% 
  mutate(Percent = percent(Percent/100,accuracy = 0.01)) %>% 
  mutate_all(as.character) %>% 
  pivot_longer(cols = c(Num,Den,Percent,Target,`Percent of Target`),names_to = "Values",values_to = "numbers") %>% 
  pivot_wider(names_from = Quarter,values_from = "numbers") %>% 
  cbind(k0)

l0 <- credentials %>% 
  filter(`local_wda_no` == x ,Program == all_funds[[3]]) %>%
mutate(Target = case_when(`local_wda_no` == 5 ~ 67.90,
                            `local_wda_no` == 14 ~ 0,
                            `local_wda_no` == 18 ~ 62.30,
                            `local_wda_no` == 19 ~ 48.20,
                            `local_wda_no` == 20 ~ 0,
                            `local_wda_no` == 22 ~ 48.20,
                            `local_wda_no` == 23 ~ 57.50),
         .keep_all=TRUE) %>% 
  summarise(Measure = "",
            Num=sum(`Texas Credential Numerator`),
            Den = length(`Texas Credential Numerator`),
            Percent = percent(Num/Den, accuracy = 0.01),
            Target = mean(Target),
            `Percent of Target` = percent((Num/Den)/(Target/100),accuracy = 0.01)) %>% 
  ungroup() %>% 
  mutate_all(as.character) %>% 
  pivot_longer(cols = c(Num,Den,Percent,Target,`Percent of Target`),
               names_to = "Values",values_to = "YTD") %>% 
  select(-c(1,2))
  
l1 <- credentials %>% 
  filter(`local_wda_no` == x ,Program == all_funds[[3]]) %>%
mutate(Target = case_when(  `local_wda_no` == 5 ~ 67.90,
                            `local_wda_no` == 14 ~ 0,
                            `local_wda_no` == 18 ~ 62.30,
                            `local_wda_no` == 19 ~ 48.20,
                            `local_wda_no` == 20 ~ 0,
                            `local_wda_no` == 22 ~ 48.20,
                            `local_wda_no` == 23 ~ 57.50),
         .keep_all=TRUE) %>% 
  group_by(Quarter) %>% 
  summarise(Measure = "",
            Num=sum(`Texas Credential Numerator`),
            Den = length(`Texas Credential Numerator`),
            Percent = round(Num/Den*100,digits = 2),
            Target = mean(Target),
            `Percent of Target` = percent((Num/Den)/(Target/100),accuracy = 0.01)) %>% 
  ungroup() %>% 
  mutate(Percent = percent(Percent/100,accuracy = 0.01)) %>% 
  mutate_all(as.character) %>% 
  pivot_longer(cols = c(Num,Den,Percent,Target,`Percent of Target`),names_to = "Values",values_to = "numbers") %>%
  pivot_wider(names_from = Quarter,values_from = "numbers") %>% 
  relocate(Measure) %>% 
  cbind(l0)

m1 <- MSG %>% 
  filter(POPULATION == 10, WDA_NO == x) %>% 
  mutate(Target = case_when(WDA_NO == 5 ~ 64.40,
                            WDA_NO == 14 ~ 76.80,
                            WDA_NO == 18 ~ 61.60,
                            WDA_NO == 19 ~ 61.60,
                            WDA_NO == 20 ~ 61.60,
                            WDA_NO == 22 ~ 61.60,
                            WDA_NO == 23 ~ 76.80)) %>% 
  # group_by(Quarter) %>% 
  summarise(Measure = "",
            Num=sum(`MSG Numerator`),
            Den = length(`MSG Denominator`),
            Percent = percent(Num / Den,accuracy = 0.01),
            Target = mean(Target),
            `Percent of Target` = percent((Num/Den)/(Target/100),accuracy = 0.01)) %>% 
  ungroup() %>% 
  mutate_all(as.character) %>% 
  pivot_longer(cols = c(Num,Den,Percent,Target,`Percent of Target`), names_to = "Values", values_to = "YTD") %>% 
  # pivot_wider(names_from = Quarter, values_from = "numbers") %>% 
  mutate(Q1 = "NA",Q2 = "NA", Q3 = "NA",Q4 = "NA",.after = Values)
  # relocate(Measure)

n1 <- MSG %>% 
  filter(POPULATION == 13, WDA_NO == x) %>%
  mutate(Target = case_when(WDA_NO == 5 ~ 64.40,
                            WDA_NO == 14 ~ 76.80,
                            WDA_NO == 18 ~ 61.60,
                            WDA_NO == 19 ~ 61.60,
                            WDA_NO == 20 ~ 61.60,
                            WDA_NO == 22 ~ 61.60,
                            WDA_NO == 23 ~ 76.80)) %>% 
  # group_by(Quarter) %>% 
  summarise(Measure = "",
            Num=sum(`MSG Numerator`),
            Den = length(`MSG Denominator`),
            Percent = percent(Num / Den,accuracy = 0.01),
            Target = mean(Target),
            `Percent of Target` = percent((Num/Den)/(Target/100),accuracy = 0.01)) %>% 
  ungroup() %>% 
  mutate_all(as.character) %>% 
  pivot_longer(cols = c(Num,Den,Percent,Target,`Percent of Target`), names_to = "Values", values_to = "YTD") %>% 
  # pivot_wider(names_from = Quarter, values_from = "numbers") %>% 
  mutate(Q1 = "NA",Q2 = "NA", Q3 = "NA",Q4 = "NA",.after = Values)
  # relocate(Measure)

o1 <- MSG %>% 
  filter(POPULATION == 17, WDA_NO == x) %>% 
  mutate(Target = case_when(WDA_NO == 5 ~ 64.40,
                            WDA_NO == 14 ~ 76.80,
                            WDA_NO == 18 ~ 61.60,
                            WDA_NO == 19 ~ 61.60,
                            WDA_NO == 20 ~ 61.60,
                            WDA_NO == 22 ~ 61.60,
                            WDA_NO == 23 ~ 76.80)) %>% 
  # group_by(Quarter) %>% 
  summarise(Measure = "",
            Num=sum(`MSG Numerator`),
            Den = sum(`MSG Denominator`),
            Percent = percent(Num / Den,accuracy = 0.01),
            Target = mean(Target),
            `Percent of Target` = percent((Num/Den)/(Target/100),accuracy = 0.01)) %>% 
  ungroup() %>% 
  mutate_all(as.character) %>% 
  pivot_longer(cols = c(Num,Den,Percent,Target,`Percent of Target`), names_to = "Values", values_to = "YTD") %>% 
  # pivot_wider(names_from = Quarter, values_from = "numbers") %>% 
  mutate(Q1 = "NA",Q2 = "NA", Q3 = "NA",Q4 = "NA",.after = Values)
  # relocate(Measure)

#adult median earnings Q2
p0 <- MEQ2 %>% 
    filter(`LWDA Number` == x ,Program == "Title I Adult - Local" ) %>% 
    group_by(`Exit Qtr`) %>%
    mutate(Target = case_when(
        `LWDA Number` == 18 ~ 7000,
        `LWDA Number` == 19 ~ 5500,
        TRUE ~ 0)) %>% 
    summarise(Num = dollar(median(Numerator)),
              Den = sum(Denominator),
              Percent =  0,
              Target = mean(Target),
              "Percent of Target" = percent(median(Numerator)/Target,
                                            accuracy = 0.01)) %>% 
    ungroup() %>% 
    mutate_all(as.character) %>% 
    pivot_longer(cols = 2:6, names_to = "Values" ) %>% 
    pivot_wider(names_from = `Exit Qtr`,
                values_from = value)
p1 <- MEQ2 %>% 
    filter(`LWDA Number` == x, Program == "Title I Adult - Local") %>% 
    summarise(Num = dollar(median(Numerator)),
              Den = sum(Denominator),
              Percent = 0,
              Target = mean(Target),
              "Percent of Target" = percent(median(Numerator)/ Target,
              accuracy = 0.01)) %>%
    mutate_all(as.character) %>% 
    pivot_longer(cols = 1:5) %>% 
    rename(YTD = value) %>% 
    select(YTD) %>% 
    cbind(p0)
    
# DW median earnings
q0 <- MEQ2 %>% 
    filter(`LWDA Number` == x,Program == "Title I DW - Local" ) %>% 
    group_by(`Exit Qtr`) %>%
    mutate(Target = case_when(
        `LWDA Number` == 18 ~ 9600,
        `LWDA Number` == 19 ~ 7300,
        TRUE ~ 0)) %>% 
    summarise(Num = dollar(median(Numerator)),
              Den = sum(Denominator),
              Percent =  0,
              Target = mean(Target),
              "Percent of Target" = percent(median(Numerator)/Target,
                                            accuracy = 0.01)) %>% 
    ungroup() %>% 
    mutate_all(as.character) %>% 
    pivot_longer(cols = 2:6, names_to = "Values" ) %>% 
    pivot_wider(names_from = `Exit Qtr`,
                values_from = value)
q1 <- MEQ2 %>% 
    filter(`LWDA Number` == x, Program == "Title I DW - Local") %>% 
    summarise(Num = dollar(median(Numerator)),
              Den = sum(Denominator),
              Percent = 0,
              Target = mean(Target),
              "Percent of Target" = percent(median(Numerator)/ Target,
                                            accuracy = 0.01)) %>%
    mutate_all(as.character) %>% 
    pivot_longer(cols = 1:5) %>% 
    rename(YTD = value) %>% 
    select(YTD) %>% 
    cbind(q0)

r0 <- MEQ2 %>% 
    filter(`LWDA Number` == x,Program == "Title I Youth - Local" ) %>% 
    group_by(`Exit Qtr`) %>%
    mutate(Target = case_when(
        `LWDA Number` == 18 ~ 3200,
        `LWDA Number` == 19 ~ 2700,
        TRUE ~ 0)) %>% 
    summarise(Num = dollar(median(Numerator)),
              Den = sum(Denominator),
              Percent =  0,
              Target = mean(Target),
              "Percent of Target" = percent(median(Numerator)/Target,
                                            accuracy = 0.01)) %>% 
    ungroup() %>% 
    mutate_all(as.character) %>% 
    pivot_longer(cols = 2:6, names_to = "Values" ) %>% 
    pivot_wider(names_from = `Exit Qtr`,
                values_from = value)

r1 <- MEQ2 %>% 
    filter(`LWDA Number` == x, Program == "Title I Youth - Local") %>% 
    summarise(Num = dollar(median(Numerator)),
              Den = sum(Denominator),
              Percent = 0,
              Target = mean(Target),
              "Percent of Target" = percent(median(Numerator)/ Target,
                                            accuracy = 0.01)) %>%
    mutate_all(as.character) %>% 
    pivot_longer(cols = 1:5) %>% 
    rename(YTD = value) %>% 
    select(YTD) %>% 
    cbind(r0)


all_measures <- bind_rows(a1,b1,c1,d1,f1,g1,h1,i1,j1,k1,l1,m1,n1,o1,p1,q1,r1) %>% 
  mutate(Q1 = ifelse(Q1 == "NA", 0 , Q1),
         Q2 = ifelse(Q2 == "NA", 0, Q2),
         Q3 = ifelse(Q3 == "NA", 0, Q3),
         Q4 = ifelse(Q4 == "NA", 0, Q4),
         Measure = ifelse(is.na(Measure), " ", Measure)) %>% 
    relocate(-YTD)

all_measures %>% 
  kbl(caption = "Summary",
      align = c('l','c','c','c','l'),
      digits = 2) %>% 
  kable_styling(bootstrap_options = c("striped","hover","condensed")) %>% 
  pack_rows("Employed Q2 Adult",1,5) %>%
  pack_rows("Employed Q2 DW",6,10) %>%
  pack_rows("Employed Q4 Adult",11,15) %>%
  pack_rows("Employed Q4 DW",16,20) %>%
  # pack_rows("Employed Q2 - Q4 All",29,35) %>%
  pack_rows("Employed Enrolled Q2 Youth",21,25) %>%
  pack_rows("Employed Enrolled Q4 Youth",26,30) %>%
  #pack_rows("Employed Enrolled Q2 All",31,35) %>%
  pack_rows("Employed Enrolled Q2 - Q4 All",31,35) %>%
  pack_rows("Credential All",36,40) %>%
  pack_rows("Credential Adult",41,45) %>%
  pack_rows("Credential DW",46,50) %>%
  pack_rows("Credential Youth",51,55) %>%
  pack_rows("MSG Adult",56,60) %>%
  pack_rows("MSG DW",61,65) %>%
  pack_rows("MSG Youth",66,70) %>% 
    pack_rows("Median Earnings Adult",71,75) %>% 
    pack_rows("Median Earnings DW", 76,80) %>% 
    pack_rows("Median Earnings Youth", 81,85)
}

X_Summary_No_Youth <-
    function(x){
        
        a <- EQ2 %>% 
            mutate(`Board ID` = `Workforce Board Number`) %>% 
            filter(`Board ID` == x, Program == funds[[1]]) %>% 
            group_by(Measure = `Measure  Name`) %>% 
            summarise(Num=sum(as.numeric(`Numerator`)),
                      Den=length(`Numerator`),
                      Percent= percent(Num/Den,accuracy = 0.01),
                      Target = mean(Target),
                      Percent_of_Target = percent((Num/Den)/(Target/100), accuracy = 0.01)) %>% 
            mutate(Fund="Adult",.after= Measure)
        
        c <- EQ4 %>% 
            filter(`Board ID` == x , grepl("Adult",Program)) %>% 
            group_by(Measure=`Measure  Name`) %>% 
            summarise(Num=sum(as.numeric(`Numerator`)),
                      Den=length(`Numerator`),
                      Percent= percent(Num/Den, accuracy = 0.01),
                      Target = mean(Target),
                      Percent_of_Target = percent((Num/Den)/(Target/100), accuracy = 0.01)) %>% 
            mutate(Fund="Adult",.after= Measure)
        
        j <- credentials %>% 
            filter(`Board Number` == x, Program == funds[[1]]) %>% 
            mutate(Target = case_when(`Board Number` == 5 ~ 68.10,
                                      `Board Number` == 14 ~ 67.40,
                                      `Board Number` == 18 ~ 84.60,
                                      `Board Number` == 19 ~ 68.70,
                                      `Board Number` == 20 ~ 72.30,
                                      `Board Number` == 22 ~ 65.60,
                                      `Board Number` == 23 ~ 79.40)) %>% 
            summarise(Num = sum(`Texas Credential Numerator`),
                      Den = n(),
                      Percent = percent(Num/Den,accuracy = 0.01),
                      Target = mean(Target),
                      Percent_of_Target = percent((Num/Den)/(Target/100), accuracy = 0.01) ) %>% 
            mutate(Measure = "Credentials",
                   Fund = "Adult", .after = Measure) %>% 
            relocate(Measure,Fund)
        # Adult MSG
        m <- MSG %>% 
            filter(POPULATION == 4, WDA_NO == x) %>% 
            mutate(Target = case_when(WDA_NO == 5 ~ 64.40,
                                      WDA_NO == 14 ~ 76.80,
                                      WDA_NO == 18 ~ 61.60,
                                      WDA_NO == 19 ~ 61.60,
                                      WDA_NO == 20 ~ 61.60,
                                      WDA_NO == 22 ~ 61.60,
                                      WDA_NO == 23 ~ 76.80)) %>% 
            summarise(Num = sum(WIOA_MSG_YTD_NUM),
                      Den = sum(WIOA_MSG_YTD_DEN),
                      Percent = percent(Num/Den,accuracy = 0.01),
                      Target = mean(Target),
                      Percent_of_Target = percent((Num/Den)/(Target/100), accuracy = 0.01)) %>% 
            mutate(Measure = "MSG",
                   Fund = "Adult", .before = Num) %>% 
            relocate(Measure,Fund)
        
        Earnings_A <- MEQ2 %>%
            filter(`LWDA Number` == x) %>% 
            mutate(Numerator = as.numeric(Numerator)) %>% 
            filter(Program =="Title I Adult - Local") %>% 
            distinct(`Twist Id`,.keep_all = TRUE) %>% 
            summarise(Num = median(Numerator,digits = 0,na.rm = TRUE),
                      Den = length(Numerator),
                      Percent = "0",
                      Target = mean(Target),
                      Percent_of_Target = percent(Num/Target)) %>% 
            mutate(Measure = "Median Earnings",
                   Fund = "Adult") %>% 
            relocate(Measure,Fund)
        
        b <- EQ2 %>% 
            filter(`Board ID` == x, grepl("DW",Program)) %>% 
            group_by(Measure=`Measure  Name`) %>% 
            summarise(Num=sum(as.numeric(`Numerator`)),
                      Den=length(`Numerator`),
                      Percent= percent(Num/Den, accuracy = 0.01),
                      Target = mean(Target),
                      Percent_of_Target = percent((Num/Den)/(Target/100), accuracy = 0.01)) %>% 
            mutate(Fund="DW",.after=Measure)
        
        d <- EQ4 %>% 
            filter(`Board ID` == x, grepl("DW",Program)) %>% 
            group_by(Measure=`Measure  Name`) %>% 
            summarise(Num=sum(as.numeric(`Numerator`)),
                      Den=length(`Numerator`),
                      Percent= percent(Num/Den, accuracy = 0.01),
                      Target = mean(Target),
                      Percent_of_Target = percent((Num/Den)/(Target/100), accuracy = 0.01)) %>% 
            mutate(Fund="DW",.after=Measure)
        
        k <- credentials %>% 
            filter(`Board Number` == x, Program == funds[[2]]) %>% 
            mutate(Target = case_when(`Board Number` == 5 ~ 79.60,
                                      `Board Number` == 14 ~ 85.30,
                                      `Board Number` == 18 ~ 79.60,
                                      `Board Number` == 19 ~ 92.50,
                                      `Board Number` == 20 ~ 92.50,
                                      `Board Number` == 22 ~ 79.60,
                                      `Board Number` == 23 ~ 92.50)) %>% 
            summarise(Num = sum(`Texas Credential Numerator`),
                      Den = n(),
                      Percent = percent(Num/Den,accuracy = 0.01),
                      Target = mean(Target),
                      Percent_of_Target = percent((Num/Den)/(Target/100), accuracy = 0.01)) %>% 
            mutate(Measure = "Credentials",
                   Fund = "DW", .after = Measure) %>% 
            relocate(Measure,Fund)
        
        n <- MSG %>% 
            filter(POPULATION == 5, WDA_NO == x) %>% 
            mutate(Target = case_when(WDA_NO == 5 ~ 72.20,
                                      WDA_NO == 14 ~ 80.60,
                                      WDA_NO == 18 ~ 71.40,
                                      WDA_NO == 19 ~ 84.20,
                                      WDA_NO == 20 ~ 81.80,
                                      WDA_NO == 22 ~ 78.40,
                                      WDA_NO == 23 ~ 87.50)) %>% 
            summarise(Num = sum(WIOA_MSG_YTD_NUM),
                      Den = sum(WIOA_MSG_YTD_DEN),
                      Percent = percent(Num/Den,accuracy = 0.01),
                      Target = mean(Target),
                      Percent_of_Target = percent((Num/Den)/(Target/100), accuracy = 0.01)) %>% 
            mutate(Measure = "MSG",
                   Fund = "DW", .before = Num) %>% 
            relocate(Measure,Fund)
        
        Earnings_DW <- MEQ2 %>%
            filter(`LWDA Number` == x) %>%
            mutate(Numerator = as.numeric(Numerator)) %>% 
            filter(Program =="Title I DW - Local") %>% 
            distinct(`Twist Id`,.keep_all = TRUE) %>% 
            summarise(Num = median(Numerator, digits = 0,na.rm = TRUE),
                      Den = length(Numerator),
                      Percent = "0",
                      Target = mean(Target),
                      Percent_of_Target = percent(Num/Target)) %>% 
            mutate(Measure = "Median Earnings",
                   Fund = "DW") %>% 
            relocate(Measure,Fund)
        
        
        all_measures <- bind_rows(a,c,j,m,Earnings_A,
                                  b,d,k,n,Earnings_DW)
        
        all_measures <- all_measures %>% 
            mutate(Num = ifelse(Num> 1000,scales::dollar(Num),Num)) %>% 
            mutate(Percent_of_Target = ifelse(Percent_of_Target == "Inf",percent(0),Percent_of_Target)) %>% 
            mutate(Target = Target %>% as.numeric()) %>% 
            mutate("+/-" = round((Den*Target/100) - Num %>% 
                                     as.numeric(), digits = 0),
                   Target = if_else( Target < 100, 
                                     scales::percent(Target/100,accuracy = 0.01),
                                     scales::dollar(Target)))
        
        all_measures %>% 
            kbl(caption = "Summary - Adult and Dislocated Workers Measures",
                align = c('l','c','c','c','l'),
                col.names = c("Measure","Fund","Num","Den","Rate","Target","% of Target",
                              "+/-"),
                digits = 2) %>% 
            kable_styling(bootstrap_options = c("striped","hover","condensed")) %>%
            column_spec(column = 7 ,color = ifelse(parse_number(all_measures$Percent_of_Target) > 90,"black",
                                                   ifelse(parse_number(all_measures$Percent_of_Target) > 0, "red","black"))) 
    }



X_Summary_Qtr_No_Youth <-
    function(x){
        
        a0 <-  EQ2 %>% 
            filter(Program == funds[[1]],`Board ID` == x) %>% 
            summarise(Num=sum(as.numeric(`Numerator`)),
                      Den=length(`Numerator`),
                      Percent= percent(Num/Den, accuracy = 0.01),
                      Target = mean(Target),
                      `Percent of Target` = percent((Num/Den*100)/Target,accuracy = 0.01)) %>%
            mutate(Measure = "") %>% 
            mutate_all(as.character)%>% 
            pivot_longer(cols = c(Num,Den,Percent,Target,`Percent of Target`),
                         names_to = "Values",values_to = "YTD") %>%
            select( -c(1,2))
        
        a1 <- EQ2 %>% 
            filter(Program == funds[[1]],`Board ID` == x) %>% 
            group_by(Measure=`Measure  Name`,Quarter) %>% 
            summarise(Num=sum(as.numeric(`Numerator`)),
                      Den=length(`Numerator`),
                      Percent= Num/Den,
                      Target = mean(Target),
                      `Percent of Target` = percent(Percent/Target*100, accuracy = 0.01)) %>%
            mutate(Measure = "",
                   Percent = percent(Percent, accuracy = 0.01)) %>% 
            mutate_all(as.character)%>% 
            pivot_longer(cols = c(Num,Den,Percent,Target,`Percent of Target`),names_to = "Values",values_to = "numbers") %>% 
            pivot_wider(names_from = "Quarter",values_from = "numbers") %>% 
            # mutate("Q4" = "NA") %>%
            cbind(a0)
        
        b0 <- EQ2 %>% 
            filter(Program == funds[[2]],`Board ID` == x) %>% 
            summarise(Num=sum(as.numeric(`Numerator`)),
                      Den=length(`Numerator`),
                      Percent= percent(Num/Den, accuracy = 0.01),
                      Target = mean(Target),
                      `Percent of Target` = percent((Num/Den*100)/Target)) %>%
            mutate(Measure = "") %>% 
            mutate_all(as.character)%>% 
            pivot_longer(cols = c(Num,Den,Percent,Target,`Percent of Target`),
                         names_to = "Values",values_to = "YTD") %>%
            select( -c(1,2))
        
        b1 <- EQ2 %>% 
            filter(Program == funds[[2]],`Board ID` == x) %>% 
            group_by(Measure=`Measure  Name`,Quarter) %>% 
            summarise(Num=sum(as.numeric(`Numerator`)),
                      Den=length(`Numerator`),
                      Percent= Num/Den,
                      Target = mean(Target),
                      `Percent of Target` = percent(Percent/Target*100,accuracy = 0.01)) %>% 
            mutate(Measure = "",
                   Percent = percent(Percent,accuracy = 0.01)) %>% 
            mutate_all(as.character)%>% 
            pivot_longer(cols = c(Num,Den,Percent,Target,`Percent of Target`),names_to = "Values",values_to = "numbers") %>% 
            pivot_wider(names_from = "Quarter",values_from = "numbers") %>% 
            cbind(b0)
        
        c0 <- EQ4 %>% 
            filter(grepl("Adult",Program),`Board ID` == x) %>% 
            summarise(Num=sum(as.numeric(`Numerator`)),
                      Den=length(`Numerator`),
                      Percent= percent(Num/Den, accuracy = 0.01),
                      Target = mean(Target),
                      `Percent of Target` = percent((Num/Den*100)/Target, accuracy = 0.01)) %>%
            mutate(Measure = "") %>% 
            mutate_all(as.character)%>% 
            pivot_longer(cols = c(Num,Den,Percent,Target,`Percent of Target`),
                         names_to = "Values",values_to = "YTD") %>%
            select( -c(1,2))
        
        c1 <- EQ4 %>% 
            filter(grepl("Adult",Program),`Board ID` == x) %>% 
            group_by(Measure=`Measure  Name`,Quarter) %>% 
            summarise(Num=sum(as.numeric(`Numerator`)),
                      Den=length(`Numerator`),
                      Percent= Num/Den,
                      Target = mean(Target),
                      `Percent of Target` = percent(Percent/Target*100, accuracy = 0.01)) %>% 
            mutate(Measure = "",
                   Percent = percent(Percent, accuracy = 0.01)) %>% 
            mutate_all(as.character)%>% 
            pivot_longer(cols = c(Num,Den,Percent,Target,`Percent of Target`),names_to = "Values",values_to = "numbers") %>% 
            pivot_wider(names_from = "Quarter",values_from = "numbers") %>% 
            cbind(c0)
        
        d0 <- EQ4 %>% 
            filter(grepl("DW",Program), `Board ID` == x) %>% 
            summarise(Num=sum(as.numeric(`Numerator`)),
                      Den=length(`Numerator`),
                      Percent= percent(Num/Den, accuracy = 0.01),
                      Target = mean(Target),
                      `Percent of Target` = percent((Num/Den*100)/Target, accuracy = 0.01)) %>%
            mutate(Measure = "") %>% 
            mutate_all(as.character)%>% 
            pivot_longer(cols = c(Num,Den,Percent,Target,`Percent of Target`),
                         names_to = "Values",values_to = "YTD") %>%
            select( -c(1,2))
        
        d1 <- EQ4 %>% 
            filter(grepl("DW",Program), `Board ID` == x) %>% 
            group_by(Measure=`Measure  Name`,Quarter) %>% 
            summarise(Num=sum(as.numeric(`Numerator`)),
                      Den=length(`Numerator`),
                      Percent= Num/Den,
                      Target = mean(Target),
                      `Percent of Target` = percent(Percent/Target*100, accuracy = 0.01)) %>% 
            mutate(Measure = "",
                   Percent = percent(Percent, accuracy = 0.01)) %>% 
            mutate_all(as.character)%>% 
            pivot_longer(cols = c(Num,Den,Percent,Target,`Percent of Target`),names_to = "Values",values_to = "numbers") %>% 
            pivot_wider(names_from = "Quarter",values_from = "numbers") %>% 
            cbind(d0)
        
        
        h0 <- EEQ2_Q4 %>%
            filter(`Board ID` == x) %>%
            summarise(Num=sum(as.numeric(`Numerator`)),
                      Den=length(`Numerator`),
                      Percent= percent(Num/Den, accuracy = 0.01),
                      Target = mean(Target),
                      `Percent of Target` = percent((Num/Den*100)/Target, accuracy = 0.01)) %>%
            mutate(Measure = "") %>%
            mutate_all(as.character)%>%
            pivot_longer(cols = c(Num,Den,Percent,Target,`Percent of Target`),
                         names_to = "Values",values_to = "YTD") %>%
            select( -c(1,2))
        
        h1 <- EEQ2_Q4 %>%
            filter(`Board ID` == x) %>%
            group_by(Measure=`Measure  Name`,Quarter) %>%
            summarise(Num=sum(as.numeric(`Numerator`)),
                      Den=length(`Numerator`),
                      Percent= Num/Den,
                      Target = mean(Target),
                      `Percent of Target` = percent(Percent/Target*100, accuracy = 0.01)) %>%
            mutate(Measure = "",
                   Percent = percent(Percent, accuracy = 0.01)) %>%
            mutate_all(as.character)%>%
            pivot_longer(cols = c(Num,Den,Percent,Target,`Percent of Target`),names_to = "Values",values_to = "numbers") %>%
            pivot_wider(names_from = "Quarter",values_from = "numbers") %>%
            cbind(h0)
        
        i0 <- credentials %>% 
            filter(local_wda_no == x) %>%
            mutate(Target = case_when(
                local_wda_no == 23 ~ 70.90,
                TRUE ~ 70
            )) %>% 
            summarise(Num=sum(`Texas Credential Numerator`),
                      Den=length(`Texas Credential Numerator`),
                      Percent= percent(Num/Den, accuracy = 0.01),
                      Target = mean(Target),
                      `Percent of Target` = percent((Num/Den)/(Target/100),accuracy = 0.01)) %>%
            mutate(Measure = "") %>% 
            mutate_all(as.character)%>% 
            pivot_longer(cols = c(Num,Den,Percent,Target,`Percent of Target`),
                         names_to = "Values",values_to = "YTD") %>%
            select( -c(1,2))
        
        i1 <- credentials %>% 
            filter( local_wda_no == x) %>%
            mutate(Target = case_when(
                local_wda_no == 23 ~ 70.90,
                TRUE ~ 70
            )) %>% 
            group_by(Quarter) %>% 
            summarise(Measure = "",
                      Num=sum(`Texas Credential Numerator`),
                      Den = sum(`Num`),
                      Percent = Num/Den,
                      Target = mean(Target),
                      `Percent of Target` = percent((Num/Den)/(Target/100),accuracy = 0.01)) %>% 
            ungroup() %>% 
            mutate(Percent = percent(Percent, accuracy = 0.01)) %>% 
            mutate_all(as.character) %>% 
            pivot_longer(cols = c(Num,Den,Percent,Target,`Percent of Target`),names_to = "Values",values_to = "numbers") %>% 
            pivot_wider(names_from = Quarter,values_from = "numbers") %>% 
            cbind(i0)
        
        j0 <- credentials %>% 
            filter(local_wda_no == x , Program == funds[[1]]) %>%
            mutate(Target = case_when(`local_wda_no` == 5 ~ 68.10,
                                      `local_wda_no` == 14 ~ 67.40,
                                      `local_wda_no` == 18 ~ 84.60,
                                      `local_wda_no` == 19 ~ 68.70,
                                      `local_wda_no` == 20 ~ 72.30,
                                      `local_wda_no` == 22 ~ 65.60,
                                      `local_wda_no` == 23 ~ 79.40),
                   .keep_all=TRUE) %>% 
            summarise(Measure = "",
                      Num=sum(`Texas Credential Numerator`),
                      Den = n(),
                      Percent = percent(Num/Den, accuracy = 0.01),
                      Target = mean(Target),
                      `Percent of Target` = percent((Num/Den)/(Target/100),accuracy = 0.01)) %>% 
            ungroup() %>% 
            mutate_all(as.character) %>% 
            pivot_longer(cols = c(Num,Den,Percent,Target,`Percent of Target`),
                         names_to = "Values",values_to = "YTD") %>% 
            select(-c(1,2))
        
        j1 <- credentials %>% 
            filter(`local_wda_no` == x , Program == funds[[1]]) %>%
            mutate(Target = case_when(`local_wda_no` == 5 ~ 68.10,
                                      `local_wda_no` == 14 ~ 67.40,
                                      `local_wda_no` == 18 ~ 84.60,
                                      `local_wda_no` == 19 ~ 68.70,
                                      `local_wda_no` == 20 ~ 72.30,
                                      `local_wda_no` == 22 ~ 65.60,
                                      `local_wda_no` == 23 ~ 79.40),
                   .keep_all=TRUE) %>% 
            group_by(Quarter) %>% 
            summarise(Measure = "",
                      Num=sum(`Texas Credential Numerator`),
                      Den = n(),
                      Percent = Num/Den,
                      Target = mean(Target),
                      `Percent of Target` = percent((Num/Den)/(Target/100),accuracy = 0.01)) %>% 
            ungroup() %>%
            mutate(Percent=percent(Percent, accuracy = 0.01)) %>% 
            mutate_all(as.character) %>% 
            pivot_longer(cols = c(Num,Den,Percent,Target,`Percent of Target`),names_to = "Values",values_to = "numbers") %>% 
            pivot_wider(names_from = Quarter,values_from = "numbers") %>% 
            cbind(j0)
        
        k0 <- credentials %>% 
            filter(`local_wda_no` == x , Program == funds[[2]]) %>%
            mutate(Target = case_when(`local_wda_no` == 5 ~ 79.60,
                                      `local_wda_no` == 14 ~ 85.30,
                                      `local_wda_no` == 18 ~ 79.60,
                                      `local_wda_no` == 19 ~ 92.50,
                                      `local_wda_no` == 20 ~ 92.50,
                                      `local_wda_no` == 22 ~ 79.60,
                                      `local_wda_no` == 23 ~ 85.00),
                   .keep_all=TRUE) %>% 
            summarise(Measure = "",
                      Num=sum(`Texas Credential Numerator`),
                      Den = n(),
                      Percent = percent(Num/Den, accuracy = 0.01),
                      Target = mean(Target),
                      `Percent of Target` = percent((Num/Den)/(Target/100),accuracy = 0.01)) %>% 
            ungroup() %>% 
            mutate_all(as.character) %>% 
            pivot_longer(cols = c(Num,Den,Percent,Target,`Percent of Target`),
                         names_to = "Values",values_to = "YTD") %>% 
            select(-c(1,2))
        
        k1 <- credentials %>% 
            filter(`local_wda_no` == x , Program == funds[[2]]) %>%
            mutate(Target = case_when(`local_wda_no` == 5 ~ 79.60,
                                      `local_wda_no` == 14 ~ 85.30,
                                      `local_wda_no` == 18 ~ 79.60,
                                      `local_wda_no` == 19 ~ 92.50,
                                      `local_wda_no` == 20 ~ 92.50,
                                      `local_wda_no` == 22 ~ 79.60,
                                      `local_wda_no` == 23 ~ 85.00),
                   .keep_all=TRUE) %>% 
            group_by(Quarter) %>% 
            summarise(Measure = "",
                      Num=sum(`Texas Credential Numerator`),
                      Den = n(),
                      Percent = round(Num/Den*100,digits = 2),
                      Target = mean(Target),
                      `Percent of Target` = percent((Num/Den)/(Target/100),accuracy = 0.01)) %>% 
            ungroup() %>% 
            mutate(Percent = percent(Percent/100,accuracy = 0.01)) %>% 
            mutate_all(as.character) %>% 
            pivot_longer(cols = c(Num,Den,Percent,Target,`Percent of Target`),names_to = "Values",values_to = "numbers") %>% 
            pivot_wider(names_from = Quarter,values_from = "numbers") %>% 
            cbind(k0)
        

        m1 <- MSG %>% 
            filter(`Population Group Code` == 10, WDA_NO == x) %>% 
            mutate(Target = case_when(x == 5 ~ 64.40,
                                      x == 14 ~ 76.80,
                                      x == 18 ~ 61.60,
                                      x == 19 ~ 61.60,
                                      x == 20 ~ 61.60,
                                      x == 22 ~ 61.60,
                                      x == 23 ~ 76.80)) %>% 
            # group_by(Quarter) %>% 
            summarise(Measure = "",
                      Num=sum(`MSG Numerator`),
                      Den = length(`MSG Denominator`),
                      Percent = percent(Num / Den,accuracy = 0.01),
                      Target = mean(Target),
                      `Percent of Target` = percent((Num/Den)/(Target/100),accuracy = 0.01)) %>% 
            ungroup() %>% 
            mutate_all(as.character) %>% 
            pivot_longer(cols = c(Num,Den,Percent,Target,`Percent of Target`), names_to = "Values", values_to = "YTD") %>% 
            # pivot_wider(names_from = Quarter, values_from = "numbers") %>% 
            mutate(Q1 = "NA",Q2 = "NA", Q3 = "NA",Q4 = "NA",.after = Values)
        # relocate(Measure)
        
        n1 <- MSG %>% 
            filter(`Population Group Code` == 13, WDA_NO == x) %>%
            mutate(Target = case_when(x == 5 ~ 64.40,
                                      x == 14 ~ 76.80,
                                      x == 18 ~ 61.60,
                                      x == 19 ~ 61.60,
                                      x == 20 ~ 61.60,
                                      x == 22 ~ 61.60,
                                      x == 23 ~ 85.00)) %>% 
            # group_by(Quarter) %>% 
            summarise(Measure = "",
                      Num=sum(`MSG Numerator`),
                      Den = length(`MSG Denominator`),
                      Percent = percent(Num / Den,accuracy = 0.01),
                      Target = mean(Target),
                      `Percent of Target` = percent((Num/Den)/(Target/100),accuracy = 0.01)) %>% 
            ungroup() %>% 
            mutate_all(as.character) %>% 
            pivot_longer(cols = c(Num,Den,Percent,Target,`Percent of Target`), names_to = "Values", values_to = "YTD") %>% 
            # pivot_wider(names_from = Quarter, values_from = "numbers") %>% 
            mutate(Q1 = "NA",Q2 = "NA", Q3 = "NA",Q4 = "NA",.after = Values)
        # relocate(Measure)
        

        
        #adult median earnings Q2
        p0 <- MEQ2 %>% 
            filter(`LWDA Number` == x ,Program == "Title I Adult - Local" ) %>% 
            group_by(`Exit Qtr`) %>%
            mutate(Target = case_when(
                `LWDA Number` == 18 ~ 5500,
                TRUE ~ 0)) %>% 
            summarise(Num = dollar(median(Numerator)),
                      Den = sum(Denominator),
                      Percent =  0,
                      Target = mean(Target),
                      "Percent of Target" = percent(median(Numerator)/Target,
                                                    accuracy = 0.01)) %>% 
            ungroup() %>% 
            mutate_all(as.character) %>% 
            pivot_longer(cols = 2:6, names_to = "Values" ) %>% 
            pivot_wider(names_from = `Exit Qtr`,
                        values_from = value)
        p1 <- MEQ2 %>% 
            filter(`LWDA Number` == x, Program == "Title I Adult - Local") %>% 
            summarise(Num = dollar(median(Numerator)),
                      Den = sum(Denominator),
                      Percent = 0,
                      Target = mean(Target),
                      "Percent of Target" = percent(median(Numerator)/ Target,
                                                    accuracy = 0.01)) %>%
            mutate_all(as.character) %>% 
            pivot_longer(cols = 1:5) %>% 
            rename(YTD = value) %>% 
            select(YTD) %>% 
            cbind(p0)
        
        # DW median earnings
        q0 <- MEQ2 %>% 
            filter(`LWDA Number` == x,Program == "Title I DW - Local" ) %>% 
            group_by(`Exit Qtr`) %>%
            mutate(Target = case_when(
                `LWDA Number` == 18 ~ 7300,
                TRUE ~ 0)) %>% 
            summarise(Num = dollar(median(Numerator)),
                      Den = sum(Denominator),
                      Percent =  0,
                      Target = mean(Target),
                      "Percent of Target" = percent(median(Numerator)/Target,
                                                    accuracy = 0.01)) %>% 
            ungroup() %>% 
            mutate_all(as.character) %>% 
            pivot_longer(cols = 2:6, names_to = "Values" ) %>% 
            pivot_wider(names_from = `Exit Qtr`,
                        values_from = value)
        q1 <- MEQ2 %>% 
            filter(`LWDA Number` == x, Program == "Title I DW - Local") %>% 
            summarise(Num = dollar(median(Numerator)),
                      Den = sum(Denominator),
                      Percent = 0,
                      Target = mean(Target),
                      "Percent of Target" = percent(median(Numerator)/ Target,
                                                    accuracy = 0.01)) %>%
            mutate_all(as.character) %>% 
            pivot_longer(cols = 1:5) %>% 
            rename(YTD = value) %>% 
            select(YTD) %>% 
            cbind(q0)
        
        
        all_measures <- bind_rows(a1,b1,c1,d1,h1,i1,j1,k1,m1,n1,p1,q1) %>% 
            mutate(Q1 = ifelse(Q1 == "NA", 0 , Q1),
                   Q2 = ifelse(Q2 == "NA", 0, Q2),
                   Q3 = ifelse(Q3 == "NA", 0, Q3),
                   Q4 = ifelse(Q4 == "NA", 0, Q4),
                   Measure = ifelse(is.na(Measure), " ", Measure)) %>% 
            relocate(-YTD)
        
        all_measures %>% 
            kbl(caption = "Summary",
                align = c('l','c','c','c','l'),
                digits = 2) %>% 
            kable_styling(bootstrap_options = c("striped","hover","condensed")) %>% 
            pack_rows("Employed Q2 Adult",1,5) %>%
            pack_rows("Employed Q2 DW",6,10) %>%
            pack_rows("Employed Q4 Adult",11,15) %>%
            pack_rows("Employed Q4 DW",16,20) %>%
            pack_rows("Employed Enrolled Q2 - Q4 All",21,25) %>%
            pack_rows("Credential All",26,30) %>%
            pack_rows("Credential Adult",31,35) %>%
            pack_rows("Credential DW",36,40) %>%
            pack_rows("MSG Adult",41,45) %>%
            pack_rows("MSG DW",46,50) %>%
            pack_rows("Median Earnings Adult",51,55) %>% 
            pack_rows("Median Earnings DW", 56,60) 
    }