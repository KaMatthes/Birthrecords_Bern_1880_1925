data.grippe.org <- read.csv(paste0("data_raw/",data.flu), sep=";",header=TRUE)

function_data_grippe <- function(n_week) {

datagrippe_tmp <- data.grippe.org %>%
    mutate(Cases.rollsum = zoo::rollsumr(Canton, k=n_week, fill = NA),
           Cases.rollsum = ifelse(is.na(Cases.rollsum), cumsum(Canton), Cases.rollsum),
           Number_weeks = n_week)%>%
    filter(!is.na(KW)) %>%
    mutate(weeks.range = paste0(KW-n_week+1,"_", KW,"_",year),
           
           
           
           weeks.range = replace(weeks.range,weeks.range=="-2_1_1919", "50_1_1919"),
           weeks.range = replace(weeks.range,weeks.range=="-1_2_1919", "51_2_1919"),
           weeks.range = replace(weeks.range,weeks.range=="0_3_1919", "52_3_1919"),
           
           weeks.range = replace(weeks.range,weeks.range=="-3_1_1919", "49_1_1919"),
           weeks.range = replace(weeks.range,weeks.range=="-2_2_1919", "50_2_1919"),
           weeks.range = replace(weeks.range,weeks.range=="-1_3_1919", "51_3_1919"),
           weeks.range = replace(weeks.range,weeks.range=="0_4_1919", "52_4_1919"),
           
           weeks.range = replace(weeks.range,weeks.range=="-4_1_1919", "48_1_1919"),
           weeks.range = replace(weeks.range,weeks.range=="-3_2_1919", "49_2_1919"),
           weeks.range = replace(weeks.range,weeks.range=="-2_3_1919", "50_3_1919"),
           weeks.range = replace(weeks.range,weeks.range=="-1_4_1919", "51_4_1919"),
           weeks.range = replace(weeks.range,weeks.range=="0_5_1919", "52_5_1919"),
           
           weeks.range = replace(weeks.range,weeks.range=="-5_1_1919", "47_1_1919"),
           weeks.range = replace(weeks.range,weeks.range=="-4_2_1919", "48_2_1919"),
           weeks.range = replace(weeks.range,weeks.range=="-3_3_1919", "49_3_1919"),
           weeks.range = replace(weeks.range,weeks.range=="-2_4_1919", "50_4_1919"),
           weeks.range = replace(weeks.range,weeks.range=="-1_5_1919", "51_5_1919"),
           weeks.range = replace(weeks.range,weeks.range=="0_6_1919", "52_6_1919"),
           
           weeks.range = replace(weeks.range,weeks.range=="-6_1_1919", "46_1_1919"),
           weeks.range = replace(weeks.range,weeks.range=="-5_2_1919", "47_2_1919"),
           weeks.range = replace(weeks.range,weeks.range=="-4_3_1919", "48_3_1919"),
           weeks.range = replace(weeks.range,weeks.range=="-3_4_1919", "49_4_1919"),
           weeks.range = replace(weeks.range,weeks.range=="-2_5_1919", "50_5_1919"),
           weeks.range = replace(weeks.range,weeks.range=="-1_6_1919", "51_6_1919"),
           weeks.range = replace(weeks.range,weeks.range=="0_7_1919", "52_7_1919"),
           
           weeks.range = replace(weeks.range,weeks.range=="-7_1_1919", "45_1_1919"),
           weeks.range = replace(weeks.range,weeks.range=="-6_2_1919", "46_2_1919"),
           weeks.range = replace(weeks.range,weeks.range=="-5_3_1919", "47_3_1919"),
           weeks.range = replace(weeks.range,weeks.range=="-4_4_1919", "48_4_1919"),
           weeks.range = replace(weeks.range,weeks.range=="-3_5_1919", "49_5_1919"),
           weeks.range = replace(weeks.range,weeks.range=="-2_6_1919", "50_6_1919"),
           weeks.range = replace(weeks.range,weeks.range=="-1_7_1919", "51_7_1919"),
           weeks.range = replace(weeks.range,weeks.range=="0_8_1919", "52_8_1919"),
           
           weeks.range = replace(weeks.range,weeks.range=="-8_1_1919", "44_1_1919"),
           weeks.range = replace(weeks.range,weeks.range=="-7_2_1919", "45_2_1919"),
           weeks.range = replace(weeks.range,weeks.range=="-6_3_1919", "46_3_1919"),
           weeks.range = replace(weeks.range,weeks.range=="-5_4_1919", "47_4_1919"),
           weeks.range = replace(weeks.range,weeks.range=="-4_5_1919", "48_5_1919"),
           weeks.range = replace(weeks.range,weeks.range=="-3_6_1919", "49_6_1919"),
           weeks.range = replace(weeks.range,weeks.range=="-2_7_1919", "50_7_1919"),
           weeks.range = replace(weeks.range,weeks.range=="-1_8_1919", "51_8_1919"),
           weeks.range = replace(weeks.range,weeks.range=="0_9_1919", "52_9_1919"),
           
           weeks.range = replace(weeks.range,weeks.range=="-9_1_1919", "43_1_1919"),
           weeks.range = replace(weeks.range,weeks.range=="-8_2_1919", "44_2_1919"),
           weeks.range = replace(weeks.range,weeks.range=="-7_3_1919", "45_3_1919"),
           weeks.range = replace(weeks.range,weeks.range=="-6_4_1919", "46_4_1919"),
           weeks.range = replace(weeks.range,weeks.range=="-5_5_1919", "47_5_1919"),
           weeks.range = replace(weeks.range,weeks.range=="-4_6_1919", "48_6_1919"),
           weeks.range = replace(weeks.range,weeks.range=="-3_7_1919", "49_7_1919"),
           weeks.range = replace(weeks.range,weeks.range=="-2_8_1919", "50_8_1919"),
           weeks.range = replace(weeks.range,weeks.range=="-1_9_1919", "51_9_1919"),
           weeks.range = replace(weeks.range,weeks.range=="0_10_1919", "52_10_1919"),
           
           weeks.range = replace(weeks.range,weeks.range=="-10_1_1919", "42_1_1919"),
           weeks.range = replace(weeks.range,weeks.range=="-9_2_1919", "43_2_1919"),
           weeks.range = replace(weeks.range,weeks.range=="-8_3_1919", "44_3_1919"),
           weeks.range = replace(weeks.range,weeks.range=="-7_4_1919", "45_4_1919"),
           weeks.range = replace(weeks.range,weeks.range=="-6_5_1919", "46_5_1919"),
           weeks.range = replace(weeks.range,weeks.range=="-5_6_1919", "47_6_1919"),
           weeks.range = replace(weeks.range,weeks.range=="-4_7_1919", "48_7_1919"),
           weeks.range = replace(weeks.range,weeks.range=="-3_8_1919", "49_8_1919"),
           weeks.range = replace(weeks.range,weeks.range=="-2_9_1919", "50_9_1919"),
           weeks.range = replace(weeks.range,weeks.range=="-1_10_1919", "51_10_1919"),
           weeks.range = replace(weeks.range,weeks.range=="0_11_1919", "52_10_1919"),
           
           weeks.range = replace(weeks.range,weeks.range=="-11_1_1919", "41_1_1919"),
           weeks.range = replace(weeks.range,weeks.range=="-10_2_1919", "42_2_1919"),
           weeks.range = replace(weeks.range,weeks.range=="-9_3_1919", "43_3_1919"),
           weeks.range = replace(weeks.range,weeks.range=="-8_4_1919", "44_4_1919"),
           weeks.range = replace(weeks.range,weeks.range=="-7_5_1919", "45_5_1919"),
           weeks.range = replace(weeks.range,weeks.range=="-6_6_1919", "46_6_1919"),
           weeks.range = replace(weeks.range,weeks.range=="-5_7_1919", "47_7_1919"),
           weeks.range = replace(weeks.range,weeks.range=="-4_8_1919", "48_8_1919"),
           weeks.range = replace(weeks.range,weeks.range=="-3_9_1919", "49_9_1919"),
           weeks.range = replace(weeks.range,weeks.range=="-2_10_1919", "50_10_1919"),
           weeks.range = replace(weeks.range,weeks.range=="-1_11_1919", "51_11_1919"),
           weeks.range = replace(weeks.range,weeks.range=="0_12_1919", "52_12_1919"),
           
           weeks.range = replace(weeks.range,weeks.range=="-12_1_1919", "40_1_1919"),
           weeks.range = replace(weeks.range,weeks.range=="-11_2_1919", "41_2_1919"),
           weeks.range = replace(weeks.range,weeks.range=="-10_3_1919", "42_3_1919"),
           weeks.range = replace(weeks.range,weeks.range=="-9_4_1919", "43_4_1919"),
           weeks.range = replace(weeks.range,weeks.range=="-8_5_1919", "44_5_1919"),
           weeks.range = replace(weeks.range,weeks.range=="-7_6_1919", "45_6_1919"),
           weeks.range = replace(weeks.range,weeks.range=="-6_7_1919", "46_7_1919"),
           weeks.range = replace(weeks.range,weeks.range=="-5_8_1919", "47_8_1919"),
           weeks.range = replace(weeks.range,weeks.range=="-4_9_1919", "48_9_1919"),
           weeks.range = replace(weeks.range,weeks.range=="-3_10_1919", "49_10_1919"),
           weeks.range = replace(weeks.range,weeks.range=="-2_11_1919", "50_11_1919"),
           weeks.range = replace(weeks.range,weeks.range=="-1_12_1919", "51_12_1919"),
           weeks.range = replace(weeks.range,weeks.range=="0_13_1919", "52_13_1919"),
           
           weeks.range = replace(weeks.range,weeks.range=="-13_1_1919", "39_1_1919"),
           weeks.range = replace(weeks.range,weeks.range=="-12_2_1919", "40_2_1919"),
           weeks.range = replace(weeks.range,weeks.range=="-11_3_1919", "41_3_1919"),
           weeks.range = replace(weeks.range,weeks.range=="-10_4_1919", "42_4_1919"),
           weeks.range = replace(weeks.range,weeks.range=="-9_5_1919", "43_5_1919"),
           weeks.range = replace(weeks.range,weeks.range=="-8_6_1919", "44_6_1919"),
           weeks.range = replace(weeks.range,weeks.range=="-7_7_1919", "45_7_1919"),
           weeks.range = replace(weeks.range,weeks.range=="-6_8_1919", "46_8_1919"),
           weeks.range = replace(weeks.range,weeks.range=="-5_9_1919", "47_9_1919"),
           weeks.range = replace(weeks.range,weeks.range=="-4_10_1919", "48_10_1919"),
           weeks.range = replace(weeks.range,weeks.range=="-3_11_1919", "49_11_1919"),
           weeks.range = replace(weeks.range,weeks.range=="-2_12_1919", "50_12_1919"),
           weeks.range = replace(weeks.range,weeks.range=="-1_13_1919", "51_13_1919"),
           weeks.range = replace(weeks.range,weeks.range=="0_14_1919", "52_14_1919"),
           
           weeks.range = replace(weeks.range,weeks.range=="-14_1_1919", "38_1_1919"),
           weeks.range = replace(weeks.range,weeks.range=="-13_2_1919", "39_2_1919"),
           weeks.range = replace(weeks.range,weeks.range=="-12_3_1919", "40_3_1919"),
           weeks.range = replace(weeks.range,weeks.range=="-11_4_1919", "41_4_1919"),
           weeks.range = replace(weeks.range,weeks.range=="-10_5_1919", "42_5_1919"),
           weeks.range = replace(weeks.range,weeks.range=="-9_6_1919", "43_6_1919"),
           weeks.range = replace(weeks.range,weeks.range=="-8_7_1919", "44_7_1919"),
           weeks.range = replace(weeks.range,weeks.range=="-7_8_1919", "45_8_1919"),
           weeks.range = replace(weeks.range,weeks.range=="-6_9_1919", "46_9_1919"),
           weeks.range = replace(weeks.range,weeks.range=="-5_10_1919", "47_10_1919"),
           weeks.range = replace(weeks.range,weeks.range=="-4_11_1919", "48_11_1919"),
           weeks.range = replace(weeks.range,weeks.range=="-3_12_1919", "49_12_1919"),
           weeks.range = replace(weeks.range,weeks.range=="-2_13_1919", "50_13_1919"),
           weeks.range = replace(weeks.range,weeks.range=="-1_14_1919", "51_14_1919"),
           weeks.range = replace(weeks.range,weeks.range=="0_15_1919", "52_15_1919"))
  
  
return(datagrippe_tmp)
  
}


datagrippe <- rbind(function_data_grippe(4), function_data_grippe(5),function_data_grippe(6),
                        function_data_grippe(7),function_data_grippe(8),function_data_grippe(9),
                        function_data_grippe(10), function_data_grippe(11),function_data_grippe(12),
                        function_data_grippe(13),function_data_grippe(14),function_data_grippe(15),
                        function_data_grippe(16))  %>%
  mutate(Cases.range = round(normalit(Cases.rollsum),2))


save(datagrippe,file=paste0("data/data_flu.RData"))
write.table(datagrippe,file=paste0("data/data_flu.csv"),row.names=FALSE, sep=";")
