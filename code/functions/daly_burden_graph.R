daly_burden_graph = function(disease, burden_data, flag = FALSE, 
                             flg_04_13_mean = 250, flg_04_13_studies = 5,
                             flg_14_21_mean = 250, flg_14_21_studies = 5){
  df_04_13 <- ictrp_split %>% 
    filter(str_detect(StandardisedCondition, str_to_sentence(disease)),
           DATE_REGISTRATION < ymd("2014-01-01")) %>% 
    count(COUNTRY) %>% 
    right_join(burden_data, by = c("COUNTRY" = "alpha_3_code"))
  
  df_14_21 <- ictrp_split %>% 
    filter(str_detect(StandardisedCondition, str_to_sentence(disease)),
           DATE_REGISTRATION >= ymd("2014-01-01")) %>% 
    count(COUNTRY) %>% 
    right_join(burden_data, by = c("COUNTRY" = "alpha_3_code"))
  
  df_04_13[which(is.na(df_04_13$n) & !(df_04_13$daly_mean_04_13_ppop == 0)), "n"] = 0
  df_14_21[which(is.na(df_14_21$n) & !(df_14_21$daly_mean_14_23_ppop == 0)), "n"] = 0
  
  if(flag == FALSE){
    p_04_13 <- ggplot(df_04_13, aes(x = daly_mean_04_13_ppop*1000000, y = n))+
      geom_point(size = 3, alpha = 0.5)+
      theme_bw() +
      labs(x = "Mean Annual DALYs per million population (2004-13)",
           y = "Number of studies") +
      scale_y_continuous(breaks = breaks_pretty(),
                         limits = c(0, max(df_04_13$n, df_14_21$n, na.rm = TRUE)))+
      scale_x_continuous(breaks = breaks_pretty(), expand = c(0,100),
                         limits = c(0, max(df_04_13$daly_mean_04_13_ppop*1000000, df_04_13$daly_mean_14_23_ppop*1000000, na.rm = TRUE)))
    
    p_14_21 <- ggplot(df_14_21, aes(x = daly_mean_14_23_ppop*1000000, y = n))+
      geom_point(size = 3, alpha = 0.5)+
      theme_bw() +
      labs(x = "Mean Annual DALYs per million population (2014-21)",
           y = "")  +
      scale_y_continuous(breaks = breaks_pretty(),
                         limits = c(0, max(df_04_13$n, df_14_21$n, na.rm = TRUE)))+
      scale_x_continuous(breaks = breaks_pretty(), expand = c(0,100),
                         limits = c(0, max(df_14_21$daly_mean_04_13_ppop*1000000, df_14_21$daly_mean_14_23_ppop*1000000, na.rm = TRUE)))
    
  } else{
    p_04_13 <- ggplot(df_04_13, aes(x = daly_mean_04_13_ppop*1000000, y = n, country = str_to_lower(alpha_2_code)))+
      geom_point(size = 5, alpha = 0.5)+
      geom_flag(data = df_04_13 %>% 
                  filter(daly_mean_04_13_ppop*1000000 > flg_04_13_mean | n > flg_04_13_studies), size = 5)+
      theme_bw() +
      labs(x = "Mean Annual DALYs per million population (2004-13)",
           y = "Number of studies") +
      scale_y_continuous(breaks = breaks_pretty(),
                         limits = c(0, max(df_04_13$n, df_14_21$n, na.rm = TRUE)))+
      scale_x_continuous(breaks = breaks_pretty(), expand = c(0,100),
                         limits = c(0, max(df_04_13$daly_mean_04_13_ppop*1000000, df_04_13$daly_mean_14_23_ppop*1000000, na.rm = TRUE)))
    
    p_14_21 <- ggplot(df_14_21, aes(x = daly_mean_14_23_ppop*1000000, y = n, country  = str_to_lower(alpha_2_code)))+
      geom_point(size = 5, alpha = 0.5)+
      geom_flag(data = df_14_21 %>% 
                  filter(daly_mean_14_23_ppop*1000000 > flg_14_21_mean | n > flg_14_21_studies), size = 5)+
      theme_bw() +
      labs(x = "Mean Annual DALYs per million population (2014-21)",
           y = "")  +
      scale_y_continuous(breaks = breaks_pretty(),
                         limits = c(0, max(df_04_13$n, df_14_21$n, na.rm = TRUE)))+
      scale_x_continuous(breaks = breaks_pretty(), expand = c(0,100),
                         limits = c(0, max(df_14_21$daly_mean_04_13_ppop*1000000, df_14_21$daly_mean_14_23_ppop*1000000, na.rm = TRUE)))
  }
  
  return(plot_grid(p_04_13, p_14_21))
  
}
