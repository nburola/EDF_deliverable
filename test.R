fishe_react<-
  #dataframe creation
  fishe_clean %>%
    filter(r_0 == 0.1)


  ggplot(data = fishe_react, aes(x = year, y = b)) +
    geom_line()

  lst.year  <- unique(sort(fishe_clean$year))
  
  lst.s  <- unique(sort(fishe_b$r_s))
  lst.erre <- unique(sort(fishe_b$r))
  lst.id  <- unique(sort(fishe_b$id))
 
    fishe_b1 <- fishe_b %>%
      filter(r_0 == 0.1,
             b_0 == 1500,
             error == 0.1,
             assess == 5,
             hcr_select == 0.35
      ) 

    fishe_b2 <- fishe_b1 %>%
      group_by(year) %>% 
      summarize(b = mean(b)) 
 
  
