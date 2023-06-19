##### Arrange chapter data for plots


#### Chapter 1
Chap1 <- Copenhagen_all %>% 
  filter(KAP == 1) %>% 
  group_by(DIAG) %>% 
  arrange(-RR)


#### Chapter 2
Chap2 <- Copenhagen_all %>% 
  filter(KAP == 2) %>% 
  group_by(DIAG) %>% 
  arrange(-RR)


#### Chapter 3
Chap3 <- Copenhagen_all %>% 
  filter(KAP == 3) %>% 
  group_by(DIAG) %>% 
  arrange(-RR)


#### Chapter 4
Chap4 <- Copenhagen_all %>% 
  filter(KAP == 4) %>% 
  group_by(DIAG) %>% 
  arrange(-RR)


#### Chapter 5
Chap5 <- Copenhagen_all %>% 
  filter(KAP == 5) %>% 
  group_by(DIAG) %>% 
  arrange(-RR)


#### Chapter 6
Chap6 <- Copenhagen_all %>% 
  filter(KAP == 6) %>% 
  group_by(DIAG) %>% 
  arrange(-RR)


#### Chapter 7
Chap7 <- Copenhagen_all %>% 
  filter(KAP == 7) %>% 
  group_by(DIAG) %>% 
  arrange(-RR)


#### Chapter 8
Chap8 <- Copenhagen_all %>% 
  filter(KAP == 8) %>% 
  group_by(DIAG) %>% 
  arrange(-RR)


#### Chapter 9
Chap9 <- Copenhagen_all %>% 
  filter(KAP == 9) %>% 
  group_by(DIAG) %>% 
  arrange(-RR)


#### Chapter 10
Chap10 <- Copenhagen_all %>% 
  filter(KAP == 10) %>% 
  group_by(DIAG) %>% 
  arrange(-RR)


#### Chapter 11
Chap11 <- Copenhagen_all %>% 
  filter(KAP == 11) %>% 
  group_by(DIAG) %>% 
  arrange(-RR)


#### Chapter 12
Chap12 <- Copenhagen_all %>% 
  filter(KAP == 12) %>% 
  mutate(DIAG = substr(str_trim(DIAG), 2, 4)) %>% 
  group_by(DIAG) %>% 
  arrange(-RR)


#### Chapter 13
Chap13 <- Copenhagen_all %>% 
  filter(KAP == 13) %>% 
  group_by(DIAG) %>% 
  arrange(-RR)


#### Chapter 14
Chap14 <- Copenhagen_all %>% 
  filter(KAP == 14) %>% 
  group_by(DIAG) %>% 
  arrange(-RR)


#### Chapter 18
Chap18 <- Copenhagen_all %>% 
  filter(KAP == 18) %>% 
  group_by(DIAG) %>% 
  arrange(-RR)


#### Chapter 18
Chap19 <- Copenhagen_all %>% 
  filter(KAP == 19) %>% 
  group_by(DIAG) %>% 
  arrange(-RR)
