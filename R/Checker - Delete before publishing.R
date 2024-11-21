##### Checker file #####
model <- glmer(Raw_Lev2_EndDay_Misuse_OtherWay ~ ComP_Lev2_Aggreg_OCS_Ave + (ComP_Lev2_Aggreg_OCS_Ave|ID),
                 data = data_test,
                 control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)),
                 family = "binomial")

REplot(model = model13, data = data_test, predictor = "ComP_Lev2_Aggreg_OCS_Ave",
       outcome = "Raw_Lev2_EndDay_Misuse_OtherWay",
       grouping_var = "ID",
       family = "binomial",
       plot_title = "ComP_Lev2_Aggreg_OCS_Ave vs. Raw_Lev2_EndDay_Misuse_OtherWay",
       y_scale = "probability", x_num_size = 12, y_num_size = 12)

REplot(model = model13, data = data_test, predictor = "ComP_Lev2_Aggreg_OCS_Ave",
       outcome = "Raw_Lev2_EndDay_Misuse_OtherWay",
       grouping_var = "ID",
       family = "binomial",
       plot_title = "ComP_Lev2_Aggreg_OCS_Ave vs. Raw_Lev2_EndDay_Misuse_OtherWay",
       y_scale = "odds", x_num_size = 12, y_num_size = 12)

REplot(model = model13, data = data_test, predictor = "ComP_Lev2_Aggreg_OCS_Ave",
       outcome = "Raw_Lev2_EndDay_Misuse_OtherWay",
       grouping_var = "ID",
       family = "binomial",
       plot_title = "ComP_Lev2_Aggreg_OCS_Ave vs. Raw_Lev2_EndDay_Misuse_OtherWay",
       y_scale = "log odds", x_num_size = 12, y_num_size = 12)
