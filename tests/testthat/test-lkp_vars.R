# #Test to check if function is returning correct data for a variable in population spec var
# test_that("lkp_var returns correct data for variable in population spec var", {
#   var_info <- lkp_var("origin")
#   expect_equal(var_info$Variable$variable, "ORIGIN")
#   expect_equal(var_info$Variable$label, "Sample origin")
#   expect_equal(var_info$Value$value[1], "1")
#   expect_equal(var_info$Value$label[1], "HEIMS")
# })
#
# #Test to check if function is returning correct data for a variable in operational spec var
# test_that("lkp_var returns correct data for variable in operational spec var", {
#   var_info <- lkp_var("QUALNUMS")
#   expect_equal(var_info$Variable$variable, "QUALNUMS")
#   expect_equal(var_info$Variable$label, "System variable: Number of qualifications")
#   expect_equal(var_info$Variable$format, "Numeric")
# })
# #
# #Test to check if function is returning correct data for a variable in master output spec var
# test_that("lkp_var returns correct data for variable in master output spec var", {
#   var_info <- lkp_var("GEOFACTOR_1")
#   expect_equal(var_info$Variable$variable, "GEOFACTOR_1")
#   expect_equal(var_info$Variable$`Variable Definition`, "Spouse/partner's employment or career")
#   expect_equal(var_info$Variable$format, "Numeric")
#   expect_equal(var_info$Value$value[1], "1")
#   expect_equal(var_info$Value$label[1], "Yes")
# })
# #
# #Test to check if function is returning correct data for a variable not found in any excel file
# test_that("lkp_var returns correct data for variable not found in any excel file", {
#   var_info <- lkp_var("Not_A_VARIABLE")
#   expect_equal(var_info$Variable$variable, "NA")
#   expect_equal(var_info$Variable$label, "NA")
#   expect_equal(var_info$Value$value, "NA")
#   expect_equal(var_info$Value$label, "NA")
# })
