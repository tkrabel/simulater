##
##                            DEVSTUFF
##

use_testthat()
use_test("simulater.R")

source("R/utils.R")
source("R/simulater.R")

simulation <- simulater(2e5, 10, 15, 10, n_noise = 10, stn = 1,
                        funs = list(sin = function(x) sin(x),
                                    exp = function(x) exp(x)))
simulation$formula
