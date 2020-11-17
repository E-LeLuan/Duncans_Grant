library(reticulate)

setwd("FixAlign")

source_python("Robodoc.py")

# type parameters.txt when prompted

source_python("make_cnt.py")

# the name of the delimited file is either 'error_ROIs2.del' or 'corr_ROIs2.del'
# region delimiter is ^
# lowest condition number is 1
# highest condition number is 2

read_csv()

source_python("question_acc.py")

# type parameters.txt when prompted
