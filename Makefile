PYTHON = python
RSCRIPT = Rscript

CV_LIST = 0.5 1.0 1.5

DIR_DATA = __data__
DIR_PLOTS = plots

FILE_TABLE = __table__20__400__.npy
FILE_LIST_VEC = $(foreach cv,$(CV_LIST),$(subst %,$(cv),$(DIR_DATA)/__results__2_data_value_vec__%__.csv.gz))

all: $(DIR_PLOTS)/1_plot_cv_distribution.tex $(DIR_PLOTS)/2_plot_value_distribution.tex $(DIR_PLOTS)/3_plot_munu_constraint.tex

$(DIR_DATA)/$(FILE_TABLE): munu_var.py | $(DIR_DATA)
	$(PYTHON) $< 20 400 0 400 $(basename $@) --verbose

$(DIR_DATA)/__results__1_data_calc_table__.csv.gz: 1_data_calc_table.py $(DIR_DATA)/$(FILE_TABLE) | $(DIR_DATA)
	$(PYTHON) $< 10 $(DIR_DATA)/$(FILE_TABLE) $@ --verbose

$(DIR_DATA)/__results__2_data_value_vec__%__.csv.gz: 2_data_value_vec.py $(DIR_DATA)/$(FILE_TABLE) | $(DIR_DATA)
	$(PYTHON) $< 10 100 $* 0 1 100 $(DIR_DATA)/$(FILE_TABLE) $@ --count 100000 --seed 1 --verbose

$(DIR_DATA)/__results__3_data_munu_constraint__.csv.gz: 3_data_munu_constraint.R $(DIR_DATA)/$(FILE_TABLE) | $(DIR_DATA)
	$(RSCRIPT) $^

$(DIR_DATA)/__results__4_data_cv_constraint__.csv.gz: 4_data_cv_constraint.R $(DIR_DATA)/$(FILE_TABLE) | $(DIR_DATA)
	$(RSCRIPT) $^

$(DIR_DATA)/__results__5_data_heuristics__.csv.gz: 5_data_heuristics.R $(DIR_DATA)/$(FILE_TABLE) | $(DIR_DATA)
	$(RSCRIPT) $^

$(DIR_DATA)/__results__6_data_heuristics_bounded__.csv.gz: 6_data_heuristics_bounded.R $(DIR_DATA)/$(FILE_TABLE) | $(DIR_DATA)
	$(RSCRIPT) $^

$(DIR_PLOTS)/1_plot_cv_distribution.tex: 1_plot_cv_distribution.R $(DIR_DATA)/__results__1_data_calc_table__.csv.gz | $(DIR_PLOTS)
	$(RSCRIPT) $^

$(DIR_PLOTS)/2_plot_value_distribution.tex: 2_plot_value_distribution.R $(FILE_LIST_VEC) | $(DIR_PLOTS)
	$(RSCRIPT) $^

$(DIR_PLOTS)/3_plot_munu_constraint.tex: 3_plot_munu_constraint.R $(DIR_DATA)/__results__3_data_munu_constraint__.csv.gz | $(DIR_PLOTS)
	$(RSCRIPT) $^

$(DIR_PLOTS)/4_data_cv_constraint.tex: 4_data_cv_constraint.R $(DIR_DATA)/__results__4_data_cv_constraint__.csv.gz | $(DIR_PLOTS)
	$(RSCRIPT) $^

$(DIR_PLOTS)/5_data_heuristics.tex: 5_data_heuristics.R $(DIR_DATA)/__results__5_data_heuristics__.csv.gz | $(DIR_PLOTS)
	$(RSCRIPT) $^

$(DIR_PLOTS)/6_data_heuristics_bounded.tex: 6_data_heuristics_bounded.R $(DIR_DATA)/__results__6_data_heuristics_bounded__.csv.gz | $(DIR_PLOTS)
	$(RSCRIPT) $^

$(DIR_DATA):
	mkdir -p $@

$(DIR_PLOTS):
	mkdir -p $@

