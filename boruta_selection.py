import pandas as pd
import numpy as np
from sklearn.ensemble import RandomForestClassifier
from boruta import BorutaPy
from utils.util import impute_scale, load_database, transform_y
import warnings
warnings.filterwarnings('ignore')

def boruta_feature_selection(x, y):
    # define random forest classifier, with utilising all cores and
    # sampling in proportion to y labels
    rf = RandomForestClassifier(n_jobs=-1, class_weight='balanced')

    # define Boruta feature selection method
    feat_selector = BorutaPy(rf, n_estimators='auto', verbose=0, random_state=1)
    feat_selector.fit(x, y)
    # call transform() on X to filter it down to selected features
    # x_filtered = feat_selector.transform(x)

    return feat_selector

def transform(boruta_feat_selector, x_train, x_val, x_train_process, x_val_process,
              x_test, x_test_process, x_eicu, x_eicu_process):
    x_train, x_val = boruta_feat_selector.transform(x_train), boruta_feat_selector.transform(x_val)
    x_train_process, x_val_process = boruta_feat_selector.transform(x_train_process), boruta_feat_selector.transform(
        x_val_process)
    x_test, x_test_process = boruta_feat_selector.transform(x_test), boruta_feat_selector.transform(
        x_test_process)
    x_eicu, x_eicu_process = boruta_feat_selector.transform(x_eicu), boruta_feat_selector.transform(
        x_eicu_process)

    return x_train, x_val, x_train_process, x_val_process, x_test, x_test_process, x_eicu, x_eicu_process