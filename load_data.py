import numpy as np, os
import pandas as pd
import xgboost as xgb
from collections import Counter
from sklearn.model_selection import StratifiedShuffleSplit, StratifiedKFold
from sklearn.utils.class_weight import  compute_sample_weight
from sklearn.linear_model import LogisticRegression
from utils.util import transform_y, impute_scale, load_database
import pickle
import joblib
from boruta_selection import boruta_feature_selection, transform
from model import BO_TPE_XGB, BO_TPE_RF, BO_TPE_LR
from sklearn.ensemble import RandomForestClassifier

set_up = 'multiclass'
feature_selection = 'all'

x, y = load_database('mimic')
eicu_x, eicu_y = load_database('eicu')

# for set_up in ['multiclass', 'two-way', 'nested_death', 'nested_longstay']:
if feature_selection == 'top_15':
    feature_order = all_data = pickle.load(open('./data/' + set_up + '/feature_order.pkl', 'rb'))
    x = x.loc[:, feature_order[:15]]
    eicu_x = eicu_x.loc[:, feature_order[:15]]

nee_max_index = x.columns.get_loc('NEE (max)')

########################################################################################################################
# split in train and test set, test set should be stable
sss = StratifiedShuffleSplit(n_splits=1, test_size=0.2, random_state=2048)
for train_index, test_index in sss.split(x, y):
    train_x = x.iloc[train_index, :]
    train_y = y.iloc[train_index]
    print('train set: ')
    print(Counter(train_y))

    test_x = x.iloc[test_index, :]
    test_y = y.iloc[test_index]
    print('internal test set: ')
    print(Counter(test_y))

if set_up == 'nested_longstay':
    train_x.drop(train_y.index[train_y == 'Raipd_death'], inplace=True)
    train_y.drop(train_y.index[train_y == 'Raipd_death'], inplace=True)
    test_x.drop(test_y.index[test_y == 'Raipd_death'], inplace=True)
    test_y.drop(test_y.index[test_y == 'Raipd_death'], inplace=True)

    eicu_x.drop(eicu_y.index[eicu_y == 'Raipd_death'], inplace=True)
    eicu_y.drop(eicu_y.index[eicu_y == 'Raipd_death'], inplace=True)

x_test = np.array(test_x)
y_test = transform_y(test_y, set_up) if set_up != 'nested_death' else test_y
x_eicu = np.array(eicu_x)
y_eicu = transform_y(eicu_y, set_up) if set_up != 'nested_death' else eicu_y

train_x_process, test_x_process, eicu_x_process = impute_scale(train_x, test_x, eicu_x)
boruta_feat_selector = boruta_feature_selection(train_x_process, train_y)

test_x_process_copy = test_x_process
eicu_x_process_copy = eicu_x_process

########################################################################################################################
# train the model
xgb_path = './model/xgb_' + set_up + '/' + feature_selection + '/'
lr_path = './model/lr_' + set_up + '/' + feature_selection + '/'
rf_path = './model/rf_' + set_up + '/' + feature_selection + '/'
for dir in [xgb_path, lr_path, rf_path]:
    if not os.path.isdir(dir):
        os.makedirs(dir)

# k=0  # no kfold
# sss = StratifiedShuffleSplit(n_splits=1, test_size=0.2, random_state=2048)
# for train_index, val_index in sss.split(train_x, train_y):
skf = StratifiedKFold(n_splits=5, shuffle=True, random_state=2048)
for k, (train_index, val_index) in enumerate(skf.split(train_x, train_y)):
    print('{}th training ..............'.format(k + 1))
    x_train = np.array(train_x.iloc[train_index, :])
    y_train = train_y.iloc[train_index]
    y_train = transform_y(y_train, set_up)

    x_val = np.array(train_x.iloc[val_index, :])
    y_val = train_y.iloc[val_index]
    y_val = transform_y(y_val, set_up)

    # compared model data used
    x_train_process = train_x_process[train_index]
    x_val_process = train_x_process[val_index]

    # train model
    sample_weights = compute_sample_weight(class_weight='balanced', y=y_train)
    # sample_weights = None

    if feature_selection == 'NEE (max)':
        x_train = x_train[:, nee_max_index].reshape(-1, 1)
        x_val = x_val[:, nee_max_index].reshape(-1, 1)
        x_train_process = x_train_process[:, nee_max_index].reshape(-1, 1)
        x_val_process = x_val_process[:, nee_max_index].reshape(-1, 1)
    elif feature_selection == 'boruta':
        x_train, x_val, x_train_process, x_val_process, x_test, test_x_process, x_eicu, eicu_x_process = \
            transform(boruta_feat_selector, x_train, x_val, x_train_process, x_val_process,
              np.array(test_x), test_x_process_copy, np.array(eicu_x), eicu_x_process_copy)

    print('XGBoost...')
    best_param = BO_TPE_XGB(x_train, y_train, x_val, y_val, sample_weights, set_up)
    xgb_model = xgb.XGBClassifier(max_depth=best_param['max_depth'],
                                  eta=best_param['learning_rate'],
                                  n_estimators=1000,
                                  subsample=best_param['subsample'],
                                  colsample_bytree=best_param['colsample_bytree'],
                                  reg_alpha=best_param['reg_alpha'],
                                  reg_lambda=best_param['reg_lambda'],
                                  objective="multi:softmax" if set_up == 'multiclass' else "binary:logistic"
                                  )

    es = xgb.callback.EarlyStopping(
        rounds=50,
        # abs_tol=1e-4,
        save_best=True,
        maximize=False if set_up == 'multiclass' else True,
        # data_name="validation",
        metric_name="merror" if set_up == 'multiclass' else 'auc',
    )

    xgb_model = xgb_model.fit(x_train, y_train, eval_set=[(x_val, y_val)], verbose=False,
                              eval_metric='merror' if set_up == 'multiclass' else 'auc',
                              callbacks=[es], sample_weight=sample_weights)
    save_model_path = xgb_path + 'model{}.json'.format(k + 1)
    xgb_model.save_model(save_model_path)

    # RandomForest
    print('RandomForest...')
    # best_rf_parm = BO_TPE_RF(x_train_process, y_train, x_val_process, y_val)
    # print(best_rf_parm)
    rf_model = RandomForestClassifier(
                                   n_estimators=500,
                                   class_weight='balanced'
                                   )
    rf_model.fit(x_train_process, y_train)
    joblib.dump(rf_model, rf_path +'model{}.joblib'.format(k+1))

    # lr model
    print('LogisticRegression...')
    best_lr_parm = BO_TPE_LR(x_train_process, y_train, x_val_process, y_val, set_up)
    log_model = LogisticRegression(solver=best_lr_parm['solver'], C=best_lr_parm['C'], class_weight='balanced')
    log_model.fit(x_train_process, y_train)
    joblib.dump(log_model, lr_path +'model{}.joblib'.format(k+1))
    print('*************************************************************\n')

    save_data = {
        'x_train': x_train,
        'x_train_process': x_train_process,
        'x_val': x_val,
        'x_val_process': x_val_process,
        'y_val': y_val,
        'x_test': x_test if feature_selection != 'NEE (max)' else x_test[:, nee_max_index].reshape(-1, 1),
        'x_test_process': test_x_process if feature_selection != 'NEE (max)' else test_x_process[:, nee_max_index].reshape(-1, 1),
        'x_eicu': x_eicu if feature_selection != 'NEE (max)' else x_eicu[:, nee_max_index].reshape(-1, 1),
        'x_eicu_process': eicu_x_process if feature_selection != 'NEE (max)' else eicu_x_process[:, nee_max_index].reshape(-1, 1),
        'y_train': y_train,
        'y_test': y_test,
        'y_eicu': y_eicu,
        'test_index': test_index
    }

    save_path = './data/' + set_up + '/' + feature_selection + '/fold_' + str(k) + '/'
    if not os.path.isdir(save_path):
        os.makedirs(save_path)
    pickle.dump(save_data, open((save_path+'save_data.pkl'), 'wb'))


