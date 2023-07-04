import pandas as pd
import numpy as np
import xgboost as xgb
from sklearn.impute import KNNImputer
from sklearn.preprocessing import OneHotEncoder, LabelEncoder, StandardScaler, LabelBinarizer
from sklearn.metrics import accuracy_score, roc_auc_score, roc_curve, auc, ConfusionMatrixDisplay, fbeta_score
from pickle import dump
from utils.return_feature_name import return_var_name_2, return_var_name_final
from collections import Counter


def encode_data(data):
    g = np.array(data['gender'])
    ad = np.array(data['admissiontype'])
    eth = np.array(data['ethni'])
    site = np.array(data['infectionsite'])
    for i in range(len(data)):
        g[i] = 1 if g[i] == 'M' or g[i] == 'Male' else 0
        ad[i] = 1 if ad[i] == 'emergency' else 0
        eth[i] = 1 if eth[i] == 'WHITE' or eth[i] == 'white' else 0
        site[i] = 1 if site[i] == 'pulmonary' else 0

    data['gender'].loc[:] = g
    data['admissiontype'].loc[:] = ad
    data['ethni'].loc[:] = eth
    data['infectionsite'].loc[:] = site
    return data


def round_data(data):
    return np.array([round(x, 3) for x in data])


def median_value(data):
    # data = data.drop(columns=['stay_id'])
    median = np.nanmedian(data, axis=0)
    median_series = pd.Series(data=median, index=data.columns.values)
    return median_series


def Standardise(data):
    scaler = StandardScaler()
    scaler.fit(data)
    return scaler


def save_test_data(train_x, train_y, x_test, y_test, save_path='./data/'):
    np.save(save_path + "train_x.npy", train_x)
    np.save(save_path + "train_y.npy", train_y)

    np.save(save_path + "test_x.npy", x_test)
    np.save(save_path + "test_y.npy", y_test)


########################################################################################################################
# impute and standarise the data
def impute_knn(x, mimic):
    imputer_ts = KNNImputer(n_neighbors=10)
    x = imputer_ts.fit_transform(x)
    x = pd.DataFrame(data=x, columns=mimic.columns.values[1:-1])
    dump(imputer_ts, open(('./data/imputer_ts.pkl'), 'wb'))
    return x


def impute_mean(train_x, test_x, external_x):
    mean = np.nanmean(train_x, axis=0)
    train_mean = pd.Series(data=mean, index=train_x.columns.values)
    train_x_fillna = train_x.fillna(train_mean, inplace=False)
    test_x_fillna = test_x.fillna(train_mean, inplace=False)
    external_x_fillna = external_x.fillna(train_mean, inplace=False)

    dump(train_mean, open(('./data/train_mean.pkl'), 'wb'))
    return train_x_fillna, test_x_fillna, external_x_fillna


def scale_data(train_x_fillna, test_x_fillna, external_x_fillna):
    scaler = Standardise(train_x_fillna)
    train_x_process = scaler.transform(train_x_fillna)
    test_x_process = scaler.transform(test_x_fillna)
    external_x_process = scaler.transform(external_x_fillna)
    return train_x_process, test_x_process, external_x_process


def impute_scale(train_x, test_x, external_x):
    train_x_fillna, test_x_fillna, external_x_fillna = impute_mean(train_x, test_x, external_x)
    train_x_process, test_x_process, external_x_process = scale_data(train_x_fillna, test_x_fillna,external_x_fillna)
    return train_x_process, test_x_process, external_x_process


def process_columns(row, A, B):
    if pd.isna(row[A]):
        return row[B]
    else:
        if pd.isna(row[B]):
            return row[A]
        else:
            return (row[A] + row[B]) / 2


def load_database(database_name, root_path='./'):
    raw_data = pd.read_csv(root_path+'data/finaldata_' + database_name + '.csv')
    variables_name = pd.read_csv(root_path+'result/variables_list.csv')
    print(Counter(raw_data['Group']))
    if database_name == 'eicu':
        raw_data = raw_data.drop(['urine_total', 'input_total'], axis=1)
        input_output = pd.read_csv('./data/sepsis_inputoutput.csv')
        raw_data = pd.merge(raw_data, input_output, on='patientunitstayid', how='inner')
        raw_data.loc[:, ['pf_max', 'pf_min', 'pf_mean']] = raw_data.loc[:, ['pf_max', 'pf_min', 'pf_mean']].apply(lambda x: x * 100)

        data_0 = raw_data.rename(
            columns={'patientunitstayid': 'stay_id', 'rr_mean': 'resprate_mean', 'rr_sd': 'resprate_sd',
                     'rr_max': 'resprate_max', 'rr_min': 'resprate_min', 'aptt_min': 'ptt_min',
                     'aptt_max': 'ptt_max', 'aptt_mean': 'ptt_mean', 'aptt_sd': 'ptt_sd', 'aptt_vr': 'ptt_vr',
                     })
        data = data_0[return_var_name_2()]
        for A, B in zip(['isbp_max', 'isbp_min', 'isbp_mean', 'isbp_vr',
                         'idbp_max', 'idbp_min', 'idbp_mean', 'idbp_vr',
                         'imbp_max', 'imbp_min', 'imbp_mean', 'imbp_vr'],
                        ['nisbp_max', 'nisbp_min', 'nisbp_mean', 'nisbp_vr',
                         'nidbp_max', 'nidbp_min', 'nidbp_mean', 'nidbp_vr',
                         'nimbp_max', 'nimbp_min', 'nimbp_mean', 'nimbp_vr']):
            def process_columns(row):
                if pd.isna(row[A]):
                    return row[B]
                else:
                    if pd.isna(row[B]):
                        return row[A]
                    else:
                        return (row[A] + row[B]) / 2
            new_col = data.apply((process_columns), axis=1)
            data[A] = new_col
    else:
        data = raw_data[return_var_name_2()]  # variables
    data = data[return_var_name_final()]
    data = data.drop(['nisbp_max', 'nisbp_min', 'nisbp_mean', 'nisbp_vr',
               'nidbp_max', 'nidbp_min', 'nidbp_mean', 'nidbp_vr',
               'nimbp_max', 'nimbp_min', 'nimbp_mean', 'nimbp_vr'], axis=1)
    data = data.rename(columns={'isbp_max': 'sbp_max', 'isbp_min': 'sbp_min', 'isbp_mean': 'sbp_mean', 'isbp_vr': 'sbp_vr',
                                'idbp_max': 'dbp_max', 'idbp_min': 'dbp_min', 'idbp_mean': 'dbp_mean', 'idbp_vr': 'dbp_vr',
                                'imbp_max': 'mbp_max', 'imbp_min': 'mbp_min', 'imbp_mean': 'mbp_mean', 'imbp_vr': 'mbp_vr',
                                })
    database = encode_data(data)  # one hot map
    imputer = KNNImputer(n_neighbors=10)  # impute height and weight
    d0 = imputer.fit_transform(database[['admission_age', 'height', 'weight_admit', 'gender', 'ethni']])
    database['height'] = d0[:, 1]
    database['weight_admit'] = d0[:, 2]
    database['bmi'] = database['weight_admit'] / (database['height'] ** 2 / 10000)
    database = database.drop(['height', 'weight_admit'], axis=1)

    x = database.iloc[:, 1:-1]  # stay_id:Group
    x = x.replace(np.inf, np.nan)
    x.columns = variables_name.iloc[:, 1]
    # missing_ratio = x.isna().mean()
    # remove nan data that more than 25%
    # remove_nan_id = np.where(x.isna().sum(axis = 1) / x.shape[1] >= 0.25)[0]
    # x.drop(remove_nan_id, axis=0, inplace = True)
    y = database['Group']

    return x, y


def transform_y(y, set_up='multiclass'):
    if set_up == 'multiclass':
        le = LabelEncoder()
        le.classes_ = ['Raipd_death', 'persistent_ill','recovery']
        return le.transform(y)
    elif set_up == 'two-way':
        bin_y = y.replace(['recovery', 'Raipd_death'], 'short_stay')
        le = LabelEncoder()
        le.classes_ = ['short_stay', 'persistent_ill']
        return le.transform(bin_y)
    elif set_up == 'nested_death':
        bin_y = y.replace(['recovery', 'persistent_ill'], 'alive')
        le = LabelEncoder()
        le.classes_ = ['alive', 'Raipd_death']
        return le.transform(bin_y)
    elif set_up == 'nested_longstay':
        le = LabelEncoder()
        le.classes_ = ['recovery', 'persistent_ill']
        return le.transform(y)
    elif set_up == 'nested_longstay_test':
        bin_y = y.replace(['Raipd_death'], 'recovery')
        le = LabelEncoder()
        le.classes_ = ['recovery', 'persistent_ill']
        return le.transform(bin_y)


def binarize_predictions(y_pred_proba, cutoff):
    return (y_pred_proba >= cutoff).astype(int)


def find_best_cutoff(y_true, y_pred_proba):
    best_cutoff, best_fscore = None, -1
    for cutoff in np.arange(0, 1.01, 0.01):
        y_pred = binarize_predictions(y_pred_proba, cutoff)
        fscore = fbeta_score(y_true, y_pred, beta=0.5)
        if fscore > best_fscore:
            best_cutoff, best_fscore = cutoff, fscore
    return best_cutoff, best_fscore

