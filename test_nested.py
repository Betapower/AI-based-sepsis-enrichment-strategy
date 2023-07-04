import pickle
import pandas as pd
import numpy as np
from model import load_model_predict, load_model_predict_single
from sklearn.metrics import precision_score, recall_score, roc_auc_score, fbeta_score, brier_score_loss, average_precision_score
from sklearn.preprocessing import LabelBinarizer
from collections import Counter
from utils.util import find_best_cutoff, binarize_predictions, round_data, transform_y


set_up = 'nested_death'              # nested_death, nested_longstay
feature_selection = 'all'
all_data = pickle.load(open('./data/' + set_up + '/' + feature_selection + '/fold_0/save_data.pkl', 'rb'))


def get_sub_metirc(y_pre_pro, y_true_label):
    best_cutoff, best_fscore = find_best_cutoff(y_true_label, y_pre_pro)
    y_pre_label = binarize_predictions(y_pre_pro, best_cutoff)
    best_fscore = round(best_fscore, 3)
    se = round(recall_score(y_true_label, y_pre_label), 3)
    ppv = round(precision_score(y_true_label, y_pre_label), 3)
    auc = round(roc_auc_score(y_true_label, y_pre_pro), 3)
    auprc = round(average_precision_score(y_true_label, y_pre_pro), 3)
    result = np.array([auc, auprc, best_fscore, ppv, se])
    # result = 'AUC: {:.3f}, F-0.5 score: {:.3f}, PPV: {:.3f}, TPR: {:.3f}'.format(auc, best_fscore, ppv, se)
    # print(result)
    return y_pre_label, result


def metric(all_data, model_type='xgb', set='internal'):
    print('result of '+ model_type + '...')
    if model_type=='xgb':
        if set == 'internal':
            x = all_data['x_test']
        else:
            x = all_data['x_eicu']
    else:
        if set == 'internal':
            x = all_data['x_test_process']
        else:
            x = all_data['x_eicu_process']
    if set == 'internal':
        y = all_data['y_test']
    else:
        y = all_data['y_eicu']

    y_death = transform_y(y, set_up='nested_death')
    path = './model/' + model_type + '_nested_death/' + feature_selection + '/'
    y_pre_pro, _ = load_model_predict_single(x, model_type, k_fold=5, path=path, set_up='nested_death')

    print('prediction metric of death...')
    y_pre_death, death_result = get_sub_metirc(y_pre_pro, y_death)
    pred_alive = np.where(y_pre_death==0)[0]
    x_pred_alive = x[pred_alive, :]

    y = y.reset_index(drop=True)
    y_long = transform_y(y.iloc[pred_alive], set_up='nested_longstay_test')
    path = './model/'+ model_type + '_nested_longstay/' + feature_selection + '/'

    y_pre_pro_long, _ = load_model_predict_single(x_pred_alive, model_type, k_fold=5, path=path, set_up='nested_longstay')
    print('prediction metric of long stay...')
    _, long_result = get_sub_metirc(y_pre_pro_long, y_long)

    result = np.column_stack((death_result, long_result))
    result = pd.DataFrame(result, columns=['Rapid death', 'Persistent ill'],
                          index=['AUC', 'AUPRC', 'F-0.5 score', 'PPV', 'TPR'])
    return result

all_result = pd.DataFrame()
for set in (['internal', 'external']):
    result = pd.DataFrame()
    print('performance of ' + set + ' ...')
    for model_type, model_name in zip(['xgb', 'ndf', 'rf', 'lr'],
                            ['Gradient boosting', 'Neural decision forest', 'Random forest', 'Logistic regression']):
        result = pd.concat([result, pd.DataFrame({'Rapid death': [model_name], 'Persistent ill': [model_name]})], axis=0)
        result = pd.concat([result, metric(all_data, model_type, set=set)], axis=0)
    all_result = pd.concat([all_result, result], axis=1)
    print()

all_result.to_excel('result/' + set_up + '_' + feature_selection + '_table.xlsx')




