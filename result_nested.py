import pickle
import pandas as pd
import numpy as np
from sklearn.utils import resample
from model import load_model_predict, load_model_predict_single
from sklearn.metrics import precision_score, recall_score, roc_auc_score, fbeta_score, brier_score_loss, average_precision_score
from sklearn.preprocessing import LabelBinarizer
from collections import Counter
from utils.util import find_best_cutoff, binarize_predictions, round_data, transform_y


set_up = 'nested_death'              # nested_death, nested_longstay
feature_selection = 'all'
all_data = pickle.load(open('./data/' + set_up + '/' + feature_selection + '/fold_0/save_data.pkl', 'rb'))


def get_sub_metirc_95ci(y, y_pre_pro):
    best_cutoff, ori_f_score = find_best_cutoff(y, y_pre_pro)
    y_pre_label = binarize_predictions(y_pre_pro, best_cutoff)
    ori_f_score = ori_f_score
    ori_se = recall_score(y, y_pre_label)
    ori_ppv = precision_score(y, y_pre_label)
    ori_auc = roc_auc_score(y, y_pre_pro)
    ori_auprc = average_precision_score(y, y_pre_pro)
    result = np.array([ori_auc, ori_auprc, ori_f_score, ori_ppv, ori_se])

    result_rep = []
    n_replicates = 2000
    for i in range(n_replicates):
        indices = resample(range(len(y)), stratify=y)
        y_true_boot = y[indices]
        y_scores_boot = y_pre_pro[indices]

        best_cutoff_boot, best_fscore_boot = find_best_cutoff(y_true_boot, y_scores_boot)
        y_pre_label_boot = binarize_predictions(y_scores_boot, best_cutoff_boot)
        se_boot = recall_score(y_true_boot, y_pre_label_boot)
        ppv_boot = precision_score(y_true_boot, y_pre_label_boot)
        auc_boot = roc_auc_score(y_true_boot, y_scores_boot)
        auprc_boot = average_precision_score(y_true_boot, y_scores_boot)
        result = np.array([auc_boot, auprc_boot, best_fscore_boot, ppv_boot, se_boot])
        result_rep.append(result)

    def ci(original_metric, metric_replicates, alpha=0.95):
        lower = np.percentile(metric_replicates, (1 - alpha) / 2 * 100)
        upper = np.percentile(metric_replicates, (1 + alpha) / 2 * 100)
        f_result = f"{original_metric:.3f} ({lower:.3f}-{upper:.3f})"
        return f_result

    result_ci = []
    for k, m in enumerate([ori_auc, ori_auprc, ori_f_score, ori_ppv, ori_se]):
        metric_replicates = np.array(result_rep)[:, k]
        f_result = ci(m, metric_replicates)
        result_ci.append(f_result)
    result_ci = np.array(result_ci)

    return y_pre_label, result_ci


def metric_95ci(all_data, model_type='xgb', set='internal'):
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
    y_pre_death, death_result = get_sub_metirc_95ci(y_death, y_pre_pro)
    pred_alive = np.where(y_pre_death==0)[0]
    x_pred_alive = x[pred_alive, :]

    y = y.reset_index(drop=True)
    y_long = transform_y(y.iloc[pred_alive], set_up='nested_longstay_test')
    path = './model/'+ model_type + '_nested_longstay/' + feature_selection + '/'

    y_pre_pro_long, _ = load_model_predict_single(x_pred_alive, model_type, k_fold=5, path=path, set_up='nested_longstay')
    print('prediction metric of long stay...')
    _, long_result = get_sub_metirc_95ci(y_long, y_pre_pro_long)

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
        result = pd.concat([result, metric_95ci(all_data, model_type, set=set)], axis=0)
    all_result = pd.concat([all_result, result], axis=1)
    print()

all_result.to_excel('result/ci_95/' + set_up + '_all_4model_table.xlsx')

