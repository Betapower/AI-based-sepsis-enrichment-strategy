import pickle
import pandas as pd
import numpy as np
from model import load_model_predict, load_model_predict_single
from sklearn.metrics import precision_score, recall_score, roc_auc_score, fbeta_score, brier_score_loss
from sklearn.preprocessing import LabelBinarizer
from collections import Counter
from utils.util import find_best_cutoff, binarize_predictions, round_data, transform_y
from sklearn.metrics import confusion_matrix, average_precision_score

set_up = 'multiclass'              #  two-way or multiclass
feature_selection = 'NEE (max)'          #  NEE (max), top_15, all, boruta

xgb_path = './model/xgb_' + set_up + '/' + feature_selection + '/'
ndf_path = './model/ndf_' + set_up + '/' + feature_selection
lr_path = './model/lr_' + set_up + '/' + feature_selection + '/'
rf_path = './model/rf_' + set_up + '/' + feature_selection + '/'
all_data = pickle.load(open('./data/' + set_up + '/' + feature_selection + '/fold_0/save_data.pkl', 'rb')) # train val mixed


def confusion_report(y_true, y_pred):
    cm = confusion_matrix(y_true, y_pred)
    result = []
    for i in range(cm.shape[0]):
        TP = cm[i, i]
        FN = np.sum(cm[i, :]) - TP
        FP = np.sum(cm[:, i]) - TP
        TN = np.sum(cm) - TP - FN - FP
        result.append([TP, FP, TN, FN])
    result = np.array(result).T
    return result

def metric(all_data, path, model_type='xgb', set=False, set_up='multiclass'):
    print('result of '+ model_type + '...')
    if model_type=='xgb':
        if set == 'train':
            x = np.concatenate((all_data['x_train'], all_data['x_val']), axis=0)
        elif set == 'internal':
            x = all_data['x_test']
        else:
            x = all_data['x_eicu']
    else:
        if set == 'train':
            x = np.concatenate((all_data['x_train_process'], all_data['x_val_process']), axis=0)
        elif set == 'internal':
            x = all_data['x_test_process']
        else:
            x = all_data['x_eicu_process']
    y = np.append(all_data['y_train'], all_data['y_val']) if set == 'train' \
        else (all_data['y_test'] if set == 'internal' else all_data['y_eicu'])

    y_pre_pro, y_pre_label = load_model_predict_single(x, model_type, k_fold=5, path=path, set_up=set_up)

    if set_up == 'multiclass':
        se = round_data(recall_score(y, y_pre_label, average=None))
        ppv = round_data(precision_score(y, y_pre_label, average=None))
        f_score = round_data(fbeta_score(y, y_pre_label, beta=0.5, average=None))
        confusion = confusion_report(y, y_pre_label)
        lb = LabelBinarizer().fit([0, 1, 2])
        auc = round_data(roc_auc_score(lb.transform(y), y_pre_pro, average=None, multi_class='ovr'))
        auprc = round_data(average_precision_score(lb.transform(y), y_pre_pro, average=None))
        bsl = round_data(np.array([brier_score_loss(lb.transform(y)[:, i], y_pre_pro[:, i]) for i in range(3)]))
        result = np.vstack((auc, auprc, f_score, ppv, se, bsl))
        result = np.concatenate((result, confusion), axis=0)
        result = pd.DataFrame(result, columns=['Rapid death', 'Persistent ill', 'Recovery'],
                              index=['AUC', 'AUPRC','F-0.5 score', 'PPV', 'TPR', 'Brier score', 'TP', 'FP', 'TN', 'FN'])
    else:
        best_cutoff, best_fscore = find_best_cutoff(y, y_pre_pro)
        y_pre_label = binarize_predictions(y_pre_pro, best_cutoff)
        best_fscore = round(best_fscore, 3)
        se = round(recall_score(y, y_pre_label), 3)
        ppv = round(precision_score(y, y_pre_label), 3)
        auc = round(roc_auc_score(y, y_pre_pro), 3)
        auprc = round(average_precision_score(y, y_pre_pro), 3)
        result = np.array([auc, auprc, best_fscore, ppv, se])
        # result = 'AUC: {:.3f}, F-0.5 score: {:.3f}, PPV: {:.3f}, TPR: {:.3f}'.format(auc, best_fscore, ppv, se)
    # print(result)
    return result


all_result = pd.DataFrame()
for set in (['internal', 'external']):
    print('performance of ' + set + ' ...')
    result = pd.DataFrame()
    # for model_type, path, model_name in zip(['xgb', 'ndf', 'rf', 'lr'], [xgb_path, ndf_path, rf_path, lr_path],
    #                                         ['Gradient boosting', 'Neural decision forest', 'Random forest', 'Logistic regression']):
    for model_type, path, model_name in zip(['xgb', 'rf', 'lr'], [xgb_path, rf_path, lr_path],
                                            ['Gradient boosting', 'Random forest', 'Logistic regression']):
        if set_up == 'multiclass':
            result = pd.concat([result, pd.DataFrame({'Rapid death': [model_name], 'Persistent ill': [model_name], 'Recovery': [model_name]})], axis=0)
            result = pd.concat([result, metric(all_data, path, model_type, set=set, set_up=set_up)], axis=0)
        else:
            result = pd.concat([result, pd.DataFrame(metric(all_data, path, model_type, set=set, set_up=set_up),
                                  index=['AUC', 'AUPRC', 'F-0.5 score', 'PPV', 'TPR'], columns=[model_name])], axis=1)
    all_result = pd.concat([all_result, result], axis=1)
    print()

# if set_up == 'multiclass':
all_result.to_excel('result/' + set_up + '_' + feature_selection + '_table.xlsx')


