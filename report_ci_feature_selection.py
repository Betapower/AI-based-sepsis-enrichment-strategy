import numpy as np
import pandas as pd
from model import load_model_predict, load_model_predict_single
from sklearn.metrics import precision_score, recall_score, roc_auc_score, fbeta_score, \
    brier_score_loss,average_precision_score
from sklearn.utils import resample
from sklearn.preprocessing import LabelBinarizer
import pickle
import warnings
warnings.filterwarnings("ignore")


def metric_95ci(all_data, path, model_type='xgb', set='internal', set_up='multiclass'):
    if set == 'internal':
        x = all_data['x_test']
    else:
        x = all_data['x_eicu']

    y = all_data['y_test'] if set == 'internal' else all_data['y_eicu']

    y_pre_pro, y_pre_label = load_model_predict_single(x, model_type, k_fold=5, path=path, set_up=set_up)

    n_replicates = 2000
    lb = LabelBinarizer().fit([0, 1, 2])
    ori_se = recall_score(y, y_pre_label, average=None)
    ori_ppv = precision_score(y, y_pre_label, average=None)
    ori_f_score = fbeta_score(y, y_pre_label, beta=0.5, average=None)
    ori_auc = roc_auc_score(lb.transform(y), y_pre_pro, average=None, multi_class='ovr')
    ori_auprc = average_precision_score(lb.transform(y), y_pre_pro, average=None)
    ori_bsl = np.array([brier_score_loss(lb.transform(y)[:, i], y_pre_pro[:, i]) for i in range(3)])

    # Perform stratified bootstrap replicates
    result_rep = []
    for i in range(n_replicates):
        # Generate a bootstrap sample by resampling with replacement
        indices = resample(range(len(y)), stratify=y)
        y_true_boot = y[indices]
        y_scores_boot = y_pre_pro[indices]
        y_pre_label_boot = y_pre_label[indices]

        # Calculate the AUC for the bootstrap sample
        se_boot = recall_score(y_true_boot, y_pre_label_boot, average=None)
        ppv_boot = precision_score(y_true_boot, y_pre_label_boot, average=None)
        f_score_boot = fbeta_score(y_true_boot, y_pre_label_boot, beta=0.5, average=None)
        auc_boot = roc_auc_score(lb.transform(y_true_boot), y_scores_boot, average=None, multi_class='ovr')
        auprc_boot = average_precision_score(lb.transform(y_true_boot), y_scores_boot, average=None)
        bsl_boot = np.array([brier_score_loss(lb.transform(y_true_boot)[:, i], y_scores_boot[:, i]) for i in range(3)])
        result = np.vstack((auc_boot, auprc_boot, f_score_boot, ppv_boot, se_boot, bsl_boot))
        result_rep.append(result)

    # Calculate the confidence interval
    def ci(original_metric, metric_replicates, alpha=0.95):
        metric_replicates = np.array(metric_replicates)
        f_result = []
        for j in range(3):
            lower = np.percentile(metric_replicates[:, j], (1 - alpha) / 2 * 100)
            upper = np.percentile(metric_replicates[:, j], (1 + alpha) / 2 * 100)
            f_result.append(f"{original_metric[j]:.3f} ({lower:.3f}-{upper:.3f})")
        return np.array(f_result)

    result_ci = []
    for k, m in enumerate([ori_auc, ori_auprc, ori_f_score, ori_ppv, ori_se, ori_bsl]):
        metric_replicates = [l[k, :] for l in result_rep]
        f_result = ci(m, metric_replicates)
        result_ci.append(f_result)

    result_ci = pd.DataFrame(np.array(result_ci), columns=['Rapid death', 'Persistent ill', 'Recovery'],
                              index=['AUC', 'AUPRC', 'F-0.5 score', 'PPV', 'TPR', 'Brier score'])

    return result_ci


set_up = 'multiclass'              #  two-way or multiclass
all_result = pd.DataFrame()

for set in (['internal', 'external']):
    print('performance of ' + set + ' ...')
    result = pd.DataFrame()
    for feature_selection in ['all', 'boruta', 'top_15', 'NEE (max)']:          #  NEE (max), top_15, all, boruta
        print('performance of ' + feature_selection + ' ...')
        xgb_path = './model/xgb_' + set_up + '/' + feature_selection + '/'
        all_data = pickle.load(open('./data/' + set_up + '/' + feature_selection + '/fold_' + str(3) + '/save_data.pkl', 'rb'))  # train val mixed

        result = pd.concat([result, pd.DataFrame({'Rapid death': [feature_selection], 'Persistent ill': [feature_selection], 'Recovery': [feature_selection]})], axis=0)
        result = pd.concat([result, metric_95ci(all_data, xgb_path, 'xgb', set=set, set_up=set_up)], axis=0)

    all_result = pd.concat([all_result, result], axis=1)
    print()

all_result.to_excel('result/ci_95/' + set_up + '_feature_selection_table.xlsx')





# aa = pd.DataFrame(data=np.concatenate((y_true.reshape(-1, 1), y_scores.reshape(-1, 1)), axis=1))
# aa.to_csv('./result/ci_test.csv')