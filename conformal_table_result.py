import pickle
import pandas as pd
import numpy as np
from plot.conformal_prediction_plot import df_predict_region, tab_predict_region


def mean_pvalue(external=False):
    p = np.zeros((len(all_data['y_test' if not external else 'y_eicu']), 3))
    for k in range(5):
        pvalue = np.array(pd.read_csv('../R/Output/Tables/pValues_isup_' + str(k) + '.csv')) if not external else\
                    np.array(pd.read_csv('../R/External_output/Tables/pValues_isup_' + str(k) + '.csv'))
        p += pvalue
    return p / 5

feature_selection = 'all' #  nee_max, top_15, all, boruta
all_data = pickle.load(open('./data/multiclass/' + feature_selection + '/fold_0/save_data.pkl', 'rb'))

# internal_pvalue = mean_pvalue(external=False)
# external_pvalue = mean_pvalue(external=True)
#
internal_pvalue = pd.read_csv('../R/Output/Tables/pValues_isup_' + str(3) + '.csv')
external_pvalue = pd.read_csv('../R/External_output/Tables/pValues_isup_' + str(3) + '.csv')

all_result = pd.DataFrame()
for external in [False, True]:
    testLabels = all_data['y_test'] if not external else all_data['y_eicu']
    pvalue = internal_pvalue if not external else external_pvalue
    result = pd.DataFrame()
    for sigfLevel in [0.05, 0.1, 0.15, 0.2, 0.25, 0.30]:
        df_pred1 = df_predict_region(pvalue, sigfLevel=sigfLevel, testLabels=testLabels + 1)
        mytable, _ = tab_predict_region(df_pred1)
        result = pd.concat([result, pd.DataFrame({'Rapid death': ['sig: '+str(sigfLevel)],
                                        'Persistent ill': ['sig: '+str(sigfLevel)], 'Recovery': ['sig: '+str(sigfLevel)]})], axis=0)
        result = pd.concat([result, mytable], axis=0)
    all_result = pd.concat([all_result, result], axis=1)

all_result.to_excel('result/all_conformal_table.xlsx')
