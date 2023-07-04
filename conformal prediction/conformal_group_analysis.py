import pickle
import pandas as pd
import numpy as np
from plot.conformal_prediction_plot import df_predict_region, tab_predict_region
from utils.util import load_database
from stastics import stastic_A_B

feature_selection = 'all' #  nee_max, top_15, all, boruta
all_data = pickle.load(open('./data/multiclass/' + feature_selection + '/fold_0/save_data.pkl', 'rb'))
internal_pvalue = pd.read_csv('../R/Output/Tables/pValues_isup_' + str(3) + '.csv')
external_pvalue = pd.read_csv('../R/External_output/Tables/pValues_isup_' + str(3) + '.csv')

external = False

# for external in [False, True]:
testLabels = all_data['y_test'] if not external else all_data['y_eicu']
pvalue = internal_pvalue if not external else external_pvalue
result = pd.DataFrame()
df_pred = df_predict_region(pvalue, sigfLevel=[0.25, 0.25, 0.15], testLabels=testLabels + 1)  # sig = np.array([0.15, 0.15, 0.1])
mytable, _ = tab_predict_region(df_pred)

df_pred['pred_result'] = df_pred['sum']
multi_count = pd.DataFrame(data=np.zeros([1, 4]), columns=['Rapid death & Persistent ill', 'Rapid death & Recovery',
                                    'Persistent ill & Recovery', 'Rapid death & Persistent ill & Recovery'])
for i in range(len(df_pred)):
    if df_pred.loc[i, 'pred_group'] == 'Multiple':
        if df_pred.loc[i, '1'] and df_pred.loc[i, '2'] and not df_pred.loc[i, '3']:
            df_pred.loc[i, 'pred_result'] = 'Rapid death & Persistent ill'
            multi_count.iloc[0, 0] += 1
        elif df_pred.loc[i, '1'] and df_pred.loc[i, '3'] and not df_pred.loc[i, '2']:
            df_pred.loc[i, 'pred_result'] = 'Rapid death & Recovery'
            multi_count.iloc[0, 1] += 1
        elif df_pred.loc[i, '2'] and df_pred.loc[i, '3'] and not df_pred.loc[i, '1']:
            df_pred.loc[i, 'pred_result'] = 'Persistent ill & Recovery'
            multi_count.iloc[0, 2] += 1
        elif df_pred.loc[i, '1'] and df_pred.loc[i, '2'] and df_pred.loc[i, '3']:
            df_pred.loc[i, 'pred_result'] = 'Rapid death & Persistent ill & Recovery'
            multi_count.iloc[0, 3] += 1

for l in ['Rapid death', 'Persistent ill', 'Recovery']:
    print('multiple in ' + l + ':...')
    multi = df_pred[(df_pred['testLabels']==l) & (df_pred['pred_group']=='Multiple')]
    print(multi['pred_result'].value_counts())
    print()



