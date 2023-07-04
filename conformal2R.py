import pickle
import pandas as pd
import numpy as np
import xgboost as xgb
# from plot.conformal_prediction_plot import df_predict_region, tab_predict_region

feature_selection = 'all'           #  nee_max, top_15, all, boruta

for k in range(5):
    all_data = pickle.load(open('data/multiclass/' + feature_selection + '/fold_' + str(k) + '/save_data.pkl', 'rb'))

    model = xgb.XGBClassifier()
    path = './model/xgb_multiclass/' + feature_selection + '/'
    model.load_model(fname=path + 'model{}.json'.format(k+1))
    calibset = pd.DataFrame(np.concatenate((all_data['y_val'].reshape(-1, 1),
                            model.predict_proba(all_data['x_val'])), axis=1), columns=['ISUP', 'X0', 'X1', 'X2'])
    testset = pd.DataFrame(np.concatenate((all_data['y_test'].reshape(-1, 1),
                            model.predict_proba(all_data['x_test'])), axis=1), columns=['ISUP', 'X0', 'X1', 'X2'])
    externalset = pd.DataFrame(np.concatenate((all_data['y_eicu'].reshape(-1, 1),
                            model.predict_proba(all_data['x_eicu'])), axis=1), columns=['ISUP', 'X0', 'X1', 'X2'])
    calibset.to_csv('../R/Data/Demo/calibset_{:d}.csv'.format(k), index=False)
    testset.to_csv('../R/Data/Demo/testset_{:d}.csv'.format(k), index=False)
    externalset.to_csv('../R/Data/Demo/externalset_{:d}.csv'.format(k), index=False)

