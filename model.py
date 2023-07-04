import pandas as pd
import numpy as np
import xgboost as xgb
from hyperopt import STATUS_OK, hp, fmin, tpe
from sklearn.metrics import accuracy_score, roc_auc_score
from sklearn.linear_model import LogisticRegression
from sklearn.ensemble import RandomForestClassifier
import joblib
from tensorflow.keras.models import load_model


def BO_TPE_XGB(x_train, y_train, x_val, y_val, sample_weights,set_up):
    def objective(params):
        xgb_model = xgb.XGBClassifier(max_depth=params['max_depth'],
                                      eta=params['learning_rate'],
                                      n_estimators=1000,
                                      subsample=params['subsample'],
                                      colsample_bytree=params['colsample_bytree'],
                                      reg_alpha=params['reg_alpha'],
                                      reg_lambda=params['reg_lambda'],
                                      objective="multi:softmax" if set_up == 'multiclass' else "binary:logistic"
                                      )

        es = xgb.callback.EarlyStopping(
            rounds=20,
            # abs_tol=1e-4,
            save_best=True,
            maximize=False if set_up == 'multiclass' else True,
            # data_name="validation",
            metric_name="merror" if set_up == 'multiclass' else 'auc',
        )
        xgb_model = xgb_model.fit(x_train, y_train, eval_set=[(x_val, y_val)], verbose=False,
                                  eval_metric='merror' if set_up == 'multiclass' else 'auc',
                                  callbacks=[es], sample_weight=sample_weights)

        y_vd_pred = xgb_model.predict(x_val, ntree_limit=xgb_model.best_ntree_limit)
        metric = accuracy_score(y_val, y_vd_pred) if set_up == 'multiclass' else\
            roc_auc_score(y_val, xgb_model.predict_proba(x_val, ntree_limit=xgb_model.best_ntree_limit)[:, 1])
        loss = 1 - metric

        return {'loss': loss, 'params': params, 'status': STATUS_OK}

    max_depths = [2, 3, 4, 5, 6, 7, 8, 9, 10]
    learning_rates = [0.001, 0.01, 0.02, 0.04, 0.06, 0.08, 0.1, 0.15, 0.2, 0.3]
    subsamples = [0.5, 0.6, 0.7, 0.8, 0.9, 1]
    colsample_bytrees = [0.5, 0.6, 0.7, 0.8, 0.9, 1]
    reg_alphas = [0.0, 0.005, 0.01, 0.05, 0.1]
    reg_lambdas = [0.8, 1, 1.5, 2, 4]

    space = {
        'max_depth': hp.choice('max_depth', max_depths),
        'learning_rate': hp.choice('learning_rate', learning_rates),
        'subsample': hp.choice('subsample', subsamples),
        'colsample_bytree': hp.choice('colsample_bytree', colsample_bytrees),
        'reg_alpha': hp.choice('reg_alpha', reg_alphas),
        'reg_lambda': hp.choice('reg_lambda', reg_lambdas),
    }

    best = fmin(fn=objective, space=space, verbose=True, algo=tpe.suggest, max_evals=20)

    best_param = {'max_depth': max_depths[(best['max_depth'])],
                  'learning_rate': learning_rates[(best['learning_rate'])],
                  'subsample': subsamples[(best['subsample'])],
                  'colsample_bytree': colsample_bytrees[(best['colsample_bytree'])],
                  'reg_alpha': reg_alphas[(best['reg_alpha'])],
                  'reg_lambda': reg_lambdas[(best['reg_lambda'])]
                  }

    return best_param


def BO_TPE_RF(x_train, y_train, x_val, y_val):
    def objective(space):
        model = RandomForestClassifier(
                                       # max_depth=space['max_depth'],
                                       min_samples_leaf=space['min_samples_leaf'],
                                       # min_samples_split=space['min_samples_split'],
                                       n_estimators=1000,
                                       class_weight='balanced'
                                       )

        model.fit(x_train, y_train)
        y_vd_pred = model.predict(x_val)
        acc = accuracy_score(y_val, y_vd_pred)
        loss = 1 - acc

        return {'loss': loss, 'status': STATUS_OK}

    # max_depths = [2, 4, 6, 8, 10, 15, 20]
    # n_estimators = [10, 50, 75, 100, 300, 750]
    space = {
             # 'max_depth': hp.choice('max_depth', max_depths),
             'min_samples_leaf': hp.uniform('min_samples_leaf', 0, 1),
             # 'min_samples_split': hp.uniform('min_samples_split', 0, 1),
             # 'n_estimators': hp.choice('n_estimators', n_estimators)
             }

    best = fmin(fn=objective, space=space, verbose=True, algo=tpe.suggest, max_evals=20)

    best_param = {
                  # 'max_depth': max_depths[best['max_depth']],
                  'min_samples_leaf': best['min_samples_leaf'],
                  # 'min_samples_split': best['min_samples_split'],
                  # 'n_estimators': n_estimators[best['n_estimators']]
    }

    return best_param


def BO_TPE_LR(x_train, y_train, x_val, y_val, set_up):
    def objective(space):
        log_model = LogisticRegression(solver=space['solver'],
                                       C=space['C'],
                                       class_weight='balanced')
        log_model.fit(x_train, y_train)
        lr_y_pre = log_model.predict(x_val)
        metric = accuracy_score(y_val, lr_y_pre) if set_up == 'multiclass' else \
            roc_auc_score(y_val, log_model.predict_proba(x_val)[:, 1])
        loss = 1 - metric

        return {'loss': loss, 'status': STATUS_OK}

    solvers = ['newton-cg', 'sag', 'saga', 'lbfgs']
    space = {
        'C': hp.lognormal('C', 0, 1.0),
        'solver': hp.choice('solver', solvers)
    }

    best = fmin(fn=objective, space=space, verbose=True, algo=tpe.suggest, max_evals=20)
    best_param = {'C': best['C'],
                  'solver': solvers[best['solver']]}

    return best_param


def load_model_predict(x_test, model_type, k_fold, path, set_up):
    test_pred = np.zeros((x_test.shape[0], 3)) if set_up == 'multiclass' else np.zeros((x_test.shape[0], 2))
    for k in range(k_fold):
        if model_type == 'xgb':
            model = xgb.XGBClassifier()
            model.load_model(fname=path + 'model{}.json'.format(k + 1))
        else:
            model = joblib.load(path + 'model{}.joblib'.format(k + 1))
        y_test_pred = model.predict_proba(x_test)
        test_pred += y_test_pred

    test_pred_pro = pd.DataFrame(test_pred/5)
    result = test_pred_pro.idxmax(axis=1)

    if set_up == 'multiclass':
        return np.array(test_pred_pro), np.array(result)
    else:
        return np.array(test_pred_pro)[:, 1], None


def load_model_predict_single(x_test, model_type, k_fold, path, set_up):
    test_pred = np.zeros((x_test.shape[0], 3)) if set_up == 'multiclass' else np.zeros((x_test.shape[0], 2))
    # for k in range(k_fold):
    k=3
    if model_type == 'xgb':
        model = xgb.XGBClassifier()
        model.load_model(fname=path + 'model{}.json'.format(k + 1))
        y_test_pred = model.predict_proba(x_test)
    elif model_type == 'ndf':
        model = load_model(path)
        y_test_pred = model.predict(x_test)
    else:
        model = joblib.load(path + 'model{}.joblib'.format(k + 1))
        y_test_pred = model.predict_proba(x_test)
    # test_pred += y_test_pred

    test_pred_pro = pd.DataFrame(y_test_pred)
    result = test_pred_pro.idxmax(axis=1)

    if set_up == 'multiclass':
        return np.array(test_pred_pro), np.array(result)
    else:
        return y_test_pred[:, 1], None