import numpy as np
import shap
import xgboost as xgb
from utils.util import load_database
import warnings
import pickle
import matplotlib.pyplot as plt
from plot import shap_rewrite
warnings.filterwarnings('ignore')
import matplotlib.font_manager as fm
# import seaborn as sns
# sns.set_style("darkgrid", {"axes.facecolor": ".95"})


set_up = 'multiclass'            # two-way
feature_selection = 'all'        # or nee_max, top_15, all
xgb_path = './model/xgb_' + set_up + '/' + feature_selection + '/'

raw_x, y = load_database('mimic')
all_data = pickle.load(open('./data/' + set_up + '/' + feature_selection + '/fold_0/save_data.pkl', 'rb'))
shap_x = np.array(raw_x.iloc[all_data['test_index'], :])
shap_label_y = np.array(y.iloc[all_data['test_index']])

feature_name = raw_x.columns.tolist()
feature_name[3] = 'Pulmonary infection'

num_class = 3

k=3
def shap_value(input_data, k_fold, model_path, num_class):
    shap.initjs()
    all_shap_values = [np.zeros((input_data.shape[0], input_data.shape[1])) for i in range(num_class)]
    for k in range(k_fold):
        model = xgb.XGBClassifier()
        model.load_model(fname=model_path + 'model{}.json'.format(k + 1))
        explainer = shap.TreeExplainer(model)
        shap_values = explainer.shap_values(input_data)
        for i in range(num_class):
            all_shap_values[i] += shap_values[i]

    for i in range(num_class):
        all_shap_values[i] = all_shap_values[i] / k_fold

    return all_shap_values


def shap_value_single(input_data, model_path, k=3):
    shap.initjs()
    model = xgb.XGBClassifier()
    model.load_model(fname=model_path + 'model{}.json'.format(k+1))
    explainer = shap.TreeExplainer(model)
    # expected_value = explainer.expected_value
    shap_values = explainer.shap_values(input_data)

    return shap_values


# shap_data = shap_value(shap_x, k_fold=5, model_path=xgb_path, num_class=num_class)
shap_data = shap_value_single(shap_x, model_path=xgb_path, k=3)
global_shap_values = []
for i in range(num_class):
    k = shap_data[i]
    l = np.abs(k).mean(0)
    global_shap_values.append(l)

for i in range(num_class):
    shap_data[i] = shap_data[i] / np.sum(global_shap_values)

labels_name = ['Raipd death', 'Persistent ill', 'Recovery']
plt.figure(num=1, dpi=300)
plt.title('Global feature importance', fontsize=14)
shap.summary_plot(shap_data, shap_x, plot_type='bar',
                  title='Feature Attribution in {} Class'.format(num_class),
                  feature_names=feature_name, class_names=labels_name, max_display=15)

fontsize = 12
fontname = 'Times New Roman'
font_path = fm.findfont(fm.FontProperties(family='Times New Roman'))
plt.rcParams['font.family'] = 'serif'
plt.rcParams['font.serif'] = fm.FontProperties(fname=font_path).get_name()
fig, axes = plt.subplots(nrows=1, ncols=3, figsize=(15, 6), dpi=600, layout='constrained')

all_xticks = [[-0.1, -0.05, 0, 0.05], [-0.05, 0, 0.05], [-0.1, -0.05, 0, 0.05]]
all_xticks_label = [['-0.10', '-0.05', '0.00', '0.05'], ['-0.05', '0.00', '0.05'], ['-0.10', '-0.05', '0.00', '0.05']]

for c, title, xticks, xticks_label in zip(range(3), labels_name, all_xticks, all_xticks_label):
    ax = axes[c]
    ax.set_title(title, fontsize=14)
    shap_rewrite.summary_legacy(ax, shap_data[c], shap_x, plot_type='dot',
                      title='Feature Attribution in {} Class'.format(num_class), alpha=0.3,
                      feature_names=feature_name, class_names=labels_name, max_display=15,
                                color_bar=True if c==2 else False)
    ax.set_xticks(xticks, xticks_label, fontsize=12)
plt.savefig('E:/G/she_lab/lab/hui_chen/Manuscript/CC/Figure4.pdf', format='pdf')
plt.show()



# top 15 features bar_plot
# feature importance order with the name list
rank = np.argsort(np.sum(np.mean(np.abs(shap_data), axis=1), axis=0))[::-1]
rank = [rank[i] for i in range(len(rank))]
feature_order = [feature_name[i] for i in rank]
shap_mean = np.mean(np.abs(shap_data), axis=1)
# pickle.dump(feature_order, open('./data/' + set_up + '/feature_order.pkl', "wb"))


top_15_index = rank[:15][::-1]  # top 15 features index
y_pos = np.arange(len(top_15_index))
left_pos = np.zeros(len(top_15_index))
color = ['#f6b6e2', '#cfd0ff', '#d5f198']
fig, ax = plt.subplots(dpi=600, layout='constrained', figsize=(4.6, 4.6))
for i, ind in enumerate(labels_name):
    global_shap_values = shap_mean[i, :]   # each class shap
    plt.barh(
        y_pos, global_shap_values[top_15_index], 0.7, left=left_pos, align='center',
        color=color[i], label=ind, edgecolor='dimgrey')
    left_pos += global_shap_values[top_15_index]
plt.xticks([0.00, 0.02, 0.04, 0.06, 0.08], ['0.00', '0.02', '0.04', '0.06', '0.08'], fontsize=12)
plt.yticks(y_pos, fontsize=12)
plt.gca().set_yticklabels([feature_name[i] for i in top_15_index])
plt.legend(fontsize=12, frameon=False)
ax.spines[['top', 'right']].set_visible(False)
ax.set_xlabel('Feature importance', fontsize=12)
plt.show()