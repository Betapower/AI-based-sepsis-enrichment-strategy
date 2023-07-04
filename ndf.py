from tensorflow import keras
from tensorflow.keras import layers
import tensorflow as tf
from tensorflow.keras.layers import \
    (Input, Conv1D, BatchNormalization, Activation, MaxPooling1D, SpatialDropout1D, LayerNormalization, Dropout,
     GlobalAveragePooling1D, Dense, Reshape, multiply, Add, Flatten)
from tensorflow.keras.models import Model
from tensorflow.keras.optimizers import Adam
from tensorflow.keras.metrics import AUC
from sklearn.utils.class_weight import compute_class_weight
from tensorflow.keras.callbacks import EarlyStopping, ReduceLROnPlateau, ModelCheckpoint
import shutil
import numpy as np,os
import pickle
from tensorflow.keras.models import load_model
from sklearn.metrics import roc_auc_score


class NeuralDecisionTree(keras.Model):
    def __init__(self, depth, num_features, used_features_rate, num_classes):
        super().__init__()
        self.depth = depth
        self.num_leaves = 2 ** depth
        self.num_classes = num_classes

        # Create a mask for the randomly selected features.
        num_used_features = int(num_features * used_features_rate)
        one_hot = np.eye(num_features)
        sampled_feature_indicies = np.random.choice(
            np.arange(num_features), num_used_features, replace=False
        )
        self.used_features_mask = one_hot[sampled_feature_indicies]

        # Initialize the weights of the classes in leaves.
        self.pi = tf.Variable(
            initial_value=tf.random_normal_initializer()(
                shape=[self.num_leaves, self.num_classes]
            ),
            dtype="float32",
            trainable=True,
        )

        # Initialize the stochastic routing layer.
        self.decision_fn = layers.Dense(
            units=self.num_leaves, activation="sigmoid", name="decision"
        )

    def call(self, features):
        batch_size = tf.shape(features)[0]

        # Apply the feature mask to the input features.
        features = tf.matmul(
            features, self.used_features_mask, transpose_b=True
        )  # [batch_size, num_used_features]
        # Compute the routing probabilities.
        decisions = tf.expand_dims(
            self.decision_fn(features), axis=2
        )  # [batch_size, num_leaves, 1]
        # Concatenate the routing probabilities with their complements.
        decisions = layers.concatenate(
            [decisions, 1 - decisions], axis=2
        )  # [batch_size, num_leaves, 2]

        mu = tf.ones([batch_size, 1, 1])

        begin_idx = 1
        end_idx = 2
        # Traverse the tree in breadth-first order.
        for level in range(self.depth):
            mu = tf.reshape(mu, [batch_size, -1, 1])  # [batch_size, 2 ** level, 1]
            mu = tf.tile(mu, (1, 1, 2))  # [batch_size, 2 ** level, 2]
            level_decisions = decisions[
                :, begin_idx:end_idx, :
            ]  # [batch_size, 2 ** level, 2]
            mu = mu * level_decisions  # [batch_size, 2**level, 2]
            begin_idx = end_idx
            end_idx = begin_idx + 2 ** (level + 1)

        mu = tf.reshape(mu, [batch_size, self.num_leaves])  # [batch_size, num_leaves]
        probabilities = keras.activations.softmax(self.pi)  # [num_leaves, num_classes]
        outputs = tf.matmul(mu, probabilities)  # [batch_size, num_classes]
        return outputs


class NeuralDecisionForest(keras.Model):
    def __init__(self, num_trees, depth, num_features, used_features_rate, num_classes):
        super().__init__()
        self.ensemble = []
        # Initialize the ensemble by adding NeuralDecisionTree instances.
        # Each tree will have its own randomly selected input features to use.
        for _ in range(num_trees):
            self.ensemble.append(
                NeuralDecisionTree(depth, num_features, used_features_rate, num_classes)
            )

    def call(self, inputs):
        # Initialize the outputs: a [batch_size, num_classes] matrix of zeros.
        batch_size = tf.shape(inputs)[0]
        outputs = tf.zeros([batch_size, 2])

        # Aggregate the outputs of trees in the ensemble.
        for tree in self.ensemble:
            outputs += tree(inputs)
        # Divide the outputs by the ensemble size to get the average.
        outputs /= len(self.ensemble)
        return outputs


def NDF(num_trees, depth, num_features, used_features_rate, num_classes,
        inputs_shape, learning_rate, loss = 'CategoricalCrossentropy'):
    inputs = Input(shape=inputs_shape)

    tree = NeuralDecisionForest(num_trees, depth, num_features, used_features_rate, num_classes)
    outputs = tree(inputs)
    model = keras.Model(inputs=inputs, outputs=outputs)

    optimizer = Adam(lr=learning_rate)
    model.compile(optimizer=optimizer,
                  loss=loss,
                  metrics=[AUC(name='auc')])

    return model


class Model_Compile():
    def __init__(self,
                 train_x,
                 train_y,
                 val_x,
                 val_y,
                 set_name,
                 epoch = '',
                 set_up = 'multiclass',
                 **params):
        self.train_x = train_x
        self.train_y = train_y
        self.val_x = val_x
        self.val_y = val_y
        self.set_name = set_name
        self.epoch = epoch
        self.set_up = set_up
        self.params = params

    def compute_classweight(self, train_y):
        class_weight = compute_class_weight(class_weight='balanced', classes=np.unique(train_y), y=train_y)
        if self.set_up == 'multiclass':
            class_weight = {0: class_weight[0], 1: (class_weight[1]), 2: (class_weight[2])}
        else:
            class_weight = {0: class_weight[0], 1: (class_weight[1])}
        return class_weight

    def dl_complie(self, model_path, model_type):
        params = self.params
        train_y = self.train_y
        val_y = self.val_y
        train_x = self.train_x
        val_x = self.val_x

        weight = self.compute_classweight(train_y)

        if self.set_up == 'multiclass':
            train_y = tf.keras.utils.to_categorical(train_y, num_classes=3)
            val_y = tf.keras.utils.to_categorical(val_y, num_classes=3)
        else:
            train_y = tf.keras.utils.to_categorical(train_y, num_classes=2)
            val_y = tf.keras.utils.to_categorical(val_y, num_classes=2)
        model = NDF(
            num_trees=params['num_trees'],
            depth=params['depth'],
            num_features=train_x[0].shape[0],
            used_features_rate=params['used_features_rate'],
            num_classes=3 if self.set_up == 'multiclass' else 2,
            inputs_shape=(train_x[0].shape[0],),
            learning_rate=params['learning_rate']
        )

        stopping = EarlyStopping(patience = 15, monitor='val_auc', mode='max')
        reduce_lr = ReduceLROnPlateau(
                factor = 0.2,
                patience = 5,
                monitor = 'val_auc',
                mode = 'max',
                min_lr = params['learning_rate'] * 0.001)

        checkpointer = ModelCheckpoint(
                save_weights_only = False,
                filepath = model_path,
                save_best_only = True,
                monitor='val_auc',
                mode = 'max')

        model.fit(
                train_x, train_y,
                batch_size = params['batch_size'],
                epochs = params['max_epochs'],
                class_weight = weight,
                validation_data = (val_x, val_y),
                callbacks = [checkpointer, stopping, reduce_lr],
                )

    def train_ndf(self):
        store_path = self.params['ndf_path'] + self.set_name + '/' + self.epoch
        if os.path.exists(store_path):
            shutil.rmtree(store_path)
        os.makedirs(store_path)
        self.dl_complie(model_path = store_path, model_type = 'NDF')


if __name__ == '__main__':
    feature_selection = 'all'
    set_up = 'nested_longstay'              #  two-way or multiclass
    all_data = pickle.load(
        open('./data/' + set_up + '/' + feature_selection + '/fold_3/save_data.pkl', 'rb'))  # train val mixed

    train_x = all_data['x_train_process']
    train_y = all_data['y_train']

    val_x = all_data['x_val_process']
    val_y = all_data['y_val']

    params = {'num_trees': 50, 'depth': 5, 'used_features_rate': 0.7, 'learning_rate': 0.001,
              'ndf_path': 'model/ndf_'+ set_up + '/', 'batch_size': 128, 'max_epochs': 100}

    Model_Compile(train_x, train_y, val_x, val_y, 'all', set_up=set_up, **params).train_ndf()

    ndf = load_model(params['ndf_path'] + 'all' + '/')
    if set_up=='multiclass':
        y_pre_pro = ndf.predict(all_data['x_test_process'])
        y_mimic = tf.keras.utils.to_categorical(all_data['y_test'], num_classes=3)
        print(roc_auc_score(y_mimic, y_pre_pro, average=None, multi_class='ovr'))

        y_eicu_pro = ndf.predict(all_data['x_eicu_process'])
        print(roc_auc_score(tf.keras.utils.to_categorical(all_data['y_eicu'], num_classes=3), y_eicu_pro, average=None, multi_class='ovr'))
    else:
        y_pre_pro = ndf.predict(all_data['x_test_process'])[:, 1]
        print('mimic: {:.3f}'.format(roc_auc_score(all_data['y_test'], y_pre_pro)))

        y_eicu_pro = ndf.predict(all_data['x_eicu_process'])[:, 1]
        print('eicu: {:.3f}'.format(roc_auc_score(all_data['y_eicu'], y_eicu_pro)))