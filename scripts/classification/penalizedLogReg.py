import numpy as np
import pandas as pd
import tpot
from sklearn.model_selection import train_test_split
from sklearn.pipeline import make_pipeline, make_union
from sklearn import svm
from sklearn.model_selection import GridSearchCV

## Now read the data
tpot_data = pd.read_csv('/Users/arose/Documents/forRan/barzilayStress/scripts/classification/forTpot.csv', sep=',', dtype=np.float64)
features = tpot_data.drop('y', axis=1).values
target = tpot_data.y

## Now decalre our pipeline optimizer
pipeline_optimizer = tpot.TPOTClassifier(verbosity=2,scoring="roc_auc",cv=5)

## Now limit our pipeline to only search across linear svm's
classifier_config_dict = {

    'sklearn.linear_model.LogisticRegression': {
        'penalty': ["l1", "l2"],
        'C': [1e-4, 1e-3, 1e-2, 1e-1, 0.5, 1., 5., 10., 15., 20., 25.],
        'dual': [True, False]
},
    # Preprocesssors
    'sklearn.preprocessing.Binarizer': {
        'threshold': np.arange(0.0, 1.01, 0.05)
    },
    
    'sklearn.decomposition.FastICA': {
        'tol': np.arange(0.0, 1.01, 0.05)
    },
    
    'sklearn.cluster.FeatureAgglomeration': {
        'linkage': ['ward', 'complete', 'average'],
        'affinity': ['euclidean', 'l1', 'l2', 'manhattan', 'cosine']
    },

    'sklearn.preprocessing.MaxAbsScaler': {
    },

    'sklearn.preprocessing.MinMaxScaler': {
    },

'sklearn.preprocessing.Normalizer': {
    'norm': ['l1', 'l2', 'max']
    },
    
    'sklearn.kernel_approximation.Nystroem': {
        'kernel': ['rbf', 'cosine', 'chi2', 'laplacian', 'polynomial', 'poly', 'linear', 'additive_chi2', 'sigmoid'],
        'gamma': np.arange(0.0, 1.01, 0.05),
        'n_components': range(1, 11)
},
    
    'sklearn.decomposition.PCA': {
        'svd_solver': ['randomized'],
        'iterated_power': range(1, 11)
},
    
    'sklearn.preprocessing.PolynomialFeatures': {
        'degree': [2],
        'include_bias': [False],
        'interaction_only': [False]
},
    
    'sklearn.kernel_approximation.RBFSampler': {
        'gamma': np.arange(0.0, 1.01, 0.05)
},
    
    'sklearn.preprocessing.RobustScaler': {
    },
    
    'sklearn.preprocessing.StandardScaler': {
    },
    
    'tpot.builtins.ZeroCount': {
    },
    
    'tpot.builtins.OneHotEncoder': {
        'minimum_fraction': [0.05, 0.1, 0.15, 0.2, 0.25],
        'sparse': [False]
},
    
    # Selectors
    'sklearn.feature_selection.SelectFwe': {
        'alpha': np.arange(0, 0.05, 0.001),
        'score_func': {
            'sklearn.feature_selection.f_classif': None
    }
    },

'sklearn.feature_selection.SelectPercentile': {
    'percentile': range(1, 100),
    'score_func': {
        'sklearn.feature_selection.f_classif': None
    }
    },
    
    'sklearn.feature_selection.VarianceThreshold': {
        'threshold': [0.0001, 0.0005, 0.001, 0.005, 0.01, 0.05, 0.1, 0.2]
},
    
    'sklearn.feature_selection.RFE': {
        'step': np.arange(0.05, 1.01, 0.05),
        'estimator': {
            'sklearn.ensemble.ExtraTreesClassifier': {
                'n_estimators': [100],
                'criterion': ['gini', 'entropy'],
                'max_features': np.arange(0.05, 1.01, 0.05)
        }
    }
},
    
    'sklearn.feature_selection.SelectFromModel': {
        'threshold': np.arange(0, 1.01, 0.05),
        'estimator': {
            'sklearn.ensemble.ExtraTreesClassifier': {
                'n_estimators': [100],
                'criterion': ['gini', 'entropy'],
                'max_features': np.arange(0.05, 1.01, 0.05)
        }
    }
}

}

pipeline_optimizer.config_dict = classifier_config_dict

## Now build a pipeline
pipeline_optimizer.fit(features, target)

## Now lets check out our svm
tmp_svm = svm.LinearSVC(C = 15.0, dual=False, loss='squared_hinge', penalty='l1', tol=0.001)
tmp_svm.fit(features, target)
clf = sklearn.calibration.CalibratedClassifierCV(tmp_svm)
clf.fit(features, target)

## Now we will need to make our ROC plot with these values
import matplotlib.pyplot as plt
plt.figure()
lw = 2
plt.plot(fpr[0], tpr[0], color='darkorange',
         lw=lw, label='ROC curve (area = %0.2f)' % roc_auc[0])
plt.plot([0, 1], [0, 1], color='navy', lw=lw, linestyle='--')
plt.xlim([-0.05, 1.0])
plt.ylim([0.0, 1.05])
plt.xlabel('False Positive Rate')
plt.ylabel('True Positive Rate')
plt.title('Receiver operating characteristic example')
plt.legend(loc="lower right")
plt.show()

