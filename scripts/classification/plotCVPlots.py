import numpy as np
import pandas as pd
import sklearn
from sklearn import calibration
from scipy import interp
import matplotlib.pyplot as plt
from itertools import cycle
from sklearn import svm, datasets
from sklearn.metrics import roc_curve, auc
from sklearn.model_selection import StratifiedKFold

### Now create a function which will let us grab the optimal cutoff point from our ROC curves
def Find_Optimal_Cutoff(target, predicted):
    """ Find the optimal probability cutoff point for a classification model related to event rate
        Parameters
        ----------
        target : Matrix with dependent or target data, where rows are observations
        
        predicted : Matrix with predicted data, where rows are observations
        
        Returns
        -------
        list type, with optimal cutoff value
        
        """
    fpr, tpr, threshold = roc_curve(target, predicted)
    i = np.arange(len(tpr))
    roc = pd.DataFrame({'tf' : pd.Series(tpr-(1-fpr), index=i), 'threshold' : pd.Series(threshold, index=i)})
    roc_t = roc.ix[(roc.tf-0).abs().argsort()[:1]]
    
    return list(roc_t['threshold'])


# #############################################################################
# Data IO and generation

# Import some data to play with
tpot_data = pd.read_csv('/Users/arose/Documents/forRan/barzilayStress/scripts/classification/forTpot.csv', sep=',', dtype=np.float64)
X = tpot_data.drop('y', axis=1).values
transformer = sklearn.preprocessing.RobustScaler(copy=False)
transformer.fit(X)
#X = transformer.transform(X)
y = tpot_data.y

# #############################################################################
# Classification and ROC analysis
# Run this procedure 100 times
# Also prepare a library for all of the mean auc values to live in
# And create our output plot
plt.plot([0, 1], [0, 1], linestyle='--', lw=2, color='r',
         label='Luck', alpha=.8)
plt.xlim([-0.05, 1.05])
plt.ylim([-0.05, 1.05])
plt.xlabel('False Positive Rate')
plt.ylabel('True Positive Rate')
plt.title('')
all_mean_auc = np.zeros(100)
all_accuracy_values = np.zeros(100)
selected_values = np.zeros(36500)
selected_values = np.reshape(selected_values, (73,500))
q = 0
for identifier in list(range(0,100)):
    # Run classifier with cross-validation and plot ROC curves
    cv = StratifiedKFold(n_splits=5, random_state=identifier)
    classifier = sklearn.linear_model.LogisticRegression(C=1.0, penalty='l1', dual=False)
    tprs = []
    aucs = []
    mean_fpr = np.linspace(0, 1, 100)
    predVals = np.zeros(115)
    i = 0
    for train, test in cv.split(X, y):
        probas_ = classifier.fit(X[train], y[train]).predict_proba(X[test])
        predVals[test] = probas_[:,1]
        tmp_model = classifier.fit(X[train], y[train])
        selected_values[:,q] = tmp_model.coef_
        q += 1
        i += 1
    # Compute ROC curve and area under the curve
    fpr, tpr, thresholds = roc_curve(y, predVals)
    tprs.append(interp(mean_fpr, fpr, tpr))
    tprs[-1][0] = 0.0
    roc_auc = auc(fpr, tpr)
    aucs.append(roc_auc)
    mean_tpr = np.mean(tprs, axis=0)
    mean_tpr[-1] = 1.0
    mean_auc = auc(mean_fpr, mean_tpr)
    mean_tpr = np.mean(tprs, axis=0)
    thresh_to_apply = Find_Optimal_Cutoff(y, predVals)
    mean_tpr[-1] = 1.0
    mean_auc = auc(mean_fpr, mean_tpr)
    # Now also compute an accuracy
    predBinary = np.where(predVals > thresh_to_apply, 1, 0)
    confuMatrix = sklearn.metrics.confusion_matrix(y, predBinary)
    accuracyValue = confuMatrix.diagonal().sum()/confuMatrix.sum()
    print(identifier, mean_auc)
    all_mean_auc[identifier] = mean_auc
    all_accuracy_values[identifier] = accuracyValue
    plt.plot(mean_fpr, mean_tpr, color='grey', lw=2, alpha=.2)

## Now write the selected_values data frame
np.savetxt("betaWeightsAllFolds.csv",selected_values, delimiter=',', fmt='%1.4f')

std_auc = np.std(all_mean_auc).round(2)
mean_auc = np.mean(all_mean_auc).round(2)
std_accuracy = np.std(all_accuracy_values).round(2)
mean_accuracy = np.mean(all_accuracy_values).round(2)

# Now add our mean values to the plot
orig_value = plt
outplot = plt
outplot.text(.6,.4, 'Mean AUC = %s'%mean_auc)
outplot.text(.6,.35, 'Mean Accuracy = %s'%mean_accuracy)
outplot.savefig("ROCCurve.png", dpi=300, format='png')

## Now train a model in everything to see which variables are used
final_model = classifier.fit(X, y)
output_betas = np.zeros(139)
output_betas = final_model.coef_
np.savetxt("betaWeightsFinal.csv", output_betas, delimiter=',',fmt='%1.4f')
