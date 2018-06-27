# Audit-Prediction

Overview of the model


In this exercise, I’ve chosen to focus on one important metric to help decide whether an audit is required or not – the charge difference that has occurred due to the change in the original coder work and the final audit (ch.final.chg.diff). 
I used the following factors as inputs to a binomial linear regression model – disposition,	site.id, ts.coder.id, age.yrs, MIPS.ts.error, MIPS.ts.coder.error, Dx.ts.error, mod.ts.error,	primary.ts.cpt, ch.delete.ch.audit.em .
While the accuracy of the model is high, this is misleading since the data is highly imbalanced. Removing rows that have NULL values is also not ideal.

Immediate Next Steps


The code, currently, has inefficiencies that are inconsequential to such a small dataset but need to be fixed for larger ones. For example, string matching and replace will be faster than using for loops, in some instances. The package ‘stringdist’ can be used to automate the reductions in the variations of the factor values caused by typos and other errors. The variation reduction is performed twice here – once for training and once for validation, which can be reduced to just once on the original dataset.
Since the data is imbalanced, we can perform minority oversampling and majority undersampling to improve predictive power.
More variables can be used – including E/M level change variables – to improve prediction.
NULL values can be imputed rather than deleting the row to preserve information.
The column ‘cc.thompson’ can also be used as a factor in the model if each symptom is given its own column by separating at the delimiter ‘;’. 
