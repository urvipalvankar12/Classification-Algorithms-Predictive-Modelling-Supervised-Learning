# Predictive-Modelling---Classification-Algorithms
Classification Problem for predictive wether a client will default or not.
Algorithms used - Logistic Regression Model, CART, RPART, Random Forest and XG Boost
The Business Context
A major bank wants to better predict the likelihood of default for its customers, as well as identify the key drivers that determine this likelihood. They hope that this would inform the bank’s decisions on who to give a credit to and what credit limit to provide, as well as also help the bank have a better understanding of their current and potential customers, which would inform their future strategy, including their planning of offering targeted credit products to their customers.

The Data
The bank collected data on 25 000 of their existing clients. Of those, 1 000 were randomly selected to participate in a pilot described below. Data about the remaining 24 000 is in the file “MMA867 A3 – credit data.xls”. The dataset contains various information, including demographic factors, credit data, history of payment, and bill statements from April to September, as well as information on the outcome: did the customer default or not in October.

Pilot Project
Your department wants to pilot a new product, a short‐term credit line with the limit of 25,000, and for the purposes of this assignment assume that the line is for 1 month at 2% per month. More so, assume that the client who was issued credit and repaid it will more likely use your bank for similar short‐term financing needs in the future, which has an additional lifetime value (CLV) of 1,000. However, if the client will default, then you will be able to recover only 20,000 out of 25,000 credit
granted. The data about 1 000 clients that were randomly selected for this pilot is in the file “new applications.xlsx".

The ultimate question: which of the 1 000 “new applicants” in the pilot should be issued credit?
In your analyses, please make the following simplifying assumptions:
1. Defaults on the previously issued credit is not your problem
2. All the clients who will be offered the credit line will use it in full
3. Your cost of capital = 0
In other words, for each client in the pilot, if the credit is issued and repaid, then the bank earns a
profit of 25,000*2% + 1,000 = 1,500; if the credit is granted but the client defaults, then the bank
loses 25,000 ‐ 20,000 = 5,000? And if the credit is not issued, then the profit=loss=0.

1) Issuing credit - Determine which of the 1000 clients in the pilot should be issued credit. 
2) Rerun the best model from 1 with and without the use of the “SEX” variable.
