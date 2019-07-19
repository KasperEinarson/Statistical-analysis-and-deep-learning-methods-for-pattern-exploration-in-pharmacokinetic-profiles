%%   ======================= TRY AGAIN =====================================================
K = 3;
decay=1;
NN1 = [3 K];
NN2 = [3 K];
drop1 = [0.3 0 0];
drop2 = [0.3 0 0];

F5 = @(x) DCCA_train6(X1,X2,XV1,XV2,XTe1,XTe2,K,NN1,NN2,2,drop1,drop2,1,[],x.eta0,x.momentum,[],x.rcov1,x.rcov2,x.l2penalty)

optimVars = [
    %optimizableVariable('batchsize',[400 600],'Type','integer')
    optimizableVariable('eta0',[1e-5 0.9],'Transform','log')
    optimizableVariable('momentum',[0.7 0.8])
    %optimizableVariable('maxepoch',[500 3000],'Type','integer')
    optimizableVariable('rcov1',[1e-4 1],'Transform','log')
    optimizableVariable('rcov2',[1e-4 1],'Transform','log')
    optimizableVariable('l2penalty',[1e-3 1],'Transform','log')
    ];


%%
results4 = bayesopt(F5,optimVars,'Verbose',0,...        
   'AcquisitionFunctionName','expected-improvement-plus','MaxObjectiveEvaluations',10)
%%
%save('results3')
%%
%results2 = load("Useful_21june.mat")
results = results4
%% 
%batchsize=results.XAtMinEstimatedObjective.batchsize
eta0=results.XAtMinEstimatedObjective.eta0
momentum = results.XAtMinEstimatedObjective.momentum
%maxepoch = results.XAtMinEstimatedObjective.maxepoch
%maxepoch=5000;
rcov1 = results.XAtMinEstimatedObjective.rcov1
rcov2 = results.XAtMinEstimatedObjective.rcov2
l2penalty = results.XAtMinEstimatedObjective.l2penalty
hiddentype ='sigmoid'
%% 
[F1opt,F2opt,CORR_train,CORR_tune,CORR_test]=DCCA_train2(X1,X2,XV1,XV2,XTe1,XTe2,K,hiddentype,NN1,NN2, ...
  rcov1,rcov2,l2penalty,batchsize,eta0,decay,momentum,maxepoch,5,drop1,drop2);  

%% 
plot(CORR_train)
xlabel('Epoch')
ylabel('Loss (Correlation)')
title('Correlation loss on train and validation data')
hold on 
plot(CORR_tune)
%hold on
%plot(CORR_test)
legend('Train','Validation','location','Southeast','FontSize',14)
%%


[corr1,grad1,grad2] =DCCA_corr(deepnetfwd(XTe1,F1opt),deepnetfwd(XTe2,F2opt),K,rcov1,rcov2)



%% 

%%
X1proj=gather(deepnetfwd(X1,F1opt)); 
XV1proj=gather(deepnetfwd(XV1,F1opt));
XTe1proj=gather(deepnetfwd(XTe1,F1opt));

X2proj=gather(deepnetfwd(X2,F2opt)); 
XV2proj=gather(deepnetfwd(XV2,F2opt));
XTe2proj=gather(deepnetfwd(XTe2,F2opt));

%% 
%%
csvwrite('C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/X1_proj_bay.txt',X1proj)
csvwrite('C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/XV1_proj_bay.txt',XV1proj)
csvwrite('C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/Xte1_proj_bay.txt',XTe1proj)%
csvwrite('C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/X2_proj_bay.txt',X2proj)
csvwrite('C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/XV2_proj_bay.txt',XV2proj)
csvwrite('C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/Xte2_proj_bay.txt',XTe2proj)




%% xscores = XTe1(:,1:K)*xcoef;
yscores = XTe2(:,1:K)*ycoef;
xscores = XTe1(:,1:K)*xcoef;
corrXxscores = corr(XTe1(:,1:K),xscores);
corrYxscores = corr(XTe2(:,1:K),xscores);
corrXyscores = corr(XTe1(:,1:K),yscores);
corrYyscores = corr(XTe2(:,1:K),yscores);
scatter(xscores(:,1),xscores(:,2))

%% loadings
scatter(corrXxscores(:,1),corrXxscores(:,2))
hold on
scatter(corrYxscores(:,1),corrYxscores(:,2))

%% 
%csvwrite('C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/xscores_s16.txt',xscores)
%csvwrite('C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/yscores_s16.txt',yscores)
%csvwrite('C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/corrXxscores_s16.txt',corrXxscores)
%csvwrite('C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/corrYxscores_s16.txt',corrYxscores)
%csvwrite('C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/corrXyscores_s16.txt',corrXyscores)
%csvwrite('C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/corrYyscores_s16.txt',corrYyscores)
%% 
results = load('ModelB_no_drop_random.mat');
%%
plot(results.CORR_train)
xlabel('Epoch')
ylabel('Loss (Correlation)')
title('Correlation loss on train and validation data')
hold on 
plot(results.CORR_tune)
%hold on
%plot(CORR_test)
legend('Train','Validation','location','Southeast','FontSize',14)
%%

X1proj=gather(deepnetfwd(X1,results.F1opt)); 
XV1proj=gather(deepnetfwd(XV1,results.F1opt));
XTe1proj=gather(deepnetfwd(XTe1,results.F1opt));

X2proj=gather(deepnetfwd(X2,results.F2opt)); 
XV2proj=gather(deepnetfwd(XV2,results.F2opt));
XTe2proj=gather(deepnetfwd(XTe2,results.F2opt));


%%
scatter(results.xscores(:,1),results.xscores(:,2))
%%
scatter(results.corrXxscores(:,1),results.corrXxscores(:,2))
%% 
csvwrite('C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/X1_train.txt',X1proj)
csvwrite('C:/Users/kaspe/OneDrive/DTU/12. Semester/Speciale/Data/X2_train.txt',X2proj)

