one_step_predictions <-function(year,nh,lambda){
  pred=matrix(c(NaN),nrow=169)
  N=2
  L=matrix(c(1,1,0,1),nrow=2)
  ft=c(1,1)
  x=cbind(1,(year-1851)[1:N])
  y=(nh)[1:N]
  FN=lambda^0*c(1,-0)%*%t(c(1,-0))+lambda^1*c(1,-1)%*%t(c(1,-1))
  hN=lambda^0*c(1,-0)*nh[2-0]+lambda^1*c(1,-1)*nh[2-1]
  theta=solve(FN,hN)
  pred[N+1]=t(ft)%*%theta
  N=N+1
  for (n in 1:161){
    FN=FN+lambda^N*c(1,-N)%*%t(c(1,-N));
    hN=lambda*solve(L)%*%hN+c(1,0)*nh[N+1]
    theta=solve(FN,hN)
    pred[N+1]=t(ft)%*%theta
    N=N+1
  }
  pred
}

relative_erros <-function(true_data,predicted_data){
  norm(true_data-predicted_data,type="2")/norm(true_data,type="2")
}
