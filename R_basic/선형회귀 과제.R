# 시각화용 코드입니다.
smoothing <- function(vec)
{
  vec1 <- c(vec[-1], vec[length(vec)])
  vec2 <- c(vec[1], vec[-length(vec)])
  return((vec1 + vec2 + vec) / 3)
}

visualize_loss <- function(loss_log)
{
  for(i in 1:100)
  {
    loss_log <- smoothing(loss_log)
    plot(loss_log)
    Sys.sleep(0.01)
  }
}
# 여기까지 그냥 실행시켜 주세요!

##############################################################################################################
#                                                                                                            #
#   이번 과제는 gradient descent를 이용한, 선형 회귀 구현 입니다. 아래에 비어있는 식을 채워주시면 됩니다!    #
#                                                                                                            #
##############################################################################################################
# 단순회귀 구현
x <- rnorm(1000, 0)
y <- 2 * x + 1
w <- 0.001
b <- 0.001
lr <- 0.01
loss_log <- c()
for(i in 1:length(x))
{
  ###                                         ###
  #            여기를 채워 주세요!              #
  ###                                         ###
  loss_log[i] <- loss
}
visualize_loss(loss_log)
if(max(abs(w-2), abs(b-1)) < 0.1)
{
  print("정답입니다!")
}else{
  print("모델을 수정하거나, 초기값, 파라미터를 수정해보세요!")
}
  
#다중회귀 구현(변수 11개)
x <- as.data.frame(matrix(rnorm(5000,0), nrow = 500, ncol = 10))
y <- x$V1 * 1 + x$V2 * 2 + x$V3 * 3 + x$V4 * 4 + x$V5 * 5 + x$V6 * 6 + x$V7 * 7 + x$V8 * 8 + x$V9 * 9 + x$V10 * 10 + 11
w <- rnorm(10,0)
b <- rnorm(1,0)
lr <- 0.01
loss_log <- c()
for(i in 1:nrow(x))
{
  ###                                         ###
  #            여기를 채워 주세요!              #
  ###                                         ###
  loss_log[i] <- loss
}
visualize_loss(loss_log)
if(max(abs(w-1:10), abs(b-11)) < 0.5)
{
  print("정답입니다!")
}else{
  print("모델을 수정하거나, 초기값, 파라미터를 수정해보세요!")
}

#다중회귀 구현(변수 n개)
linear_regression <- function(n)
{
  x <- as.data.frame(matrix(rnorm(30*n*n,0), nrow = 30*n, ncol = n))
  y <- 1:n %*% t(x) + (n+1)
  w <- rnorm(n,0)
  b <- rnorm(1,0)
  lr <- 0.01
  loss_log <- c()
  iter <- 0
  iter_max <- 10000
  loss <- 1000000000000
  len <- nrow(x)
  while(loss > 0.001 && iter < iter_max)
  {
    ###                                         ###
    #            여기를 채워 주세요!              #
    ###                                         ###
    loss_log[iter] <- loss
  }
  visualize_loss(loss_log)
  if(max(abs(w-1:n), abs(b-n-1)) < 0.5)
  {
    print("정답입니다!")
  }else{
    print("모델을 수정하거나, 초기값, 파라미터를 수정해보세요!")
  }
  return(list(w = w, b = b))
}

linear_regression(10)
linear_regression(15)
linear_regression(20)
