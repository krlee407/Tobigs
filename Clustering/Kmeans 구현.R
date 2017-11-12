# clustering 결과를 시각화 해주는 함수
visualize_cluster <- function(df, cluster, centroid = NULL){
  nc <- ncol(df)
  k <- length(unique(cluster))
  if(nc > 2){
    par(mar=c(1,1,1,1))
    par(mfrow = c(nc, nc))
    for(r in 1:nc){
      for(c in 1:nc){
        if(r == c){
          plot.new()
          text(0.5, 0.5, labels = paste(colnames(df)[r], colnames(df)[c], sep = "-"))
          next
        }
        plot(df[,c(r,c)])
        for(clus in 1:k){
          points(df[which(cluster == clus), c(r,c)], col = clus + 1)
          points(centroid[clus, r], centroid[clus, c], col = clus + 1, pch = 3)
        }
      }
    }
  }
  else if(nc == 2){
    par(mar=c(5.1,4.1,3.1,2.1))
    par(mfrow = c(1,1))
    plot(df)
    for(clus in 1:k){#clus <- 1
      points(df[which(cluster == clus),], col = clus + 1)
      if(!is.null(centroid)){
        points(centroid[clus, 1], centroid[clus, 2], col = clus + 1, pch = 3)
      }
    }
  }
}

# KmeansKR : 유클리드 거리 따른 군집화 함수, 군집 결과를 return
## parameter 설명
## 
kmeansKR <- function(df, k, nstart = 10, iter_max = 100, visu = FALSE){
  if(nrow(df) < k || nstart <= 0 || iter_max <= 0 || k <= 0){
    return(-1)
  }
  cluster_list <- list()
  for(n_iter in 1:nstart){
    iter <- 0
    nr <- nrow(df)
    pre_cluster <- rep(0, nr)
    # 초기 센트로이드는  forgy 알고리즘으로 선정
    centroid <- df[sample(1:nr, k),]
    while(1){
      iter <- iter + 1
      # centroid <-> data 간 거리 구하기
      dummy_dist <- matrix(rowSums(centroid ^ 2), k, nr) + matrix(rowSums(df ^ 2), k, nr, byrow = T)
      dummy_dist <- dummy_dist - 2 * as.matrix(centroid) %*% t(df)
      # 각 data row에서 가장 가까운 centroid를 찾고, 이를 cluster에 저장
      cluster <- apply(dummy_dist, 2, which.min)
      
      # clustering 과정 시각화
      if(visu){
        visualize_cluster(df, cluster, centroid)
        invisible(readline(prompt="Press [enter] to continue"))
      }
      ## 시각화 끝
      
      # 현재 cluster 기반의 새로운 centroid 설정(각 cluster의 무게중심)
      for(i in 1:k){
        i_cluster <- which(cluster == i)
        if(length(i_cluster) == 1){
          centroid[i,] <- df[i_cluster,]
        } else {
          centroid[i,] <- colSums(df[which(cluster == i),]) / length(which(cluster == i))
        }
      }
      
      # 이전 cluster와 현재 cluster를 비교하여, 같을 경우, 혹은 최대 반복 횟수(iter_max)를 넘길 경우, 반복문 종료
      if(sum(cluster != pre_cluster) == 0 || iter > iter_max){
        cluster_list[[n_iter]] <- cluster
        break
      }
      
      # 현재 cluster를 이전 cluster로 이전
      pre_cluster <- cluster
    }
  }
  
  # nstart가 1일 경우, 모델이 1개만 생성되기 때문에, 모델 평가 없이 해당 모델 출력
  if(nstart == 1){
    return(cluster)
  }
  
  # 모든 row간의 거리 값 미리 계산
  dist_matrix <- matrix(rowSums(df ^ 2), nr, nr) + matrix(rowSums(df ^ 2), nr, nr, byrow = T)
  dist_matrix <- dist_matrix - 2 * as.matrix(df) %*% t(df)
  
  # 실루엣(silhouette) 기반의 모델 평가 후, 최선의 모델을 출력
  max_silhouette <- -1
  best_cluster <- c()
  for(n_iter in 1:nstart){
    # cluster별로 gruop by(mean)을 시행
    cluster_dist_matrix <- aggregate(dist_matrix, by = list(cluster_list[[n_iter]]), FUN = mean)
    # aggregate의 결과가 1번째 column에 group index를 남기기 때문에 이를 제거
    cluster_dist_matrix <- cluster_dist_matrix[,-1]
    ai <- cluster_dist_matrix[cbind(cluster_list[[n_iter]], 1:nr)]
    # 자신이 속한 cluster까지의 거리 평균을 제외하기 위해, 해당 값에 각 열의 최대값 + 1을 삽입
    cluster_dist_matrix[cbind(cluster_list[[n_iter]], 1:nr)] <- apply(cluster_dist_matrix, 2, max) + 1
    bi <- apply(cluster_dist_matrix, 2, min)
    # 모든 지점에서의 silhouette 계산
    silhouette_array <- (bi - ai) / apply(rbind(ai, bi), 2, max)
    silhouette_average <- mean(silhouette_array)
    # max_silhouette값과 비교하여, silhouette_average가 더 클 경우, 최적의 cluster(best_cluster)와 max_silhouette 갱신
    if(max_silhouette < silhouette_average){
      best_cluster <- cluster_list[[n_iter]]
      max_silhouette <- silhouette_average
    }
  }
  result <- list()
  result[["cluster"]] <- best_cluster
  result[["centers"]] <- centroid
  result[["silhouette"]] <- max_silhouette
  return(result)
}

# test case1. rnorm
cl <- kmeansKR(matrix(rnorm(100),50,2), 3, nstart = 1, visu = TRUE)
table(cl$cluster)

# test case2. iris
iris_cl <- kmeansKR(iris[,1:4], 3, nstart = 10, visu = FALSE)
table(iris_cl$cluster, iris[,5])

library("cluster")
?kmeans
