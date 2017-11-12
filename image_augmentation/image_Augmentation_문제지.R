# Tobigs 5주차 과제 : image_Augmentation

## 0. 필요한 라이브러리 받기
### 필요하신 분들만 받아주세요!
#install.packages("jpeg")
#install.packages("stringr")

## 1. 라이브러리 및 함수 부르기
### 따로 작성하실 필요없이, 함수를 쭉 부르시면 됩니다!
### imgur_crawling 함수의 경우, 크롤링 강의에서 배웠던 함수들을 이용하여 만들었으니, 복습 겸 참고하시면 좋습니다!
library(jpeg)
library(stringr)

imgur_crawling <- function(code, save_name){
  image_url <- paste0("http://imgur.com/a/", code)
  imageHTML <- readLines(image_url)
  imageInfo <- imageHTML[which(str_detect(imageHTML, "post-image-container"))]
  len <- length(imageInfo)
  image_list <- list()
  for(i in 1:len){
    index <- regexpr("<div id=", imageInfo[[i]])[1]
    serial <- substr(imageInfo[[i]], index + 9, index + 15)
    download.file(paste0("http://i.imgur.com/", serial, ".jpg"), save_name[i], mode = "wb")
    image_list[[save_name[i]]] <- readJPEG(save_name[i])
    file.remove(save_name[i])
  }
  return(image_list)
}

show <- function(img, case = 'full', clear = T){
  if(clear){
    tryCatch(dev.off(), error = function(e) a<-1)
    plot.new()
  }
  if(case == 'answer'){
    text(0.25, 1, 'answer')
    rasterImage(img,0,0.41,0.49,0.9)
  } else if (case == 'result'){
    text(0.75, 1, 'result')
    rasterImage(img, 0.51,0.41,1,0.9)
  } else if (case == 'full'){
    rasterImage(img, 0,0,1,1)
  }
}

popart6 <- function(img1, img2, img3, img4, img5, img6){
  tryCatch(dev.off(), error = function(e) a<-1)
  plot.new()
  rasterImage(img1,0,0.5,0.33,1)
  rasterImage(img2,0.33,0.5,0.66,1)
  rasterImage(img3,0.66,0.5,1,1)
  rasterImage(img4,0,0,0.33,0.5)
  rasterImage(img5,0.33,0,0.66,0.5)
  rasterImage(img6,0.66,0,1,0.5)
}


image_code <- "jRppa"
image_name <- c('thumbnail', 
                'original', 
                'pooling', 
                'shift1', 'shift2', 'shift3', 'shift4', 
                'brightness1', 'brightness2', 
                'rotate1', 'rotate2', 'rotate3', 
                'crop1', 'crop2', 
                'resize1', 'resize2',
                'gray_scale',
                'RGB1', 'RGB2', 'RGB3', 'RGB4', 'RGB5', 'RGB6',
                'filtering1', 'filtering2', 'filtering3', 'filtering4', 'filtering5')

images <- imgur_crawling(image_code, image_name)


## 2. 과제 시작!
### 먼저 과제를 도와줄 친구입니다!
ori_img <- images[['original']]
show(ori_img)


### 첫 번째 과제입니다. 아래에 비어있는 pooling 함수를 구현해 주세요!
### 입력으로 변형할 이미지 배열(img)와 변형할 이미지의 높이(h), 변형할 이미지의 너비(w)를 받아
### 출력으로 크기가 h*w*3인 이미지가 출력되어야 합니다.
### 입력받는 h와 W는 항상 원래 이미지의 높이와 너비보다 작습니다.
### 아래 show(images[['pooling']], 'right', 'F')줄을 실행시키면, 정답 이미지를 볼 수 있습니다.
pooling <- function(img, h, w){
  
}

pimg <- pooling(ori_img, 128, 128)
show(images[['pooling']], 'answer', 'T')
show(pimg, 'result', 'F')


### 두 번째 과제입니다. 아래에 비어있는 shift 함수를 구현해 주세요!
### 입력으로 변형할 이미지 배열(img)와 이미지를 이동 시킬 좌표 (y, x)가 주어집니다.
### 출력으로 입력한 이미지에서 아래로 y픽셀만큼, 오른쪽으로 x픽셀만큼 이동시킨 이미지가 출력되어야 합니다.
### 밀려나서 빈 부분은 검은색(R,G,B = 0,0,0)으로 출력되게 해주세요!
### 아래 show(images[['shift[n]']], 'right', 'F')줄을 실행시키면, 각각의 정답 이미지를 볼 수 있습니다. 
shift <- function(img, cord){
  
}

shift_image1 <- shift(pimg, c(30, -40))
show(images[['shift1']], 'answer', 'T')
show(shift_image1, 'result', 'F')
shift_image2 <- shift(pimg, c(0, 50))
show(images[['shift2']], 'answer', 'T')
show(shift_image2, 'result', 'F')
shift_image3 <- shift(pimg, c(100, 0))
show(images[['shift3']], 'answer', 'T')
show(shift_image3, 'result', 'F')
shift_image4 <- shift(pimg, c(60, 60))
show(images[['shift4']], 'answer', 'T')
show(shift_image4, 'result', 'F')


### 세 번째 과제입니다. 아래에 비어있는 brightness 함수를 구현해 주세요!
### 입력으로 변형할 이미지 배열(img)와 변화할 밝기의 밝음정도(br)가 주어집니다.
### 출력으로 입력한 이미지에서 br만큼 밝아진(R,B,G 값이 각각 br만 큼 커진) 이미지가 출력되어야 합니다.
### 아래 show(images[['brightness[n]]']], 'right', 'F')줄을 실행시키면, 각각의 정답 이미지를 볼 수 있습니다. 
brightness <- function(img, br){
  
}

bright_image1 <- brightness(pimg, 0.3)
show(images[['brightness1']], 'answer', 'T')
show(bright_image1, 'result', 'F')
bright_image2 <- brightness(pimg, -0.3)
show(images[['brightness2']], 'answer', 'T')
show(bright_image2, 'result', 'F')


### 네 번째 과제입니다. 아래에 비어있는 rotate 함수를 구현해 주세요!
### 입력으로 변형할 이미지 배열(img)와 회전 시킬 각도(angle)가 주어집니다.
### 출력으로 입력한 이미지에서 시계 방향으로 angle(degree)만큼 회전시킨 이미지가 출력되어야 합니다.
### 아래 show(images[['rotate[n]]']], 'right', 'F')줄을 실행시키면, 각각의 정답 이미지를 볼 수 있습니다. 
rotate <- function(img, angle){
  
}

rotate_image1 <- rotate(pimg, 90)
show(images[['rotate1']], 'answer', 'T')
show(rotate_image1, 'result', 'F')
rotate_image2 <- rotate(pimg, 180)
show(images[['rotate2']], 'answer', 'T')
show(rotate_image2, 'result', 'F')
rotate_image3 <- rotate(pimg, 270)
show(images[['rotate3']], 'answer', 'T')
show(rotate_image3, 'result', 'F')


### 다섯 번째 과제입니다. 아래에 비어있는 crop 함수를 구현해 주세요!
### 입력으로 변형할 이미지 배열(img)와 출력한 이미지의 두 대각선에 위치한 점의 좌표(sy, sx), (ey, ex)가 주어집니다.
### 출력으로 입력한 이미지에서 y, x축에 평행하고 좌표(sy, sx), (ey, ex)를 이은 선을 대각선으로 가지는 직사각형의 내 속한 이미지의 일부가 출력되어야 합니다.
### 아래 show(images[['crop[n]]']], 'right', 'F')줄을 실행시키면, 각각의 정답 이미지를 볼 수 있습니다. 
crop <- function(img, sy, sx, ey, ex){
  
}

crop_image1 <- crop(pimg, 20, 20, 80, 80)
show(images[['crop1']], 'answer', 'T')
show(crop_image1, 'result', 'F')
crop_image2 <- crop(pimg, 120, 30, 20, 80)
show(images[['crop2']], 'answer', 'T')
show(crop_image2, 'result', 'F')


### 여섯 번째 과제입니다. 아래에 비어있는 resizing 함수를 구현해 주세요!
### 입력으로 변형할 이미지 배열(img)와 변형할 이미지의 높이(h), 변형할 이미지의 너비(w)를 받아
### 출력으로 크기가 h*w*3인 이미지가 출력되어야 합니다.
### pooling 함수와 다른 점은, 사이즈가 커질 수도 있습니다!
### 아래 show(images[['resizing[n]']], 'right', 'F')줄을 실행시키면, 정답 이미지를 볼 수 있습니다.
resizing <- function(img, h, w){

}

### vectorization을 하지 않았을 경우 시간이 오래 걸릴 수 있습니다!
resize_image1 <- resizing(pimg, 30, 30)
show(images[['resize1']], 'answer', 'T')
show(resize_image1, 'result', 'F')
resize_image2 <- resizing(resize_image1, 128, 128)
show(images[['resize2']], 'answer', 'T')
show(resize_image2, 'result', 'F')


### 일곱 번째 과제입니다. 아래에 비어있는 gray_scale 함수를 구현해 주세요!
### 입력으로 변형할 이미지 배열(img)을 받아
### 출력으로 흑백 이미지가 출력되어야 합니다.
### 아래 show(images[['gray_scale']], 'right', 'F')줄을 실행시키면, 정답 이미지를 볼 수 있습니다.
gray_scale <- function(img){

}

gray_image <- gray_scale(pimg)
show(images[['gray_scale']], 'answer', 'T')
show(gray_image, 'result', 'F')


### 여덟 번째 과제입니다. 아래에 비어있는 RGB 함수를 구현해 주세요!
### 입력으로 변형할 이미지 배열(img)과, 이미지 각 픽셀의 색깔을 결정하는 세 가지 값 R, G, B의 비율 값을 받아
### 출력으로 RGB 비율이 변경된 이미지가 출력되어야 합니다.
### 아래 show(images[['RGB_image[n]']], 'right', 'F')줄을 실행시키면, 정답 이미지를 볼 수 있습니다.
RGB <- function(img, R, G, B){

}

RGB_image1 <- RGB(pimg, 1, 0, 0)
show(images[['RGB1']], 'answer', 'T')
show(RGB_image1, 'result', 'F')
RGB_image2 <- RGB(pimg, 0, 1, 0)
show(images[['RGB2']], 'answer', 'T')
show(RGB_image2, 'result', 'F')
RGB_image3 <- RGB(pimg, 0, 0, 1)
show(images[['RGB3']], 'answer', 'T')
show(RGB_image3, 'result', 'F')
RGB_image4 <- RGB(pimg, 0.5, 0.5, 0)
show(images[['RGB4']], 'answer', 'T')
show(RGB_image4, 'result', 'F')
RGB_image5 <- RGB(pimg, 0.1, 0.5, 0.8)
show(images[['RGB5']], 'answer', 'T')
show(RGB_image5, 'result', 'F')
RGB_image6 <- RGB(pimg, 0.8, 0.3, 0.8)
show(images[['RGB6']], 'answer', 'T')
show(RGB_image6, 'result', 'F')

### 이런것도 가능하겠죠?
popart6(RGB_image1, RGB_image2, RGB_image3, RGB_image4, RGB_image5, RGB_image6)


### 아홉 번째 과제입니다. 아래에 비어있는 filtering 함수를 구현해 주세요!
### 입력으로 변형할 이미지 배열(img)와 해당 이미지를 변형시킬 filter를 받아
### 출력으로 필터를 거친 이미지가 출력되어야 합니다.
### http://www.datamarket.kr/xe/index.php?mid=board_jPWY12&page=3&document_srl=24256
### 위의 링크의 "이미지에 필터 합성곱하기"를 참고해주세요!
### p.s. 이미지의 각 값이 0~1에 있지 않으면, image를 출력하지 못하기 때문에, 벗어난 값들에 대해 0과 1로 처리를 해줘야겠죠?
### 아래 show(images[['filtering[n]']], 'right', 'F')줄을 실행시키면, 정답 이미지를 볼 수 있습니다.
filtering <- function(image, filter){

}

### vectorization을 하지 않았을 경우 시간이 많이 오래 걸릴 수 있습니다!
filter1 <- matrix(c(3,0,0,
                    0,-1,0,
                    0,0,-1), ncol = 3, byrow = F)
filtering_image1 <- filtering(pimg, filter1)
show(images[['filtering1']], 'answer', 'T')
show(filtering_image1, 'result', 'F')

filter2 <- matrix(c(-1/8,-1/8,-1/8,
                    -1/8,2,-1/8
                    ,-1/8,-1/8,-1/8), ncol = 3, byrow = F)
filtering_image2 <- filtering(pimg, filter2)
show(images[['filtering2']], 'answer', 'T')
show(filtering_image2, 'result', 'F')

filter3 <- matrix(c(-1,1,1,
                    -1,1,1,
                    -1,-1,1), ncol = 3, byrow = F)
filtering_image3 <- filtering(pimg, filter3)
show(images[['filtering3']], 'answer', 'T')
show(filtering_image3, 'result', 'F')

filter4 <- matrix(c(1,-1,1,
                    -1,1,-1,
                    1,-1,1), ncol = 3, byrow = F)
filtering_image4 <- filtering(pimg, filter4)
show(images[['filtering4']], 'answer', 'T')
show(filtering_image4, 'result', 'F')

filter5 <- matrix(c(0.1,0.1,0.1,
                    0.1,0.2,0.1,
                    0.1,0.1,0.1), ncol = 3, byrow = F)
filtering_image5 <- filtering(pimg, filter5)
show(images[['filtering5']], 'answer', 'T')
show(filtering_image5, 'result', 'F')

### 지금까지 이미지 처리에 대해 실습해보았습니다.
### 위의 과정들은 순수하게 이미지 처리를 하는데에 쓰이기도 하지만(포토샵의 이미지 필터 등),
### 머신러닝 분야에서는 부족한 이미지의 수를 늘리기 위해 사용되기도 합니다.
### 저희가 처리한 모든 이미지들은 각기 다른 모습을 보여도, 분류를 하게 되면 결국 귀여운 강아지로 분류되기 때문입니다.
### 고생하셨습니다!