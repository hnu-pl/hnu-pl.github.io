---
layout: post
title: IHaskell 도커 구동 스크립트
categories:
- Haskell
feature_image: "https://picsum.photos/id/872/1300/200"
---

지난번에는 주피터 노트북 하스켈 커널인 IHaskell을 도커(Docker) 이미지로 활용하기 위한 설치 방법을
WSL2를 기준으로 [안내하는 글](/haskell/2019/07/19/ihaskell-wsl2/)을 작성했다.
그렇게 설치된 환경에서 긴 도커 명령어와 옵션을 매번 기억할 필요 없이, 그리고 도커의 휘발성 데이타 문제를
직접 신경쓰지 않고, 미리 작성된 스크립트로 간편하게
[crosscompass/ihaskell-notebook](https://github.com/jamesdbrock/ihaskell-notebook)
도커 이미지를 구동과 필요한 활용할 LTS 라이브러리 패키지를 설치를 돕는 템플릿을
아래 [GitHub 저장소](https://github.com/hnu-pl/ihaskell-pwd)에 정리해 놓았다.

 * [https://github.com/hnu-pl/ihaskell-pwd]

자세한 설명은 생략하고 일단 실행 명령 및 스크린샷을 중심으로 간략한 사용방법만 알아보자.
이미 도커 서비스가 설치된 WSL2를 기준으로 안내한다.

우선 깃헙 저장소의 내용을 복제하고 디렉토리에 들어간다.
```
kyagrd@kya18LG:~/work$ git clone https://github.com/hnu-pl/ihaskell-pwd
Cloning into 'ihaskell-pwd'...
remote: Enumerating objects: 79, done.
remote: Counting objects: 100% (79/79), done.
remote: Compressing objects: 100% (62/62), done.
remote: Total 79 (delta 40), reused 43 (delta 15), pack-reused 0
Unpacking objects: 100% (79/79), done.
kyagrd@kya18LG:~/work$ cd ihaskell-pwd/
kyagrd@kya18LG:~/work/ihaskell-pwd$ ls
install.sh  LambaCalc.ipynb  LICENSE  README.md  run.sh  Setup.hs  wsl2browser.sh
```
도커 서버 구동을 위해서는 `run.sh` 명령을 사용한다.
아래는 `run.sh` 까지 실행시킨 WSL2 명령창의 스크린샷이다.

![run.sh](/assets/blog/ihaskell-pwd-01.png)

위의 창은 그대로 두고 새로운 WSL2 창을 열어서 웹브라우저에서 접속할 주소를 `ip address show dev eth0` 명령으로 알아보자.

![ip addr](/assets/blog/ihaskell-pwd-02.png)

이제 위에 표시된 주소의 8888 포트 즉 위의 화면 기준으로는 172.30.166.63:8888을 웹브라우저 주소창에 입력하여 접속하면 된다.
이렇게 명령어로 주소를 알아내 직접 주소창에 입력하는 번거로움을 덜어주기 위해,
일반적인 위치에 구글 크롬 브라우저가 설치된 윈도우즈 10의 경우 WSL2 명령창에서 바로 실행하면
크롬 브라우저를 이 주소로 접속시켜 주는 `wsl2browser.sh` 스크립트도 마련해 놓았다.
크롬 브라우저를 기본 설정으로 설치한 경우라면 바로 이 명령어만 입력해도 아래와
같이 웹브라우저로 방금 전에 구동한 주피터 서버에 접속할 수 있다.

![jupyter login](/assets/blog/ihaskell-pwd-03.png)

토큰으로 x를 입력하고 `Log in`하면 아래와 같이 주피터 서버를 사용하기 시작할 수 있다.

![jupyter dirs](/assets/blog/ihaskell-pwd-04.png)

위 화면에 가장 위의 `ihaskell_examples` 디렉토리는 IHaskell 개발자들이 제공하는 예제를 가져다 놓은 디렉토리이고
가장 아래의 `work`는 그냥 빈 디렉토리이며
`pwd` 디렉토리가 바로 도커 외부와 연동된 바로 리눅스에서 `run.sh`를 실행한 디렉토리다. 클릭하여 `pwd` 디렉토리로
들어가면 아래와 같이 우리가 도커를 실행하면서 본 디렉토리 내용이 그대로 나오는 것을 확인할 수 있다. 그래서
이곳에다 작업할 노트북 파일을 만들면 도커가 종료되더라도 도커 외부인 리눅스 디렉토리에 그대로 작업 내용이 남아있게 된다.

![jupyter pwd](/assets/blog/ihaskell-pwd-05.png)

이 글은 최대한 단순하고 간편하게 IHaskell 도커 이미지를 활용할 수 있도록 안내하는 내용이다.
더 다양한 방법으로 (예를 들면 `work` 디렉토리도 리눅스상의 다른 디렉토리와 연동해 보고 싶다던지) 활용하려면
[crosscompass/ihaskell-notebook](https://github.com/jamesdbrock/ihaskell-notebook)의 문서와 소스코드를 참고하라.
