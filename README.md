# cleanbot
This is a basic path-planning simulator of a robot vacuum cleaner. The aim is to span all tiles in a randomly generated map with an efficient method. The core algorithm is a simple _Boustrophedon Method_. For detailed method, results and future work see the [report](/doc).

## Environment
The application is written in Clojure programming language, runs on JVM. I tested all on **JVM8** and **Ubuntu 18.04 LTS**.

Dependencies _[these will be automatically downloaded from Maven if you are using Leiningen]_:

- Clojure   v1.7.0
- play-clj  v1.1.1 (game library, for UI)
- libGDX    v1.9.3

## Demo
[Here](https://volkanokbay.wixsite.com/cleanbot) is a pseudo product site (presented in the scope of CENG561: Artificial Intelligence course at Middle East Technical University).


https://user-images.githubusercontent.com/97564250/232139285-ef095776-8c36-487a-bb28-ffbcbc9b541b.mp4

_Fig 1: Demonstration on a small map_
