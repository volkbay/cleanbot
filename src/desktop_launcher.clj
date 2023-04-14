(ns cleanbot.core.desktop-launcher
  (:require [cleanbot.core :refer :all])
  (:import [com.badlogic.gdx.backends.lwjgl LwjglApplication]
           [org.lwjgl.input Keyboard])
  (:gen-class))

(defn -main
  []
  (LwjglApplication. cleanbot-game "cleanbot" 800 600)
  (Keyboard/enableRepeatEvents true))
