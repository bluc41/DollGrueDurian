(ns adventure.core
  (:require [clojure.core.match :refer [match]]
            [clojure.string :as str])
  (:gen-class))

(def the-map
  {:foyer {:desc "The walls are freshly painted but do not have any pictures.  You get the feeling it was just created for a game or something. How do I escape?"
           :title "in the foyer"
           :dir {:down :basement-main
                 :north :kitchen
                 :east :living-room}
           :contents #{:raw-egg}
           :actions #{:take-egg}}
   :basement-main {:desc "It's just about pitch black but you can make out a couple shapes. There's water coming under the door to the west. To the east lies a large steel door with blood smeared across it. A normal bedroom door lies to south. You hear growling coming from the north. "
                   :title "in the basement"
                   :dir {:north :grue-pen
                         :east :slaughter-entrance
                         :west :pool
                         :south :guest-bedroom}
                   :contents #{}
                   :actions #{}}
   :pool {:desc "It looks extremely refreshing. Something shiny catches your eye at the bottom of the pool. "
          :title "in the pool room"
          :dir {:east :basement-main}
          :actions #{:dive}
          :contents #{:shield}}
   :slaughter-entrance {:desc "There is a second door that appears to be locked. You need some sort of key to get in. "
                        :title "in the entrance to the slaughter house"
                        :dir {:west :basement-main}
                        :actions #{:unlock-door}}
   :slaughter-main {:desc "A metallic, cloying scent of blood fills your nostrils. You observe a large amount of severed limbs sprawled across the room. You spot a cleaver on the table. This could be useful. "
                    :title "in the main slaughter room"
                    :dir {:west :slaughter-entrance}
                    :contents #{:cleaver}
                    :actions #{:grab-cleaver}}
   :guest-bedroom {:desc "It is fairly clean. You notice a small safe in the corner of the room. "
                   :title "in the guest bedroom"
                   :dir {:north :basement-main}
                   :actions #{:crack-safe}
                   :contents #{:slaughterKey}}

   :grue-pen {:desc "It is very dark. The growling grows louder. A grue spawns and runs towards you. "
              :title "in the grue pen"
              :dir {:south :basement-main}
              :contents #{:grue-heart}
              :actions #{:fight}}

    ;first floor
   :kitchen {:desc "There are so many things here to eat!"
             :title "in the kitchen"
             :dir {:south :foyer
                   :west :piano-room
                   :east :wine-cellar
                   :north :dining-room}
             :contents #{:durian}
             :actions #{:eat-apple :eat-orange :take-durian}}
   :wine-cellar {:desc "So much wine in one room! Weird that the cellar isn't in the basement. "
                 :title "in the wine cellar"
                 :dir {:west :kitchen}
                 :contents #{:empty-bottle :wine-casket}
                 :actions #{:open-casket}}
   :living-room {:desc "There are many cabinets here. Something valuable might be inside one of them. "
                 :title "in the living room"
                 :dir {:west :foyer}
                 :contents #{:recipe}
                 :actions #{:search-cabinets}}
   :piano-room {:desc "There is a beautiful 9 foot grand piano in the middle of the room. "
                :title "in the piano room"
                :dir {:east :kitchen}
                :contents #{:piano}
                :actions #{:play-piano}}
   :alchemy-room {:desc "There is an alchemy table in the middle of the room. "
                  :title "in the alchemy room"
                  :dir {:south :piano-room}
                  :contents #{}
                  :actions #{:use-table}}
   :dining-room {:desc "There is a large wooden table and some chicken on the table. "
                 :title "in the dining room"
                 :dir {:south :kitchen}
                 :contents #{:chicken}
                 :actions #{:eat-chicken :take-chicken}}})


(def winPotion #{:grue-heart :chicken :durian :wine-bottle :raw-egg})


(defn safeAction [player]
    (cond
        (not= (player :location) :guest-bedroom) (do (println "You can't do that here.") player)
        (contains? (player :inventory) :slaughterKey) (do (println "You already have the key!") player)
        (not (contains? (player :inventory) :slaughterKey))
        (do (println "The screen on the safe flashes: What direction should you had to go back to the basement?")
            (let [answer (read-line)]
                (if (= answer "north")
                    (do (println "The safe opens! You find a key inside and place it in your pocket.")
                        (update-in player [:inventory] #(conj % :slaughterKey)))
                    (do (println "The safe remains locked.") player))))))


(defn options [player]
    (let [location (player :location)
            neighbors (-> the-map location :dir)
            directions (keys neighbors)
            actions (-> the-map location :actions)]
        (do (println (concat directions actions)) player)))



(def adventurer
  {:location :foyer
   :inventory #{}
   :tick 0
   :seen #{}})

(defn status [player]
  (let [location (player :location)]
    (print (str "You are " (-> the-map location :title) ". "))
    (when-not ((player :seen) location)
      (print (-> the-map location :desc)))
    (update-in player [:seen] #(conj % location)))) ;takes  a function whereas assoc-in takes value

(defn to-keywords [commands]
  (mapv keyword (str/split commands #"[.,?! ]+")))

(defn go [dir player]
  (let [location (player :location)
        dest (->> the-map location :dir dir)]
    (if (nil? dest)
      (do (println "You can't go that way.")
          player)
      (assoc-in player [:location] dest))))

(defn tock [player]
  (update-in player [:tick] inc))

(defn slaughterAction [player]
    (cond
        (not= (player :location) :slaughter-entrance) (do (println "You can't do that here.") player)
        (contains? (player :inventory) :slaughterKey) (assoc-in player [:location] :slaughter-main)
        (not (contains? (player :inventory) :slaughterKey)) (do (println "The way is shut.") player)))


(defn grabItem [player]
    (let [location (player :location)
            items (-> the-map location :contents)
            item (first items)]
        (if (contains? (player :inventory) item)
            (do (println (str "You already have the " (name item) "!")) player)
            (do (println (str "You pick up the " (name item) " and place it in your inventory."))
                (update-in player [:inventory] #(conj % item))))))

(defn fightGrue [player]
    (let [inventory (player :inventory)]
        (cond
            (not= (player :location) :grue-pen) (do (println "You can't do that here.") player)
            (and (contains? inventory :cleaver) (contains? inventory :shield)) (do (println "You kill the grue and carve out its heart.") (update-in player [:inventory] #(conj % :grue-heart)))
            (contains? inventory :cleaver) (do (println "You kill the grue but it also mortally wounds you in the process. ") (System/exit 0))
            (contains? inventory :shield) (do (println "How do you kill this thing? The grue's hide is too tough to punch through and it eventually wears down your shield and kills you.") (System/exit 0))
            :else (do (println "You are completely unarmed. The grue destroys you.") (System/exit 0)))))

;;TESTING PURPOSES, REMOVE this
(defn addItem [player]
    (let [item (read-line)]
        (update-in player [:inventory] #(conj % (keyword item)))))

(defn removeItem [player]
    (let [item (read-line)]
        (update-in player [:inventory] #(disj % (keyword item)))))

;BUG if you already have the recipe
(defn searchCabinets [player]
    (if (= (player :location) :living-room)
        (do (println "Which one do you want to search? 1, 2, or 3?")
            (let [cabNumber (read-line)]
                (if (= cabNumber "3")
                    (do (println "You found a strange recipe! You place the recipe in your inventory.")
                        (update-in player [:inventory] #(conj % :recipe)))
                    (do (println "Nothing here.") player))))
        (do (println "You can't do that here.") player)))


(defn readRecipe [player]
    ;check that you have it
    (if (contains? (player :inventory) :recipe)
        (do (println "Recipe:
                    Heart of Beast
                    Chicken Leg
                    Odorous Fruit
                    Raw Egg
                    Wine\n

                    Hint: D Major Chord"
                     player))
        (do (println "What recipe?") player)))


(defn eatChicken [player]
    (if (contains? (player :inventory) :chicken)
        (do (println "I might need this later. Eat anyway? Y/N")
            (let [ans (read-line)]
                (if (= ans "Y")
                    (do (println "Delicious!")
                        (update-in player [:inventory] #(disj % :chicken)))
                    (do (println "I feel like I made the right choice.") player))))
        (do (println "What chicken?") player)))

(defn openCasket [player]
    (if (= (player :location) :wine-cellar)
        (if (contains? (player :inventory) :cleaver)
            (do (println "You use your cleaver to open the casket and pour some wine into one of the empty bottles.")
                (update-in player [:inventory] #(conj % :wine-bottle)))
            (do (println "You can't seem to open the wine casket. Perhaps you need something large and sharp to open it.") player))
        (do (println "You can't do that here.") player)))

(defn playPiano [player]
    (if (= (player :location) :piano-room)
        (do (println "What notes should you play? (i.e ceg#a#)")
            (let [chord (read-line)]
                (if (= chord "df#a")
                    (do (println "The wall behind the piano swings open and you step in!")
                        (assoc-in player [:location] :alchemy-room))
                    (do (println "That didn't seem to do anything. Maybe something will tell me what to play?") player))))
        (do (println "You can't do that here.") player)))

(defn useTable [player]
    (if (= (player :location) :alchemy-room)
        (do (println "What should you mix? (space separated)")
            (let [ingredients (map #(keyword %) (str/split (read-line) #"\s+"))]
                (if (every? (player :inventory) ingredients)
                    (if (= (set ingredients) winPotion)
                        (do (println "The pot bubbles and turns to a golden hue! You drink the potion and immediately teleport out! You win!.")
                            (System/exit 0))
                        (do (println "The pot bubbles and turns to a cloudy black color. You drink the potion and you turn into a grue!")
                            (System/exit 0)))
                    (do (println "You don't have all of those.") player))))
        (do (println "You can't do that here.") player)))




;returns player
(defn respond [player command]
  (match command
         [:look] (update-in player [:seen] #(disj % (-> player :location)))
         (:or [:n] [:north] ) (go :north player)
         [:south] (go :south player)
         [:west] (go :west player)
         [:east] (go :east player)
         [:down] (go :down player)
         [:up] (go :up player)
         ;utilities
         [:i] (do (println (player :inventory)) player)
         [:o] (options player)
         [:addItem] (addItem player)
         [:removeItem] (removeItem player)

         ;action
         [:crack-safe] (safeAction player)
         [:unlock-door] (slaughterAction player)
         [:grab-cleaver] (grabItem player)
         [:dive] (grabItem player)
         [:fight] (fightGrue player)


         ;first floor actions
         [:take-egg] (grabItem player)
         [:search-cabinets] (searchCabinets player)
         [:eat-apple] (do (println "It's poisonous! Your vision fades and everything turns black.") (System/exit 0))
         [:eat-orange] (do (println "It's delicious! You feel refreshed.") player)
         [:take-durian] (grabItem player)
         [:read-recipe] (readRecipe player)
         [:take-chicken] (grabItem player)
         [:eat-chicken] (eatChicken player)
         [:open-casket] (openCasket player)
         [:play-piano] (playPiano player)
         [:use-table] (useTable player)


         _ (do (println "I don't understand you.")
               player)))




(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (loop [local-map the-map
         local-player adventurer]
    (let [pl (status local-player)
          _  (println "What do you want to do?")
          command (read-line)]
      (recur local-map (respond pl (to-keywords command))))))
