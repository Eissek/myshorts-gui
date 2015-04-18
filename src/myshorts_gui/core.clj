(ns myshorts-gui.core
  (:import [javax.swing JFrame
            JLabel JPanel JButton
            SwingUtilities JMenuBar
            JMenu JMenuItem]
           [javax.accessibility.AccessibleContext])
  (:require [clojure.string :as str]
            [cheshire.core :refer :all]
            [clojure.java.io :refer :all]
            [clojure.pprint :refer [print-table]])
  (:gen-class))


(defn swing
  []
  (let [frame (JFrame. "MyShorts")
        panel (JPanel.)
        button (JButton. "Click Here")
        menubar (JMenuBar.)
        menu (JMenu. "File")
        menu2 (JMenu. "Edit")
        menu3 (JMenu. "Help")
        ]
    (.setSize frame 600 400)
    (.setVisible frame true)
    (.setContentPane frame panel)
    (let [menuItem (JMenuItem. "New Shortcut")
          menuItem2 (JMenuItem. "Save")
          menuItem3 (JMenuItem. "Import")
          menuItem4 (JMenuItem. "Exit")]
      (.add menu menuItem)
      (.add menu menuItem2)
      (.add menu menuItem3)
      (.add menu menuItem4))
    (let [access (.getAccessibleContext menu)]
      (.setAccessibleDescription access "Dunno"))
    (let [menu (JMenu. "Edit")])
    (.add menubar menu)
    (.add menubar menu2)
    (.add menubar menu3)
    (.setJMenuBar frame menubar)
    (.add panel button)
    (.revalidate button)))

(defn gen-uuid
  []
  (java.util.UUID/randomUUID))

(defn load-shortcuts
  [filename])

(defn write-shortcut
  [args])

(def saved-file "shortcuts2.json")

(defn list-shortcuts
  []
  (when (.exists (as-file saved-file))
    (println "Reading file")
    (print-table [:short :desc :tags] (parse-string (slurp saved-file) true))))

(defn store-shortcut
  [args]
  (if (.exists (as-file saved-file))
    (do
      (let [shortcuts
            (parse-string (slurp saved-file) true)]
        (spit saved-file
              (generate-string
               (conj shortcuts args) {:pretty true})))
      (println "Shortcut successfully stored "))
    (do
      (spit saved-file (generate-string [args]))
      (println "Saved to new file")))
  (println "DONE"))


 (defn add-shortcut
 ([]
   (println "Enter:")
   (let [shortcut
         (loop [x (read-line)]
           (if (empty? x)
             (do (println "Enter shortcut")
                 (recur (read-line)))
             (str x)))]
     (println "Enter shortcut Description")
     (let [desc
           (loop [x (read-line)]
             (if (empty? x)
               (do
                 (println "Enter a shortcut description ")
                 (recur (read-line)))
               (str x)))]
       (println "Enter tags(Optional):")
       (let [tags (read-line)]
         (if (empty? tags)
           (println "No tags entered.")
           (println tags))
         (store-shortcut (hash-map
                          :desc desc
                          :id (gen-uuid)
                          :short shortcut
                          :tags tags))))))
 ([shortcut desc tags]
  (store-shortcut (hash-map
                   :desc desc
                   :id (gen-uuid)
                   :short shortcut
                   :tags (if (empty? tags)
                            " "
                          tags)))))


(defn create
  [shortcut desc & tags]
  (str "short "
       shortcut "desc " desc "tags " tags))


(defn read-shortcuts-file
  []
  (parse-string (slurp saved-file) true))

(defn search-shortcuts
  [tag]
  (print-table [:short :desc :tags]
               (filter #(.contains (:tags %) tag)
                       (read-shortcuts-file))))

(defn delete-shortcut
  [args]
  (let [shorts (read-shortcuts-file)]
    (if (empty?
         (filter (comp #{args} :short)
                 shorts))
      (println "Shortcut not found")
      (do
        (let [updated-shorts
              (filter
               #(not= (:short %) args)
               shorts)]
          (spit saved-file
                (generate-string updated-shorts
                                 {:pretty true}))
          (println updated-shorts))
        (println "Shortcut deleted")))))


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!")
  (swing))
