(ns myshorts-gui.core
  (:import [javax.swing JFrame JScrollPane
            JList Box KeyStroke JTextField
            JLabel JPanel JButton JOptionPane
            SwingUtilities JMenuBar
            JMenu JMenuItem BorderFactory
            BoxLayout JTable]
           [javax.swing.event DocumentListener DocumentEvent]
           [java.awt Dimension BorderLayout]
           [javax.swing.border TitledBorder]
           [java.awt.event ActionListener
            WindowListener ItemListener
            KeyEvent ActionEvent]
           [javax.accessibility.AccessibleContext])
  (:require [clojure.string :as str]
            [cheshire.core :refer :all]
            [clojure.java.io :refer :all]
            [clojure.pprint :refer [print-table]])
  (:gen-class))


(defn say-hello []
  (println "ysh")
  (System/exit 0))

(defn filter-tags []
  (let [frame (JFrame. "test")]
    (doto frame
      (JOptionPane.
       (.showMessageDialog "Hi")))))

(defn act [func]
  (reify ActionListener
    (actionPerformed [this event]
      (func))))

(defn doc-listener [func]
  (reify DocumentListener
    (insertUpdate [this event] (func))
    (removeUpdate [this event] (func))
    (changedUpdate [this event ])))


(def select-values (comp vals select-keys))
(def column-names ["Shortcut" "Description" "Tag"])



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


(defn swing
  []
  (let [frame (JFrame. "MyShorts")
        panel (JPanel.)
        list-panel (JPanel.)
        button-panel (JPanel.)
        filter-panel (JPanel.)
        button (JButton. "Add")
        menubar (JMenuBar.)
        menu (JMenu. "File")
        menu2 (JMenu. "Edit")
        menu3 (JMenu. "Help")]
    
    (doto frame
      (.setSize 600 400)
      (.setVisible true)
      (.setContentPane panel)
      (.setResizable false)
      (.setDefaultCloseOperation
       JFrame/DISPOSE_ON_CLOSE))
    

    (let [menuItem (JMenuItem. "New Shortcut")
          menuItem2 (JMenuItem. "Save")
          menuItem3 (JMenuItem. "Import")
          menuItem4 (JMenuItem. "Exit")]

      (.setAccelerator menuItem
                       (KeyStroke/getKeyStroke
                        (KeyEvent/VK_N)
                        (ActionEvent/CTRL_MASK)))
      (.setAccelerator menuItem2
                       (KeyStroke/getKeyStroke
                        (KeyEvent/VK_S)
                        (ActionEvent/CTRL_MASK)))
      (.setMnemonic menu
                    (KeyEvent/VK_F))
      (.setMnemonic menuItem4
                    (KeyEvent/VK_X))
      (.addActionListener menuItem4 (act say-hello))
      (.add menu menuItem)
      (.addSeparator menu)
      (.add menu menuItem2)
      (.add menu menuItem3)
      (.setEnabled menuItem3 false)
      (.addSeparator menu)
      (.add menu menuItem4))

    (let [access (.getAccessibleContext menu)]
      (.setAccessibleDescription access "Dunno"))
    
    (let [menuItem (JMenuItem. "Undo")]
      (.add menu2 menuItem)
      (.setEnabled menuItem false))
    
    (let [menuItem (JMenuItem. "Quick Guide")
          menuItem2 (JMenuItem. "About")]
      (.add menu3 menuItem)
      (.setEnabled menuItem false)
      (.addSeparator menu3)
      (.add menu3 menuItem2))
    
    (.add menubar menu)
    (.add menubar menu2)
    (.add menubar menu3)
    (.setJMenuBar frame menubar)
    
    (.add button-panel button)
    (let [edit (JButton. "Edit")]
      (.add button-panel edit))
    (let [delete (JButton. "Delete")]
      (.add button-panel delete))
    
    (doto button-panel
      (.add (Box/createRigidArea (Dimension. 10 0)))
      (.setLayout (BoxLayout. button-panel BoxLayout/X_AXIS)))
    (let [scroll-pane (JScrollPane.
                       (JTable. (to-array-2d (read-shortcuts-file))
                                (into-array column-names)))])
    
    (doto filter-panel
      (.setPreferredSize (Dimension. 450 30))
      (.add (Box/createRigidArea (Dimension. 10 0)))
      (.add (let [filter-label (JLabel. "Filter Tags:")]
              (doto filter-label
                (.setHorizontalTextPosition (JLabel/CENTER)))))
      (.add (let [filter-field (JTextField. 30)]
              (.addDocumentListener (.getDocument filter-field) (doc-listener say-hello))
              (doto filter-field
                (.revalidate)))))
    
    (doto list-panel
      (.add (Box/createRigidArea (Dimension. 10 0)))
      (.setLayout (BoxLayout. list-panel BoxLayout/Y_AXIS))
      (.setBorder
       (BorderFactory/createTitledBorder "Title"))
      (.add (JScrollPane.
             (JTable.
              (to-array-2d (map #(reverse (select-values % [:short :desc :tags])) (read-shortcuts-file)))
              (into-array column-names))))
      (.setPreferredSize (Dimension. 560 271)))
    
    (doto panel )
    (.add (.getContentPane frame) button-panel
          (BorderLayout/PAGE_START))
    (.add (.getContentPane frame) filter-panel)
    (.add (.getContentPane frame) list-panel
                           BorderLayout/CENTER)
    (.revalidate button)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!")
  (swing))
