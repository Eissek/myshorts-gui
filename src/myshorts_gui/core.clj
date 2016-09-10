(ns myshorts-gui.core
  (:import [javax.swing JFrame JScrollPane
            JList Box KeyStroke JTextField
            JLabel JPanel JButton JOptionPane
            SwingUtilities JMenuBar JTextArea
            JMenu JMenuItem BorderFactory
            BoxLayout JTable JLayeredPane SwingConstants]
           [javax.swing.event DocumentListener
            TableModelListener DocumentEvent]
           [java.awt Dimension BorderLayout Color Font]
           [javax.swing.border TitledBorder Border]
           [java.awt.event ActionListener
            WindowListener ItemListener 
            KeyEvent ActionEvent]
           [javax.swing.table TableModel DefaultTableModel]
           [javax.accessibility.AccessibleContext])
  (:require [clojure.string :as str]
            [cheshire.core :refer :all]
            [clojure.java.io :refer :all]
            [clojure.pprint :refer [print-table]])
  (:gen-class))


(def select-values (comp vals select-keys))
(def column-names ["Shortcut" "Description" "Tag"])
(def saved-file "shortcuts2.json")
(defn read-shortcuts-file
  []
  (parse-string (slurp saved-file) true))

(defn search-shortcuts
  [tag]
  [:short :desc :tags]
               (filter #(.contains (:tags %) tag)
                       (read-shortcuts-file)))


(defn apply-filter [arg panel tbl filter-field]
  ;; (.setVisible panel false)
  ;; (.setDataVector (.getModel tbl)
                  ;; (to-array-2d (map #(reverse (select-values % [:short :desc :tags])) (search-shortcuts "emacs")))
                  ;; (into-array column-names) )
  (.remove (.getViewport panel) tbl )
  
  ;; (if (.isValid (.getViewport panel))
    ;; (.remove (.getViewport panel) (JTable.)))

  (.remove (.getViewport panel) (JTable.))
  (.add (.getViewport panel) (JTable.
                              (to-array-2d (map #(reverse (select-values % [:short :desc :tags])) (search-shortcuts (.getText filter-field))))
                              (into-array column-names)))
  (.repaint arg))

(defn scroll-pane
  ([]
   (JScrollPane.
    (JTable.
     (to-array-2d (map #(reverse (select-values % [:short :desc :tags])) (read-shortcuts-file)))
     (into-array column-names))))
  
  ([filter-field pane arg]
   (.remove (.getViewport pane) (JTable.))
   (.add (.getViewport pane) (JTable.
                              (to-array-2d (map #(reverse (select-values % [:short :desc :tags])) (search-shortcuts (.getText filter-field))))
                              (into-array column-names)))
     (.repaint arg)))

(defn say-hello []
  (println "ysh")
  (System/exit 0))

(defn filter-tags []
  (let [frame (JFrame. "test")]
    (doto frame
      (JOptionPane.
       (.showMessageDialog "Hi")))))

(defn act
  ([func]
   (reify ActionListener
     (actionPerformed [this event]
       (func))))
  ([func arg1 arg2 arg3 text]
   (reify ActionListener
     (actionPerformed [this event]
       (func arg1 arg2 arg3 text)))))

(defn doc-listener
  ([func]
   (reify DocumentListener
     (insertUpdate [this event] (func))
     (removeUpdate [this event] (func))
     (changedUpdate [this event ])))
  ([func query panel listpanel]
   (reify DocumentListener
     (insertUpdate [this event] (func query panel listpanel))
     (removeUpdate [this event] (func query panel listpanel))
     (changedUpdate [this event ]))))


(defn gen-uuid
  []
  (java.util.UUID/randomUUID))

(defn load-shortcuts
  [filename])

(defn write-shortcut
  [args])



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




(defn display-status [label]
  (future (Thread/sleep 1000)
          (.setVisible label false)))


(defn add-short-window
  [table]
  (let [add-frame (JFrame. "Add Shortcut")
        layered-pane (JLayeredPane.)
        panel (JPanel.)
        status-label (JLabel. "Added Shortcut")
        top-panel-left (JPanel.)
        bottom-panel (JPanel.)
        scroll-pane (JScrollPane.)]

    (doto add-frame
      (.setSize 450 240)
      (.setVisible true)
      (.setContentPane panel)
      (.setDefaultCloseOperation
       JFrame/DISPOSE_ON_CLOSE)
      (.add layered-pane (.getContentPane add-frame))
      (.add bottom-panel (.getContentPane add-frame)))

    
    
    (doto panel
      (.setLayout (BoxLayout. panel  BoxLayout/Y_AXIS)))

    (let [shortcut-field (JTextField. 12)
          tags-field (JTextField. 12)
          desc-area (JTextArea. 5 37)
          desc-label (JLabel. "Shortcut Description:")
          short-label (JLabel. "Shortcut:")
          tag-label (JLabel. "Tags:")]
      (.setPreferredSize shortcut-field (Dimension. 10 25))
      (.setPreferredSize tags-field (Dimension. 10 25))

      (doto layered-pane
        (.setPreferredSize (Dimension. 300 200))
        ;; (.setBorder (BorderFactory/createLineBorder Color/BLACK))
        (.add short-label (Integer. 1))
        (.add shortcut-field (Integer. 2))
        ;; (.add (Box/createRigidArea (Dimension. 10 0)) 1)
        (.add tag-label 1)
        (.add tags-field (Integer. 2))        
        (.add desc-label 1)
        (.add (.add (.getViewport scroll-pane)
                    desc-area)scroll-pane)
        (.add status-label (Integer. 3)))

      (doto status-label
        (.setBounds 130 30 160 30)
        (.setOpaque true)
        (.setHorizontalAlignment SwingConstants/CENTER)
        (.setBackground Color/GREEN)
        (.setFont (Font. "Arial" Font/BOLD 16))
        (.setVisible false))
      
      (.setBounds short-label 11 3 80 20)
      (.setBounds shortcut-field 10 25 200 30)
      (.setBounds tag-label 231 3 80 20)
      (.setBounds tags-field   230 25 200 30)
      (.setBounds desc-label 11 55 189 20)
      (.setBounds desc-area 10 76 420 95)
      
      (.setHorizontalTextPosition desc-label (JLabel/LEADING))
      (.setPreferredSize desc-area  (Dimension. 300 200))
      (.setBorder desc-area (BorderFactory/createLineBorder Color/BLACK))

      (let [add-button (JButton. "Add")
          close-button (JButton. "Close")]
      (doto bottom-panel
        (.setPreferredSize (Dimension. 50 40))
        (.add add-button)
        (.add close-button))

      (doto add-button
        (.addActionListener
         (reify ActionListener
           (actionPerformed [this event]
             (.insertRow (.getModel table) 0
                         (to-array
                          (vector (str (.getText shortcut-field))
                           (str (.getText desc-area))
                           (str (.getText tags-field)))
                          ))
             (add-shortcut
              (str (.getText shortcut-field))
              (str (.getText desc-area))
              (str (.getText tags-field)))
             (.setText shortcut-field "")
             (.setText desc-area "")
             (.setText tags-field "")
             (.setVisible status-label true)
             (display-status status-label)))))
      
      (doto close-button
        (.addActionListener (reify ActionListener
                              (actionPerformed [this event]
                                (.setVisible add-frame false)
                                (.dispose add-frame)))))))))

(def table-shortcuts-list
  (JTable.
   (to-array-2d (map #(reverse (select-values % [:short :desc :tags])) (search-shortcuts "")))
   (into-array column-names)))

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
        menu3 (JMenu. "Help")
        filter-field (JTextField.  20)
        model (DefaultTableModel.
                (to-array-2d (map #(reverse (select-values % [:short :desc :tags])) (search-shortcuts "")))
                (into-array column-names))
        table (JTable. model)
        scroll-pane2 (JScrollPane.)
        filter-button (JButton. "Apply")]
    
    (doto frame
      (.setSize 600 400)
      (.setVisible true)
      (.setContentPane panel)
      (.setResizable false)
      (.setDefaultCloseOperation
       JFrame/DISPOSE_ON_CLOSE))

    (.setFillsViewportHeight table true)

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
      (.add button-panel edit)
      (.setEnabled edit false))
    (let [delete (JButton. "Delete")]
      (.add button-panel delete)
      (doto delete
        (.addActionListener
         (reify ActionListener
           (actionPerformed [this event]
             (delete-shortcut
              (.getValueAt table
                           (.getSelectedRow table)
                           (.getSelectedColumn table)))
             (.removeRow (.getModel table) (.getSelectedRow table))
             (.revalidate panel)
             (.repaint panel))))))
    
    (doto button-panel
      (.add (Box/createRigidArea (Dimension. 10 0)))
      (.setLayout (BoxLayout. button-panel BoxLayout/X_AXIS)))

      (doto button
        (.addActionListener
         (reify ActionListener
           (actionPerformed [this event]
             (add-short-window table)))))
      
    (doto filter-panel
      (.setPreferredSize (Dimension. 450 30))
      (.add (Box/createRigidArea (Dimension. 10 0)))
      (.add (let [filter-label (JLabel. "Filter Tags:")]
              (doto filter-label
                (.setHorizontalTextPosition (JLabel/CENTER)))))
      (.add filter-field
            (.addDocumentListener
             (.getDocument filter-field)
             (doc-listener scroll-pane filter-field scroll-pane2 list-panel)))
      
      (.add filter-button
            (.addActionListener filter-button
                                (act apply-filter list-panel scroll-pane2 table filter-field))))


    (.add (.getViewport scroll-pane2) table)
    
    (doto list-panel
      (.add (Box/createRigidArea (Dimension. 10 0)))
      (.setLayout (BoxLayout. list-panel BoxLayout/Y_AXIS))
      (.setBorder
       (BorderFactory/createTitledBorder "Title"))
       (.add scroll-pane2)

      (.setPreferredSize (Dimension. 560 271)))
    
    (doto panel )
    (.add (.getContentPane frame) button-panel
          (BorderLayout/PAGE_START))
    (.add (.getContentPane frame) filter-panel)
    (.add (.getContentPane frame) list-panel
                           BorderLayout/CENTER)
    (.repaint scroll-pane2)
    (.revalidate button)))



(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  ;; (println "Hello, World!")
  (swing))
