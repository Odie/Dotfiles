(import java.io.File)

;; Get credentials from a GPG encrypted file.
(configure-repositories!
  (let [creds-file (File. (boot.App/bootdir) "credentials.edn")
        ;;creds-data (gpg-decrypt creds-file :as :edn)
        creds-data (clojure.edn/read-string (slurp creds-file))
        ]
		;;(println "creds-file" creds-file)
		;;(println "creds-data" creds-data)
    (fn [{:keys [url] :as repo-map}]
    	;;(println "------------------------------")
    	;;(println "looking for settings for:" url)

      ;;(println "repo-map")
      ;;(clojure.pprint/pprint repo-map)

      ;;(println "(creds-data url)")
      ;;(clojure.pprint/pprint (get creds-data url))

      ;;(println "merged")
      ;;(clojure.pprint/pprint (merge repo-map (creds-data url)))

      (merge repo-map (creds-data url)))))

(set-env! :dependencies #(apply conj % '[[spyscope "0.1.6"]
                                         [zprint "0.4.5"]
																				 [org.clojure/spec.alpha "0.1.143"]]))

;; Enable spyscope
(require 'spyscope.core)
(boot.core/load-data-readers!)

(deftask cider "CIDER profile"
  []
  (require 'boot.repl)
  (swap! @(resolve 'boot.repl/*default-dependencies*)
         concat '[[org.clojure/tools.nrepl "0.2.12"]
                  [cider/cider-nrepl "0.15.1"]
                  [refactor-nrepl "2.3.1"]])
  (swap! @(resolve 'boot.repl/*default-middleware*)
         concat '[cider.nrepl/cider-middleware
                  refactor-nrepl.middleware/wrap-refactor])
  identity)

