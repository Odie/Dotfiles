;; (deftask cider "CIDER profile"
;;   []
;;   (require 'boot.repl)
;;   (swap! @(resolve 'boot.repl/*default-dependencies*)
;;          concat '[[org.clojure/tools.nrepl "0.2.12"]
;;                   [cider/cider-nrepl "0.10.0"]
;;                   [refactor-nrepl "2.0.0-SNAPSHOT"]])

;;   (swap! @(resolve 'boot.repl/*default-middleware*)
;;          concat '[cider.nrepl/cider-middleware
;;                   refactor-nrepl.middleware/wrap-refactor])

;;   identity)

;; Add spyscope
(set-env! :dependencies #(conj % '[spyscope "0.1.6"]))
(require 'spyscope.core)
(boot.core/load-data-readers!)

;; Make sure nrepl and cider is configured
(require 'boot.repl)
(swap! boot.repl/*default-dependencies*
       concat '[[refactor-nrepl "2.2.0"]
                [cider/cider-nrepl "0.14.0"]])

(swap! boot.repl/*default-middleware*
       conj 'cider.nrepl/cider-middleware
            'refactor-nrepl.middleware/wrap-refactor)
