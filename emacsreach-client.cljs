#!/usr/Bin/env planck -m

(require '[planck.core :as planck]
         '[planck.io :as io]
         '[planck.http :as http]
         '[planck.shell :as shell]
         '[clojure.string :as str]
         '[clojure.set :as set])

(defn pair-env-vars
  "Given an env var in the form \"foo=bar\", return [:foo bar]"
  [envvar]
  (let [pair (str/split envvar #"=")]
    (if (= 2 (count pair))
      [(keyword (first pair)) (second pair)]
      [(keyword (first pair)) true])))

(def *env* (->> (shell/sh "env")
                :out
                (#(str/split % #"\n"))
                (map pair-env-vars)
                (apply concat)
                (apply hash-map)))

(def *user* (:USER *env*))

(def *groups* (-> (shell/sh "groups")
                :out
                (str/split "\n")
                (first)
                (str/split " ")))

(def *machine* (:MACHINE *env*))

(defn path-exists?
  "Determine if the file or directory exists"
  [path]
  (or (io/file? path)
      (io/directory? path)))

(defn can-sudo?
  "Can the current user use sudo?"
  []
  (let [result (shell/sh "sudo" "-vn")]
    (cond (= (:exit result) 0) true
          (= (:err result) "sudo: a password is required\n") true
          :default false)))

(defn file-permissions
  "Get the permissions, user & group for a file"
  [filename]
  (let [attributes (io/file-attributes filename)
        user (:uname attributes)
        group (:gname attributes)
        permissions (:permissions attributes)]
    [permissions user group]))

(defn usable-bitmask
  "Given the user and group of a file, return the read-write bitmask"
  [user group]
  (+ (if (= user *user*) 8r600 0)
     (if (some? group *groups*) 8r60 0)
     6))

(defn read-writable?
  "Given permissions and the rw-bitmask, return whether we have rw"
  [permissions bitmask]
  (let [usable-permissions (bit-and permissions bitmask)
        user (bit-shift-right usable-permissions 6)
        group (bit-and (bit-shift-right usable-permissions 3) 2r111)
        other (bit-and usable-permissions 2r111)
        result (bit-or user group other)]
    (= result 6)))

(defn tramp-prefix
  "Wrap the specified file with an appropriate TRAMP prefix if needed"
  [filename]
  (let [check-path (if (path-exists? filename)
                     filename
                     (-> filename
                         (str/split "/")
                         drop-last
                         (#(str/join "/" %))))
        [permissions user group] (file-permissions check-path)
        bitmask (usable-bitmask user group)
        rw (read-writable? permissions bitmask)
        sudo-spec (if (and (not rw) (can-sudo?))
                    (str "sudo:" *machine*))
        machine-spec (if *machine* (str "ssh:" *machine*))
        tramp-spec (if (or *machine*
                           sudo-spec)
                     (str "/" (str/join "|" (remove nil?
                                                    [machine-spec sudo-spec]))
                          ":"))]
    (str tramp-spec filename)))


(defn parse-args [args]
  (loop [args args
         options {:create false
                  :wait false}]
    (if (empty? args)
      options
      (let [arg (first args)]
        (cond (str/starts-with? arg "--")
              (cond (= arg "--create") (recur (rest args)
                                              (assoc options :create true))
                    (= arg "--no-wait") (recur (rest args)
                                               (assoc options :wait false))
                    (= arg "--wait") (recur (rest args)
                                            (assoc options :wait true))
                    (= arg "--eval") (recur (nthrest args 2)
                                            (assoc options :eval
                                                   (second args)))
                    :default (recur (rest args)
                                    (assoc options :error true)))
              (str/starts-with? arg "-")
              (let [shortopts (rest arg)]
                (recur (if (some #{\e} shortopts)
                         (nthrest args 2)
                         (rest args))
                       (into options [(if (some #{\e} shortopts)
                                        [:eval (second args)])
                                      (if (some #{\n} shortopts)
                                        [:wait false])
                                      (if (some #{\w} shortopts)
                                        [:wait true])
                                      (if (some #{\c} shortopts)
                                        [:create true])
                                      (if (not (empty? (set/difference
                                                        (set shortopts)
                                                        #{\n \c \e \w})))
                                        [:error true])])))
              :default
              (recur (rest args)
                     (assoc options :args (conj (options :args) arg))))))))

(defn build-options
  [options]
  (if options
    (->> options
         (map #(str %1 "=" %2))
         (str/join "&")
         (str "?"))))

(defn emacsreach
  "Call emacsreach API with specified TRAMP spec, file, and options"
  [file options]
  (http/get (str "https://127.0.0.1:36227/"
                 (tramp-prefix file)
                 (build-options options))))

(defn -main []
  (let [parsed-args (parse-args planck/*command-line-args*)
        options (dissoc parsed-args :args)
        args (:args parsed-args)]
    (doseq [file args]
      (emacsreach file options))))
