(ns schema.client.objc
  (:use plumbing.core)
  (:require [clojure.string :as str]
            [schema.core :as s]
            [schema.utils :as s])
  (:import [schema.core EnumSchema NamedSchema OptionalKey]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Analysis

(s/defschema StorageAttribute
  (s/enum :strong :weak :readWrite :copy))

(s/defschema ObjcAtomType
  (s/enum :NSInteger :long-long :NSString :CGFloat))

(s/defschema NamedEnumSchema
  (s/both NamedSchema (s/pred #(instance? EnumSchema (:schema %)))))

(s/defschema ObjcType
  (s/either ObjcAtomType NamedEnumSchema String))

(s/defschema Property
  {:name String
   :type ObjcType
   :storage StorageAttribute
   :atomic? Boolean
   :optional? Boolean})

(s/defschema ClassSpec
  {:class-name String
   :dynamic-enums [NamedEnumSchema]
   :properties [Property]})

(defn lisp->camel-case [sym]
  (str/replace (name sym) #"(-[a-z])"
               (fn [[^String c]] (.toUpperCase (.substring c 1)))))

(s/defn clj->objc-type :- ObjcType
  [schema]
  (cond
   (instance? NamedSchema schema) (utils/safe-get schema :name)
   (instance? EnumSchema schema) (s/named schema nil) ;; NamedEnumSchema
   (= String schema) :NSString
   (#{Integer int} schema) :NSInteger
   (#{Long long} schema) :long-long
   (#{Float Double} schema) :CGFloat))

(s/defn object? :- Boolean
  [objc-type :- ObjcType]
  ;; TODO: pretty this up
  (and (s/check NamedEnumSchema objc-type)
       (not (#{:long-long} objc-type))))

(s/defn analyze-property :- Property
  [[k v]]
  (let [optional? (instance? OptionalKey k)
        k (if optional? (utils/safe-get k :k) k)
        t (clj->objc-type v)
        o? (object? t)]
    {:name (lisp->camel-case k)
     :type t
     :object? o?
     :atomic? false
     :storage (when o? :strong)
     :optional? optional?}))

(s/defn analyze-map :- ClassSpec
  [schema]
  (assert (instance? NamedSchema schema))
  {:class-name (utils/safe-get schema :name)
   :properties (map analyze-property (utils/safe-get schema :schema))})

(s/defn referenced-classes :- [String]
  [class-spec :- ClassSpec]
  (set (filter string? (map :type (:properties class-spec)))))

(s/defn pull-out-enums :- ClassSpec
  [{:keys [properties class-name] :as class-spec} :- ClassSpec]
  "Pulls out ObjcEnum types into :dynamic-enums"
  (let [enum-name (fn [p] (lisp->camel-case (str class-name "-" (:name p))))]
    (assoc class-spec
      :properties
      (for [{:keys [type] :as p} properties]
        (if (not (s/check NamedEnumSchema type))
          (assoc p :type (s/named (:schema type) (enum-name p)))
          p))
      :dynamic-enums
      (for [{:keys [type] :as p} properties
            :when (not (s/check NamedEnumSchema type))]
        (s/named (:schema type) (enum-name p))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Class Emission

(s/defn property-type-name :- String
  [property :- Property]
  "Generates the class name for a given Prop"
  (let [property-type (utils/safe-get property :type)]
    (if (not (s/check NamedEnumSchema property-type))
      (:name property-type)
      (name property-type))))

(s/defn obj-reference :- String
  [property :- Property]
  (let [object? (utils/safe-get property :object?)]
    (str (.replace (property-type-name property) "-" " ") (when object? " *"))))

(s/defn emit-property :- String
  [property :- Property]
  (let [property-name (name (utils/safe-get property :name))]
    (format
     "@property (%s%s) %s %s;"
     (if (utils/safe-get property :atomic?)
       "atomic"
       "nonatomic")
     (if-let [storage (utils/safe-get property :storage)]
       (str "," (name storage))
       "")
     (obj-reference property)
     (str property-name))))

(s/defn emit-dynamic-enum :- String
  [enum-schema :- NamedEnumSchema]
  (let [enum-names (->> (utils/safe-get-in enum-schema [:schema :vs])
                        (map #(lisp->camel-case (str (:name enum-schema) "-" (name %)))))]
    (format
     "typedef enum {\n%s\n} %s;"
     (str/join ",\n" (map #(str "  " %) enum-names))
     (utils/safe-get enum-schema :name))))

(s/defn from-dict-declaration :- String
  [class-name :- String]
  (format "+ (%s *) fromDictionary:(NSDictionary *)data" class-name))

(s/defn emit-h :- String
  [class-spec :- ClassSpec]
  (let [class-name (utils/safe-get class-spec :class-name)
        sb (StringBuilder.)]
    ;; Authorship

    ;; Import Referenced Classes
    (doseq [c (referenced-classes class-spec)]
      (.append sb (format "#import \"%s.h\"\n" c)))

    (.append sb "\n")

    ;; Dynamic Enums
    (doseq [dynamic-enum (utils/safe-get class-spec :dynamic-enums)]
      (.append sb (emit-dynamic-enum dynamic-enum)))

    (.append sb "\n\n")

    ;; Interface
    (doto sb
      (.append (format "@interface %s : NSObject" class-name)))

    (.append sb "\n\n")

    ;; Properties
    (doseq [^Property p (utils/safe-get class-spec :properties)]
      (.append sb (str (emit-property p) "\n")))

    (.append sb "\n")

    ;; fromDictionary declaration
    (doto sb
      (.append (from-dict-declaration class-name))
      (.append ";\n"))

    ;; Close
    (.append sb "\n@end")

    (.toString sb)))

(s/defn from-dict-impl :- String
  [class-spec :- ClassSpec]
  (let [sb (StringBuilder.)
        properties (utils/safe-get class-spec :properties)
        class-name (utils/safe-get class-spec :class-name)
        vble-name (str (.toLowerCase (subs class-name 0 1)) (subs class-name 1))
        ]
    ;; initialize class
    (.append sb (format "%s *%s = [%s new];\n"
                        class-name
                        vble-name
                        class-name))

    ;; TODO: type conversion from data dictionary
    ;; assign properties from dictionary
    (doseq [p properties
            :let [property-name (utils/safe-get p :name)
                  property-type (utils/safe-get p :type)
                  data-val (format "data[@\"%s\"]" property-name)]]
      (.append
       sb
       (str
        (format "%s.%s = " vble-name property-name)
        (cond
         ;; TODO(iw): verify we don't need special casting rules
         ;; Simple assignment
         (not (s/check ObjcAtomType property-type))
         (format "(%s) %s" (obj-reference p) data-val)

         ;; Enum
         (not (s/check NamedEnumSchema property-type))
         (let [enum-name (:name property-type)
               enum-list (->> (-> property-type :schema :vs)
                              (map name)
                              (map (partial format "@\"%s\""))
                              (str/join ", "))]
           (format "(%s) [@[%s] indexOfObject:%s]"
                   enum-name enum-list data-val))

         ;; Class
         :else
         (format "[%s fromDictionary:%s]" property-type data-val))))
      (.append sb ";\n"))


    ;; return
    (.append sb (format "return %s;\n" vble-name))

    (.toString sb)))

(s/defn emit-m :- String
  [class-spec :- ClassSpec]
  (let [class-name (utils/safe-get class-spec :class-name)
        sb (StringBuilder.)]
    ;; Import .h file
    (.append sb (format "#import \"%s.h\"\n\n" class-name))

    ;; @implementation
    (.append sb (str "@implementation " class-name "\n"))

    ;; Implement fromDictionary
    (doto sb
      (.append (from-dict-declaration class-name))
      (.append "{\n")
      (.append "  ")
      (.append (->> (.split (from-dict-impl class-spec) "\n")
                    (str/join "\n  ")))
      (.append "\n}\n"))

    (.append sb "@end")

    (.toString sb)))

(s/defn write-class!
  [class-spec :- ClassSpec
   output-folder :- String]
  (doseq [[ext body] [["h" (emit-h class-spec)]
                      ["m" (emit-m class-spec)]]]
    (spit (format (str output-folder "%s.%s")
                  (:class-name class-spec) ext)
          body)))

(comment
  ;; Test schemas for write-class!
  (s/defschema Person {:name String :birth-day Long})
  (s/defschema Author {:name String (s/optional-key :url) String})

  (s/defschema PersonId
    {:id long
     :action (s/enum :comment :read)
     :person Person}))
