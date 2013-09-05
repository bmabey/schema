(ns schema.client.objc
  "Generate Objective-C domain classes from a limited subset of schemas. For instance,
    (s/defschema Person {:name String :birth-day long :age int :height double})
    will generate this https://gist.github.com/aria42/2bffe6ba8176f21d1911"
  (:require [clojure.string :as str]
            [schema.core :as s]
            [schema.utils :as utils])
  (:import [schema.core EnumSchema NamedSchema OptionalKey]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Analysis

(s/defschema StorageAttribute
  (s/enum :strong :weak :readWrite :copy))

(s/defschema ObjcPrimitiveNumberType
  (s/enum :NSInteger :long-long :CGFloat))

(s/defschema ObjcAtomType
  (s/either ObjcPrimitiveNumberType (s/eq :NSString)))

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
       (s/check ObjcPrimitiveNumberType objc-type)))

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
  [schema :- NamedSchema]
  {:class-name (utils/safe-get schema :name)
   :properties (map analyze-property (utils/safe-get schema :schema))})

(s/defn referenced-classes :- [String]
  [class-spec :- ClassSpec]
  (set (filter string? (map :type (:properties class-spec)))))

(s/defn pull-out-enums :- ClassSpec
  "Pulls out ObjcEnum types into :dynamic-enums"
  [{:keys [properties class-name] :as class-spec} :- ClassSpec]
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
  "Generates the class name for a given Prop"
  [property :- Property]
  (let [property-type (utils/safe-get property :type)]
    (if (not (s/check NamedEnumSchema property-type))
      (:name property-type)
      (name property-type))))

(s/defn number-cast :- String
  [num-type :- ObjcPrimitiveNumberType
   data-val :- String]
  (format "((NSNumber *) %s).%sValue"
          data-val
          (case num-type
            :long-long "longLong"
            :CGFloat "float"
            :NSInteger "integer")))

(s/defn obj-reference :- String
  [property :- Property]
  (let [object? (utils/safe-get property :object?)]
    (str (.replace (property-type-name property) "-" " ") (when object? " *"))))

(s/defn obj-cast :- String
  [property :- Property data-val :- String]
  (let [prop-type (utils/safe-get property :type)]
    (cond
     (utils/safe-get property :object?) (format "(%s) %s" (obj-reference property) data-val)
     (nil? (s/check ObjcPrimitiveNumberType prop-type)) (number-cast prop-type data-val)
     :else (throw (RuntimeException. (str "Don't know how to cast " property))))))

(s/defn emit-property :- String
  [property :- Property]
  (format
   "@property (%s%s) %s %s;"
   (if (utils/safe-get property :atomic?)
     "atomic"
     "nonatomic")
   (if-let [storage (utils/safe-get property :storage)]
     (str "," (name storage))
     "")
   (obj-reference property)
   (name (utils/safe-get property :name))))

(s/defn emit-dynamic-enum :- String
  [enum-schema :- NamedEnumSchema]
  (let [enum-names (->> (get-in enum-schema [:schema :vs])
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
         (obj-cast p data-val)

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
  (s/defschema Person {:name String :birth-day long :age int :height double})
  (s/defschema Author {:name String (s/optional-key :url) String})

  (s/defschema PersonId
    {:id long
     :action (s/enum :comment :read)
     :person Person}))
