(ns ClojureLink.core
	(:use [clojure.contrib.string :only [replace-str]]) 
	(:import com.wolfram.jlink.Expr)
)


(def characters {:Dot "\u2024" :Dash "\u2011" :Underscore "\u02cd" :Colon "\u0589" 
:Astrisk "\u204e" :Question "\u2047" :Exclamation "\u203c"})

(def java-object-map)

(defn part [a b] (.part a b))

(defn head [a] (.head a))

(defn length [a] (.length a))

(defn from-mathematica-symbol-name [s]
	(reduce #(replace-str (%2 0) (%2 1) %1) s [["`" "/"] [(characters :Astrisk) "*"] [(characters :Exclamation) "!"] [(characters :Dash) "-"] [(characters :Underscore) "_"] [(characters :Colon) ":"] [(characters :Dot) "."]])
)

(defn to-mathematica-symbol-name [s]
	(reduce #(replace-str (%2 0) (%2 1) %1) s [["/" "`"] [(characters :Astrisk) "*"] [(characters :Exclamation) "!"] ["-" (characters :Dash)] ["_" (characters :Underscore)] [":" (characters :Colon)] ["." (characters :Dot)]])
)

(defn to-clojure-symbol [expr] 
	(let [sname (.asString expr)] 
		(cond 
			(= sname "True") true
			(= sname "False") false
			(= sname "Null") nil
			(= sname "nil") nil
			(= sname "Equal") '=
			(= sname "SameQ") '=
			(= sname "Plus") '+
			(= sname "Times") '*
			(= sname "Division") '/
			(= sname "Rational") '/
			(= sname "Greater") '>
			(= sname "GreaterEqual") '>=
			(= sname "Less") '<
			(= sname "LessEqual") '<=
			(= sname "Unequal") 'not=
			(re-matches #"JLink`Object.*" sname) (.getObject (.getObjectHandler (com.wolfram.jlink.Install/getStdLink)) sname)
			(= (subs sname 0 1) (characters :Colon)) (keyword (from-mathematica-symbol-name (subs sname 1 (count sname))))
			true (symbol (from-mathematica-symbol-name sname))
		)
	))

(defn to-s-expression [expr] 
	(let [conpart (fn [seq expr p] (conj seq (to-s-expression (part expr p))))] 
	(cond	
		(and (.atomQ expr) (not (.rationalQ expr))) 
			(cond 
				(.symbolQ expr) (to-clojure-symbol expr)
				(.integerQ expr) (.asInt expr)
				(.realQ expr) (.asDouble expr)
				(.stringQ expr) (.asString expr))
		(= (.asString (head expr)) "List") (reduce #(conpart %1 expr (+ %2 1)) [] (range (length expr)))
		(= (.asString (head expr)) "HashMap") (into {} (reduce #(conpart %1 expr (+ %2 1)) [] (range (length expr))))
		(= (.asString (head expr)) "HashSet") (into #{} (reduce #(conpart %1 expr (+ %2 1)) [] (range (length expr))))
		(= (.asString (head expr)) "ClojureSymbol") (symbol (.asString (part expr 1)))
		true (reverse (reduce #(conpart %1 expr (+ %2 1)) (conj '() (to-s-expression (head expr))) (range (length expr))))
	)))
		
(defn to-mathematica-symbol [expr] 
	(let [sname  (to-mathematica-symbol-name (str expr))] 
		(cond
			(= sname "+") (Expr. 4 "Plus") 
			(= sname ">") (Expr. 4 "Greater")
			(= sname ">=") (Expr. 4 "GreaterEqual")
			(= sname "<")  (Expr. 4 "Less")
			(= sname "<=") (Expr. 4 "LessEqual")
			(= sname "=") (Expr. 4 "Equal")
			(= sname "not=") (Expr. 4 "Unequal")
			(empty? (re-matches #"(\w|\/|_|-|\.|:|\$)*" (str expr))) (Expr. (Expr.  4 "ClojureSymbol") (into-array Expr [(Expr. sname)]))
			true (Expr. 4 sname)
		)
))		
			
(defn createExprSub [struct] 
	(cond
		(= (class struct) com.wolfram.jlink.Expr) struct
		(= struct true) (Expr. 4 "True")
		(= struct false) (Expr. 4 "False")
		(= struct nil) (Expr. 4 "Null")
		(ratio? struct) (Expr. (Expr.  4 "Rational") (into-array Expr (map createExprSub [(numerator struct) (denominator struct)])))
		(keyword? struct) (to-mathematica-symbol struct)
		(symbol? struct) (to-mathematica-symbol struct)
		(integer? struct) (Expr.  1 (str struct))
		(float? struct) (Expr.  2 (str struct))
		(string? struct) (Expr. struct)
		(list? struct) (Expr. (createExprSub (first struct)) (into-array Expr (map createExprSub (rest struct))))
		(= clojure.lang.MapEntry (class struct)) (Expr. (Expr.  4 "Rule") (into-array Expr (map createExprSub struct)))
		(vector? struct) (Expr. (Expr.  4 "List") (into-array Expr (map createExprSub struct)))
		(map? struct) (Expr. (Expr.  4 "List") (into-array Expr (map createExprSub struct)))
		(set? struct) (Expr. (Expr.  4 "HashSet") (into-array Expr (map createExprSub struct)))
		(seq? struct) (Expr. (Expr.  4 "List") (into-array Expr (map createExprSub struct)))
		true (let [s (gensym)] (set! java-object-map (assoc java-object-map s struct)) (createExprSub s))
	)	
)	

		

(defn createExpr [struct] 
	(binding [java-object-map {}] 
		(let [resultexpr (createExprSub struct)] (object-array [(createExprSub (map first java-object-map)) (object-array (map last java-object-map)) resultexpr]) )
	) 	
)

(defn evalm [expr] 
	(binding [*ns* (create-ns 'user)] 
		(let [res (eval expr)]
			(if (ratio? res) 
					(Expr. (Expr.  4 "Rational") (into-array Expr (map createExprSub [(numerator res) (denominator res)])))
				res))))			