(ns myproj.editor
	(:gen-class))

(defn print-image
	[image]
	(doseq [row image] (println row)))

(defn new-image
	"Creates new image with m columns and n rows"
	[m n]
	(vec (repeat n (vec (repeat m "O")))))

(defn paint
	"Colours list of coordinates, each in format [y x]. Returns original
	 image if any coordinates are outside bounds of image"
	[image colour c-list]
	(reduce
		(fn [img coords]
			(if (get-in img coords)
				(assoc-in img coords colour)
				image))
		image
		c-list))

(defn formatter
	"Formats range and number that represent line into list of coordinates"
	[xrange y]
	(if (first xrange)
		(cons (vector y (first xrange))
		      (formatter (rest xrange) y))
		(vector)))

(defn horiz-list
	"Determines list of coordinates which plot given line specification"
	[m1 m2 n]
	(let [x1 (- m1 1)
	      x2 (- m2 1)
        xrange (range x1 (+ x2 1))
	      y (- n 1)]
		(formatter xrange y)))

(defn neighbours
	"Returns all 'common-side' neighbours of given list of coordinates"
	[c-list]
	(set
		(mapcat
			(fn [coords]
				(let [y (first coords)
				      x (second coords)]
					(concat [[y x]]
					        (for [dx [-1 1]] [y (+ dx x)])
					        (for [dy [-1 1]] [(+ dy y) x]))))
			c-list)))

(defn filter-neighbours
	"Filters out neighbours of different colour and those outside bounds"
	[image colour c-list]
	(filter
		(fn [coords]
			(= colour (get-in image coords)))
		c-list))

(defn get-region
	"Gets all sets of coordinates in same region as a given point. If given
	 point is outside bounds of image, returns nil"
	[image [m n]]
	(let [coords [(- n 1) (- m 1)]
				colour (get-in image coords)]
		(if colour
			(loop [current [coords]
		       previous nil]
			(if (= current previous)
				current
				(recur (filter-neighbours image colour (neighbours current))
				       current))))))

(defn parse-and-execute
	"Parses commands and calls appropriate methods"
	[input image]
	(let [characters (re-seq #"\w" input)
	      command (first characters)
	      colour (last characters)
	      digits (re-seq #"\d+" input)
	      digit-1 (first digits)
	      digit-2 (second digits)
	      digit-3 (last digits)]
	(cond
		(= command "I")
			(new-image (read-string digit-1) (read-string digit-2))
		(= command "C")
			(new-image (count (first image)) (count image))
		(= command "L")
			(let [x (- (read-string digit-1) 1)
			      y (- (read-string digit-2) 1)]
				(paint image colour [[y x]]))
		(= command "V")
			(paint image colour (map reverse (horiz-list (read-string digit-2)
			                                             (read-string digit-3)
			                                             (read-string digit-1))))
		(= command "H")
			(paint image colour (horiz-list (read-string digit-1)
			                                (read-string digit-2)
			                                (read-string digit-3)))
		(= command "F")
			(paint image colour (get-region image [(read-string digit-1)
			                                       (read-string digit-2)]																				))
		:else image)))

(defn -main
	"Loop that gets input from *in* and feeds parse-and-execute method"
	[]
	(loop [input (read-line)
	       image []]
		(if (= input "X")
			"Bye!"
			(do (if (= input "S")
			      (print-image image))
			    (recur(read-line)(parse-and-execute input image))))))
