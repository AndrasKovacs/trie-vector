persistent-vector
=================

Clojure-style persistent vector for Haskell.

WIP currently, and also extremely ugly from the inside, and I'm still playing around with various hideous unsafe tricks, but preliminary benchmark data shows that this can be just about the fastest and most compact random access persistent structure around.

I plan to eventually build RRB-trees and/or hashmaps with linear hashing atop these vectors.

