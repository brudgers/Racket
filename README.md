Racket
======

###FunWithImageSorting.rkt:###
  
  * Language: ISL
  
  * Functions for sorting (listof Image) by area and perimeter. 
  
  * Boxes image to data structure (make-graphic pic area perimeter)
  
  * Illustrates some abstraction for helper functions

The idea of using a data structure to add key values came about because I recently started grazing volume 3 of TAOCP. One of the first exercises is about randomizing a set of records and Knuth mentions adding temporary keys to records and then discarding the keys after they have done their work.


###Quick-select.rkt###
  
  * Thanks to Pavel Lepin
  
  * Language: ISL
  
Demonstration of quick selection method. Usage (quick-select listof-number i) returns the i'th lowest value in list.

###Sudoku template notes.rkt###

  * Language: ISL

Shows how the templates for mutal-reference, self-reference, generative recursion, and backtracking search all fit together for the Sudoku solver program. Variables and function names have been renamed for consistency across templates. This allows the logical relationships to be more obvious when the templates are blended.

  
