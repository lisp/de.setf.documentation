;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: de.setf.utility.implementation; -*-


(in-package :de.setf.utility.implementation)

(:documentation
  "This file defines projection operators to present system metadata."
 
 (copyright
  "Copyright 2010 [james anderson](mailto:james.anderson@setf.de)  All Rights Reserved"
  "'de.setf.utility' is free software: you can redistribute it and/or modify
 it under the terms of version 3 of the GNU Lesser General Public License as published by
 the Free Software Foundation.

 'de.setf.utility' is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
 without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 See the GNU Lesser General Public License for more details.

 A copy of the GNU Lesser General Public License should be included with 'de.setf.utility, as `lgpl.txt`.
 If not, see the GNU [site](http://www.gnu.org/licenses/).")

 (description
  "The simplest view of 'code' is the text of a program file. A programmer reads that, edits it and considers
 the implications the text has on the sequence of events to be programmed. One aspect of this process is
 to infer the effects of relations to other code passages. At first statically, but eventually also as
 a temporal process.  One can trace a running program and even animate dynamic displayes of a program's state,
 to clarify progressions and it behavior in relation to constraints.
 Which helps to understand what the program does, but still not why. That requires to render the relations
 implicit in the source.

 These relations occupy several dimensions. A file contains  definitions for language entities.
 The files are combined in groups which contribute to aspects of a program or constitute its entirety.
 A group of language entities can constitute an active component - a protocol, or record state which
 models the application. in order to develop or modify these entities, it helps to see a comprehensive
 presentation of the implications of a relation.

 These operators permit to position an observer within the network of relations implicit in
 software, navigate through that netowrk, and select, filter, and present the relations as a
 a graph which responds interactively to support investigation.

 This introspector permits numerous relations

 - requirement
   designates dependency relations between source components

 - component
   designates the part relation between system/module source components and the constituent to module, component, 
   or program entity definition.

 - definition
   designates the part relation between a program entity and a location in a source component.

 - function, macro, class
   (condition compiler-macro constant method-combination symbol-macro type variable) remain unimplemented
   designate the relation between a name and the respective named entity.

 - used-by imports uses relations other (? other is uncertain and not now used)
   designates the visibility relations between packages

 - internal external
   designates the relation between a package and the constituent symbols

 - subclass superclass binds specializes relations other (? other is uncertain and not now used)
   designates the relation between a class, other classes, and operators. presently abstract only, without instance information.

 - callers calls relations other  (? other is uncertain and not now used)
   designates the relation between a function and other functions, classes, and variables (?vars should be references).

 - method
   designates the part relation between a generic function and a method.


 The definition-walker class supports all relations in one walker.
 There are three generic categories:
 - the location of definitions in source code
 - the control flow of a process
 - the concept in a model

 Every object corresponds to an unique node in a definition graph. The position can be designated
 either globally in terms identifiers for each of the three aspects. A field of view can be specified
 in terms of filtering and presentation specifications to select, order, and visually distinguish the values in each category's
 domain. A path of motion can be specified in terms of substitution rules and/or parameteric generators which are controlled
 by user interaction.

 The rendering process starts as a root set.
 The root set comprises some combination of functions, classes, packages and code components.
 Each of which can implicate further instances in each category.
 Given the permitted visibility in each category, the implicated elements are included up to the
 point where the grap closes.
 Presentations are iterative: given a position with constraints on categories and depth, and mappinge between
 category domain values and visual properties, the 'next' graph involves changes to the view:
 - root set member
   - replace by the result of a traversal within category or across category
   - delete
   - add an arbitrary
 - traversal rule
   - add
   - remove
   - change constraint
 - presentation rule
   - map between catagory/instance/classification and depth
   -  '' and display property - color, size


 The controls for this combine a general, 'remote' panel for global settings and instance menus in the graph itself.
 The control panel contains
  - property settings for global properties
    - depth
    - closed/open boundary
  - a multiple-selection menu for entity
    - function
    - class
    - package
    - file
  - as many regions as criteria. Each contains
    - a multiple-selection menu for the qualifiers in that category
    - property settings for each
      - depth
      - visuals : color, weight, line type
  - an input field for roots

 The instance menu allows to
 - turn off the given class and turn on others which are not present
 - to enable/disable qualifiers which pertain to that class
 - to add boundry nodes to the roots
 - remove the respective node from the roots

 Given these controls it should be possible to expand/contract the network, change what is emphasized in the presentation,
 and navigate across categories. for example

 - select a single node and remove it from the graph. eg,
   - select a module and eliminate it and all files
   - select a function and eliminate it
 - select a node and add its linked nodes by some qualifier
   - select a class/function and add its callers, called functions, or specialized functions
   - select a class/function and add the files for all functions in the graph
   - select a file and extend to include all definitions

 change urn scheme, such that up through the class is base urn with the individual #'d on as for http uri.
 the type information is the path and the instane identifier is the object, which appears as a fragment
 rfc 3986 indicates that the frament is removed prior to resource dereference and that the client
 does the extraction for the reource docuemnt. this is is not equivalent with the requirement, that
 the components be individually adressable.
"))

;; trying to work with agraph
;; ? what it the purpose of part= rather than interning them?
;; ? why the multiple cached representations - per object v/s forgetting the print form and allowing a global prefix cache
;; they claim abbreviationa are meaningles, but that's no more thru than qualified symbols and could be handled the same way
;; of a prefix is declared, it applies, otherwise the external form is the full form.
;; part->concise is superfluous
;; why the extr wrappers on literals?
;; why not just print in ntriples format? rather than with pprint-subject,object?
;; { format is not readable
;; why does one want the upi internally, what good is that concept?  it's as if one would refere to slip objects with their hash code.

(in-package :triple-store-user)

(register-namespace 
 "rdf" "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
(register-namespace
 "owl" "http://www.w3.org/2002/07/owl#")
(register-namespace
 "rdfs" "http://www.w3.org/2000/01/rdf-schema#")

(register-namespace
 "cl"  "US-ANSI-INCITS-226-1994-R2004#")



(add-triple !cl:callable !rdfs:subTypeOf !cl:program-component)
(add-triple !cl:generic-function !rdfs:subTypeOf !cl:callable)
(add-triple !cl:function !rdfs:subTypeOf !cl:callable)
(add-triple !cl:macro !rdfs:subTypeOf !cl:callable)
(add-triple !cl:setf-expander !rdfs:subTypeOf !cl:callable)
(add-triple !cl:method

(add-triple !cl:class !rdfs:subTypeOf !cl:program-component
(add-triple !cl:condition
(add-triple !cl:structure
(add-triple !cl:object
standard-object
built-in-class
method-combination
compiler-macro
modify-macro
setf
binding
symbol-macro
parameter
constant
variable
package
symbol
type


