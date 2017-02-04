; COMP 3007 - Assignment 3
; Saturday, November 22, 2014
; Danen Van De Ven, 100820351

#lang racket

; Question 2: Define a procedure called make-graph which makes a graph with no nodes
; or edges and returns a graph object with local state as discussed in class. Use a
; dispatch or message-passing style as discussed in class. Write procedures called
; make-edge and make- node that return an edge and node respectively. Write
; procedures called add-node, add-edge that add nodes and edges to a graph
; respectively. Note, an edge can only be added if the corresponding nodes are
; already part of the graph. Write procedures called delete- node, delete-edge that
; delete nodes and edges from a graph respectively. When a node is deleted, all
; edges connected to the deleted node should automatically be removed. Finally,
; write a procedure print-graph that prints out the structure of the graph.

;(define (make-edge edge . edge-node)
;    (set! edges (cons (cons edge edge-node) edges))
;    (reverse edges))

;(define (make-node node . node-edge)
;    (cons node (list node-edge)))

; Nodes and edges in the graph are stored in their own lists.
(define (make-graph nodes edges)
  
  ; A node is takes the name it was given.
  (define (make-node node) node)
  
  ; Edges are represented by the name of the edge followed by a list or heads it connects.
  ; Edges must connect two nodes
  (define (make-edge edge x y)
    (list edge (list x y)))
  
  ; Adds a node to the graph nodes list.
  ; Very basic error checking is applied.
  (define (add-node node)
    (if (member node nodes)
        (error "Node already exists!")
        (set! nodes (cons node nodes))))
  
  ; Adds an edge to the graph, making sure the nodes it connects do infact exist.
  (define (add-edge edge)
    (when (member edge edges)
      (error "Edge already exists!"))
    (let ((node-x (caadr edge))
          (node-y (cadadr edge)))
      (if (not (and (member node-x nodes)
                    (member node-y nodes)))
          (error "Edge connected to a node that does not exist!")
          (set! edges (cons edge edges)))))
  
  ; Deletes an edge from the graphs edges list.
  (define (delete-edge edge)
    (set! edges (filter (lambda (x) (not (eq? edge x))) edges)))

  ; Deletes a node while deleting any edges connected to that node.
  (define (delete-node node)
    
    (define (get-nodes edge)
      (cadr edge))
    
    (define (delete-node-edges node edges-copy)
      (cond ((null? edges-copy) (display ""))
            ((member node (get-nodes (car edges-copy))) (delete-edge (car edges-copy))
                                                        (delete-node-edges node (cdr edges-copy)))
            (else (delete-node-edges node (cdr edges-copy)))))
    
    (set! nodes (filter (lambda (x) (not (eq? node x))) nodes))
    (delete-node-edges node edges))
  
  ; Prints the text representation of the graph.
  (define (print-graph)
    
    (define (print-graph-helper edges)
      (cond ((null? edges) (display "..."))
            (else 
             (newline)
             (display (caar edges))
             (display " connects nodes ")
             (display (cdar edges))
             (print-graph-helper (cdr edges)))))
          
    
    (display "This graph contains nodes: ") (display nodes)
    (newline)
    (print-graph-helper edges))
  
  ; Dispatch controls the flow of the object via text commands.
  (define (dispatch m)
    (cond ((eq? m 'make-node) make-node)
          ((eq? m 'make-edge) make-edge)
          ((eq? m 'add-node) add-node)
          ((eq? m 'add-edge) add-edge)
          ((eq? m 'delete-node) delete-node)
          ((eq? m 'delete-edge) delete-edge)
          ((eq? m 'print-graph) print-graph)))
  
  dispatch)

(display "Making graph:") (newline)
(define graph1 (make-graph '() '())) (newline)
((graph1 'print-graph)) (newline) (newline)

(display "Creating nodes:") (newline)
(define node1 ((graph1 'make-node) 'node1))
(define node2 ((graph1'make-node) 'node2))
(define node3 ((graph1 'make-node) 'node3))
node1
node2
node3 (newline)

(display "Creating edges:") (newline)
(define edge1 ((graph1 'make-edge) 'edge1 node1 node2))
(define edge2 ((graph1 'make-edge) 'edge2 node2 node3))
(define edge3 ((graph1 'make-edge) 'edge3 node3 node1))
edge1
edge2
edge3 (newline) (newline)

(display "Adding nodes to the graph:") (newline) (newline)
((graph1 'add-node) node1)
((graph1 'add-node) node2)
((graph1 'add-node) node3)
((graph1 'print-graph)) (newline) (newline)

(display "Adding edges to the graph:") (newline) (newline)
((graph1 'add-edge) edge1)
((graph1 'add-edge) edge2)
((graph1 'add-edge) edge3)
((graph1 'print-graph)) (newline) (newline)

(display "Deleteing an edge:") (newline) (newline)
((graph1 'delete-edge) edge1)
((graph1 'print-graph)) (newline) (newline)

(display "Deleting a node:") (newline) (newline)
((graph1 'delete-node) node1)
((graph1 'print-graph)) (newline) (newline)

(display "Creating new graph: ") (newline) (newline)
(define graph2 (make-graph '() '()))
((graph2 'print-graph)) (newline) (newline)

(display "Adding nodes and edges: ") (newline) (newline)
((graph2 'add-node) node1)
((graph2 'add-node) node3)
((graph2 'add-edge) edge3)
((graph2 'print-graph))



















