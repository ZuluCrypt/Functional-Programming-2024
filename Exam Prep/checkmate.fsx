type Piece =
    |Knight

type Cell =
    |Empty
    |Piece of Piece

type Cluster=
    |Cluster of Cell * Cell *Cell *Cell *Cell *Cell *Cell *Cell 

type Board =
    |Board of Cluster * Cluster * Cluster* Cluster* Cluster* Cluster* Cluster* Cluster

 