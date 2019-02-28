: FACTORIAL ( +n1 -- +n2)
  DUP 2 < IF			\ S: n1 n1 2
    DROP 1			\ S: 1
    EXIT
  THEN				\ S: n1
  DUP 1- 			\ S: n1 n1'
  RECURSE			\ S: n1 n1'
  *				\ S: n2
;
