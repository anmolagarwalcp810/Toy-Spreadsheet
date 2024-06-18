open Printf
type cell=Empty|Float of float
type sheet = cell list list;;
type index = {integer1:int;integer2:int};;
type range = {ind1:index;ind2:index};;
type listcell = cell list;;  

exception SheetIndexOutofRange			(*called when ranges are incorrect*)
exception CellIndexOutofRange			(*called when function goes beyond the size of the sheet*)
exception CellIndexOutofRangeinmodifycell	(*same exception as above, execpt raised exclusively in modifycell function*)
exception InvalidInput				(*raised when inputs are not valid,i.e., ranges are not correct, same as the first exception*)
exception InvalidInputRangeIndex		(*same as InvalidInput, created differently for debugging*)
exception ColIndexOutofRange			(*called when functions try to get columns outside the sheet*)
exception EmptyCellException			(*called when operation is being done on the empty cell*)
exception DivisionByZeroException		(*raised when division by zero is taking place*) 
exception IncompatibleRangesException		(*raised when operation is being done on ranges of different size or shape*)
exception IncompatibleColRangesException	(*Again same as IncompatibleRangesException, used for debugging*)
exception IncompatibleRowColException		(*Again same as IncompatibleRangesException, used for debugging*)
exception IncompatibleAddRangesException	(*Again same as IncompatibleRangesException, used for debugging*)

(*gets the float value of cell,else raises EmptyCellException if cell is Empty*)
let floatofcell (c:cell) : float = 
	match c with
	| Empty -> raise EmptyCellException
	| Float x -> x 

let rec c_list (n:int) (elist:cell list) (value:cell) : cell list = 
	if(n=0) then elist
	else (c_list (n-1) (elist@[value]) value);;

let rec slist (n:int) (elist:sheet) (clist:listcell) : sheet = 
	if(n=0) then elist
	else (slist (n-1) (clist::elist) clist);;		(*clist::elist*)

let rec findcindex (n:int) (c:cell list) : cell = 
	if(n=0) then
		match c with
		| [] -> raise CellIndexOutofRange
		| x::xs -> x
	else
		match c with
		| [] -> raise CellIndexOutofRange
		| x::xs -> (findcindex (n-1) xs)

let rec findindex (i1:int) (i2:int) (s:sheet) : cell = 
	if(i1=0) then
		match s with
		| [] -> raise SheetIndexOutofRange
		| x::xs -> (findcindex i2 x)
	else
		match s with
		| [] -> raise SheetIndexOutofRange
		| x::xs -> (findindex (i1-1) i2 xs)

let rec findrow (i1:int) (s:sheet) : cell list = 
	if(i1=0) then
		match s with
		| [] -> raise SheetIndexOutofRange
		| x::xs -> x
	else
		match s with
		| [] -> raise SheetIndexOutofRange
		| x::xs -> (findrow (i1-1) xs)

let rec printrow (c:listcell) : unit = 
	match c with
	| [] -> Printf.printf "\n"
	| x::xs ->  if (x=Empty) then Printf.printf "-\t\t"
		else Printf.printf "%f\t" (floatofcell x);
		(printrow xs);;

let rec printsheet (s:sheet) : unit = 
	match s with
	| [] -> Printf.printf "\n"
	| x::xs -> (printrow x); (printsheet xs);;

let rec modifycell (i2:int) (c:cell list) (c1:cell list) (k:cell): cell list = 
	if(i2=0) then
		match c with
		| [] -> raise CellIndexOutofRangeinmodifycell
		| x::xs -> c1@(k::xs)
	else
		match c with
		| [] -> raise CellIndexOutofRangeinmodifycell
		| x::xs -> (modifycell (i2-1) xs (c1@[x]) k)

let rec modifysheet (i1:int) (c:cell list) (s:sheet) (s1:sheet): sheet = 
	if(i1=0) then
		match s with
		| [] -> raise SheetIndexOutofRange
		| x::xs -> s1@(c::xs)
	else
		match s with
		| [] -> raise SheetIndexOutofRange
		| x::xs -> (modifysheet (i1-1) c xs (s1@[x]))	

let modifycellofsheet (i1:int) (i2:int) (s:sheet) (k:cell):sheet = 
	let c = findrow i1 s in
	let c1 = (modifycell i2 c [] k) in (modifysheet i1 c1 s [])

	
(*gets column of the sheet*)
let rec getcol (s:sheet) (i:int) (r:int) (count:int) (c:cell list) : cell list = 
	if(count<=r) then
		match s with
		| [] -> raise ColIndexOutofRange
		| x::xs -> (getcol s i r (count+1) ((findindex count i s)::c))
	else c

let checkrange (r:range): bool = 
		
	let i1,i2,i3,i4 = r.ind1.integer1, r.ind1.integer2, r.ind2.integer1, r.ind2.integer2 in
	(
	if(i1<=i3 && i2<=i4) then true
	else false)

let checkindex (i:index):bool = true;;
let s= [];;
let c= [];;
let n=500;;
let c=(c_list 500 c Empty);;
let s=(slist 500 s c);;

(*keep increasing i1 till i3, then increase i2, then repeat the same process with i1=initi1*)
let rec fullcount (s:sheet) (initi1:int) (i1:int) (i2:int) (i3:int) (i4:int) (count:int) : int = 
	if(i1<=i3 && i2<=i4) then
		let c1 = (findindex i1	i2 s) in	(*then get cell of row*)
			if(c1 <> Empty) then
				(fullcount s initi1 (i1+1) i2 i3 i4 (count+1)) 
			else
				(fullcount s initi1 (i1+1) i2 i3 i4 count)
	else if(i2<=i4) then				(*i1 exhausted, i2 to next column*)
		(fullcount s initi1 initi1 (i2+1) i3 i4 count)
	else						(*i2 exhausted*)
		count
(*call fullcount function to calculate total count in given range, and then modify sheet by calling modifying functions*)
let full_count (s:sheet) (r:range) (i:index) : sheet =
	if ((checkrange r) && (checkindex i)) then
		let i1,i2,i3,i4 = r.ind1.integer1,r.ind1.integer2,r.ind2.integer1,r.ind2.integer2 in
		let n = (float_of_int (fullcount s i1 i1 i2 i3 i4 0)) in (modifycellofsheet i.integer1 i.integer2 s (Float n))
	else raise InvalidInputRangeIndex

(*creating function to take out sublist*)
let rec subcell (c:cell list) (c1:cell list) (i1:int) (i2:int) (count:int):cell list = 
	match c with
	| [] -> c1
	| x::xs -> if(count<i1) then (subcell xs c1 i1 i2 (count+1))
		   else if(count>=i1 && count<=i2) then
			(subcell xs (c1@[x]) i1 i2 (count+1))
		   else
			c1

(*creating cellistcount function to count valid entries in given cell list*)
let rec cellistcount (c:cell list) (count:int):int = 
	match c with
	| [] -> count
	| x::xs -> if(x=Empty) then (cellistcount xs count) else (cellistcount xs (count+1))

(*first call findrow, then take sublist using subcell, and then call cellistcount, then put it in given index, then new index is [r++,c], new range is ([i1++:i2],[i3:i4])*)
let rec rowcount (s:sheet) (r:range) (i:index) : sheet = 
	if ((checkrange r) && (checkindex i)) then
		let i1,i2,i3,i4,n1,n2 = r.ind1.integer1,r.ind1.integer2,r.ind2.integer1,r.ind2.integer2,i.integer1,i.integer2 in
		let r1 = (findrow i1 s) in
		let c1 = (subcell r1 [] i2 i4 0) in
		let f = (float_of_int (cellistcount c1 0)) in
		(rowcount (modifycellofsheet n1 n2 s (Float f)) {ind1={integer1=i1+1;integer2=i2};ind2={integer1=i3;integer2=i4}} {integer1=n1+1;integer2=n2})
	else s

(*get column till given index of sheet, then run cellistcount and then update the sheet using modifysheet, then change i2 to i2+1, and index.r to index.r+1*)
let rec colcount (s:sheet) (r:range) (i:index) : sheet = 
	if((checkrange r) && (checkindex i)) then
		let i1,i2,i3,i4,n1,n2 = r.ind1.integer1,r.ind1.integer2,r.ind2.integer1,r.ind2.integer2,i.integer1,i.integer2 in
		let col = (getcol s i2 i3 i1 []) in
		let f = float_of_int (cellistcount col 0) in
		(colcount (modifycellofsheet n1 n2 s (Float f)) {ind1={integer1=i1;integer2=i2+1};ind2={integer1=i3;integer2=i4}} {integer1=n1;integer2=n2+1})
	else s

let row_count (s:sheet) (r:range) (i:index) : sheet = 
	if ((checkrange r) && (checkindex i)) then rowcount s r i
	else raise InvalidInput

let col_count (s:sheet) (r:range) (i:index) : sheet = 
	if((checkrange r) && (checkindex i)) then colcount s r i
	else raise InvalidInput

let rec fullsum (s:sheet) (initi1:int) (i1:int) (i2:int) (i3:int) (i4:int) (count:float) : float = 
	if(i1<=i3 && i2<=i4) then
		let c1 = (findindex i1	i2 s) in	(*then get cell of row*)
			if(c1 <> Empty) then
				(fullsum s initi1 (i1+1) i2 i3 i4 (count+.(floatofcell c1))) 
			else
				raise EmptyCellException
	else if(i2<=i4) then				(*i1 exhausted, i2 to next column*)
		(fullsum s initi1 initi1 (i2+1) i3 i4 count)
	else						(*i2 exhausted*)
		count

(*call fullsum function to calculate total count in given range, and then modify sheet by calling modifying functions*)
let full_sum (s:sheet) (r:range) (i:index) : sheet =
	if ((checkrange r) && (checkindex i)) then
		let i1,i2,i3,i4 = r.ind1.integer1,r.ind1.integer2,r.ind2.integer1,r.ind2.integer2 in
		let n,n1,n2 = (fullsum s i1 i1 i2 i3 i4 0.0),i.integer1,i.integer2 in (modifycellofsheet n1 n2 s (Float n))
	else raise InvalidInput

(*creating cellistsum function to sum valid entries in given cell list*)
let rec cellistsum (c:cell list) (count:float):float = 
	match c with
	| [] -> count
	| x::xs -> match x with
		| Empty -> raise EmptyCellException
		| Float value -> (cellistsum xs (count+.value))

(*first call findrow, then take sublist using subcell, and then call cellistsum, then put it in given index, then new index is [r++,c], new range is ([i1++:i2],[i3:i4])*)
let rec rowsum (s:sheet) (r:range) (i:index) : sheet = 
	if ((checkrange r) && (checkindex i)) then
		let i1,i2,i3,i4,n1,n2 = r.ind1.integer1,r.ind1.integer2,r.ind2.integer1,r.ind2.integer2,i.integer1,i.integer2 in
		let r1 = (findrow i1 s) in
		let c1 = (subcell r1 [] i2 i4 0) in
		let f = (cellistsum c1 0.0) in
		(rowsum (modifycellofsheet n1 n2 s (Float f)) {ind1={integer1=i1+1;integer2=i2};ind2={integer1=i3;integer2=i4}} {integer1=n1+1;integer2=n2})
	else s

(*get column till given index of sheet, then run cellistsum and then update the sheet using modifysheet, then change i2 to i2+1, and index.r to index.r+1*)
let rec colsum (s:sheet) (r:range) (i:index) : sheet = 
	if((checkrange r) && (checkindex i)) then
		let i1,i2,i3,i4,n1,n2 = r.ind1.integer1,r.ind1.integer2,r.ind2.integer1,r.ind2.integer2,i.integer1,i.integer2 in
		let col = (getcol s i2 i3 i1 []) in
		let f = (cellistsum col 0.0) in
		(colsum (modifycellofsheet n1 n2 s (Float f)) {ind1={integer1=i1;integer2=i2+1};ind2={integer1=i3;integer2=i4}} {integer1=n1;integer2=n2+1})
	else s

let row_sum (s:sheet) (r:range) (i:index) : sheet = 
	if ((checkrange r) && (checkindex i)) then rowsum s r i
	else raise InvalidInput

let col_sum (s:sheet) (r:range) (i:index) : sheet = 
	if((checkrange r) && (checkindex i)) then colsum s r i
	else raise InvalidInput

(*avg functions are similar except the fact that average value will be updated in the specified index*)
let full_avg (s:sheet) (r:range) (i:index) : sheet = 
	if ((checkrange r) && (checkindex i)) then
		let i1,i2,i3,i4 = r.ind1.integer1,r.ind1.integer2,r.ind2.integer1,r.ind2.integer2 in
		let n,n1,n2 = (fullsum s i1 i1 i2 i3 i4 0.0)/.(float_of_int (fullcount s i1 i1 i2 i3 i4 0)),i.integer1,i.integer2 in (modifycellofsheet n1 n2 s (Float n))
	else raise InvalidInput

let rec rowavg (s:sheet) (r:range) (i:index) : sheet = 
	if ((checkrange r) && (checkindex i)) then
		let i1,i2,i3,i4,n1,n2 = r.ind1.integer1,r.ind1.integer2,r.ind2.integer1,r.ind2.integer2,i.integer1,i.integer2 in
		let r1 = (findrow i1 s) in
		let c1 = (subcell r1 [] i2 i4 0) in
		let f = (cellistsum c1 0.0)/.(float_of_int (cellistcount c1 0)) in
		(rowavg (modifycellofsheet n1 n2 s (Float f)) {ind1={integer1=i1+1;integer2=i2};ind2={integer1=i3;integer2=i4}} {integer1=n1+1;integer2=n2})
	else s

let rec colavg (s:sheet) (r:range) (i:index) : sheet =	
	if((checkrange r) && (checkindex i)) then
		let i1,i2,i3,i4,n1,n2 = r.ind1.integer1,r.ind1.integer2,r.ind2.integer1,r.ind2.integer2,i.integer1,i.integer2 in
		let col = (getcol s i2 i3 i1 []) in
		let f = (cellistsum col 0.0)/.(float_of_int (cellistcount col 0)) in
		(colavg (modifycellofsheet n1 n2 s (Float f)) {ind1={integer1=i1;integer2=i2+1};ind2={integer1=i3;integer2=i4}} {integer1=n1;integer2=n2+1})
	else s

let row_avg (s:sheet) (r:range) (i:index) : sheet = 
	if ((checkrange r) && (checkindex i)) then rowavg s r i
	else raise InvalidInput

let col_avg (s:sheet) (r:range) (i:index) : sheet =	
	if((checkrange r) && (checkindex i)) then colavg s r i
	else raise InvalidInput

(*creating cellistmin function to find minimum of all entries in given cell list*)
let rec cellistmin (c:cell list) (min:float):float = 
	match c with
	| [] -> min
	| x::xs -> if(x<>Empty) then
			if(min>(floatofcell x)) then (cellistmin xs (floatofcell x))
			else (cellistmin xs min) 
		 else raise EmptyCellException 

(*creating cellistmax function to find maximum of all entries in given cell list*)
let rec cellistmax (c:cell list) (max:float):float = 
	match c with
	| [] -> max
	| x::xs -> if(x<>Empty) then
			if(max<(floatofcell x)) then (cellistmax xs (floatofcell x))
			else (cellistmax xs max) 
		 else raise EmptyCellException 

(*creating fullmin function to find the minimum of all cells in the given range*)
let rec fullmin (s:sheet) (initi1:int) (i1:int) (i2:int) (i3:int) (i4:int) (min:float) : float = 
	if(i1<=i3 && i2<=i4) then
		let c1 = (findindex i1	i2 s) in	(*then get cell of row*)
			if(c1 <> Empty) then
				if(min>(floatofcell c1)) then (fullmin s initi1 (i1+1) i2 i3 i4 (floatofcell c1))
				else (fullmin s initi1 (i1+1) i2 i3 i4 min)
			else
				raise EmptyCellException
	else if(i2<=i4) then				(*i1 exhausted, i2 to next column*)
		(fullmin s initi1 initi1 (i2+1) i3 i4 min)
	else						(*i2 exhausted*)
		min

(*call fullmin function to calculate total count in given range, and then modify sheet by calling modifying functions*)
let full_min (s:sheet) (r:range) (i:index) : sheet =
	if ((checkrange r) && (checkindex i)) then
		let i1,i2,i3,i4 = r.ind1.integer1,r.ind1.integer2,r.ind2.integer1,r.ind2.integer2 in
		let min = findindex i1 i2 s in
		if(min<>Empty) then
			let n,n1,n2 = (fullmin s i1 i1 i2 i3 i4 (floatofcell (findindex i1 i2 s))),i.integer1,i.integer2 in (modifycellofsheet n1 n2 s (Float n))
		else raise EmptyCellException	
	else raise InvalidInput

let rec rowmin (s:sheet) (r:range) (i:index) : sheet = 
	if ((checkrange r) && (checkindex i)) then
		let i1,i2,i3,i4,n1,n2 = r.ind1.integer1,r.ind1.integer2,r.ind2.integer1,r.ind2.integer2,i.integer1,i.integer2 in
		let r1 = (findrow i1 s) in
		let c1 = (subcell r1 [] i2 i4 0) in
		let f = (cellistmin c1 (floatofcell (findindex i1 i2 s))) in
		(rowmin (modifycellofsheet n1 n2 s (Float f)) {ind1={integer1=i1+1;integer2=i2};ind2={integer1=i3;integer2=i4}} {integer1=n1+1;integer2=n2})
	else s

let rec colmin (s:sheet) (r:range) (i:index) : sheet = 
	if((checkrange r) && (checkindex i)) then
		let i1,i2,i3,i4,n1,n2 = r.ind1.integer1,r.ind1.integer2,r.ind2.integer1,r.ind2.integer2,i.integer1,i.integer2 in
		let col = (getcol s i2 i3 i1 []) in
		let f = (cellistmin col (floatofcell (findindex i1 i2 s))) in
		(colmin (modifycellofsheet n1 n2 s (Float f)) {ind1={integer1=i1;integer2=i2+1};ind2={integer1=i3;integer2=i4}} {integer1=n1;integer2=n2+1})
	else s

let row_min (s:sheet) (r:range) (i:index) : sheet = 
	if ((checkrange r) && (checkindex i)) then rowmin s r i
	else raise InvalidInput

let col_min (s:sheet) (r:range) (i:index) : sheet = 
	if((checkrange r) && (checkindex i)) then colmin s r i
	else raise InvalidInput

(*creating fullmin function to find the minimum of all cells in the given range*)
let rec fullmax (s:sheet) (initi1:int) (i1:int) (i2:int) (i3:int) (i4:int) (min:float) : float = 
	if(i1<=i3 && i2<=i4) then
		let c1 = (findindex i1	i2 s) in	(*then get cell of row*)
			if(c1 <> Empty) then
				if(min<(floatofcell c1)) then (fullmax s initi1 (i1+1) i2 i3 i4 (floatofcell c1))
				else (fullmax s initi1 (i1+1) i2 i3 i4 min)
			else
				raise EmptyCellException
	else if(i2<=i4) then				(*i1 exhausted, i2 to next column*)
		(fullmax s initi1 initi1 (i2+1) i3 i4 min)
	else						(*i2 exhausted*)
		min

(*call fullmax function to calculate total count in given range, and then modify sheet by calling modifying functions*)
let full_max (s:sheet) (r:range) (i:index) : sheet =
	if ((checkrange r) && (checkindex i)) then
		let i1,i2,i3,i4 = r.ind1.integer1,r.ind1.integer2,r.ind2.integer1,r.ind2.integer2 in
		let max = findindex i1 i2 s in
		if(max<>Empty) then
			let n,n1,n2 = (fullmax s i1 i1 i2 i3 i4 (floatofcell (findindex i1 i2 s))),i.integer1,i.integer2 in (modifycellofsheet n1 n2 s (Float n))
		else raise EmptyCellException	
	else raise InvalidInput

let rec rowmax (s:sheet) (r:range) (i:index) : sheet = 
	if ((checkrange r) && (checkindex i)) then
		let i1,i2,i3,i4,n1,n2 = r.ind1.integer1,r.ind1.integer2,r.ind2.integer1,r.ind2.integer2,i.integer1,i.integer2 in
		let r1 = (findrow i1 s) in
		let c1 = (subcell r1 [] i2 i4 0) in
		let f = (cellistmax c1 (floatofcell (findindex i1 i2 s))) in
		(rowmax (modifycellofsheet n1 n2 s (Float f)) {ind1={integer1=i1+1;integer2=i2};ind2={integer1=i3;integer2=i4}} {integer1=n1+1;integer2=n2})
	else s

let rec colmax (s:sheet) (r:range) (i:index) : sheet = 
	if((checkrange r) && (checkindex i)) then
		let i1,i2,i3,i4,n1,n2 = r.ind1.integer1,r.ind1.integer2,r.ind2.integer1,r.ind2.integer2,i.integer1,i.integer2 in
		let col = (getcol s i2 i3 i1 []) in
		let f = (cellistmax col (floatofcell (findindex i1 i2 s))) in
		(colmax (modifycellofsheet n1 n2 s (Float f)) {ind1={integer1=i1;integer2=i2+1};ind2={integer1=i3;integer2=i4}} {integer1=n1;integer2=n2+1})
	else s

let row_max (s:sheet) (r:range) (i:index) : sheet = 
	if ((checkrange r) && (checkindex i)) then rowmax s r i
	else raise InvalidInput

let col_max (s:sheet) (r:range) (i:index) : sheet = 
	if((checkrange r) && (checkindex i)) then colmax s r i
	else raise InvalidInput

(*i1<=i3, i1++ and add constant to each cell, then check i2<=i4, if yes then i2++ and i1=initi1, i5=initi5, else return sheet*) 
let rec addconst (s:sheet) (initi1:int) (i1:int) (i2:int) (i3:int) (i4:int) (f:float) (initi5:int) (i5:int) (i6:int) : sheet =
	if(i1<=i3 && i2<=i4) then
		let k= findindex i1 i2 s in
		if(k<>Empty) then
			(addconst (modifycellofsheet i5 i6 s (Float ((floatofcell k)+.f))) initi1 (i1+1) i2 i3 i4 f initi5 (i5+1) i6)  
		else raise EmptyCellException
	else if(i2<=i4) then
		(addconst s initi1 initi1 (i2+1) i3 i4 f initi5 initi5 (i6+1))
	else s

(*check if ranges and indexes are valid, then call addconst*)
let add_const (s:sheet) (r:range) (f:float) (i:index) : sheet = 
	if((checkrange r) && (checkindex i)) then 
		let i1,i2,i3,i4,i5,i6 = r.ind1.integer1,r.ind1.integer2,r.ind2.integer1,r.ind2.integer2,i.integer1,i.integer2 in
		(addconst s i1 i1 i2 i3 i4 f i5 i5 i6)
	else
		raise InvalidInput

(*similarly with all other constant functions*)
(*i1<=i3, i1++ and sub constant to each cell, then check i2<=i4, if yes then i2++ and i1=initi1, i5=initi5, else return sheet*) 
let rec subconst (s:sheet) (initi1:int) (i1:int) (i2:int) (i3:int) (i4:int) (f:float) (initi5:int) (i5:int) (i6:int) : sheet =
	if(i1<=i3 && i2<=i4) then
		let k= findindex i1 i2 s in
		if(k<>Empty) then
			(subconst (modifycellofsheet i5 i6 s (Float ((floatofcell k)-.f))) initi1 (i1+1) i2 i3 i4 f initi5 (i5+1) i6)  
		else raise EmptyCellException
	else if(i2<=i4) then
		(subconst s initi1 initi1 (i2+1) i3 i4 f initi5 initi5 (i6+1))
	else s

(*check if ranges and indexes are valid, then call subconst*)
let subt_const (s:sheet) (r:range) (f:float) (i:index) : sheet = 
	if((checkrange r) && (checkindex i)) then 
		let i1,i2,i3,i4,i5,i6 = r.ind1.integer1,r.ind1.integer2,r.ind2.integer1,r.ind2.integer2,i.integer1,i.integer2 in
		(subconst s i1 i1 i2 i3 i4 f i5 i5 i6)
	else
		raise InvalidInput

(*i1<=i3, i1++ and mult constant to each cell, then check i2<=i4, if yes then i2++ and i1=initi1, i5=initi5, else return sheet*) 
let rec multconst (s:sheet) (initi1:int) (i1:int) (i2:int) (i3:int) (i4:int) (f:float) (initi5:int) (i5:int) (i6:int) : sheet =
	if(i1<=i3 && i2<=i4) then
		let k= findindex i1 i2 s in
		if(k<>Empty) then
			(multconst (modifycellofsheet i5 i6 s (Float ((floatofcell k)*.f))) initi1 (i1+1) i2 i3 i4 f initi5 (i5+1) i6)  
		else raise EmptyCellException
	else if(i2<=i4) then
		(multconst s initi1 initi1 (i2+1) i3 i4 f initi5 initi5 (i6+1))
	else s

(*check if ranges and indexes are valid, then call multconst*)
let mult_const (s:sheet) (r:range) (f:float) (i:index) : sheet = 
	if((checkrange r) && (checkindex i)) then 
		let i1,i2,i3,i4,i5,i6 = r.ind1.integer1,r.ind1.integer2,r.ind2.integer1,r.ind2.integer2,i.integer1,i.integer2 in
		(multconst s i1 i1 i2 i3 i4 f i5 i5 i6)
	else
		raise InvalidInput

(*i1<=i3, i1++ and div constant to each cell, then check i2<=i4, if yes then i2++ and i1=initi1, i5=initi5, else return sheet*) 
let rec divconst (s:sheet) (initi1:int) (i1:int) (i2:int) (i3:int) (i4:int) (f:float) (initi5:int) (i5:int) (i6:int) : sheet =
	if(i1<=i3 && i2<=i4) then
		let k= findindex i1 i2 s in
		if(k<>Empty && f<>0.0) then
			(divconst (modifycellofsheet i5 i6 s (Float ((floatofcell k)/.f))) initi1 (i1+1) i2 i3 i4 f initi5 (i5+1) i6)  
		else if(f=0.0) then raise DivisionByZeroException 		
		else raise EmptyCellException
	else if(i2<=i4) then
		(divconst s initi1 initi1 (i2+1) i3 i4 f initi5 initi5 (i6+1))
	else s

(*check if ranges and indexes are valid, then call divconst*)
let div_const (s:sheet) (r:range) (f:float) (i:index) : sheet = 
	if((checkrange r) && (checkindex i)) then 
		let i1,i2,i3,i4,i5,i6 = r.ind1.integer1,r.ind1.integer2,r.ind2.integer1,r.ind2.integer2,i.integer1,i.integer2 in
		(divconst s i1 i1 i2 i3 i4 f i5 i5 i6)
	else
		raise InvalidInput

(*simple addcol function *)
(*in this,initi1,initi5,initi9, i1,i2,i3,i4 ranges for sheet1, i5,i6,i7,i8 for sheet2, i9,i10 index *)
(*i1-i3==i5-i7 && i2-i4==i6-i8,(else invalidinput) i1<=i3,i2<=i4,i5<=i7,i6<=i8 findindex, add, modifysheet, i1++,i5++,i9++*)
(*if i2<=i4,i6<=i8 i2++,i6++,i1=initi1,i5=initi5,i9=initi9,else return s*)
let rec addcol (s:sheet) (initi1:int) (i1:int) (i2:int) (i3:int) (i4:int) (initi5:int) (i5:int) (i6:int) (i7:int) (i8:int) (initi9:int) (i9:int) (i10:int) : sheet=
	if(((i1-i3)=(i5-i7))&&((i2-i4)=(i6-i8))) then 
		if(i1<=i3 && i2<=i4 && i5<=i7 && i6<=i8) then	
			let c1,c2 = (findindex i1 i2 s),(findindex i5 i6 s) in
				(addcol (modifycellofsheet i9 i10 s (Float ((floatofcell c1)+.(floatofcell c2)))) initi1 (i1+1) i2 i3 i4 initi5 (i5+1) i6 i7 i8 initi9 (i9+1) i10)
		else if(i2<=i4&&i6<=i8) then
			(addcol s initi1 initi1 (i2+1) i3 i4 initi5 initi5 (i6+1) i7 i8 initi9 initi9 (i10+1))
		else s
	else (raise IncompatibleColRangesException)

(*just swap everthing between i5 and i6*)
let rec addrowcol (s:sheet) (initi1:int) (i1:int) (i2:int) (i3:int) (i4:int) (initi6:int) (i5:int) (i6:int) (i7:int) (i8:int) (initi9:int) (i9:int) (i10:int) : sheet=
	if(((i1-i3)=(i6-i8))&&((i2-i4)=(i5-i7))) then 
		if(i1<=i3 && i2<=i4 && i5<=i7 && i6<=i8) then	
			let c1,c2 = (findindex i1 i2 s),(findindex i5 i6 s) in
				(addrowcol (modifycellofsheet i9 i10 s (Float ((floatofcell c1)+.(floatofcell c2)))) initi1 (i1+1) i2 i3 i4 initi6 i5 (i6+1) i7 i8 initi9 (i9+1) i10)
		else if(i2<=i4&&i5<=i7) then
			(addrowcol s initi1 initi1 (i2+1) i3 i4 initi6 (i5+1) initi6 i7 i8 initi9 initi9 (i10+1))
		else s
	else raise IncompatibleRowColException

(*Now compare i1,i2,i3,i4,i5,i6,i7,i8 if i1-i3==i5-i7 && i2-i4==i6-i8 then addcol else addrowcol else exception*)
let addrange (s:sheet) (i1:int) (i2:int) (i3:int) (i4:int) (i5:int) (i6:int) (i7:int) (i8:int) (i9:int) (i10:int) : sheet =
	if(((i1-i3)=(i5-i7))&&((i2-i4)=(i6-i8))) then (addcol s i1 i1 i2 i3 i4 i5 i5 i6 i7 i8 i9 i9 i10)
	else if(((i1-i3)=(i6-i8))&&((i2-i4)=(i5-i7))) then (addrowcol s i1 i1 i2 i3 i4 i6 i5 i6 i7 i8 i9 i9 i10)
	else (raise IncompatibleAddRangesException)

let rec subtcol (s:sheet) (initi1:int) (i1:int) (i2:int) (i3:int) (i4:int) (initi5:int) (i5:int) (i6:int) (i7:int) (i8:int) (initi9:int) (i9:int) (i10:int) : sheet=
	if(((i1-i3)=(i5-i7))&&((i2-i4)=(i6-i8))) then 
		if(i1<=i3 && i2<=i4 && i5<=i7 && i6<=i8) then	
			let c1,c2 = (findindex i1 i2 s),(findindex i5 i6 s) in
				(subtcol (modifycellofsheet i9 i10 s (Float ((floatofcell c1)-.(floatofcell c2)))) initi1 (i1+1) i2 i3 i4 initi5 (i5+1) i6 i7 i8 initi9 (i9+1) i10)
		else if(i2<=i4&&i6<=i8) then
			(subtcol s initi1 initi1 (i2+1) i3 i4 initi5 initi5 (i6+1) i7 i8 initi9 initi9 (i10+1))
		else s
	else raise IncompatibleRangesException

(*just swap everthing between i5 and i6*)
let rec subtrowcol (s:sheet) (initi1:int) (i1:int) (i2:int) (i3:int) (i4:int) (initi6:int) (i5:int) (i6:int) (i7:int) (i8:int) (initi9:int) (i9:int) (i10:int) : sheet=
	if(((i1-i3)=(i6-i8))&&((i2-i4)=(i5-i7))) then 
		if(i1<=i3 && i2<=i4 && i5<=i7 && i6<=i8) then	
			let c1,c2 = (findindex i1 i2 s),(findindex i5 i6 s) in
				(subtrowcol (modifycellofsheet i9 i10 s (Float ((floatofcell c1)-.(floatofcell c2)))) initi1 (i1+1) i2 i3 i4 initi6 i5 (i6+1) i7 i8 initi9 (i9+1) i10)
		else if(i2<=i4&&i5<=i7) then
			(subtrowcol s initi1 initi1 (i2+1) i3 i4 initi6 (i5+1) initi6 i7 i8 initi9 initi9 (i10+1))
		else s
	else raise IncompatibleRangesException

(*Now compare i1,i2,i3,i4,i5,i6,i7,i8 if i1-i3==i5-i7 && i2-i4==i6-i8 then subtcol else subtrowcol else exception*)
let subtrange (s:sheet) (i1:int) (i2:int) (i3:int) (i4:int) (i5:int) (i6:int) (i7:int) (i8:int) (i9:int) (i10:int) : sheet =
	if(((i1-i3)=(i5-i7))&&((i2-i4)=(i6-i8))) then (subtcol s i1 i1 i2 i3 i4 i5 i5 i6 i7 i8 i9 i9 i10)
	else if(((i1-i3)=(i6-i8))&&((i2-i4)=(i5-i7))) then (subtrowcol s i1 i1 i2 i3 i4 i6 i5 i6 i7 i8 i9 i9 i10)
	else raise IncompatibleRangesException

let rec multcol (s:sheet) (initi1:int) (i1:int) (i2:int) (i3:int) (i4:int) (initi5:int) (i5:int) (i6:int) (i7:int) (i8:int) (initi9:int) (i9:int) (i10:int) : sheet=
	if(((i1-i3)=(i5-i7))&&((i2-i4)=(i6-i8))) then 
		if(i1<=i3 && i2<=i4 && i5<=i7 && i6<=i8) then	
			let c1,c2 = (findindex i1 i2 s),(findindex i5 i6 s) in
				(multcol (modifycellofsheet i9 i10 s (Float ((floatofcell c1)*.(floatofcell c2)))) initi1 (i1+1) i2 i3 i4 initi5 (i5+1) i6 i7 i8 initi9 (i9+1) i10)
		else if(i2<=i4&&i6<=i8) then
			(multcol s initi1 initi1 (i2+1) i3 i4 initi5 initi5 (i6+1) i7 i8 initi9 initi9 (i10+1))
		else s
	else raise IncompatibleRangesException

(*just swap everthing between i5 and i6*)
let rec multrowcol (s:sheet) (initi1:int) (i1:int) (i2:int) (i3:int) (i4:int) (initi6:int) (i5:int) (i6:int) (i7:int) (i8:int) (initi9:int) (i9:int) (i10:int) : sheet=
	if(((i1-i3)=(i6-i8))&&((i2-i4)=(i5-i7))) then 
		if(i1<=i3 && i2<=i4 && i5<=i7 && i6<=i8) then	
			let c1,c2 = (findindex i1 i2 s),(findindex i5 i6 s) in
				(multrowcol (modifycellofsheet i9 i10 s (Float ((floatofcell c1)*.(floatofcell c2)))) initi1 (i1+1) i2 i3 i4 initi6 i5 (i6+1) i7 i8 initi9 (i9+1) i10)
		else if(i2<=i4&&i5<=i7) then
			(multrowcol s initi1 initi1 (i2+1) i3 i4 initi6 (i5+1) initi6 i7 i8 initi9 initi9 (i10+1))
		else s
	else raise IncompatibleRangesException

(*Now compare i1,i2,i3,i4,i5,i6,i7,i8 if i1-i3==i5-i7 && i2-i4==i6-i8 then multcol else multrowcol else exception*)
let multrange (s:sheet) (i1:int) (i2:int) (i3:int) (i4:int) (i5:int) (i6:int) (i7:int) (i8:int) (i9:int) (i10:int) : sheet =
	if(((i1-i3)=(i5-i7))&&((i2-i4)=(i6-i8))) then (multcol s i1 i1 i2 i3 i4 i5 i5 i6 i7 i8 i9 i9 i10)
	else if(((i1-i3)=(i6-i8))&&((i2-i4)=(i5-i7))) then (multrowcol s i1 i1 i2 i3 i4 i6 i5 i6 i7 i8 i9 i9 i10)
	else raise IncompatibleRangesException

let rec divcol (s:sheet) (initi1:int) (i1:int) (i2:int) (i3:int) (i4:int) (initi5:int) (i5:int) (i6:int) (i7:int) (i8:int) (initi9:int) (i9:int) (i10:int) : sheet=
	if(((i1-i3)=(i5-i7))&&((i2-i4)=(i6-i8))) then 
		if(i1<=i3 && i2<=i4 && i5<=i7 && i6<=i8) then	
			let c1,c2 = (findindex i1 i2 s),(findindex i5 i6 s) in
				if((floatofcell c2)=0.0) then raise DivisionByZeroException
				else
				(divcol (modifycellofsheet i9 i10 s (Float ((floatofcell c1)/.(floatofcell c2)))) initi1 (i1+1) i2 i3 i4 initi5 (i5+1) i6 i7 i8 initi9 (i9+1) i10)
		else if(i2<=i4&&i6<=i8) then
			(divcol s initi1 initi1 (i2+1) i3 i4 initi5 initi5 (i6+1) i7 i8 initi9 initi9 (i10+1))
		else s
	else raise IncompatibleRangesException

(*just swap everthing between i5 and i6*)
let rec divrowcol (s:sheet) (initi1:int) (i1:int) (i2:int) (i3:int) (i4:int) (initi6:int) (i5:int) (i6:int) (i7:int) (i8:int) (initi9:int) (i9:int) (i10:int) : sheet=
	if(((i1-i3)=(i6-i8))&&((i2-i4)=(i5-i7))) then 
		if(i1<=i3 && i2<=i4 && i5<=i7 && i6<=i8) then	
			let c1,c2 = (findindex i1 i2 s),(findindex i5 i6 s) in
				if((floatofcell c2)=0.0) then raise DivisionByZeroException
				else
				(divrowcol (modifycellofsheet i9 i10 s (Float ((floatofcell c1)/.(floatofcell c2)))) initi1 (i1+1) i2 i3 i4 initi6 i5 (i6+1) i7 i8 initi9 (i9+1) i10)
		else if(i2<=i4&&i5<=i7) then
			(divrowcol s initi1 initi1 (i2+1) i3 i4 initi6 (i5+1) initi6 i7 i8 initi9 initi9 (i10+1))
		else s
	else raise IncompatibleRangesException

(*Now compare i1,i2,i3,i4,i5,i6,i7,i8 if i1-i3==i5-i7 && i2-i4==i6-i8 then divcol else divrowcol else exception*)
let divrange (s:sheet) (i1:int) (i2:int) (i3:int) (i4:int) (i5:int) (i6:int) (i7:int) (i8:int) (i9:int) (i10:int) : sheet =
	if(((i1-i3)=(i5-i7))&&((i2-i4)=(i6-i8))) then (divcol s i1 i1 i2 i3 i4 i5 i5 i6 i7 i8 i9 i9 i10)
	else if(((i1-i3)=(i6-i8))&&((i2-i4)=(i5-i7))) then (divrowcol s i1 i1 i2 i3 i4 i6 i5 i6 i7 i8 i9 i9 i10)
	else raise IncompatibleRangesException

(*Now simply calling the functions which were pre defined*)
let add_range (s:sheet) (r1:range) (r2:range) (i:index) : sheet = 
	if((checkrange r1) && (checkrange r2) && (checkindex i)) then
		let i1,i2,i3,i4,i5,i6,i7,i8,i9,i10 = r1.ind1.integer1,r1.ind1.integer2,r1.ind2.integer1,r1.ind2.integer2,r2.ind1.integer1,r2.ind1.integer2,r2.ind2.integer1,r2.ind2.integer2,i.integer1,i.integer2 in
		(addrange s i1 i2 i3 i4 i5 i6 i7 i8 i9 i10)
	else raise InvalidInput

let subt_range (s:sheet) (r1:range) (r2:range) (i:index) : sheet = 
	if((checkrange r1) && (checkrange r2) && (checkindex i)) then
		let i1,i2,i3,i4,i5,i6,i7,i8,i9,i10 = r1.ind1.integer1,r1.ind1.integer2,r1.ind2.integer1,r1.ind2.integer2,r2.ind1.integer1,r2.ind1.integer2,r2.ind2.integer1,r2.ind2.integer2,i.integer1,i.integer2 in
		(subtrange s i1 i2 i3 i4 i5 i6 i7 i8 i9 i10)
	else raise InvalidInput

let mult_range (s:sheet) (r1:range) (r2:range) (i:index) : sheet = 
	if((checkrange r1) && (checkrange r2) && (checkindex i)) then
		let i1,i2,i3,i4,i5,i6,i7,i8,i9,i10 = r1.ind1.integer1,r1.ind1.integer2,r1.ind2.integer1,r1.ind2.integer2,r2.ind1.integer1,r2.ind1.integer2,r2.ind2.integer1,r2.ind2.integer2,i.integer1,i.integer2 in
		(multrange s i1 i2 i3 i4 i5 i6 i7 i8 i9 i10)
	else raise InvalidInput

let div_range (s:sheet) (r1:range) (r2:range) (i:index) : sheet = 
	if((checkrange r1) && (checkrange r2) && (checkindex i)) then
		let i1,i2,i3,i4,i5,i6,i7,i8,i9,i10 = r1.ind1.integer1,r1.ind1.integer2,r1.ind2.integer1,r1.ind2.integer2,r2.ind1.integer1,r2.ind1.integer2,r2.ind2.integer1,r2.ind2.integer2,i.integer1,i.integer2 in
		(divrange s i1 i2 i3 i4 i5 i6 i7 i8 i9 i10)
	else raise InvalidInput

let add_index (s:sheet) (r:range) (index1:index) (index2:index) : sheet =
	if((checkrange r) && (checkindex index1) && (checkindex index2)) then
		let f = (findindex index1.integer1 index1.integer2 s) in
		(add_const s r (floatofcell f) index2)
	else raise InvalidInput

let subt_index (s:sheet) (r:range) (index1:index) (index2:index) : sheet =
	if((checkrange r) && (checkindex index1) && (checkindex index2)) then
		let f = (findindex index1.integer1 index1.integer2 s) in
		(subt_const s r (floatofcell f) index2)
	else raise InvalidInput

let mult_index (s:sheet) (r:range) (index1:index) (index2:index) : sheet =
	if((checkrange r) && (checkindex index1) && (checkindex index2)) then
		let f = (findindex index1.integer1 index1.integer2 s) in
		(mult_const s r (floatofcell f) index2)
	else raise InvalidInput

let div_index (s:sheet) (r:range) (index1:index) (index2:index) : sheet =
	if((checkrange r) && (checkindex index1) && (checkindex index2)) then
		let f = (findindex index1.integer1 index1.integer2 s) in
		(div_const s r (floatofcell f) index2)
	else raise InvalidInput

(*takes the list of string as the input and gives the list of cells as the output, which would now be added to sheet by calling insertrowinsheet function*)
let rec makecellrow (l:string list) (c:cell list) : cell list = 
	match l with
	| [] -> c
	| x::xs -> if(x="") then (makecellrow xs (c@[Empty])) else (makecellrow xs (c@[(Float (float_of_string x))]))
(*simply takes the list of cell as input and appends/adds it to the sheet*)
let insertrowinsheet (s:sheet) (c:cell list) : sheet = s@[c]
