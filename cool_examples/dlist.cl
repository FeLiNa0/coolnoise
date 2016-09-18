(* This class represents an empty list, or the ends of a doubly-linked list. *)
class DList {
  io : IO <- new IO;
  length() : Int { 0 };
};

(* This class represents a node in a doubly-linked list that has one or more
elements. *) 
class DListElem inherits DList {
  next : DList <- new DList;
  prev : DList <- new DList;

  (* Returns 1 + the number of elements after this element. *)
  length() : Int { 1 + get_next().length() };

  get_prev() : DList { prev };  

  set_prev(p : DList) : SELF_TYPE {{ 
    prev <- p; self; }};

  get_next() : DList { next };  

  set_next(p : DList) : SELF_TYPE {{ 
    next <- p; self; }};

  (* Erases all references to other DListElems. *)
  clear_links() : SELF_TYPE {{
    next <- new DList;
    prev <- new DList;
    self; }};

  (* Returns the element at the end of the list. *)
  to_end() : DListElem {
    let end : DListElem <- self
    in{ while 0 < end.get_next().length() loop
          case end.get_next() of e : DListElem => end <- e; esac  
        pool;
        end; }};

  (* Returns the element at the start of the list. *)
  to_start() : DListElem {
    let start : DListElem <- self
    in{ while 0 < start.get_prev().length() loop
          case start.get_prev() of e : DListElem => start <- e; esac 
        pool;
        start; }};

  (* Adds an element to the end of the list. *)
  append_element(new_elem : DListElem) : DListElem {
    to_end().insert_after(new_elem) };

  (* Adds an element to the start of the list. *)
  prepend_element(new_elem : DListElem) : DListElem {
    to_start().insert_before(new_elem) };

  (* Inserts an element after this element. Returns the newly added element. *)
  insert_after(new_next : DListElem) : DListElem {
    let old_next : DList <- next
    in{ new_next.set_prev(self)
                .set_next(old_next);
        next <- new_next;
        case old_next of
          old_next_elem : DListElem => 
            old_next_elem.set_prev(next);
          empty : DList => false;
        esac;
        new_next; }};

  (* Inserts an element before this element. Returns the newly added element. *)
  insert_before(new_prev : DListElem) : DListElem { 
    let old_prev : DList <- prev
    in{ new_prev.set_prev(old_prev)
                .set_next(self);
        prev <- new_prev;
        case old_prev of
          old_prev_elem : DListElem => 
            old_prev_elem.set_next(new_prev); 
          empty : DList => false; 
        esac;
        new_prev; }};

  out_content() : SELF_TYPE {{
    io.out_string("_"); self; }};

  (* Prints every element in the list. Output resembles: 
  <self.out_content()> <delim> <self.next.out_content()> ... <last_element.out_contents()> *)
  out_list_with_delim(delim : String) : SELF_TYPE {{
    out_content();

    case get_next() of 
      next_elem : DListElem => {
        io.out_string(delim);
        next_elem.out_list_with_delim(delim); };
      empty : DList => io.out_string("\n");
    esac;  
    self;
  }};

  out_list() : SELF_TYPE { out_list_with_delim(", ") };
};

(* Represents a node in a doubly-linked list that contains a string. *)
class DListStrElem inherits DListElem {
  contents : String <- "none!";

  init(s : String) : SELF_TYPE {{
    set_str(s); self; }};

  get_str() : String { contents };

  set_str(s : String) : SELF_TYPE {{ 
    contents <- s; self; }};

  a(s : String) : SELF_TYPE { 
    let new_elem : SELF_TYPE <- (new SELF_TYPE).set_str(s)
    in{ insert_after(new_elem);
        new_elem; }};

  b(s : String) : SELF_TYPE { 
    let new_elem : SELF_TYPE <- (new SELF_TYPE).set_str(s)
    in{ insert_before(new_elem);
        new_elem; }};

  out_content() : SELF_TYPE {{ 
    io.out_string(contents); self; }};
};

(* Represents a node in a doubly-linked list that contains an integer. *)
class DListIntElem inherits DListStrElem {
  a2i : A2I <- new A2I;
  int_contents : Int <- 0;

  get_int() : Int { int_contents };

  set_int(i : Int) : SELF_TYPE {{
    int_contents <- i; self; }};

  set_str(s : String) : SELF_TYPE {{
    contents <- s; 
    set_int(a2i.a2i(s)); 
    self; }};

  out_content() : SELF_TYPE {{ 
    io.out_int(int_contents); self; }};
};

