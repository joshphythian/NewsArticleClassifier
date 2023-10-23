
structure NaiveBayes :> NAIVE_BAYES_CLASSIFIER =
struct

    type category = string

    type labeled_document = category * string Seq.seq
    type document = string Seq.seq
        
    type statistics = 
          (category,int) Dict.dict           (* maps each category to number of documents with that category *)
        * (category,int) Dict.dict           (* maps each category to number of words in documents with that category *)
        * (category * string, int) Dict.dict (* maps each (cat,word) to frequency *)
        * category Seq.seq                   (* list of categories (no duplicates) *)
        * int                                (* total number of documents *)
        * int                                (* total number of different words *)

    (* TASK *)
    fun ssC((s, t), (s', t')) =
    case String.compare(s, s') of 
    GREATER => GREATER
    | LESS => LESS
    | EQUAL => String.compare(t, t') 


    fun gather (train : labeled_document MR.mapreducable) : statistics = 
    ((* 1 *)
    ExtractCombine.extractcombine(
      String.compare,
      fn(c, w) => Seq.singleton((c, 1)),
      fn(v, v') => v+v',
      train
    ),
    (* 2 *)
    ExtractCombine.extractcombine(
      String.compare,
      fn(c, w) => Seq.singleton(c, Seq.mapreduce(fn(x) => 1, 0, (fn(x, y) => x+y), w)),
      fn(v, v') => v+v',
      train
    ),
    (* 3 *)
    ExtractCombine.extractcombine(
      ssC,
      fn(c, words) => Seq.map(fn w => ((c, w), 1), words),
      fn(v, v') => v+v',
      train
    ),
    (* 4 *)
    Seq.map(fn (x, y) => x, Dict.toSeq(ExtractCombine.extractcombine(
      String.compare,
      fn(c, w) => Seq.singleton(c, 1), 
      fn(v, v') => v+v',
      train
    ))), 
    (* 5 *)
    MR.mapreduce(
    fn (x) => 1,
    0,
    fn(x,y) => x+y,
    train),
    (* 6 *)
    Seq.mapreduce(
    fn (x) => 1,
    0,
    fn(x, y) => x+y,
    Dict.toSeq(ExtractCombine.extractcombine(
    String.compare,
     fn(c, w) => Seq.map(fn(x) => (x,1),w),
    fn(x,y) => x+y,
    train
    )))
   )
     
    (* TASK *)
    fun s2r(opt): real = 
    case opt of
    SOME(k) => real(k)
    | NONE => real(0)


    fun possible_classifications 
        ((num_docs_by_cat,
          num_words_by_cat,
          freqs,
          all_categories, 
          total_num_docs,
          total_num_words) : statistics,
         test_doc : document) : (category * real) Seq.seq =
            Seq.map(
                fn(x) => (x, (
                  (Math.ln(s2r(Dict.lookup(String.compare, num_docs_by_cat, x))/ 
                real(total_num_docs)) + 

                  let val total_words_in_C = (s2r(Dict.lookup(String.compare, num_words_by_cat, x)))
                    in
                      (* real Seq.mapreduce(fn(wi) => (case s2r(Dict.lookup(ssC, freqs, (x, wi))) of
                        real(0) => Math.ln(real(1)/real (total_num_words))
                        | _ => Math.ln(s2r(Dict.lookup(String.compare, (x, wi),freqs)))/total_words_in_C), *)
                        Seq.mapreduce(fn(wi) => (case (Dict.lookup(ssC, freqs, (x, wi))) of
                        NONE => Math.ln(real(1)/real (total_num_words))
                        | SOME x => Math.ln(real(x)/total_words_in_C)),
                      real(0),
                      fn(x,y) => x+y,
                      test_doc
                      )
                    end
                  )
                )
              ), all_categories
            )
         
         
         
         

    fun tC((x,y), (x', y')) =
    case Real.compare(y,y') of
    GREATER => (x,y)
    | EQUAL => (x,y)
    | LESS => (x',y')
    


    (* TASK *)
    fun classify (stats : statistics, test_doc : document) : (category * real) = 
    Seq.reduce(
      tC, 
      ("", Real.negInf),
      possible_classifications(stats, test_doc)
      )

    (* TASK *)
    fun train_classifier (train : labeled_document MR.mapreducable) : document -> (category * real) =
   let val t = gather(train)
        in 
        fn doc => classify(t, doc)
        end
   (* 
   Small: 5/8
   Medium: 680/808
   Big: 70122/78899
   big train and medium test: 704/808
   It appears that it does help to do a large train on a medium test.
   
   *)     
end
