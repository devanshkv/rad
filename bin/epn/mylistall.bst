%
%    BIBLIOGRAPHY style `MNRAS'
%
%                        NADGERED by PAH & JDB

%   NOTES:
%         The style file MNRAS.STY MUST be used in documentstyle
%         command as this defines \cite (ie. (NAME DATE) citation)) and
%         \scite (ie. NAME (DATE) citation) and pcite{}
%
%         The journal abbreviations are embedded in the middle of this 
%	  file. e.g. MNRAS = "Mon. Not. R. astr. Soc."

                                                                 
          
ENTRY
  { abstract
    address
    annote
    author
    booktitle
    chapter
    edition
    editor
    howpublished
    index                           
    institution
    journal
    key
    month
    note
    number
    organization
    pages
    publisher
    school
    series
    title
    type
    volume
    year
  }
  {}
  { label extra.label sort.label }

INTEGERS { output.state before.all mid.sentence after.sentence after.block }

FUNCTION{init.state.consts}
{ #0 'before.all :=
  #1 'mid.sentence :=
  #2 'after.sentence :=
  #3 'after.block :=
}

STRINGS { s t }

Function{output.nonnull}
{ 's :=
  output.state mid.sentence =
    { ", " * write$
    }
    { output.state after.block =
	{ 
	  add.period$ write$
	}
	{ output.state before.all =
	    'write$
	    { add.period$ " " * write$ }
	  if$
	}
      if$
    }
  if$
  mid.sentence 'output.state :=
  s
}

FUNCTION{output}
{ 's  :=
  s "" =
    'skip$
    { s output.nonnull
    }
  if$
}

FUNCTION{output.check}
{ 's  :=
  't  :=
  s "" =
    { "Warning: the " t * " shouldn't be empty in " * cite$ * top$
    }
    { s output.nonnull
    }
  if$
}

FUNCTION{output.bibitem}
{ newline$
  "<tr><td valign=top><a NAME=" write$
  cite$ write$
  "></a><b>" write$
  cite$ write$
  "</b>:</td><td> " write$
  ""
 before.all   'output.state :=
}

FUNCTION{fin.entry}
{ add.period$
  write$
  "</td></tr> " write$
  newline$
}

FUNCTION{new.block}
{ output.state before.all =
    'skip$
    { after.block 'output.state := }
  if$
}

FUNCTION{new.sentence}
{ output.state after.block =
    'skip$
    { output.state before.all =
	'skip$
	{  after.sentence 'output.state := }
      if$
    }
  if$
}

FUNCTION{not}
{   { #0 }
    { #1 }
  if$
}

FUNCTION{and}
{   'skip$
    { pop$ #0 }
  if$
}

FUNCTION{or}
{   { pop$ #1 }
    'skip$
  if$
}

FUNCTION{field.or.null}
{ duplicate$
  empty$
    { pop$ ""
    }
    'skip$
  if$
}

FUNCTION{italicize}
{ 's  :=
  s "" =
    { "" }
    { " " s * "" * }
  if$
}

INTEGERS{ nameptr namesleft numnames }

STRINGS{ nameresult }

FUNCTION{format.names}              
{ 's  :=
  #1 'nameptr  :=
  "" 'nameresult  :=
  s num.names$ 'numnames  :=
  numnames 'namesleft  :=
    { namesleft #0 > }
    {
      s nameptr "{vv }{ll}{, jj}{, f.}" format.name$ 't  :=
      nameptr #1 >
	{ namesleft #1 >
	    { nameresult ", " * t * 'nameresult  := }
	    { numnames #2 >
		{ nameresult " " * 'nameresult  := }
		'skip$
	      if$
	      t "others" =
		{ nameresult " {\it et al.}" * 'nameresult  := }
		{ nameresult " & " * t * 'nameresult  := }
	      if$
	    }
	  if$
	}
	{ nameresult t * 'nameresult  := }
      if$
      nameptr #1 + 'nameptr  :=
      namesleft #1 - 'namesleft  :=
    }    
  while$
  nameresult

}            



FUNCTION{format.authors}                   
{ author empty$
    {""}
    {author format.names}
  if$
}

FUNCTION{format.editors}
{ editor empty$
    { ""
    }
    { editor num.names$ #1 >
	{ "eds " editor format.names * }
	{ "ed. " editor format.names * }
      if$
    }
  if$
}

FUNCTION{format.title}
{ title empty$
    { "" }
    { "<i> " title * "</i>" * }
  if$
}

FUNCTION {format.booktitle}
{ booktitle empty$
    { "" }
    { " <b> " booktitle * "</b>" *}
  if$
}

FUNCTION{entry.string.max} { #100 }

FUNCTION{global.string.max} { #500 }

STRINGS{ pageresult }

FUNCTION{n.dashify}
{ 't  :=
  "" 'pageresult  :=
    { t "" = not }
    { t #1 #1 substring$ "-" =
	{ t #1 #2 substring$ "--" = not
	    { pageresult "--" * 'pageresult  :=
	      t #2 global.string.max substring$ 't  :=
	    }
	    {   { t #1 #1 substring$ "-" =}
		{ pageresult "-" * 'pageresult  :=
		  t #2 global.string.max substring$ 't  :=
		}
	      while$
	    }
	  if$
	}
	{ pageresult t #1 #1 substring$ * 'pageresult  :=
	  t #2 global.string.max substring$ 't  :=
	}
      if$
    }
  while$
  pageresult
}

STRINGS{ firstpage}
FUNCTION{extract.firstpage}
{ 't  :=
  "" 'firstpage  :=
     {t #1 #1 substring$ "-" = not 
      t #1 #1 substring$ "" = not 
      and
     }
       {firstpage t #1 #1 substring$ * 'firstpage  :=
        t #2 global.string.max substring$ 't  :=
     }
     while$
   firstpage
}

FUNCTION{format.vol.num.pages}
{ " " volume field.or.null * 
  number empty$
    { "" }
    { "(" number * ")" * }
  if$
  * "" *
  pages empty$
    { "" }
    { ", " pages n.dashify * }
  if$
  *
}

FUNCTION{format.iau.num}
{ "IAU Circ. No. " number field.or.null * 
}

FUNCTION{format.date}
{ year empty$
    { month empty$
	{ "" }
	{ "Warning: here's a month but no year in " cite$ * top$
	  month
	}
      if$
    }    
    { month empty$
	{ "" year * extra.label * "" * }
	{ "" year * "" * }
      if$
    }
  if$
}

FUNCTION{format.btitle}
{ title field.or.null
  italicize
}

FUNCTION{format.bvolume}
{ volume empty$
    { series empty$
	{ "" }
	{ series italicize }
      if$
    }
    {   series empty$
	'skip$
	{ "{\bf " volume * ",}" * }
	{ "Volume" volume * " of " * series italicize * }
      if$
    }
  if$
}

FUNCTION{format.edition}
{ edition empty$
    { "" }
    { edition "ll" change.case$ " edition" * }
  if$
}

INTEGERS{ multiresult }

FUNCTION{multi.page.check}
{ 't  :=
  #0 'multiresult  :=
    { multiresult not
      t "" = not
      and
    }
    { t #1 #1 substring$ "-" =
      t #1 #1 substring$ "," =
      or
	{ #1 'multiresult  := }
	{ t #2 global.string.max substring$ 't  := }
      if$
    }
  while$
  multiresult
}

%FUNCTION{format.pages}
%{ pages empty$
%    { "" }
%    { pages multi.page.check
%	{ "p." pages extract.firstpage * }
%	{ pages extract.firstpage }
%      if$
%    }
%  if$
%}

FUNCTION {format.pages}
{ pages missing$
    { "" }
    { pages multi.page.check
	{ "" pages n.dashify * }
	{ "" pages n.dashify * }
      if$
    }
  if$
}



FUNCTION{format.chapter.pages}
{ chapter empty$
    'format.pages
    { "chapter " chapter *
      pages empty$
	'skip$
	{ ", " * format.pages * }
      if$
    }
  if$
}

FUNCTION{format.in.ed.booktitle}
{ booktitle empty$
    { ""
    }
    { format.editors 's  :=
      s "" =
	{ "In: " booktitle italicize * }
	{ "In: " s * ", " * booktitle italicize * }
      if$
    }
  if$
}

FUNCTION{format.tr.number}
{ type empty$
    { "Technical Report" }
    { type }
  if$
  number empty$
    'skip$
    { " " * number * }
  if$
}

FUNCTION{article}        % OK
{ output.bibitem
  "author" format.authors output.check
  "year" format.date output.check
  new.block
  "title" format.title output.check
  new.block
  "journal"  journal field.or.null italicize output.check 
  format.vol.num.pages output
  new.block
  note field.or.null output
  fin.entry
}        
FUNCTION{iauc}
{ output.bibitem
  "author" format.authors output.check
  "title" format.title output.check
  new.block
  "year" format.date output.check
  new.block
  format.iau.num italicize output
  new.block
  note field.or.null output
  fin.entry
}        

FUNCTION{book}
{ output.bibitem
  author empty$
    { "author & editor" format.editors output.check
    }
    { "author" format.authors output.check
    }
  if$
  "year" format.date output.check
  new.block
  format.bvolume output
  "title" format.btitle output.check
  "publisher" publisher field.or.null output.check
  address field.or.null output
  format.edition output
  new.block
  note field.or.null output
  fin.entry
}

FUNCTION{booklet}
{ output.bibitem
  format.authors output
  new.block
  howpublished field.or.null output
  address field.or.null output
  format.date output
  new.block
  note field.or.null output
  fin.entry
}

FUNCTION{inbook}
{ output.bibitem
  author empty$
    { "author & editor" format.editors output.check
    }
    { "author" format.authors output.check
    }
  if$
  new.block
  "title" format.btitle output.check
  "chapter & pages" format.chapter.pages output.check
  new.block
  format.bvolume output
  "publisher" publisher field.or.null output.check
  address field.or.null output
  format.edition output
  "year" format.date output.check
  new.block
  note field.or.null output
  fin.entry
}

FUNCTION{incollection}
{ output.bibitem
  "authors" format.authors output.check
  new.block
  "booktitle" format.in.ed.booktitle output.check
  format.chapter.pages output
  "publisher" publisher field.or.null output.check
  address field.or.null output
  "year" format.date output.check
  new.block
  note field.or.null output
  fin.entry
}

FUNCTION{inproceedings}       % OK
{ output.bibitem
  "author" format.authors output.check
  "year" format.date output.check
  new.block                        
  "title" format.title output.check
  new.block
  "booktitle" format.booktitle output.check
  "pages" format.pages output.check
  "editor" format.editors output.check
  organization field.or.null output
  publisher field.or.null output
  address field.or.null output
  new.block
  note field.or.null output
  fin.entry
}

FUNCTION{conference} {inproceedings}

FUNCTION{manual}
{ output.bibitem
  format.authors output
  new.block
  "title" format.btitle output.check
  new.block
  organization field.or.null output
  address field.or.null output
  format.edition output
  format.date output
  new.block
  note field.or.null output
  fin.entry
}

FUNCTION{mastersthesis}
{ output.bibitem
  "author" format.authors output.check
  new.block
  "Master's thesis" output
  "school" school field.or.null output.check
  address field.or.null output
  "year" format.date output.check
  new.block
  note field.or.null output
  fin.entry
}

FUNCTION{misc}               % OK
{ output.bibitem 
  "author" format.authors output.check
  "year" format.date output.check
  new.block
  "title" format.title italicize output.check
  "note" note field.or.null italicize output.check
  howpublished field.or.null output
  new.block
  fin.entry
}

FUNCTION{phdthesis}
{ output.bibitem
  "author" format.authors output.check
  "year" format.date output.check
  new.block
  "{\it PhD thesis}" output
  "school" school field.or.null output.check
  address field.or.null output
  new.block
  note field.or.null output
  fin.entry
}

FUNCTION{proceedings}
{ output.bibitem
  editor empty$
    { organization empty$
	'skip$
	{ organization field.or.null output }
      if$
    }
    { format.editors output }
  if$
  new.block
  "title" format.btitle output.check
  editor empty$
    'skip$
    { organization field.or.null output }
  if$
  publisher field.or.null output
  address field.or.null output
  "year" format.date output.check
  new.block
  note field.or.null output
  fin.entry
}

FUNCTION{techreport}
{ output.bibitem
  "author" format.authors output.check
  "year" format.date output.check
  new.block
  format.tr.number output
  "institution" institution field.or.null output.check
  address field.or.null output
  new.block
  note field.or.null output
  fin.entry
}

FUNCTION{unpublished}
{ output.bibitem
  "author" format.authors output.check
  new.block
  format.date output
  new.block
  "note" note field.or.null output.check
  fin.entry
}

FUNCTION{default.type} {book}

MACRO{jan} {"January"}

MACRO{feb} {"February"}

MACRO{mar} {"March"}

MACRO{apr} {"April"}

MACRO{may} {"May"}

MACRO{jun} {"June"}

MACRO{jul} {"July"}

MACRO{aug} {"August"}

MACRO{sep} {"September"}

MACRO{oct} {"October"}

MACRO{nov} {"November"}

MACRO{dec} {"December"}

% journal names

MACRO{aa}{"Astr.\,Astrophys."}

MACRO{acta }{ "Acta Astron."}

MACRO{aj}{"Astron.\,J."}

MACRO{apj}{"Astrophys.\,J."}

MACRO{anyas}{"Ann.\,N.\,Y.\,Acad.\,Sci."}

MACRO{apjl}{"Astrophys.\,J.\,Lett."}

MACRO{apjsupp}{"Astrophys.\,J.\,Supp.\,Series"}

MACRO{anj}{"Astron.\,J."}

MACRO{apl}{"Astrophys.\,Lett."}

MACRO{aasup}{"Astr.\,Astrophys.\,Suppl.\,Ser."}

MACRO{apspsci}{"Astrophys.\,Space\,Sci."}

MACRO{araa}{"Ann.\,Rev.\,Astr.\,Ap."}

MACRO{ajp}{"Aust.\,J.\,Phys."}

MACRO{baas}{ "Bull. American Astron. Soc."}

MACRO{ban}{"Bull.\,Astr.\,Inst.\,Netherlands"}

MACRO{cntphy}{"Contemp.\,Phys."}

MACRO{comas}{"Comments\,Astrophys."}

MACRO{cursci}{"Curr.\,Sci."}

MACRO{gjras}{ "Geophys.\,J.\,R.\,astr.\,soc."}

MACRO{iauc}{"IAU Circ"}

MACRO{jaa}{"J.\,Astrophys.\,Astr."}

MACRO{jatp}{"J.\,Atm.\,Phys."}

MACRO{MemRAS }{ "Mem. R. astr. Soc."}

MACRO{mnras}{"Mon.\,Not.\,R.\,astr.\,Soc."}

MACRO{na}{"Nature"}

MACRO{nap}{"Nature Phys.\,Sci."}

MACRO{obs}{"Observatory\,"}

MACRO{physrev}{"Phys.\,Rev.\,"}

MACRO{prl}{"Phys.\,Rev.\,Lett."}

MACRO{pnas}{"Proc.\,Nat.\,Acad.\,Sci."}

MACRO{qjras}{"Q.\,Jl\,R.\,ast.\,Soc.\,"}

MACRO{radsci}{"Rad.\,Sci."}

MACRO{revgsp}{ "Reviews of Geophysics and Space Physics"}

MACRO{sa}{"Sov.\,Astron."}

MACRO{sal}{"Sov.\,Astron.\,Lett."}

MACRO{sci}{"Science"}

MACRO{sphys}{"Soviet\,Phys.\,JETP\,"}

MACRO{spscrev}{"Space Sci.\,Rev."}


READ                                  

FUNCTION{sortify}
{ purify$
  "l" change.case$
}

FUNCTION{format.lab.names}
{ 's :=
  s num.names$ 'numnames  :=
  numnames #1 >
    { numnames #3 >
     { s #1 "{vv }{ll}" format.name$ 't  :=
           t " {\it et al.}" * 'nameresult  :=  }
     { numnames 'namesleft  := 
      "" 'nameresult  :=
      #1 'nameptr  :=
	{ namesleft #0 > }
	{ nameptr numnames =
	    { s nameptr "{ff}{vv}{ll}{jj}" format.name$ "others" =
		{ nameresult " {\it et al.}" * 'nameresult  := }
		{ nameresult " & " * 
                   s nameptr "{vv}{ll}" format.name$ * 'nameresult :=
		}
	      if$
	    }
	    { namesleft #2 =
              { nameresult s nameptr "{vv}{ll}" format.name$ * 'nameresult := }
              { nameresult s nameptr "{vv}{ll}" format.name$ * ", " * 'nameresult := 
              }
            if$
	    }
	  if$
	  nameptr #1 + 'nameptr  :=
	  namesleft #1 - 'namesleft  :=
	}
      while$
     }
     if$
    }
    { s #1 "{vv }{ll}" format.name$ 't  :=
      t 'nameresult  :=
    }
  if$
  nameresult
}

FUNCTION{calc.label}
{
  author empty$
    { editor empty$
	{ key empty$
	    { "Warning: need a key to make a label in " cite$ * top$
	      title field.or.null purify$ #1 #3 substring$
	    }
	    { key purify$ }
	  if$
	}
	{ editor format.lab.names }
      if$
    }
    { author format.lab.names }
  if$
   "<" * year field.or.null purify$  * 
  'label :=
   label sortify 'sort.label :=

}

FUNCTION{sort.format.names}
{
 's :=
  #1 'nameptr :=
  "" 'nameresult :=
  s num.names$ 'numnames :=
  numnames 'namesleft :=
    { namesleft #0 > }
    { nameptr #1 >
	{ nameresult "   " * 'nameresult := }
	'skip$
      if$
      s nameptr "{vv{ } }{ll{ }}{  ff{ }}{  jj{ }}" format.name$ 't :=
      nameptr numnames = t "others" = and
	{ nameresult "et al" * 'nameresult := }
	{ nameresult t sortify * 'nameresult := }
      if$
      nameptr #1 + 'nameptr :=
      namesleft #1 - 'namesleft :=
    }
  while$
  nameresult
}

INTEGERS { len }

FUNCTION{chop.word}
{ 's  :=
  'len  :=
  s #1 len substring$ =
    { s len #1 + global.string.max substring$ }
    { s }
  if$
}

FUNCTION{sort.format.Btitle}
{ 't  :=
  "A " #2
    "An " #3
      "The " #4 t chop.word
    chop.word
  chop.word
  #1 global.string.max substring$
  sortify
}

FUNCTION{presort}
{
  calc.label
  sort.label
  author empty$
    { editor empty$
	{ key field.or.null sortify }
	{ editor sort.format.names }
      if$
    }
    { author sort.format.names }
  if$
  *
  volume field.or.null
  *
  " "
  *
  pages field.or.null
  *
  " "
  *
  number field.or.null
  *
  #1 entry.string.max substring$
  'sort.key$  :=
}

ITERATE{presort}

SORT

STRINGS{longest.label last.sort.label next.extra}

INTEGERS{longest.label.width last.extra.num}

FUNCTION{initialize.longest.label}
{  "" 'longest.label :=
  "" 'last.sort.label :=
  "" 'next.extra :=
  #0 'longest.label.width :=
  #0 'last.extra.num :=
}

FUNCTION{forward.pass}
{ last.sort.label sort.label =
    { last.extra.num #1 + 'last.extra.num :=
      last.extra.num int.to.chr$ 'extra.label :=
    }
    { "a" chr.to.int$ 'last.extra.num :=
      "" 'extra.label :=
      sort.label 'last.sort.label :=
    }
  if$
}

FUNCTION{reverse.pass}
{ next.extra "b" =
    {  "a" 'extra.label := }
    'skip$
  if$
  label extra.label * ">" * 'label  :=
  label width$ longest.label.width >
    {  label 'longest.label :=
       label width$  'longest.label.width :=
    }
    'skip$
  if$
  extra.label 'next.extra :=
}

EXECUTE{initialize.longest.label}

ITERATE{forward.pass}

REVERSE{reverse.pass}

FUNCTION{preamble}
{ " " write$
  newline$
}

EXECUTE{preamble}

EXECUTE{init.state.consts}

ITERATE{call.type$}

FUNCTION{finish.up}{ newline$ " " write$ newline$}

EXECUTE{finish.up}
