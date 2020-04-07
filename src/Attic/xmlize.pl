:- module(xmlize, [xmlize/1, xmlize/2, xmlOutput/3, xmlHeader/1], []).

:- use_module(library(write)).

xmlize(T,F) :-
        open(F,write,S),
        numbervars(T,0,_),
        xmlHeader(S),
        xmlOutput(T,S,0),
        close(S).
        
xmlize(T) :-
        numbervars(T,0,_),
        xmlHeader(user_output),
        xmlOutput(T,user_output,0).
        

xmlHeader(S) :-
        write(S,'<?xml version=''1.0'' encoding=''utf-8''?>'),
        nl(S).
        
xmlOutput('$VAR'(X),S,I) :-     % print variables
        !,
        indent(I,Spaces),
        write(S,Spaces),
        write(S,'<var>'),
        write(S,X),
        write(S,'</var>'),
        nl(S).
xmlOutput(X,S,I) :-             % print atoms terms
        atomic(X),
        !,
        indent(I,Spaces),
        write(S,Spaces),
        write(S,'<const>'),
        write(S,X),
        write(S,'</const>'),
        nl(S).
xmlOutput([X|Xs],S,I) :-        % handle complete lists separately
        completeList([X|Xs]),
        !,
        indent(I,Spaces),
        write(S,Spaces),
        write(S,'<cons>'),
        nl(S),
        incrementIndent(I,I1),
        xmlArgElements([X|Xs],S,I1),
        write(S,Spaces),
        write(S,'</cons>'),
        nl(S).
xmlOutput(string(A),S,I) :-     % handle string terms separately
        !,
        indent(I,Spaces),
        write(S,Spaces),
        write(S,'<string>'),
        write(S,A),
        write(S,'</string>'),
        nl(S).
xmlOutput(T,S,I) :-             % print structured terms
        T =.. [F|Zs],
        xmlName(F,FX),
        indent(I,Spaces),
        write(S,Spaces),
        write(S,'<'),
        write(S,FX),
        write(S,'>'),
        nl(S),
        incrementIndent(I,I1),
        xmlArgElements(Zs,S,I1),
        write(S,Spaces),
        write(S,'</'),
        write(S,FX),
        write(S,'>'),
        nl(S).
        
xmlArgElements([],_,_).
xmlArgElements([X|Xs],S,I1) :-
        xmlOutput(X,S,I1),
        xmlArgElements(Xs,S,I1).
        
completeList([]).
completeList([_|Xs]) :-
        completeList(Xs).
        
        
indent(I,Spaces) :-
        buildSpaces(I,[],Spaces).

buildSpaces(0,Sp,Spaces) :-
        name(Spaces,Sp).
buildSpaces(I,Sp,Spaces) :-
        I > 0,
        I1 is I-1,
        buildSpaces(I1,[32|Sp],Spaces).
        
incrementIndent(I,I1) :-
        indentation(X),
        I1 is I+X.
        
indentation(1). % number of spaces to indent

xmlName(X,Y) :-
        definedName(X,Y),
        !.
xmlName(X,X).

definedName('.',cons).
definedName('/',slash).
definedName(',',comma).
definedName('+',plus).
definedName('-',minus).
definedName(':-',if).
definedName('=',equals).
