/*
    Author:        Eshel Yaron
    E-mail:        eshel@swi-prolog.org
    Copyright (c)  2022, SWI-Prolog Solutions b.v.
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

:- module(sweep,
          [ sweep_colourise_buffer/2,
            sweep_colourise_some_terms/2,
            sweep_documentation/2,
            sweep_identifier_at_point/2,
            sweep_expand_file_name/2,
            sweep_path_module/2,
            sweep_predicate_location/2,
            sweep_predicates_collection/2,
            sweep_modules_collection/2,
            sweep_packs_collection/2,
            sweep_pack_install/2,
            sweep_module_path/2
          ]).

:- use_module(library(pldoc)).
:- use_module(library(listing)).
:- use_module(library(prolog_source)).
:- use_module(library(prolog_colour)).
:- use_module(library(pldoc/doc_process)).
:- use_module(library(pldoc/doc_wiki)).
:- use_module(library(pldoc/doc_modes)).
:- use_module(library(pldoc/doc_man)).
:- use_module(library(pldoc/man_index)).
:- use_module(library(lynx/html_text)).
:- use_module(library(prolog_pack)).
:- use_module(library(prolog_server)).

:- dynamic sweep_current_color/3,
           sweep_open/2,
           sweep_source_time/2,
           sweep_current_comment/3.

:- multifile prolog:xref_source_time/2,
             prolog:xref_open_source/2,
             prolog:xref_open_source/2,
             prolog:quasi_quotation_syntax/2.

prolog:quasi_quotation_syntax(graphql, library(http/graphql)).

prolog:xref_source_time(Source, Time) :-
    sweep_source_time(Source, Time).

prolog:xref_open_source(Source, Stream) :-
    sweep_open(Source, Stream).

prolog:xref_close_source(Source, Stream) :-
    sweep_open(Source, Stream).

sweep_colourise_buffer([String|Path], Colors) :-
    setup_call_cleanup(( new_memory_file(H),
                         insert_memory_file(H, 0, String),
                         open_memory_file(H, read, Contents, [encoding(utf8)])
                       ),
                       sweep_colourise_buffer_(Path, Contents, Colors),
                       ( close(Contents),
                         free_memory_file(H)
                       )).

sweep_colourise_buffer_(Path0, Contents, []) :-
    atom_string(Path, Path0),
    set_stream(Contents, encoding(utf8)),
    set_stream(Contents, file_name(Path)),
    get_time(Time),
    asserta(sweep_open(Path, Contents), Ref0),
    asserta(sweep_source_time(Path, Time), Ref1),
    xref_source(Path, []),
    seek(Contents, 0, bof, _),
    prolog_colourise_stream(Contents,
                            Path,
                            sweep_handle_query_color(1)),
    erase(Ref0),
    erase(Ref1).


sweep_identifier_at_point([Contents0, Path, Point], Identifier) :-
    setup_call_cleanup(( new_memory_file(H),
                         insert_memory_file(H, 0, Contents0),
                         open_memory_file(H, read, Contents, [encoding(utf8)])
                       ),
                       sweep_identifier_at_point_(Path, Point, Contents, Identifier),
                       ( close(Contents),
                         free_memory_file(H)
                       )).

:- dynamic sweep_current_identifier_at_point/1.


sweep_identifier_at_point_(Path0, Point, Contents, Identifier) :-
    atom_string(Path, Path0),
    (   xref_module(Path, M)
    ->  true
    ;   M = user
    ),
    set_stream(Contents, encoding(utf8)),
    set_stream(Contents, file_name(Path)),
    seek(Contents, 0, bof, _),
    retractall(sweep_current_identifier_at_point(_)),
    prolog_colourise_term(Contents, Path,
                          sweep_handle_identifier_at_point(Path, M, Point),
                          []),
    sweep_current_identifier_at_point(Identifier0),
    term_string(Identifier0, Identifier).


sweep_handle_identifier_at_point(Path, M, Point, Col, Beg, Len) :-
    Beg =< Point,
    Point =< Beg + Len,
    !,
    sweep_handle_identifier_at_point_(Path, M, Col).
sweep_handle_identifier_at_point(_, _, _, _, _, _).

sweep_handle_identifier_at_point_(Path, M0, goal_term(_Kind, Goal)) :-
    !,
    pi_head(PI0, Goal),
    (   PI0 = M:PI
    ->  true
    ;   xref_defined(Path, Goal, imported(Other)), xref_module(Other, M)
    ->  PI = PI0
    ;   predicate_property(M0:Goal, imported_from(M))
    ->  PI = PI0
    ;   '$autoload':library_index(Goal, M, _)
    ->  PI = PI0
    ;   M = M0, PI = PI0
    ),
    asserta(sweep_current_identifier_at_point(M:PI)).
sweep_handle_identifier_at_point_(_, _, _).

sweep_colourise_some_terms([String,Path,Offset], Colors) :-
    setup_call_cleanup(( new_memory_file(H),
                         insert_memory_file(H, 0, String),
                         open_memory_file(H, read, Contents, [encoding(utf8)])
                       ),
                       sweep_colourise_some_terms_(Path, Offset, Contents, Colors),
                       ( close(Contents),
                         free_memory_file(H)
                       )).

sweep_colourise_some_terms_(Path0, Offset, Contents, []) :-
    atom_string(Path, Path0),
    set_stream(Contents, encoding(utf8)),
    set_stream(Contents, file_name(Path)),
    seek(Contents, 0, bof, _),
    prolog_colourise_stream(Contents,
                            Path,
                            sweep_handle_query_color(Offset)).

sweep_documentation([Path, Functor, Arity], Docs) :-
    atom_string(P, Path),
    atom_string(F, Functor),
    PI = F/Arity,
    pi_head(PI, Head),
    (   module_property(M, file(P)),
        \+ predicate_property(M:Head, imported_from(_))
    ->  true
    ;   module_property(M0, file(P)),
        predicate_property(M0:Head, imported_from(M))
    ->  true
    ;   M=user
    ),
    findall(Doc, sweep_documentation_(M, PI, Doc), Docs).

sweep_documentation_(M, PI, Docs) :-
   doc_comment(M:PI, Pos, OneLiner, Comment),
   is_structured_comment(Comment, Prefixes),
   string_codes(Comment, Codes),
   indented_lines(Codes, Prefixes, Lines),
   pldoc_modes:mode_lines(Lines, ModeText, [], _),
   pldoc_modes:modes(ModeText, M, Pos, Modes),
   sweep_documentation_modes(Modes, OneLiner, Docs).
sweep_documentation_(_, PI, Docs) :-
    pldoc_man:load_man_object(PI, _, _, Dom),
    with_output_to(string(DomS), html_text(Dom, [])),
    sub_string(DomS, EOL, _, _, '\n'),
    sub_string(DomS, 0, EOL, _, FLine),
    sub_string(DomS, EOL, _, 0, Rest),
    (   sub_string(Rest, EOS, _, _, '. ')
    ->  sub_string(Rest, 0, EOS, _, OneLiner2)
    ;   OneLiner2=Rest
    ),
    format(string(Docs), '~w.    ~w.~n', [FLine, OneLiner2]),
    !.

sweep_documentation_modes([mode(Mode0, Args)|_], OneLiner, Docs) :-
    maplist([Name=Var]>>(Var='$VAR'(Name)), Args),
    (   Mode0=(Mode1 is Det)
    ->  true
    ;   Mode1=Mode0,
        Det=unspec
    ),
    format(string(Docs),
           '~W is ~w.~n    ~w~n',
           [ Mode1,
             [module(pldoc_modes), numbervars(true)],
             Det,
             OneLiner
           ]).
sweep_documentation_modes([_|T], OneLiner, Docs) :-
    sweep_documentation_modes(T, OneLiner, Docs).


sweep_module_path(ModuleName, Path) :-
    atom_string(Module, ModuleName),
    sweep_module_path_(Module, Path0),
    atom_string(Path0, Path).


sweep_module_path_(Module, Path) :-
    module_property(Module, file(Path)), !.
sweep_module_path_(Module, Path) :-
    '$autoload':library_index(_, Module, Path0), !, string_concat(Path0, ".pl", Path).


sweep_modules_collection([], Modules) :-
    findall([M|P], ( module_property(M, file(P0)), atom_string(P0, P) ), Modules0, Tail),
    setof([M|P], P0^N^('$autoload':library_index(N, M, P0), string_concat(P0, ".pl", P) ), Tail),
    list_to_set(Modules0, Modules1),
    maplist(sweep_module_description, Modules1, Modules).

sweep_module_description([M0|P], [M|[P|D]]) :-
   doc_comment(M0:module(D0), _, _, _),
   atom_string(M0, M),
   atom_string(D0, D).
sweep_module_description([M0|P], [M|[P]]) :- atom_string(M0, M).

sweep_predicate_location(MFN, [Path|Line]) :-
    term_string(M:F/N, MFN),
    pi_head(F/N, H),
    predicate_property(M:H, line_count(Line)),
    predicate_property(M:H, file(Path0)), atom_string(Path0, Path).

sweep_local_predicate_completion([Mod|Sub], Preds) :-
    atom_string(M, Mod),
    findall(F/N,
            @(current_predicate(F/N), M),
            Preds0,
            Tail),
    findall(XF/XN,
            (   xref_module(SourceId, M),
                xref_defined(SourceId, H, _),
                H \= _:_,
                pi_head(XF/XN, H)
            ),
            Tail),
    list_to_set(Preds0, Preds1),
    convlist(sweep_predicate_completion_annotated(Sub, M), Preds1, Preds).

sweep_predicate_completion_annotated(Sub, M, F/N, [S|A]) :-
    format(string(S), '~W/~w', [F, [quoted(true), character_escapes(true)], N]),
    sub_string(S, _, _, _, Sub),
    \+ sub_string(S, _, _, _, "$"),
    pi_head(F/N, Head),
    findall(P, @(predicate_property(Head, P), M), Ps0),
    sweep_predicate_completion_op_annotation(F, Ps0, Ps),
    phrase(sweep_head_annotation(Ps), A).

sweep_predicate_completion_op_annotation(F, Ps, [op(Pri,Fix)|Ps]) :-
    current_op(Pri, Fix, F),
    !.
sweep_predicate_completion_op_annotation(_, Ps, Ps).

sweep_head_annotation([H|T]) -->
    sweep_head_annotation_(H),
    sweep_head_annotation(T).
sweep_head_annotation([]) --> [].

sweep_head_annotation_(built_in)          --> !, ["built-in"].
sweep_head_annotation_(det)               --> !, ["!"].
sweep_head_annotation_(dynamic)           --> !, ["dynamic"].
sweep_head_annotation_(foreign)           --> !, ["C"].
sweep_head_annotation_(iso)               --> !, ["iso"].
sweep_head_annotation_(multifile)         --> !, ["multifile"].
sweep_head_annotation_(meta_predicate(_)) --> !, [":"].
sweep_head_annotation_(non_terminal)      --> !, ["//"].
sweep_head_annotation_(ssu)               --> !, ["=>"].
sweep_head_annotation_(tabled)            --> !, ["table"].
sweep_head_annotation_(tabled(_))         --> !, ["table"].
sweep_head_annotation_(thread_local)      --> !, ["thread-local"].
sweep_head_annotation_(op(_,_))           --> !, ["op"].
sweep_head_annotation_(_)                 --> [].

sweep_predicates_collection(Sub, Preds) :-
    findall(M:F/N,
            ( current_predicate(M:F/N),
              pi_head(F/N, H),
              \+ (predicate_property(M:H, imported_from(M1)), M \= M1)
            ),
            Preds0,
            Tail),
    findall(M:F/N,
            ( '$autoload':library_index(H, M, _),
              pi_head(F/N, H)
            ),
            Tail),
    list_to_set(Preds0, Preds1),
    maplist(sweep_predicate_description, Preds1, Preds2),
    include(sweep_predicate_non_hidden, Preds2, Preds3),
    (   Sub == []
    ->  Preds = Preds3
    ;   include(sweep_predicate_matches(Sub), Preds3, Preds)
    ).

sweep_predicate_matches(Sub, [String|_]) :-
    sub_string(String, _, _, _, Sub).

sweep_predicate_non_hidden([String|_]) :-
    \+ sub_string(String, _, _, _, ":'$").

sweep_predicate_description(M:F/N, [S|T]) :-
    sweep_predicate_description_(M, F, N, T),
    format(string(S),
           '~w:~W/~w',
           [M, F, [quoted(true), character_escapes(true)], N]).

sweep_predicate_description_(M, F, N, [D]) :-
    doc_comment(M:F/N, _, D0, _), !, atom_string(D0, D).
sweep_predicate_description_(_M, F, N, [D]) :-
    man_object_property(F/N, summary(D0)), !, atom_string(D0, D).
sweep_predicate_description_(_, _, _, []).

sweep_packs_collection(SearchString, Packs) :-
    prolog_pack:query_pack_server(search(SearchString), true(Packs0), []),
    maplist(sweep_pack_info, Packs0, Packs).

sweep_pack_info(pack(Name0, _, Desc0, Version0, URLS0), [Name, Desc, Version, URLS]) :-
    atom_string(Name0, Name),
    atom_string(Desc0, Desc),
    atom_string(Version0, Version),
    maplist(atom_string, URLS0, URLS).

sweep_pack_install(PackName, []) :-
    atom_string(Pack, PackName), pack_install(Pack, [silent(true), upgrade(true), interactive(false)]).


sweep_colourise_query([String|Offset], _) :-
    prolog_colourise_query(String, module(sweep), sweep_handle_query_color(Offset)).

sweep_handle_query_color(Offset, Col, Beg, Len) :-
    sweep_color_normalized(Offset, Col, Nom),
    Start is Beg + Offset,
    sweep_funcall("sweep--colourise", [Start,Len|Nom], _).

sweep_color_normalized(Offset, Col, Nom) :-
    Col =.. [Nom0|Rest],
    sweep_color_normalized_(Offset, Nom0, Rest, Nom).

sweep_color_normalized_(_, Goal0, [Kind0,Head|_], [Goal,Kind,F,N]) :-
    sweep_color_goal(Goal0),
    !,
    atom_string(Goal0, Goal),
    term_string(Kind0, Kind),
    pi_head(F0/N, Head),
    atom_string(F0, F).
sweep_color_normalized_(Offset, syntax_error, [Message0,Start0-End0|_], ["syntax_error", Message, Start, End]) :-
    !,
    Start is Start0 + Offset,
    End   is End0   + Offset,
    atom_string(Message0, Message).
sweep_color_normalized_(_, comment, [Kind0|_], ["comment"|Kind]) :-
    !,
    atom_string(Kind0, Kind).
sweep_color_normalized_(_, Nom0, _, Nom) :-
    atom_string(Nom0, Nom).

sweep_color_goal(goal).
sweep_color_goal(goal_term).
sweep_color_goal(head).
sweep_color_goal(head_term).

sweep_expand_file_name([String|Dir], Exp) :-
    term_string(Spec, String, [syntax_errors(quiet)]),
    sweep_expand_file_name_(Dir, Spec, Atom),
    (   exists_file(Atom)
    ->  true
    ;   exists_directory(Atom)
    ),
    atom_string(Atom, Exp).

sweep_expand_file_name_([], Spec, Atom) :-
    absolute_file_name(Spec, Atom, [file_errors(fail),
                                    solutions(all),
                                    extensions(['', '.pl'])]).
sweep_expand_file_name_(Dir, Spec, Exp) :-
    !,
    absolute_file_name(Spec, Exp, [file_errors(fail),
                                   relative_to(Dir),
                                   solutions(all),
                                   extensions(['', '.pl'])]).

sweep_path_module([], "user")    :- !.
sweep_path_module(Path0, Module) :-
    atom_string(Path, Path0),
    xref_module(Path, Module0),
    atom_string(Module0, Module).
