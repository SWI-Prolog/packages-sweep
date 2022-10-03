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
            sweep_setup_message_hook/2,
            sweep_current_prolog_flags/2,
            sweep_set_prolog_flag/2,
            sweep_documentation/2,
            sweep_definition_at_point/2,
            sweep_file_at_point/2,
            sweep_identifier_at_point/2,
            sweep_expand_file_name/2,
            sweep_path_module/2,
            sweep_load_buffer/2,
            sweep_colourise_query/2,
            sweep_predicate_references/2,
            sweep_predicate_location/2,
            sweep_predicate_apropos/2,
            sweep_predicates_collection/2,
            sweep_local_predicate_completion/2,
            sweep_modules_collection/2,
            sweep_packs_collection/2,
            sweep_pack_install/2,
            sweep_prefix_ops/2,
            sweep_op_info/2,
            sweep_imenu_index/2,
            sweep_module_path/2,
            sweep_top_level_server/2,
            sweep_top_level_threads/2,
            sweep_accept_top_level_client/2,
            write_sweep_module_location/0
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
:- use_module(library(help)).
:- use_module(library(prolog_server)).

:- meta_predicate with_buffer_stream(-, +, 0).

:- dynamic sweep_current_color/3,
           sweep_open/2,
           sweep_top_level_thread_buffer/2,
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

sweep_top_level_threads(_, Ts) :-
    findall([Id, Buffer, Status, Stack, CPUTime],
            (   sweep_top_level_thread_buffer(Id, Buffer),
                thread_property(Id, status(Status0)),
                term_string(Status0, Status),
                thread_statistics(Id, stack, Stack),
                thread_statistics(Id, cputime, CPUTime)
            ),
            Ts).

sweep_current_prolog_flags(Sub, Flags) :-
    findall([Flag|Value],
            (current_prolog_flag(Flag0, Value0),
             atom_string(Flag0, Flag),
             once(sub_string(Flag, _, _, _, Sub)),
             term_string(Value0, Value)),
            Flags).

sweep_set_prolog_flag([Flag0|Value0], []) :-
    atom_string(Flag, Flag0),
    term_string(Value, Value0),
    set_prolog_flag(Flag, Value).

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
    xref_source(Path, [comments(store)]),
    seek(Contents, 0, bof, _),
    retractall(sweep_current_comment(_, _, _)),
    prolog_colourise_stream(Contents,
                            Path,
                            sweep_handle_color(1)),
    forall(sweep_current_comment(Kind, Start, Len),
           ( atom_string(Kind, String),
             user:sweep_funcall("sweeprolog--colourise", [Start,Len,"comment"|String], _)
           )),
    erase(Ref0),
    erase(Ref1).

sweep_definition_at_point([Contents|Path0], Result) :-
    atom_string(Path, Path0),
    with_buffer_stream(Stream,
                       Contents,
                       sweep_definition_at_point_(Stream, Path, Result)).

:- dynamic sweep_current_defintion_at_point/1.

sweep_definition_at_point_(Stream, Path, [Beg,F,N]) :-
    set_stream(Stream, file_name(Path)),
    retractall(sweep_current_defintion_at_point(_)),
    prolog_colourise_term(Stream, Path,
                          sweep_handle_definition_at_point,
                          []),
    sweep_current_defintion_at_point(Beg-Def),
    (   Def = M:F0/N
    ->  term_string(M:F0, F)
    ;   Def = F0/N,
        term_string(F0, F)
    ).

sweep_handle_definition_at_point(head_term(_Kind, Goal), Beg, _Len) :-
    !,
    pi_head(PI, Goal),
    asserta(sweep_current_defintion_at_point(Beg-PI)).
sweep_handle_definition_at_point(_, _, _).


sweep_file_at_point([Contents,Path0,Point], Result) :-
    atom_string(Path, Path0),
    with_buffer_stream(Stream,
                       Contents,
                       sweep_file_at_point_(Stream, Path, Point, Result)).

:- dynamic sweep_current_file_at_point/1.

sweep_file_at_point_(Stream, Path, Point, File) :-
    set_stream(Stream, file_name(Path)),
    retractall(sweep_current_file_at_point(_)),
    prolog_colourise_term(Stream, Path,
                          sweep_handle_file_at_point(Point),
                          []),
    sweep_current_file_at_point(File0),
    atom_string(File0, File).

sweep_handle_file_at_point(Point, file_no_depend(File), Beg, Len) :-
    Beg =< Point,
    Point =< Beg + Len,
    !,
    asserta(sweep_current_file_at_point(File)).
sweep_handle_file_at_point(Point, file(File), Beg, Len) :-
    Beg =< Point,
    Point =< Beg + Len,
    !,
    asserta(sweep_current_file_at_point(File)).
sweep_handle_file_at_point(_, _, _, _).


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

sweep_handle_identifier_at_point_(Path, M0, goal_term(Kind, Goal)) :-
    !,
    sweep_handle_identifier_at_point_goal(Path, M0, Kind, Goal).
sweep_handle_identifier_at_point_(Path, M0, goal(Kind, Goal)) :-
    !,
    sweep_handle_identifier_at_point_goal(Path, M0, Kind, Goal).
sweep_handle_identifier_at_point_(_Path, M0, head_term(_Kind, Goal)) :-
    !,
    sweep_handle_identifier_at_point_head(M0, Goal).
sweep_handle_identifier_at_point_(_, _, _).


sweep_handle_identifier_at_point_head(_, M:Goal) :-
    !,
    pi_head(PI, Goal),
    asserta(sweep_current_identifier_at_point(M:PI)).
sweep_handle_identifier_at_point_head(M, Goal) :-
    !,
    pi_head(PI, Goal),
    asserta(sweep_current_identifier_at_point(M:PI)).

sweep_handle_identifier_at_point_goal(_Path, M, local(_), Goal) :-
    !,
    pi_head(PI, Goal),
    asserta(sweep_current_identifier_at_point(M:PI)).
sweep_handle_identifier_at_point_goal(_Path, _M, recursion, M:Goal) :-
    !,
    pi_head(PI, Goal),
    asserta(sweep_current_identifier_at_point(M:PI)).
sweep_handle_identifier_at_point_goal(_Path, M, recursion, Goal) :-
    !,
    pi_head(PI, Goal),
    asserta(sweep_current_identifier_at_point(M:PI)).
sweep_handle_identifier_at_point_goal(_Path, _M0, built_in, Goal) :-
    !,
    pi_head(PI, Goal),
    asserta(sweep_current_identifier_at_point(PI)).
sweep_handle_identifier_at_point_goal(_Path, _M0, imported(Path), Goal) :-
    !,
    pi_head(PI, Goal),
    xref_source(Path, [comments(store)]),
    xref_module(Path, M),
    asserta(sweep_current_identifier_at_point(M:PI)).
sweep_handle_identifier_at_point_goal(_Path, _M0, Extern, Goal) :-
    sweep_is_extern(Extern, M),
    !,
    pi_head(PI, Goal),
    asserta(sweep_current_identifier_at_point(M:PI)).
sweep_handle_identifier_at_point_goal(_Path, _M0, autoload(Path), Goal) :-
    !,
    pi_head(PI, Goal),
    (   '$autoload':library_index(Goal, M, Path)
    ->  true
    ;   file_name_extension(Base, _, Path), '$autoload':library_index(Goal, M, Base)
    ),
    asserta(sweep_current_identifier_at_point(M:PI)).
sweep_handle_identifier_at_point_goal(_Path, _M0, Global, Goal) :-
    sweep_is_global(Global),
    !,
    pi_head(PI, Goal),
    asserta(sweep_current_identifier_at_point(user:PI)).
sweep_handle_identifier_at_point_goal(_Path, _M0, undefined, M:Goal) :-
    !,
    pi_head(PI, Goal),
    asserta(sweep_current_identifier_at_point(M:PI)).
sweep_handle_identifier_at_point_goal(_Path, _M0, undefined, Goal) :-
    !,
    pi_head(PI, Goal),
    asserta(sweep_current_identifier_at_point(undefined:PI)).
sweep_handle_identifier_at_point_goal(_Path, _M0, meta, _:Goal) :-
    !,
    pi_head(PI, Goal),
    asserta(sweep_current_identifier_at_point(meta:PI)).
sweep_handle_identifier_at_point_goal(_Path, _M0, meta, Goal) :-
    !,
    pi_head(PI, Goal),
    asserta(sweep_current_identifier_at_point(meta:PI)).
sweep_handle_identifier_at_point_goal(Path, M0, _Kind, Goal) :-
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

sweep_is_global(global).
sweep_is_global(global(_,_)).

sweep_is_extern(extern(M),   M).
sweep_is_extern(extern(M,_), M).

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
    findall(Op, xref_op(Path, Op), Ops),
    retractall(sweep_current_comment(_, _, _)),
    prolog_colourise_stream(Contents,
                            Path,
                            sweep_handle_color(Offset),
                            [operators(Ops)]),
    forall(sweep_current_comment(Kind, Start, Len),
           ( atom_string(Kind, String),
             user:sweep_funcall("sweeprolog--colourise", [Start,Len,"comment"|String], _)
           )).

sweep_documentation(PI0, Docs) :-
    term_string(PI1, PI0),
    (   PI1 = M:PI
    ->  true
    ;   M=user, PI=PI1
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

sweep_predicate_references(MFN, Refs) :-
    term_string(M:F/N, MFN),
    pi_head(F/N, H),
    findall([B,Path|Line],
            (xref_called(Path0, H, B0, _, Line),
             pi_head(B1, B0),
             term_string(B1, B),
             atom_string(Path0, Path)),
            Refs,
            Tail),
    findall([B,Path|Line],
            (xref_called(Path0, M:H, B0, _, Line),
             pi_head(B1, B0),
             term_string(B1, B),
             atom_string(Path0, Path)),
            Tail).


sweep_predicate_location(MFN, [Path|Line]) :-
    term_string(M:F/N, MFN),
    !,
    pi_head(F/N, H),
    (   sweep_predicate_location_(M, H, Path, Line)
    ->  true
    ;   sweep_predicate_location_(H, Path, Line)
    ).
sweep_predicate_location(FN, [Path|Line]) :-
    term_string(F/N, FN),
    !,
    pi_head(F/N, H),
    sweep_predicate_location_(H, Path, Line).

sweep_predicate_apropos(Query0, Matches) :-
    atom_string(Query, Query0),
    findall([S,Path|Line],
            (prolog_help:apropos(Query, M:F/N, _, _),
             format(string(S), '~w:~W/~w', [M, F, [quoted(true), character_escapes(true)], N]),
             pi_head(F/N, Head),
             sweep_predicate_location_(M, Head, Path, Line)),
            Matches, Tail),
    findall([S,Path],
            (prolog_help:apropos(Query, F/N, _, _),
             format(string(S), '~W/~w', [F, [quoted(true), character_escapes(true)], N]),
             pi_head(F/N, Head),
             sweep_predicate_location_(Head, Path, Line)),
            Tail).

sweep_predicate_location_(H, Path, Line) :-
    predicate_property(H, file(Path0)),
    predicate_property(H, line_count(Line)),
    !,
    atom_string(Path0, Path).
sweep_predicate_location_(H, Path, Line) :-
    xref_defined(Path0, H, How),
    atom_string(Path0, Path),
    (   xref_definition_line(How, Line)
    ->  true
    ;   Line = []
    ).

sweep_predicate_location_(M, H, Path, Line) :-
    predicate_property(M:H, file(Path0)),
    predicate_property(M:H, line_count(Line)),
    !,
    atom_string(Path0, Path).
sweep_predicate_location_(M, H, Path, Line) :-
    xref_defined(Path0, M:H, How),
    atom_string(Path0, Path),
    (   xref_definition_line(How, Line)
    ->  true
    ;   Line = []
    ).

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
            Tail0),
    findall(M:F/N,
            ( '$autoload':library_index(H, M, _),
              pi_head(F/N, H)
            ),
            Tail0,
            Tail1),
    findall(M:F/N,
            ( xref_defined(SourceId, H, local(_)),
              (   xref_module(SourceId, M)
              ->  true
              ;   M = user
              ),
              pi_head(F/N, H)
            ),
            Tail1,
            Tail),
    findall(M:F/N,
            ( xref_defined(_, H, imported(SourceId)),
              (   xref_module(SourceId, M)
              ->  true
              ;   M = user
              ),
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

sweep_handle_color(Offset, comment(Kind), Beg, Len) :-
    !,
    Start is Beg + Offset,
    asserta(sweep_current_comment(Kind, Start, Len)).
sweep_handle_color(Offset, Col, Beg, Len) :-
    sweep_handle_query_color(Offset, Col, Beg, Len).

sweep_colourise_query([String|Offset], _) :-
    prolog_colourise_query(String, module(sweep), sweep_handle_query_color(Offset)).

sweep_handle_query_color(Offset, Col, Beg, Len) :-
    sweep_color_normalized(Offset, Col, Nom),
    Start is Beg + Offset,
    user:sweep_funcall("sweeprolog--colourise", [Start,Len|Nom], _).

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
sweep_color_normalized_(_, qq_content, [Type0|_], ["qq_content"|Type]) :-
    !,
    atom_string(Type0, Type).
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


sweep_setup_message_hook(_, _) :-
    asserta((
             user:thread_message_hook(Term, Kind, Lines) :-
                 sweep_message_hook(Term, Kind, Lines)
            ),
            Ref),
    at_halt(erase(Ref)).

sweep_message_hook(Term, Kind0, _Lines) :-
    should_handle_message_kind(Kind0, Kind),
    !,
    message_to_string(Term, String),
    user:sweep_funcall("sweeprolog-message", [Kind|String], _).

should_handle_message_kind(error, "error").
should_handle_message_kind(warning, "warning").
should_handle_message_kind(informational, "informational").
should_handle_message_kind(debug(Topic0), ["debug"|Topic]) :- atom_string(Topic0, Topic).

sweep_prefix_ops(Path0, Ops) :-
    atom_string(Path, Path0),
    findall(Op, current_op(_, fx, Op),        Ops0, Tail0),
    findall(Op, current_op(_, fy, Op),        Tail0, Tail1),
    findall(Op, xref_op(Path, op(_, fx, Op)), Tail1, Tail),
    findall(Op, xref_op(Path, op(_, fy, Op)), Tail),
    maplist(atom_string, Ops0, Ops1),
    list_to_set(Ops1, Ops).

sweep_op_info([Op0|Path0], Info) :-
    atom_string(Path, Path0),
    atom_string(Op, Op0),
    sweep_op_info_(Op, Path, Info).

sweep_op_info_(Op, Path, [Type|Pred]) :-
    xref_op(Path, op(Pred, Type0, Op)),
    atom_string(Type0, Type).
sweep_op_info_(Op, _Path, [Type|Pred]) :-
    current_op(Pred, Type0, Op),
    atom_string(Type0, Type).

sweep_load_buffer([String|Path0], Result) :-
    atom_string(Path, Path0),
    with_buffer_stream(Stream,
                       String,
                       sweep_load_buffer_(Stream, Path, Result)).

sweep_load_buffer_(Stream, Path, []) :-
    set_stream(Stream, file_name(Path)),
    @(load_files(Path, [stream(Stream)]), user).

with_buffer_stream(Stream, String, Goal) :-
    setup_call_cleanup(( new_memory_file(H),
                         insert_memory_file(H, 0, String),
                         open_memory_file(H, read, Stream, [encoding(utf8)]),
                         set_stream(Stream, encoding(utf8))
                       ),
                       Goal,
                       ( close(Stream),
                         free_memory_file(H)
                       )).

sweep_imenu_index(Path, Index) :-
    atom_string(Atom, Path),
    findall([String|L],
            ( xref_defined(Atom, D, H),
              xref_definition_line(H, L),
              pi_head(PI, D),
              term_string(PI, String)
            ),
            Index).

write_sweep_module_location :-
    absolute_file_name(foreign('sweep-module'),
                       Path,
                       [file_type(executable), access(read)]),
    writeln(Path).

sweep_top_level_server(_, Port) :-
    tcp_socket(ServerSocket),
    tcp_setopt(ServerSocket, reuseaddr),
    tcp_bind(ServerSocket, Port),
    tcp_listen(ServerSocket, 5),
    thread_create(sweep_top_level_server_loop(ServerSocket), T,
                  [ alias(sweep_top_level_server)
                  ]),
    at_halt((   is_thread(T),
                thread_property(T, status(running))
            ->  thread_signal(T, thread_exit(0)),
                thread_join(T, _)
            ;   true
            )).

sweep_top_level_server_loop(ServerSocket) :-
    thread_get_message(Message),
    sweep_top_level_server_loop_(Message, ServerSocket).

sweep_top_level_server_loop_(accept(Buffer), ServerSocket) :-
    !,
    tcp_accept(ServerSocket, Slave, Peer),
    tcp_open_socket(Slave, InStream, OutStream),
    set_stream(InStream, close_on_abort(false)),
    set_stream(OutStream, close_on_abort(false)),
    thread_create(sweep_top_level_client(InStream, OutStream, Peer), T, [detached(true)]),
    thread_property(T, id(Id)),
    asserta(sweep_top_level_thread_buffer(Id, Buffer)),
    sweep_top_level_server_loop(ServerSocket).
sweep_top_level_server_loop_(_, _).

sweep_top_level_client(InStream, OutStream, ip(127,0,0,1)) :-
    !,
    set_prolog_IO(InStream, OutStream, OutStream),
    set_stream(InStream, tty(true)),
    set_prolog_flag(tty_control, false),
    current_prolog_flag(encoding, Enc),
    set_stream(user_input, encoding(Enc)),
    set_stream(user_output, encoding(Enc)),
    set_stream(user_error, encoding(Enc)),
    set_stream(user_input, newline(detect)),
    set_stream(user_output, newline(dos)),
    set_stream(user_error, newline(dos)),
    call_cleanup(prolog,
                 ( close(InStream, [force(true)]),
                   close(OutStream, [force(true)]),
                   thread_self(Self),
                   thread_property(Self, id(Id)),
                   retractall(sweep_top_level_thread_buffer(Id, _))
                 )).
sweep_top_level_client(InStream, OutStream, _) :-
    close(InStream),
    close(OutStream),
    thread_self(Self),
    thread_property(Self, id(Id)),
    retractall(sweep_top_level_thread_buffer(Id, _)).

sweep_accept_top_level_client(Buffer, _) :-
    thread_send_message(sweep_top_level_server, accept(Buffer)).
