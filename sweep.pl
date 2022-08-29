:- module(sweep,
          [ sweep_colors/2,
            sweep_documentation/2,
            sweep_predicate_location/2,
            sweep_predicates_collection/2,
            sweep_modules_collection/2,
            sweep_packs_collection/2,
            sweep_start_prolog_server/2,
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

sweep_colors([Path, String], Colors) :-
    setup_call_cleanup(( new_memory_file(H),
                         insert_memory_file(H, 0, String),
                         open_memory_file(H, read, Contents)
                       ),
                       sweep_colors(Path, Contents, Colors),
                       ( close(Contents),
                         free_memory_file(H)
                       )).
sweep_colors(Path, Contents, Colors) :-
    set_stream(Contents, encoding(utf8)),
    set_stream(Contents, file_name(Path)),
    get_time(Time),
    asserta(sweep_open(Path, Contents), Ref0),
    asserta(sweep_source_time(Path, Time), Ref1),
    xref_source(Path, []),
    retractall(sweep_current_color(_, _, _)),
    retractall(sweep_current_comment(_, _, _)),
    seek(Contents, 0, bof, _),
    prolog_colourise_stream(Contents,
                            Path,
                            sweep_server_handle_color),
    erase(Ref0),
    erase(Ref1),
    findall([B,L,T],
            sweep_current_color(B, L, T),
            Colors,
            Comments),
    findall([B,L,T],
            sweep_current_comment(B, L, T),
            Comments).

sweep_server_handle_color(comment(C), B0, L) =>
    B is B0 + 1,
    assertz(sweep_current_comment(B, L, C)).
sweep_server_handle_color(syntax_error(D, EB-EE), _B, _L) =>
    EL is EE-EB,
    assertz(sweep_current_color(EB,
                                  EL,
                                  syntax_error(D, EB-EE))).
sweep_server_handle_color(head_term(meta, Head), B0, L) =>
    B is B0 + 1,
    assertz(sweep_current_color(B, L, head_term(meta, Head))).
sweep_server_handle_color(head_term(Kind, Head), B0, L) =>
    B is B0+1,
    pi_head(PI, Head),
    assertz(sweep_current_color(B,
                                L,
                                head_term(Kind, PI))).
sweep_server_handle_color(head(Kind, Head), B0, L) =>
    B is B0+1,
    pi_head(PI, Head),
    assertz(sweep_current_color(B, L, head(Kind, PI))).
sweep_server_handle_color(goal(Kind, Head), B0, L) =>
    B is B0+1,
    pi_head(PI, Head),
    assertz(sweep_current_color(B, L, goal(Kind, PI))).
sweep_server_handle_color(goal_term(meta, Goal), B0, L) =>
    B is B0 + 1,
    assertz(sweep_current_color(B, L, goal_term(meta, Goal))).
sweep_server_handle_color(goal_term(Kind, Goal), B0, L) =>
    B is B0 + 1,
    pi_head(PI, Goal),
    assertz(sweep_current_color(B, L, goal_term(Kind, PI))).
sweep_server_handle_color(T, B0, L) =>
    B is B0 + 1,
    assertz(sweep_current_color(B, L, T)).

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

sweep_predicates_collection([], Preds) :-
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
    maplist(sweep_predicate_description, Preds1, Preds).

sweep_predicate_description(M:F/N, [S|T]) :-
    sweep_predicate_description_(M, F, N, T), format(string(S), '~w:~w/~w', [M, F, N]).

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


% sweep_expand_file_name([SpecString|_Dir], Path) :-
%     term_string(Spec, String),
%     absolute_file_name(library(lists), Path, [access(exist), extensions(['pl', '']), solutions(all)]).

sweep_start_prolog_server(Port, []) :-
    prolog_server(Port, []).
