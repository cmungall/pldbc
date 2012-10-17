% * -*- Mode: Prolog -*- */

:- module(pldbc_cli_mysql).
:- use_module(pldbc).

/** pldbc implementation to mysql via command line

  VERY HACKY - mainly for illustrative purposes


  Note this may in future be distributed in its own repo

  It may also be generalized into a general purposes SQL-over-CL
  implementation

  Currently to connect you pass in a term

===  
  cli_mysql([opts(ConnectionArgsAtom)])
===  

  NOT STABLE!!
  
===

:- use_module(library(pldbc)).
:- use_module(library(pldbc/pldbc_cli_mysql)).   % may be auto-loaded in future

test :-  
        pldbc_connect(cli_mysql([opts('-A -hmysql.ebi.ac.uk -ugo_select -pamigo -P4085 go_latest')]), Connection, []),
        pldbc_query(Connection, 'SELECT * FROM term LIMIT 25',Row),
        writeln(Row),
        fail.
===  
  
  
*/


% ----------------------------------------
% CONNECTION MANAGEMENT
% ----------------------------------------

:- multifile pldbc:pldbc_current_table_hook/3.
:- multifile pldbc:pldbc_table_column_hook/4.
:- multifile pldbc:pldbc_data_source_hook/2.
:- multifile pldbc:pldbc_statistics_hook/1.
:- multifile pldbc:pldbc_connect_hook/3.
:- multifile pldbc:pldbc_disconnect_hook/3.

% note for CLI we don't really 'connect' - we just
% unify the Connection variable with a structure
% describing how to run a query.
% A connection is opened each time - obviously
% inefficient, but this is the tradeoff for CLI
pldbc:pldbc_connect_hook(Driver, Driver, _Options) :-
        Driver = cli_mysql(_).

% ----------------------------------------
% STATEMENTS / QUERIES
% ----------------------------------------


pldbc:pldbc_disconnect_hook(_Connection) :-
        % no connection to disconnect!
        true.

:- multifile pldbc:pldbc_query_hook/4.
:- multifile pldbc:pldbc_execute_hook/3.
:- multifile pldbc:pldbc_prepare_hook/5.
:- multifile pldbc:pldbc_fetch_hook/3.
pldbc:pldbc_query_hook(Connection, SQL, Row, _Options) :-
        % first check we can handle this
        Connection = cli_mysql(ConnArgs),
        option(opts(OptAtom),ConnArgs,''),
        atomic_list_concat([echo,' "',SQL,'" | mysql ',OptAtom],Cmd),
        writeln(cmd(Cmd)),
        open(pipe(Cmd),read,IO,[]),
        parse_rows(IO,Row).

pldbc:pldbc_prepare_hook(_Connection, _SQL, _Parameters, _Statement, _Options) :-
    not_implemented.

pldbc:pldbc_fetch_hook(_Statement, _Row, _Option) :-
    not_implemented.
pldbc:pldbc_execute_hook(_Statement, _Parameters, _Options) :-
    not_implemented.
pldbc:pldbc_execute_hook(_Statement, _Parameters) :-
    not_implemented.
pldbc:pldbc_close_statement_hook(_Statement) :-
    not_implemented.
pldbc:pldbc_free_statement_hook(_Statement) :-
    not_implemented.
pldbc:pldbc_current_table_hook(_Connection, _Table) :-
    not_implemented.
pldbc:pldbc_current_table_hook(_Connection, _Table, _Facet) :-
    not_implemented.
pldbc:pldbc_table_column_hook(_Connection, _Table, _Column) :-
    not_implemented.
pldbc:pldbc_table_column_hook(_Connection, _Table, _Column, _Facet) :-
    not_implemented.
pldbc:pldbc_data_source_hook(_DSN, _Description) :-
    not_implemented.
pldbc:pldbc_statistics_hook(_Key) :-
    not_implemented.

not_implemented :- throw(error(not_implemented)).


parse_rows(IO,row(Vals)) :-
        read_stream_to_codes(IO,Codes),
        atom_codes(BlockAtom,Codes),
        atomic_list_concat([_HdrAtom|LineAtoms],'\n',BlockAtom),
        member(Line,LineAtoms),
        atomic_list_concat(Vals,'\t',Line),
        Vals\=[].
parse_rows(IO,_) :- close(IO),fail.



test :-
        pldbc_connect(cli_mysql([opts('-A -hmysql.ebi.ac.uk -ugo_select -pamigo -P4085 go_latest')]), Connection, []),
        pldbc_query(Connection, 'SELECT * FROM term LIMIT 25',Row),
        writeln(Row),
        fail.

                    
