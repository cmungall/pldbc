% * -*- Mode: Prolog -*- */

:- module(pldbc/pldbc_odbc,[]).

% NOT TESTED

% Do not use this module directly! See pldbc

% Note this may in future be distributed in its own repo

% ----------------------------------------
% CONNECTION MANAGEMENT
% ----------------------------------------

:- multifile pldbc:pldbc:pldbc_query/4
:- multifile pldbc:pldbc_current_table_hook/3.
:- multifile pldbc:pldbc_table_column_hook/4.
:- multifile pldbc:pldbc_data_source_hook/2.
:- multifile pldbc:pldbc_statistics_hook/1.
:- multifile pldbc:pldbc_connect_hook/3.
:- multifile pldbc:pldbc_disconnect_hook/3.
pldbc:pldbc_connect_hook(DriverString, Connection, Options) :-
    odbc_connect(DriverString, Connection, Options).


% ----------------------------------------
% STATEMENTS / QUERIES
% ----------------------------------------


pldbc:pldbc_disconnect_hook(Connection) :-
    odbc_disconnect(Connection).
:- multifile pldbc:pldbc_query_hook/4.
:- multifile pldbc:pldbc_execute_hook/3.
:- multifile pldbc:pldbc_prepare_hook/5.
:- multifile pldbc:pldbc_fetch_hook/3.
pldbc:pldbc_query_hook(Connection, SQL, Row, Options) :-
    odbc_query(Connection, SQL, Row, Options).
pldbc:pldbc_prepare_hook(Connection, SQL, Parameters, Statement, Options) :-
    odbc_prepare(Connection, SQL, Parameters, Statement, Options).
pldbc:pldbc_fetch_hook(Statement, Row, Option) :-
    odbc_fetch(Statement, Row, Option).
pldbc:pldbc_execute_hook(Statement, Parameters, Options) :-
    odbc_execute(Statement, Parameters, Options).
pldbc:pldbc_execute_hook(Statement, Parameters) :-
    odbc_execute(Statement, Parameters).
pldbc:pldbc_close_statement_hook(Statement) :-
    odbc_close_statement(Statement).
pldbc:pldbc_free_statement_hook(Statement) :-
    odbc_free_statement(Statement).
pldbc:pldbc_current_table_hook(Connection, Table) :-
    odbc_current_table(Connection, Table).
pldbc:pldbc_current_table_hook(Connection, Table, Facet) :-
    odbc_current_table(Connection, Table, Facet).
pldbc:pldbc_table_column_hook(Connection, Table, Column) :-
    odbc_table_column(Connection, Table, Column).
pldbc:pldbc_table_column_hook(Connection, Table, Column, Facet) :-
    odbc_table_column(Connection, Table, Column, Facet).
pldbc:pldbc_data_source_hook(DSN, Description) :-
    odbc_data_source(DSN, Description).
pldbc:pldbc_statistics_hook(Key) :-
    odbc_statistics(Key).
