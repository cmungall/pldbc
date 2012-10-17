% * -*- Mode: Prolog -*- */

/** <module> Prolog database connectivity

  ---+ Description

  This module is a DRAFT of a proposed prolog interface for connecting
  to relational databases. It is intended to be neutral with respect
  to the DBMS (MySQL, PostgreSQL etc) as well as to the connection
  mechanism (currently only ODBC is supported).

  The interface is essentially identical to a subset of the existing
  SWI-Prolog odbc interface. These are already abstract enough such
  that they are applicable to any connection mechanism. If development
  of pldbc continues, then additional predicates may be added.

  ---+ Current implementations

  There is one semi-complete implementation - pldbc-odbc

  [available in this repository, but may be part of a separate
  project/pack in the future]
  
  So at this point, using this module buys you absolutely nothing.

  But it allows you to write code such that you can swap in another
  implementation.

  There is also a proof-of-concept cli_mysql partial implementation
  
  
  ---+ Writing a pldbc implementation

  The idea is that developers can implement the pldbc interface for
  any desired connection mechanism. For example, there may be a SQL
  database that can only be called over http, requiring a specialized
  code using the http library. The application can call the pldbc
  predicates, but the implementation would be via a separate module.

  The implementing module would implement hook predicates such as:

  
  
===
:- module(pldoc/sql_over_http).
:- use_module(library(pldbc)).


:- multifile pldbc:pldbc_query/4
pldbc:pldbc_query(Connection, SQL, Row, Options) :-
       % DETERMINE WHETHER WE CAN HANDLE THIS
       Connection=sql_over_http(_),
       % ...
       % IMPLEMENTATION SPECIFIC CODE
       % ...
===

  TBD: recommendations for the structure of the Connection term and
  the Driver term for pldbc_connect/3. See docs below.

  If no implementation hook is found, this module should throw a
  not-implemented error (TODO)

  ---+ Future development

  This module assumes a need to write portable code that can sometimes
  use non ODBC based connectivity - if such a need exists others may
  contribute implementations - see perl's DBI and implementations such
  as DBD::mysql as an example.

  If there is no such need, then this module may be retired.



  */

:- module(pldbc,
	  [ pldbc_connect/3,		% +DSN, -Conn, +Options
	    pldbc_connect/2,		% +DSN, -Conn
	    pldbc_disconnect/1,		% +Conn

	    %pldbc_current_connection/2,	% ?Conn, -DSN
	    %pldbc_set_connection/2,	% +Conn, +Option
	    %pldbc_get_connection/2,	% +Conn, ?Option
	    %pldbc_end_transaction/2,	% +Conn, +CommitRollback
            
            pldbc_query/4,      % +Conn, +SQL, -Row, +Options
	    pldbc_query/3,		% +Conn, +SQL, -Row
	    pldbc_query/2,		% +Conn, +SQL

	    pldbc_prepare/4,		% +Conn, +SQL, +Parms, -Qid
	    pldbc_prepare/5,		% +Conn, +SQL, +Parms, -Qid, +Options
	    pldbc_execute/2,		% +Qid, +Parms
	    pldbc_execute/3,		% +Qid, +Parms, -Row
	    pldbc_fetch/3,		% +Qid, -Row, +Options
	    pldbc_close_statement/1,	% +Statement
	    pldbc_free_statement/1,	% +Statement

					% DB dictionary info
	    %pldbc_current_table/2,	% +Conn, -Table
	    %pldbc_current_table/3,	% +Conn, -Table, ?Facet
	    %pldbc_table_column/3,	% +Conn, ?Table, ?Column
	    %pldbc_table_column/4,	% +Conn, ?Table, ?Column, ?Facet
	    %pldbc_type/3,		% +Conn, ?Type, -Facet
	    %pldbc_data_source/2,		% ?DSN, ?Description

	    pldbc_statistics/1		% -Value
	    %pldbc_debug/1		% +Level
	  ]).

:- multifile pldbc_current_table_hook/3.
:- multifile pldbc_table_column_hook/4.
:- multifile pldbc_data_source_hook/2.
:- multifile pldbc_statistics_hook/1.

% ----------------------------------------
% CONNECTION MANAGEMENT
% ----------------------------------------

:- multifile pldbc_connect_hook/3.
:- multifile pldbc_disconnect_hook/3.

%%	pldbc_connect(+Driver, -Connection, +Options) is det.
%
%       The Driver is implementation-specific. We may want some
%       registry mechanism such that the relevant module is loaded
%       based on the structure of the DriverString term
%
%  TBD: structure of Driver term
%  
%  It probably makes sense to reserve atoms for ODBC drivers.
%
%  Others could be of the form:
%
%    DriverType(ConnectionArgs)
%
%  E.g.
%
%   sql_over_http([url(...), ...])
%
pldbc_connect(DriverString, Connection, Options) :-
        pldbc_connect_hook(DriverString, Connection, Options).

%% pldbc_connect(DriverString, Connection) is det.
%
% 
pldbc_connect(DriverString, Connection) :-
        pldbc_connect(DriverString, Connection, []).


%% pldbc_disconnect(Connection) is det.
%
% 
pldbc_disconnect(Connection) :-
        pldbc_disconnect_hook(Connection).

% ----------------------------------------
% STATEMENTS QUERIES
% ----------------------------------------

:- multifile pldbc_query_hook/4.
:- multifile pldbc_execute_hook/3.
:- multifile pldbc_prepare_hook/5.
:- multifile pldbc_fetch_hook/3.


%%	pldbc_query(+Connection, +SQL, -Row, +Options)
%
%	See pldbc_query/4
pldbc_query(Connection, SQL, Row, Options) :-
        pldbc_query_hook(Connection, SQL, Row, Options).

%%	pldbc_query(+Connection, +SQL, -Row)
%
%	Run query without options.
pldbc_query(Connection, SQL, Row) :-
	pldbc_query(Connection, SQL, Row, []).

%%	pldbc_query(+Connection, +SQL)
%
%	Execute SQL-statement that does not produce a result

pldbc_query(Connection, SQL) :-
	pldbc_query(Connection, SQL, Row), !,
	(   Row = affected(_)
	->  true
	;   print_message(warning, pldbc(unexpected_result(Row)))
	).

pldbc_prepare(Connection, SQL, Parameters, Statement) :-
        pldbc_prepare(Connection, SQL, Parameters, Statement, []).


%% pldbc_prepare(+Connection, +SQL, +Parameters, -Statement, +Options)
%
%    Create a statement from the given SQL (which may be a format
%    specification as described with odbc_query/3) statement that
%    normally has one or more parameter-indicators (?) and unify
%    Statement with a handle to the created statement. Parameters is a
%    list of descriptions, one for each parameter.
pldbc_prepare(Connection, SQL, Parameters, Statement, Options) :-
        pldbc_prepare_hook(Connection, SQL, Parameters, Statement, Options).

%% pldbc_fetch(+Statement, -Row, +Option) :-
%

pldbc_fetch(Statement, Row, Option) :-
        pldbc_fetch_hook(Statement, Row, Option).

%% pldbc_execute(+Statement, +Parameters, +Options)
%

pldbc_execute(Statement, Parameters, Options) :-
        pldbc_execute_hook(Statement, Parameters, Options).

%% pldbc_execute(+Statement, +Parameters)
%

pldbc_execute(Statement, Parameters) :-
	pldbc_execute(Statement, Parameters, Row), !,
	(   Row = affected(_)
	->  true
	;   print_message(warning, pldbc(unexpected_result(Row)))
	).



pldbc_execute(Statement, Parameters) :-
        pldbc_execute_hook(Statement, Parameters).

%% pldbc_close_statement(+Statement) 
%

pldbc_close_statement(Statement) :-
        pldbc_close_statement_hook(Statement).

%% pldbc_free_statement(+Statement) 
%

pldbc_free_statement(Statement) :-
        pldbc_free_statement_hook(Statement).



% ----------------------------------------
% INTROSPECTION
% ----------------------------------------

% NOT EXPORTED YET!!!

%%	pldbc_current_table(-Table, -Facet)
%
%	Enumerate the existing tables.
pldbc_current_table(Connection, Table) :-
        pldbc_current_table_hook(Connection, Table).

pldbc_current_table(Connection, Table, Facet) :-
        pldbc_current_table_hook(Connection, Table, Facet).

%%	pldbc_table_column(+Connection, +Table, +Column) is semidet.
%%	pldbc_table_column(+Connection, +Table, -Column) is nondet.
%
%	True if Column appears in Table on Connection.
pldbc_table_column(Connection, Table, Column) :-
        pldbc_table_column_hook(Connection, Table, Column).

%%	pldbc_table_column(+Connection, +Table, ?Column, -Facet)
pldbc_table_column(Connection, Table, Column, Facet) :-
        pldbc_table_column_hook(Connection, Table, Column, Facet).

pldbc_data_source(DSN, Description) :-
        pldbc_data_source_hook(DSN, Description).

pldbc_statistics(Key) :-
        pldbc_statistics_hook(Key).


		 /*******************************
		 *	      MESSAGES		*
		 *******************************/

:- multifile
	prolog:message/3.

prolog:message(error(pldbc(PLDBCCode, _NativeCode, Comment), _)) -->
	[ 'PLDBC: State ~w: ~w'-[PLDBCCode, Comment] ].
prolog:message(error(context_error(Obj, Error, What), _)) -->
	[ 'Context error: ~w ~w: '-[What, Obj] ],
	context(Error).

prolog:message(pldbc(PLDBCCode, _NativeCode, Comment)) -->
	[ 'PLDBC: State ~w: ~w'-[PLDBCCode, Comment] ].
prolog:message(pldbc(unexpected_result(Row))) -->
	[ 'PLDBC: Unexpected result-row: ~p'-[Row] ].

context(in_use) -->
	[ 'object is in use' ].

