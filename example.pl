:- use_module(pldbc).
:- use_module(pldbc_cli_mysql).   % may be auto-loaded in future

test :-  
        pldbc_connect(cli_mysql([opts('-A -hmysql.ebi.ac.uk -ugo_select -pamigo -P4085 go_latest')]), Connection, []),
        pldbc_query(Connection, 'SELECT * FROM term LIMIT 25',Row),
        writeln(Row),
        fail.
