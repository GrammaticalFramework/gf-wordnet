.open data/panlex.db

.print "create tables"

create table source(id integer primary key not null,reg_date,label,url,isbn,author,title,publisher,year integer,quality integer,grp integer,note,license,ip_claim,ip_claimant,ip_claimant_email);
create table langvar(id integer primary key not null,lang_code char(3),var_code integer,mutable char,name_expr integer,script_expr integer,meaning integer,region_expr integer,uid_expr integer,grp integer);
create table expr(id integer primary key not null,langvar integer not null,txt text not null,txt_degr text not null);
create table meaning(id integer primary key not null,source integer not null);
create table denotation(id integer primary key not null,meaning integer not null,expr integer not null);

.separator ","
.headers on

.print "import source"

begin transaction;
.import "data/panlex-csv/source.csv" source
end transaction;

.print "import langvar"

begin transaction;
.import "data/panlex-csv/langvar.csv" langvar
end transaction;

.print "import expr"

begin transaction;
.import "data/panlex-csv/expr.csv" expr
end transaction;

.print "import meaning"

begin transaction;
.import "data/panlex-csv/meaning.csv" meaning
end transaction;

create index langvar_code on langvar(lang_code);
create index expr_txt on expr(langvar,txt);
create index expr_langvar on expr(langvar);
create index meaning_source on meaning(source);
create index denotation_meaning on denotation(meaning);
create index denotation_expr_meaning on denotation(expr,meaning);
 
.print "import denotation"

begin transaction;
.import "data/panlex-csv/denotation.csv" denotation
end transaction;

create index denontation_expr on denotation(expr);

.quit
