.open panlex.db

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
.import source.csv source
end transaction;

.print "import langvar"

begin transaction;
.import langvar.csv langvar
end transaction;

.print "import expr"

begin transaction;
.import expr.csv expr
end transaction;

.print "import meaning"

begin transaction;
.import meaning.csv meaning
end transaction;

create index langvar_code on langvar(lang_code);
create index expr_txt on expr(langvar,txt);
create index expr_langvar on expr(langvar);
create index meaning_source on meaning(source);
create index denotation_meaning on denotation(meaning);

.print "import denotation"

begin transaction;
.import denotation.csv denotation
end transaction;

create index denontation_expr on denotation(expr);

.print "extract wordnet"

create table pairs(meaning integer not null,
                   expr1 integer not null, expr2 integer not null,
                   s integer not null default 0, t integer not null default 0, d integer not null default 0,
                   srank integer not null default 0, trank integer not null default 0, drank integer not null default 0);

begin transaction;
insert into pairs(meaning,expr1,expr2) select m.id, e1.id, e2.id from meaning m join denotation d1 on m.id=d1.meaning join expr e1 on e1.id=d1.expr join denotation d2 on m.id=d2.meaning join expr e2 on e2.id=d2.expr where m.source=4620 and e1.langvar < e2.langvar;
end transaction;

create index pairs_expr1 on pairs(expr1);
create index pairs_expr2 on pairs(expr2);

.print "compute scores"

create table wordnet_meaning(expr1 integer not null, expr2 integer not null, meaning integer not null);
insert into wordnet_meaning(expr1,expr2,meaning)
select distinct w.expr1, w.expr2, d1.meaning
      from pairs w
      join denotation d1 on w.expr1 = d1.expr
      join denotation d2 on w.expr2 = d2.expr and d2.meaning = d1.meaning;

create index wordnet_meaning_expr on wordnet_meaning(expr1,expr2);
create index wordnet_meaning_meaning on wordnet_meaning(meaning);

create table expr_expr(expr1 integer not null, expr2 integer not null, c integer not null);

insert into expr_expr(expr1,expr2,c)
select s.expr1,s.expr2,sum(s.quality) as quality 
from (select w.expr1, w.expr2, max(s.quality) as quality
      from wordnet_meaning w
      join meaning m on m.id = w.meaning
      join source s on m.source = s.id
      group by w.expr1, w.expr2, s.grp) s
group by s.expr1, s.expr2;

create index expr_expr_expr on expr_expr(expr1,expr2);

update pairs set t = (select c from expr_expr where expr1=pairs.expr1 and expr2=pairs.expr2);

delete from expr_expr;

insert into expr_expr(expr1,expr2,c)
select w.expr1, w.expr2, count(*) as c
from wordnet_meaning w
join meaning m on m.id = w.meaning and m.source = 4620
group by w.expr1, w.expr2;

update pairs set s = (select c from expr_expr where expr1=pairs.expr1 and expr2=pairs.expr2);

drop table expr_expr;
drop table wordnet_meaning;

.load ./levenstein

update pairs set d = levenstein((select txt from expr where id=expr1), (select txt from expr where id=expr2));

.print "compute ranks"

alter table pairs add column langvar1 integer;
alter table pairs add column langvar2 integer;

update pairs set langvar1 = (select langvar from expr where expr.id=expr1);
update pairs set langvar2 = (select langvar from expr where expr.id=expr2);

create index wordnet_s on pairs(meaning,langvar1,langvar2,s);
create index wordnet_t on pairs(meaning,langvar1,langvar2,t);
create index wordnet_d on pairs(meaning,langvar1,langvar2,d);

update pairs set srank = (select count(distinct s) from pairs w where w.meaning=pairs.meaning and w.langvar1=pairs.langvar1 and w.langvar2=pairs.langvar2 and w.s > pairs.s);
update pairs set trank = (select count(distinct t) from pairs w where w.meaning=pairs.meaning and w.langvar1=pairs.langvar1 and w.langvar2=pairs.langvar2 and w.t > pairs.t);
update pairs set drank = (select count(distinct d) from pairs w where w.meaning=pairs.meaning and w.langvar1=pairs.langvar1 and w.langvar2=pairs.langvar2 and w.d < pairs.d);

drop index wordnet_s;
drop index wordnet_t;
drop index wordnet_d;

create table glosses(meaning integer, wordnet_id char(10), gloss text);
create table abstract_words(id integer primary key, meaning integer, rank integer);
create table concrete_words(id integer primary key not null, abs_id integer not null, lang_code char(3), expr integer not null, new integer);

create index concrete_words_abs on concrete_words(abs_id);
create index concrete_words_expr on concrete_words(expr);
create index concrete_words_lang_code on concrete_words(lang_code);
create index glosses_meaning on glosses(meaning);

.quit
