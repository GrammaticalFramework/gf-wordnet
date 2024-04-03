import sys
import json
import gzip

from dataclasses import dataclass

tag2cat = {
  'noun': 'N',
  'verb': 'V',
  'adj': 'A',
  'name': 'PN',
  'pron': 'Pron',
  'adv': 'Adv',
  'det': 'Det'
  }

params = {
  'definite': ('Def','Species'),
  'indefinite': ('Indef','Species'),
  'singular': ('Sg','Number'),
  'plural': ('Pl','Number'),
  'nominative': ('Nom','Case'),
  'accusative': ('Acc','Case'),
  'dative': ('Dat','Case'),
  'genitive': ('Gen','Case'),
  'vocative': ('Voc','Case'),
  'partitive': ('Part','Case'),
  'inessive': ('Iness','Case'),
  'elative': ('Elat','Case'),
  'illative': ('Illat','Case'),
  'adessive': ('Adess','Case'),
  'ablative': ('Ablat','Case'),
  'allative': ('Allat','Case'),
  'essive': ('Ess','Case'),
  'translative': ('Transl','Case'),
  'instructive': ('Instr','Case'),
  'abessive': ('Abess','Case'),
  'comitative': ('Comit','Case'),
  'possessive': ('Poss','Case'),
  'locative': ('Loc','Case'),
  'copulative': ('Cop','Case'),
  'unspecified': ('Unspecified','Distance'),
  'proximal': ('Proximal','Distance'),
  'distal': ('Distal','Distance'),
  'first-person': ('P1','Person'),
  'second-person': ('P2','Person'),
  'third-person': ('P3','Person'),
  'imperfective': ('Imperf','Tense'),
  'imperfect': ('Imperfect','Tense'),
  'aorist': ('Aorist','Tense'),
  'perfect': ('Perf','Tense'),
  'present': ('Pres','Tense'),
  'masculine': ('Masc','Gender'),
  'feminine': ('Fem','Gender'),
  'neuter': ('Neuter','Gender'),
}

param_order = [
  'Species',
  'Distance',
  'Case',
  'count-form',
  'multiword-construction',
  'Tense',
  'Number',
  'Person',
  'mutation'
]


ignore_tags = ['adjective', 'canonical', 'diminutive', 'romanization', 'table-tags', 'inflection-template']

class GFType:
    def printParamDefs(self,f,defs):
        pass

@dataclass(frozen=True)
class GFStr(GFType):
    def __repr__(self):
        return "Str"

    def renderOper(self,indent,evars):
        return vars.pop(0)

@dataclass(frozen=True)
class GFParam(GFType):
    name: str
    
    def __repr__(self):
        return self.name

    def renderOper(self,indent,vars):
        return vars.pop(0)

@dataclass(frozen=True)
class GFTable(GFType):
    param_type: GFType
    param_cons: tuple[str]
    res_type: GFType

    def __repr__(self):
        return self.param_type.__repr__() + " => " + self.res_type.__repr__()

    def renderOper(self,indent,vars):
        s = 'table {\n'
        first = True
        for pcon in self.param_cons:
            if not first:
                s += ' ;\n'
            s += ' '*(indent+2)+pcon+' => '+self.res_type.renderOper(indent+len(pcon)+6,vars)
            first = False
        s += '\n' + ' '*indent + '}'
        return s

    def printParamDefs(self,f,pdefs):
        pdef = "param "+str(self.param_type)+" = "+" | ".join(self.param_cons)+" ;\n"
        if pdef not in pdefs:
            f.write(pdef)
            pdefs.add(pdef)


@dataclass(frozen=True)
class GFRecord(GFType):
    fields: tuple[tuple[str,GFType]]

    def __repr__(self):
        s = ""
        for lbl,ty in self.fields:
            lbl = "".join([(c if c != '-' else '_') for c in str(lbl)])
            if s:
                s = s + "; "
            s = s + lbl+": "+ty.__repr__()
        return "{"+s+"}"
        
    def renderOper(self,indent,vars):
        s  = '{ '
        ind = 0
        for lbl,ty in self.fields:
            lbl = "".join([(c if c != '-' else '_') for c in str(lbl)])
            if ind > 0:
                s += ' ;\n'
            s += ' '*ind + lbl + ' = ' + ty.renderOper(indent+len(lbl)+5,vars)
            ind = (indent+2)
        s += '\n' + ' '*indent + '}'
        return s

    def printParamDefs(self,f,defs):
        for lbl,ty in self.fields:
           ty.printParamDefs(f,defs)

def getTypeOf(o):
    if type(o) is str:
        return GFStr()
    else:
        table  = {}
        record = []
        pcons  = []
        for tag,val in o.items():
            param = params.get(tag)
            if param == None:
                table = None
            else:
                param_con, param_type = param
                pcons.append(param_con)
            val_type = getTypeOf(val)
            if table != None:
                old_type = table.get(param_type)
                if old_type and old_type != val_type:
                    table = None
                else:
                    table[param_type] = val_type
            record.append((tag,val_type))

        if table and len(table) == 1:
            param_type,val_type = table.popitem()
            return GFTable(GFParam(param_type),tuple(pcons),val_type)
        else:
            return GFRecord(tuple(record))

def get_order(tag):
    try:
        return param_order.index(params.get(tag,(None,tag))[1])
    except:
        return 10000000

lin_types = {}
# the file should come from https://kaikki.org/dictionary/rawdata.html
with gzip.open('data/raw-wiktextract-data.json.gz','r') as f:
    for line in f:
        record = json.loads(line)
        if record.get("lang_code")==sys.argv[1]:
            table = {}
            word  = record["word"]
            forms = []
            for form in record.get("forms",[]):
                w    = form["form"]
                tags = form.get("tags",[])
                tags = [tag for tag in tags if tag not in ignore_tags]
                tags = sorted(tags,key=get_order)

                if not tags:
                    continue

                t = table
                for tag in tags[:-1]:
                    t1 = t.setdefault(tag,{})
                    if type(t1) is str:
                        t1 = {None: t1}
                        t[tag] = t1
                    t = t1

                t[tags[-1]] = w
                forms.append(w)
            if table:
                typ = getTypeOf(table)
                lin_types.setdefault(record.get("pos"),{}).setdefault(typ,[]).append((word,forms))

iso3 = {
    "mk": "Mkd",
    "sq": "Alb"
}

pdefs = set()
lang_code = iso3.get(sys.argv[1],sys.argv[1])
with open('Res'+lang_code+'.gf','w') as fr, \
     open('Cat'+lang_code+'.gf','w') as fc, \
     open('Dict'+lang_code+'.gf','w') as fd, \
     open('Dict'+lang_code+'Abs.gf','w') as fa:
    fr.write('resource Res'+lang_code+' = {\n')
    fr.write('\n')
    fc.write('concrete Cat'+lang_code+' of Cat = open Res'+lang_code+' in {\n')
    fc.write('\n')
    fd.write('concrete Dict'+lang_code+' of Dict'+lang_code+'Abs = Cat'+lang_code+' ** open Res'+lang_code+' {\n')
    fd.write('\n')
    fa.write('abstract Dict'+lang_code+'Abs = Cat ** {\n')
    fa.write('\n')
    for tag, types in lin_types.items():
        cat_name = tag2cat.get(tag)
        if not cat_name:
            continue

        fc.write('lin '+cat_name+tag2cat[tag]+' = '+tag.title()+' ;\n')

        for i,(typ,lexemes) in enumerate(sorted(types.items(),key=lambda x: -len(x[1]))):
            type_name = tag.title()+str(i+1)
            n_forms = len(lexemes[0][1])
            
            typ.printParamDefs(fr,pdefs)

            fr.write('oper '+type_name+' = '+str(typ)+' ; -- '+str(len(lexemes))+'\n')
            fr.write('oper mk'+type_name+' : ')
            if n_forms == 1:
                fr.write('Str')
            elif n_forms == 2:
                fr.write('Str -> Str')
            elif n_forms == 3:
                fr.write('Str -> Str -> Str')
            else:
                fr.write('('+(',_'*n_forms)[1:]+' : Str)')
            fr.write(' -> '+type_name+' =\n')
            vars = ['f'+str(i) for i in range(1,n_forms+1)]
            fr.write('       \\'+','.join(vars)+' ->\n')
            fr.write('          '+typ.renderOper(10,vars)+" ;\n")
            fr.write('\n')

            for lexeme,forms in lexemes:
                fa.write('fun \''+lexeme+'_'+cat_name+'\' : '+cat_name+' ;\n')
                fd.write('lin \''+lexeme+'_'+cat_name+'\' = mk'+type_name+' '+' '.join(('"'+form+'"' if form != '-' else 'nonExist') for form in forms)+' ;\n')

        fr.write('\n')
    fa.write('\n')
    fa.write('}\n')
    fd.write('\n')
    fd.write('}\n')
    fc.write('\n')
    fc.write('}\n')
    fr.write('\n')
    fr.write('}\n')
