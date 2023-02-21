import sys
import json
import gzip

from dataclasses import dataclass

params = {
  'definite': 'Species',
  'indefinite': 'Species',
  'singular': 'Number',
  'plural': 'Number',
  'nominative': 'Case',
  'accusative': 'Case',
  'dative': 'Case',
  'genitive': 'Case',
  'vocative': 'Case',
  'partitive': 'Case',
  'inessive': 'Case',
  'elative': 'Case',
  'illative': 'Case',
  'adessive': 'Case',
  'ablative': 'Case',
  'allative': 'Case',
  'essive': 'Case',
  'translative': 'Case',
  'instructive': 'Case',
  'abessive': 'Case',
  'comitative': 'Case',
  'possessive': 'Case',
  'locative': 'Case',
  'copulative': 'Case',
  'unspecified': 'Distance',
  'proximal': 'Distance',
  'distal': 'Distance',
  'first-person': 'Person',
  'second-person': 'Person',
  'third-person': 'Person',
  'imperfective': 'Tense',
  'imperfect': 'Tense',
  'aorist': 'Tense',
  'perfect': 'Tense',
  'present': 'Tense'
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


ignore_tags = ['adjective', 'canonical', 'diminutive', 'feminine', 'masculine', 'romanization', 'table-tags', 'inflection-template']

class GFType:
    pass

@dataclass(frozen=True)
class GFStr(GFType):
    def __repr__(self):
        return "Str"

@dataclass(frozen=True)
class GFParam(GFType):
    name: str
    
    def __repr__(self):
        return self.name

@dataclass(frozen=True)
class GFTable(GFType):
    key: GFType
    value: GFType

    def __repr__(self):
        return self.key.__repr__() + " => " + self.value.__repr__()

@dataclass(frozen=True)
class GFRecord(GFType):
    fields: tuple[tuple[str,GFType]]

    def __repr__(self):
        s = ""
        for lbl,ty in self.fields:
            if s:
                s = s + "; "
            s = s + lbl+": "+ty.__repr__()
        return "{"+s+"}"

def getTypeOf(o):
    if type(o) is str:
        return GFStr()
    else:
        table  = {}
        record = []
        for tag,val in o.items():
            cat_name = params.get(tag)
            if cat_name == None:
                table = None
            val_type = getTypeOf(val)
            if table != None:
                old_type = table.get(cat_name)
                if old_type and old_type != val_type:
                    table = None
                else:
                    table[cat_name] = val_type
            record.append((tag,val_type))

        if table and len(table) == 1:
            cat,ty = table.popitem()
            return GFTable(GFParam(cat),ty)
        else:
            return GFRecord(tuple(record))

def get_order(tag):
    try:
        return param_order.index(params.get(tag,tag))
    except:
        return 10000000

all_types = {}
with gzip.open('data/raw-wiktextract-data.json.gz','r') as f:
    for line in f:
        record = json.loads(line)
        if record.get("lang_code")==sys.argv[1]:
            table = {}
            for form in record.get("forms",[]):
                w    = form["form"]
                tags = form.get("tags",[])
                tags = [tag for tag in tags if tag not in ignore_tags]
                tags = sorted(tags,key=get_order)

                if not tags:
                    continue

                t = table
                for tag in tags[:-1]:
                    if type(t) is str:
                        t = {None: t}
                    t = t.setdefault(tag,{})
                    
                if type(t) is str:
                    t = {None: t}
                t[tags[-1]] = w
            if table:
                typ = getTypeOf(table)
                types = all_types.setdefault(record.get("pos"),{})
                types[typ] = types.get(typ,0)+1

for tag, types in all_types.items():
    print(tag,len(types))
    for typ,count in sorted(types.items(),key=lambda x: -x[1]):
        print(count,typ)
