import sys
import ijson
import bz2
import math
import pgf
import daison
from wordnet.semantics import *
import hashlib
import subprocess


def extract(wiki_fpath):
    with bz2.open(wiki_fpath, "rb") as f:
        
        with open("data/names.txt", "w") as out:
            for record in ijson.items(f, "item"):
                name_type = None
                gender = None
                for claim in record["claims"].get("P31",[]):
                    if "datavalue" not in claim["mainsnak"]:
                        continue
                    typ = claim["mainsnak"]["datavalue"]["value"]["id"]
                    if typ in ["Q12308941","Q11879590","Q101352"]:
                        name_type = typ
                    if typ in ["Q18972245","Q18972207"]:
                        gender = typ
                if name_type:
                    if gender:
                        name_type = gender
                    names = {}
                    for lang,val in record["labels"].items():
                        names[lang] = val["value"]
                    descr = record["descriptions"].get("en",{}).get("value","")
                    out.write(str((record["id"],descr,[],name_type,names))+"\n")

                given_names = []
                for given_name_claim in record["claims"].get("P735",[]):
                    if "datavalue" not in given_name_claim["mainsnak"]:
                        continue
                    given_names.append(given_name_claim["mainsnak"]["datavalue"]["value"]["id"])
                family_names = []
                for family_name_claim in record["claims"].get("P734",[]):
                    if "datavalue" not in family_name_claim["mainsnak"]:
                        continue                        
                    family_names.append(family_name_claim["mainsnak"]["datavalue"]["value"]["id"])
                geo_names = []
                for geo_name_claim in record["claims"].get("P1566",[]):
                    if "datavalue" not in geo_name_claim["mainsnak"]:
                        continue
                    geo_names.append(geo_name_claim["mainsnak"]["datavalue"]["value"])
                synsets = []
                for synset_claim in record["claims"].get("P8814",[]):
                    if "datavalue" not in synset_claim["mainsnak"]:
                        continue
                    synsets.append(synset_claim["mainsnak"]["datavalue"]["value"])

                title = None
                if "sitelinks" in record:
                    title = record["sitelinks"].get("enwiki",{"title":None}).get("title")
                if title:
                    url = "https://en.wikipedia.org/wiki/"+title.replace(' ','_')
                else:
                    url = "http://www.wikidata.org/entity/"+record["id"]
                images = []
                for prop in ["P18","P6802","P117","P8224","P242","P41","P94"]:
                    for image_claim in record["claims"].get(prop,[]):
                        if "datavalue" not in image_claim["mainsnak"]:
                            continue
                        img = image_claim["mainsnak"]["datavalue"]["value"]
                        img = img.replace(' ','_')
                        h = hashlib.md5(img.encode("utf-8")).hexdigest()
                        img = "commons/"+h[0]+"/"+h[0:2]+"/"+img
                        images.append((record["id"],url,img))

                if given_names or family_names:
                    label = record["labels"].get("en",{}).get("value")
                    out.write(str((record["id"],label,synsets,given_names,family_names))+"\n")

                if geo_names:
                    names = {}
                    for lang,val in record["labels"].items():
                        names[lang] = val["value"]
                    descr = record["descriptions"].get("en",{}).get("value","")
                    out.write(str((record["id"],descr,images,synsets,geo_names,names))+"\n")


def cyr(s):
    #DOUBLE LETTERS UPPERCASE
    s = s.replace("YU", "Ю")
    s = s.replace("YA", "Я")
    s = s.replace("ZH", "Ж")
    s = s.replace("TS", "Ц")
    s = s.replace("CH", "Ч")
    s = s.replace("SHT", "Щ")
    s = s.replace("SH", "Ш")
    s = s.replace("QU", "КУ")
    s = s.replace("FF", "Ф")
    s = s.replace("KK", "К")
    s = s.replace("OO", "О")
    #DOUBLE LETTERS MIXED CASE
    s = s.replace("Yu", "Ю")
    s = s.replace("Ya", "Я")
    s = s.replace("Zh", "Ж")
    s = s.replace("Ts", "Ц")
    s = s.replace("Ch", "Ч")
    s = s.replace("Sht", "Щ")
    s = s.replace("Sh", "Ш")
    s = s.replace("Qf", "Ку")
    s = s.replace("Ff", "Ф")
    s = s.replace("Kk", "К")
    s = s.replace("Oo", "О")
    #DOUBLE LETTERS LOWERCASE
    s = s.replace("yu", "ю")
    s = s.replace("ya", "я")
    s = s.replace("zh", "ж")
    s = s.replace("ts", "ц")
    s = s.replace("ch", "ч")
    s = s.replace("sht", "щ")
    s = s.replace("sh", "ш")
    s = s.replace("qu", "ку")
    s = s.replace("ff", "ф")
    s = s.replace("kk", "k")
    s = s.replace("oo", "о")
    #UPPERCASE LETTERS
    s = s.replace("A", "A")
    s = s.replace("B", "Б")
    s = s.replace("C", "К")
    s = s.replace("V", "В")
    s = s.replace("G", "Г")
    s = s.replace("D", "Д")
    s = s.replace("E", "E")
    s = s.replace("Z", "З")
    s = s.replace("I", "И")
    s = s.replace("J", "Й")
    s = s.replace("K", "К")
    s = s.replace("L", "Л")
    s = s.replace("M", "M")
    s = s.replace("N", "Н")
    s = s.replace("O", "O")
    s = s.replace("P", "П")
    s = s.replace("R", "Р")
    s = s.replace("S", "С")
    s = s.replace("T", "T")
    s = s.replace("Ț", "T")
    s = s.replace("W", "В")
    s = s.replace("F", "Ф")
    s = s.replace("H", "X")
    s = s.replace("U", "У")
    s = s.replace("Y", "Й")
    s = s.replace("Æ", "Е")
    s = s.replace("Ä", "Е")
    s = s.replace("É", "Е")
    s = s.replace("Ə", "А")
    s = s.replace("Î", "И")
    s = s.replace("Ö", "Йо")
    s = s.replace("Ø", "Йо")
    s = s.replace("Õ", "Йо")
    s = s.replace("Ó", "О")
    s = s.replace("Å", "А")
    s = s.replace("A", "А")
    s = s.replace("E", "Е")
    s = s.replace("M", "М")
    s = s.replace("O", "О")
    s = s.replace("Q", "К")
    s = s.replace("T", "Т")
    s = s.replace("X", "Кс")
    s = s.replace("À", "А")
    s = s.replace("Á", "А")
    s = s.replace("Â", "А")
    s = s.replace("Ç", "Ч")
    s = s.replace("È", "Е")
    s = s.replace("Ê", "Е")
    s = s.replace("Í", "И")
    s = s.replace("Ñ", "Н")
    s = s.replace("Ò", "О")
    s = s.replace("Ô", "О")
    s = s.replace("Ú", "У")
    s = s.replace("Ü", "У")
    s = s.replace("Ý", "Й")
    s = s.replace("Þ", "Т")
    s = s.replace("Ā", "А")
    s = s.replace("Ą", "А")
    s = s.replace("Ć", "Ц")
    s = s.replace("Č", "Ц")
    s = s.replace("Ď", "Д")
    s = s.replace("Đ", "Д")
    s = s.replace("Ē", "Е")
    s = s.replace("Ė", "Е")
    s = s.replace("Ě", "Е")
    s = s.replace("Ĝ", "Г")
    s = s.replace("Ğ", "Г")
    s = s.replace("Ġ", "Г")
    s = s.replace("Ģ", "Г")
    s = s.replace("Ī", "И")
    s = s.replace("İ", "И")
    s = s.replace("Ķ", "К")
    s = s.replace("Ļ", "Л")
    s = s.replace("Ľ", "Л")
    s = s.replace("Ł", "Л")
    s = s.replace("Ņ", "Н")
    s = s.replace("Ō", "О")
    s = s.replace("Ř", "Р")
    s = s.replace("Œ", "Е")
    s = s.replace("Ś", "С")
    s = s.replace("Ş", "С")
    s = s.replace("Š", "С")
    s = s.replace("Ţ", "Т")
    s = s.replace("Ť", "Т")
    s = s.replace("Ū", "У")
    s = s.replace("Ź", "З")
    s = s.replace("Ż", "З")
    s = s.replace("Ž", "З")
    s = s.replace("Ǎ", "А")
    s = s.replace("Ș", "Ш")
    s = s.replace("Ё", "Е")
    s = s.replace("Є", "Е")
    s = s.replace("Ј", "Й")
    s = s.replace("Љ", "Л")
    s = s.replace("Њ", "Н")
    s = s.replace("Џ", "У")
    s = s.replace("Э", "Е")
    s = s.replace("Ḥ", "Х")
    s = s.replace("Ṣ", "С")
    s = s.replace("Ỷ", "Й")
    #LOWERCASE LETTERS
    s = s.replace("a", "а")
    s = s.replace("b", "б")
    s = s.replace("c", "к")
    s = s.replace("v", "в")
    s = s.replace("g", "г")
    s = s.replace("d", "д")
    s = s.replace("e", "е")
    s = s.replace("z", "з")
    s = s.replace("i", "и")
    s = s.replace("j", "й")
    s = s.replace("k", "к")
    s = s.replace("l", "л")
    s = s.replace("m", "м")
    s = s.replace("n", "н")
    s = s.replace("o", "о")
    s = s.replace("p", "п")
    s = s.replace("r", "р")
    s = s.replace("s", "с")
    s = s.replace("t", "т")
    s = s.replace("ț", "т")
    s = s.replace("w", "в")
    s = s.replace("f", "ф")
    s = s.replace("h", "х")
    s = s.replace("u", "у")
    s = s.replace("y", "и")
    s = s.replace("æ", "е")
    s = s.replace("ä", "е")
    s = s.replace("é", "е")
    s = s.replace("ə", "а")
    s = s.replace("î", "и")
    s = s.replace("ö", "йо")
    s = s.replace("ø", "йо")
    s = s.replace("õ", "йо")
    s = s.replace("ó", "о")
    s = s.replace("å", "о")
    s = s.replace("ß", "с")
    s = s.replace("à", "а")
    s = s.replace("á", "а")
    s = s.replace("â", "а")
    s = s.replace("ã", "а")
    s = s.replace("ç", "ч")
    s = s.replace("è", "е")
    s = s.replace("ê", "е")
    s = s.replace("ë", "е")
    s = s.replace("ì", "и")
    s = s.replace("í", "и")
    s = s.replace("ï", "и")
    s = s.replace("ð", "о")
    s = s.replace("ñ", "н")
    s = s.replace("ò", "о")
    s = s.replace("ô", "о")
    s = s.replace("ù", "у")
    s = s.replace("ú", "у")
    s = s.replace("û", "у")
    s = s.replace("ü", "у")
    s = s.replace("ý", "й")
    s = s.replace("þ", "т")
    s = s.replace("ÿ", "й")
    s = s.replace("ā", "а")
    s = s.replace("ă", "а")
    s = s.replace("ą", "а")
    s = s.replace("ć", "ч")
    s = s.replace("ċ", "ч")
    s = s.replace("č", "ч")
    s = s.replace("ď", "д")
    s = s.replace("đ", "д")
    s = s.replace("ē", "е")
    s = s.replace("ė", "е")
    s = s.replace("ę", "е")
    s = s.replace("ě", "е")
    s = s.replace("ğ", "г")
    s = s.replace("ģ", "г")
    s = s.replace("ī", "и")
    s = s.replace("ĭ", "и")
    s = s.replace("ı", "и")
    s = s.replace("ķ", "к")
    s = s.replace("ĺ", "л")
    s = s.replace("ļ", "л")
    s = s.replace("ľ", "л")
    s = s.replace("ł", "л")
    s = s.replace("ń", "н")
    s = s.replace("ņ", "н")
    s = s.replace("ň", "н")
    s = s.replace("ō", "о")
    s = s.replace("ő", "о")
    s = s.replace("œ", "е")
    s = s.replace("ŕ", "г")
    s = s.replace("ř", "г")
    s = s.replace("ś", "с")
    s = s.replace("ş", "с")
    s = s.replace("š", "с")
    s = s.replace("ţ", "л")
    s = s.replace("ť", "л")
    s = s.replace("ũ", "у")
    s = s.replace("ū", "у")
    s = s.replace("ŭ", "у")
    s = s.replace("ů", "у")
    s = s.replace("ű", "у")
    s = s.replace("ŷ", "й")
    s = s.replace("ź", "з")
    s = s.replace("ż", "з")
    s = s.replace("ž", "з")
    s = s.replace("ơ", "о")
    s = s.replace("ư", "у")
    s = s.replace("ǎ", "а")
    s = s.replace("ǐ", "и")
    s = s.replace("ǒ", "о")
    s = s.replace("ǔ", "у")
    s = s.replace("ș", "ш")
    s = s.replace("ϊ", "и")
    s = s.replace("э", "е")
    s = s.replace("ё", "е")
    s = s.replace("є", "е")
    s = s.replace("і", "и")
    s = s.replace("ј", "й")
    s = s.replace("љ", "л")
    s = s.replace("њ", "н")
    s = s.replace("ў", "й")
    s = s.replace("џ", "у")
    s = s.replace("ḥ", "х")
    s = s.replace("ṅ", "н")
    s = s.replace("ṇ", "н")
    s = s.replace("ṣ", "с")
    s = s.replace("ạ", "а")
    s = s.replace("ả", "а")
    s = s.replace("ấ", "а")
    s = s.replace("ầ", "а")
    s = s.replace("ẫ", "а")
    s = s.replace("ậ", "а")
    s = s.replace("ắ", "а")
    s = s.replace("ặ", "а")
    s = s.replace("ế", "е")
    s = s.replace("ề", "е")
    s = s.replace("ễ", "е")
    s = s.replace("ệ", "е")
    s = s.replace("ọ", "о")
    s = s.replace("ố", "о")
    s = s.replace("ồ", "о")
    s = s.replace("ổ", "о")
    s = s.replace("ỗ", "о")
    s = s.replace("ớ", "о")
    s = s.replace("ợ", "о")
    s = s.replace("ụ", "у")
    s = s.replace("ứ", "у")
    s = s.replace("ử", "у")
    s = s.replace("ữ", "у")
    s = s.replace("ỳ", "й")
    s = s.replace("ỹ", "й")
    s = s.replace("q", "к")
    s = s.replace("x", "кс")

    return s

langs = [
  ("Afr",["af","nl","en"]),
  ("Bul",["bg","sr","ru","en"]),
  ("Cat",["ca","en"]),
  ("Chi",["zh"]),
  ("Dut",["nl","en"]),
  ("Eng",["en"]),
  ("Fin",["fi","en"]),
  ("Ger",["de","en"]),
  ("Kor",["ko"]),
  ("Ron",["ro","en"]),
  ("Som",["so","en"]),
  ("Swa",["sw","en"]),
  ("Tha",["th"]),
  ("Fre",["fr","en"]),
  ("Ita",["it","en"]),
  ("Mlt",["mt","en"]),
  ("Por",["pt","en"]),
  ("Rus",["ru","bg","sr","en"]),
  ("Slv",["sl","en"]),
  ("Spa",["es","en"]),
  ("Swe",["sv","en"]),
  ("Tur",["tr","en"]),
  ]

def quote(s):
    plain = True
    if s[0].isdigit():
        plain = False
    else:
        for c in s:
            if c not in "abcdefghijklmnopqrstuvwxyz_SGN0123456789":
                plain=False
                break

    if plain:
        q = s
    else:
        q = "'"
        for c in s:
            if c == "'":
                q = q + "\\'"
            elif c == "\\":
                q = q + "\\\\"
            else:
                q = q + c
        q = q + "'"

    return q

def dquote(s):
    q = "\""
    for c in s:
        if c == "\"":
            q = q + "\\\""
        elif c == "\\":
            q = q + "\\\\"
        else:
            q = q + c
    q = q + "\""
    return q

def generate(names_fpath,semantics_fpath,grammar_fpath):
    with daison.openDB(semantics_fpath) as db:
        with db.run("r") as t:
            existing_qids = set(qid for qid,keys in t.cursor(synsets_qid))

        # first pass to compute probabilities
        with open(names_fpath, "r") as f:
            q_ids  = {}
            counts = {"GN": {}, "SN": {}, "PN": {}}
            for line in f:
                record = eval(line)
                if type(record[-1]) is not dict:
                    for qid in record[3]:
                        cs = counts["GN"]
                        cs[qid] = cs.get(qid,1) + 1
                    for qid in record[4]:
                        cs = counts["SN"]
                        cs[qid] = cs.get(qid,1) + 1
                else:
                    if len(record[-1]) == 0:
                        continue
                    if len(record) == 5:
                        if record[3] in ["Q12308941","Q11879590"]:
                            tag = "GN"
                        else:
                            tag = "SN"
                    else:
                        tag = "PN"
                    counts[tag].setdefault(record[0],1)
                    q_ids[record[0]] = (tag,record)

            totals = {tag : sum(cs.values()) for tag,cs in counts.items()}

        gr = pgf.readNGF(grammar_fpath)

        with db.run("w") as s_t, gr.newTransaction() as g_t:
            for q_id,(tag,record) in sorted(q_ids.items(),key=lambda p: p[1]):
                if q_id in existing_qids:
                    continue

                name = record[-1].get("en",next(record[-1].items().__iter__())[0])
                name = name.split('/')[0].strip()
                name = name.lower()
                paren = name.find("(")
                if paren >= 0:
                    name = name[:paren].strip()
                name = name.replace(" ","_")

                typ = pgf.readType(tag)

                prob = counts[tag].get(q_id,0)/totals[tag]
                if prob == 0:
                    prob = float('inf')
                else:
                    prob = -math.log(prob)

                gf_id = g_t.createFunction(name + "_%d_" + tag, typ, 0, prob)
                q_ids[q_id] = (gf_id,tag,record)

                descr = record[1]

                status = []
                for lang,lang_codes in langs:
                    if lang_codes[0] in record[-1]:
                        st = Status.Checked
                    else:
                        st = Status.Guessed
                    status.append(("Parse"+lang,st))

                images = record[2]
                if not images:
                    images.append((q_id,"http://www.wikidata.org/entity/"+q_id,""))

                id = s_t.store(synsets,None,Synset(q_id,[],[],descr,images))
                s_t.store(lexemes,None,Lexeme(gf_id,prob,status,id,[],[],[],[]))

        with subprocess.Popen(["gf","-run",grammar_fpath], stdin=subprocess.PIPE) as proc:
            for lang,lang_codes in langs:
                proc.stdin.write(bytes("\ni -resource alltenses/Paradigms"+lang+".gfo\n\n","utf-8"))
                proc.stdin.write(b"transaction start\n\n")
                for q_id,info in sorted(q_ids.items(),key=lambda p: p[1]):
                    if q_id in existing_qids:
                        continue

                    (gf_id,tag,record) = info

                    if len(record) == 5:
                        name_type = record[3]
                    else:
                        name_type = None
                    labels = record[-1]
                    lins = []
                    lin_list = []
                    for lang_code in lang_codes:
                        s = labels.get(lang_code)
                        if s:
                            paren = s.find("(")
                            if paren >= 0:
                                s = s[:paren]
                            if lang in ["Bul","Rus"] and lang_code not in ["bg","ru"]:
                                s  = cyr(s)
                            lin_list = s.split("/")
                            break
                    for lin in lin_list:
                        lin = lin.strip()
                        if lang in ["Afr","Chi","Dut","Est","Fin","Kor","Swe","Tha","Tur"]:
                            lin = "mkPN "+dquote(lin)
                        elif lang in ["Som"]:
                            if name_type in ["Q12308941","Q18972245"]:
                                lin = "mkPN "+dquote(lin)+" sgMasc"
                            elif name_type in ["Q11879590","Q18972207"]:
                                lin = "mkPN "+dquote(lin)+" sgFem"
                            else:
                                lin = "mkPN "+dquote(lin)
                        elif lang in ["Swa"]:
                            lin = "mkPN "+dquote(lin)+" a_wa"
                        elif lang in ["Ron"]:
                            if name_type in ["Q12308941","Q18972245"]:
                                lin = "mkPN "+dquote(lin)+" Masculine"
                            elif name_type in ["Q11879590","Q18972207"]:
                                lin = "mkPN "+dquote(lin)+" Feminine"
                            else:
                                lin = "mkPN "+dquote(lin)
                        elif lang in ["Rus"]:
                            if name_type in ["Q12308941","Q18972245"]:
                                lin = "mkPN "+dquote(lin)+" masculine animate"
                            elif name_type in ["Q11879590","Q18972207"]:
                                lin = "mkPN "+dquote(lin)+" feminine animate"
                            else:
                                lin = "mkPN "+dquote(lin)
                        elif lang in ["Bul","Ger","Slv"]:
                            if name_type in ["Q12308941"]:
                                lin = "mkGN "+dquote(lin)+" male"
                            elif name_type in ["Q11879590"]:
                                lin = "mkGN "+dquote(lin)+" female"
                            elif name_type in ["Q18972245","Q18972207"]:
                                lin = "mkSN "+dquote(lin)
                            elif tag == "GN":
                                lin = "mkGN "+dquote(lin)+" male"
                            elif tag == "SN":
                                lin = "mkSN "+dquote(lin)
                            elif lang in ["Slv"]:
                                lin = "mkPN "+dquote(lin)+" masculine singular"
                            else:
                                lin = "mkPN "+dquote(lin)
                        elif lang == "Eng":
                            if name_type in ["Q12308941","Q18972245"]:
                                lin = "mkPN "+dquote(lin)+" masculine"
                            elif name_type in ["Q11879590","Q18972207"]:
                                lin = "mkPN "+dquote(lin)+" feminine"
                            else:
                                lin = "mkPN "+dquote(lin)+" nonhuman"
                        else:
                            if name_type in ["Q12308941","Q18972245"]:
                                lin = "mkPN "+dquote(lin)+" masculine"
                            elif name_type in ["Q11879590","Q18972207"]:
                                lin = "mkPN "+dquote(lin)+" feminine"
                            else:
                                lin = "mkPN "+dquote(lin)
                        lins.append(lin)
                    if len(lins) == 1:
                        lin = lins[0]
                    else:
                        lin = "variants {"+"; ".join(lins)+"}"
                    if tag in ["GN","SN"] and lang not in ["Bul","Ger","Slv"]:
                        lin = "lin "+tag+" <"+lin+" : PN>"
                    proc.stdin.write(bytes("create -lang=Parse"+lang+" lin "+quote(gf_id)+" = "+lin+"\n","utf-8"))
                proc.stdin.write(b"transaction commit\n\n")

def help():
    print("Syntax: names.py extract <path to wikidata archive>")
    print("        names.py generate <path to names.txt> <path to semantics.db> <path to Parse.ngf>")

if len(sys.argv) < 2:
    help()
elif sys.argv[1] == "extract":
    if len(sys.argv) < 3:
        help()
    else:
        extract(sys.argv[2])
elif sys.argv[1] == "generate":
    if len(sys.argv) < 5:
        help()
    else:
        generate(sys.argv[2],sys.argv[3],sys.argv[4])
else:
    help()
