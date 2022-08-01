import ijson
import bz2

def extract():
    with bz2.open("wikidata-2022-07-31.json.bz2", "rb") as f:
        with open("names.txt", "w") as out:
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
                    out.write(str((record["id"],name_type,names))+"\n")

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
    s = s.replace("Å", "О")
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
    
    return s;

def generate():
    with open("names.txt", "r") as f:
        gf_ids = {}
        q_ids  = {}
        for line in f:
            record = eval(line)
            if len(record[2]) == 0:
                continue
            name = record[2].get("en",next(record[2].items().__iter__())[0])
            name = name.split('/')[0].strip()
            name = name.lower()
            if record[1] in ["Q12308941","Q11879590"]:
                tag = "GN"
            else:
                tag = "SN"
            nt = (name,tag)
            info = gf_ids.get(nt)
            if not info:
                gf_ids[nt] = [record[0]]
                gf_id = name+"_"+tag
            else:
                if len(info) == 1:
                    q_ids[info[0]] = (name+"_1_"+tag,tag,record[1],record[2])
                info.append(record[0])
                gf_id = name+"_"+str(len(info))+"_"+tag
            q_ids[record[0]] = (gf_id,tag,record[1],record[2])

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
                    else:
                        q = q + c
                q = q + "'"

            return q

        with open("Names.gf","w") as out:
            out.write("abstract Names = {\n")
            out.write("\n")
            out.write("cat GN ; SN ;\n")
            out.write("\n")
            for q_id,(gf_id,tag,name_type,labels) in sorted(q_ids.items(),key=lambda p: p[1]):
                out.write(("fun "+quote(gf_id)+" : "+tag+" ;").ljust(40)+"-- "+q_id+"\n")
            out.write("}\n")

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
          ("Pol",["pl","en"]),
          ("Ron",["ro","en"]),
          ("Som",["so","en"]),
          ("Swa",["sw","en"]),
          ("Tha",["th"]),
          ("Est",["fi","en"]),
          ("Fre",["fr","en"]),
          ("Ita",["it","en"]),
          ("Mlt",["mt","en"]),
          ("Por",["pt","en"]),
          ("Slv",["sl","en"]),
          ("Spa",["es","en"]),
          ("Swe",["sv","en"]),
          ("Tur",["tr","en"]),
          ]

        for lang,lang_codes  in langs:
            with open("Names"+lang+".gf","w") as out:
                out.write("concrete Names"+lang+" of Names = Cat"+lang+" ** open Paradigms"+lang+" in {\n")
                out.write("\n")
                out.write("lincat GN, SN = PN ;\n")
                out.write("\n")
                for q_id,(gf_id,tag,name_type,labels) in sorted(q_ids.items(),key=lambda p: p[1]):
                    lins = []
                    lin_list = []
                    for lang_code in lang_codes:
                        s = labels.get(lang_code)
                        if s:
                            if lang == "Bul" and lang_code != "bg":
                                s  = cyr(s)
                            lin_list = s.split("/")
                            break
                    for lin in lin_list:
                        lin = lin.strip()
                        if lang == "Eng":
                            if name_type in ["Q12308941","Q18972245"]:
                                lin = "regGenPN \""+lin+"\" masculine"
                            elif name_type in ["Q11879590","Q18972207"]:
                                lin = "regGenPN \""+lin+"\" feminine"
                            else:
                                lin = "mkPN \""+lin+"\""
                        elif lang == "Bul":
                            if name_type in ["Q11879590","Q18972207"]:
                                lin = "mkPN \""+lin+"\" Fem"
                            else:
                                lin = "mkPN \""+lin+"\" Masc"
                        lins.append(lin)
                    if len(lins) == 1:
                        out.write("lin "+quote(gf_id)+" = "+lins[0]+" ;\n")
                    else:
                        out.write("lin "+quote(gf_id)+" = variants {"+"; ".join(lins)+"} ;\n")
                out.write("}\n")

generate()
