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
          ("Bul",["bg","sr","ru"]),
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
                out.write("concrete Names"+lang+" of Names = open Cat"+lang+", Paradigms"+lang+" in {\n")
                out.write("\n")
                out.write("lincat GN, SN = PN ;\n")
                out.write("\n")
                for q_id,(gf_id,tag,name_type,labels) in sorted(q_ids.items(),key=lambda p: p[1]):
                    lins = []
                    lin_list = []
                    for lang_code in lang_codes:
                        s = labels.get(lang_code)
                        if s:
                            lin_list = s.split("/")
                            break
                    for lin in lin_list:
                        lin = lin.strip()
                        if name_type in ["Q12308941","Q18972245"]:
                            lin = "regGenPN \""+lin+"\" masculine"
                        elif name_type in ["Q11879590","Q18972207"]:
                            lin = "regGenPN \""+lin+"\" feminine"
                        else:
                            lin = "mkPN \""+lin+"\""
                        lins.append(lin)
                    if len(lins) == 1:
                        out.write("lin "+quote(gf_id)+" = "+lins[0]+" ;\n")
                    else:
                        out.write("lin "+quote(gf_id)+" = variants {"+"; ".join(lins)+"} ;\n")
                out.write("}\n")

generate()
