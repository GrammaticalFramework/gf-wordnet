import xml.etree.ElementTree as ET
import pgf
import math

def wordCount(xs):
	counts = {}
	for x in xs:
		if x in counts:
			counts[x] = counts[x]+1
		else:
			counts[x] = 1
	return counts

def cosSim(s1,s2):
	c1 = wordCount(s1)
	c2 = wordCount(s2)
	s = 0
	for w in set(c1).intersection(c2):
		s = s + c1[w]*c2[w]
	s1 = 0
	for w in c1:
		s1 = s1 + c1[w]*c1[w]
	s2 = 0
	for w in c2:
		s2 = s2 + c2[w]*c2[w]
	return s/math.sqrt(s1*s2)
	
gr  = pgf.readPGF("build/ParseEng.pgf")
eng = gr.languages["ParseEng"]

abs_ids = {}
f = open("WordNet.gf")
for l in f:
	ws = l.split()
	if len(ws) > 1 and ws[0] == "fun":
		ts = l.split('\t')
		if len(ts) > 1:
			abs_ids.setdefault(ts[0][-10:],[]).append(ws[1])
f.close()

sense_ids = {}
f = open("data/index.sense")
for l in f:
	ws = l.split()
	if len(ws) >= 2:
		c = ws[0][ws[0].index('%')+1]
		if c == "1":
			tag = "n"
		elif c == "2":
			tag = "v"
		elif c == "3":
			tag = "a"
		elif c == "4":
			tag = "r"
		elif c == "5":
			tag = "a"

		sense_ids[ws[0]] = abs_ids.get(ws[1]+"-"+tag,[])
f.close()
del abs_ids

d = {}
f = open("data/semcor.gold.key.txt")
for l in f:
	ws = l.split()
	d[ws[0]] = sense_ids.get(ws[1],[])
f.close()
del sense_ids

tree = ET.parse('data/semcor.data.xml')
for text in tree.getroot():
	print("# newdoc id = "+text.attrib["id"])

	doc = []
	f = open("data/brown/c"+text.attrib["source"][3:])
	for l in f:
		sent = []
		for w_t in l.split():
			xs = w_t.split("/")
			sent.append(("/".join(xs[0:-1]),xs[-1]))
		if len(sent) > 0:
			doc.append(sent)
	f.close()

	sentNo = 0
	for sentence in text:
		print("# sent_id = "+sentence.attrib["id"])

		ws  = []
		ass = []
		for child in sentence:
			ws.append(child.text)
			ass.append(child.attrib)

		maxCos = 0
		minDist= 10000000000
		i      = 0
		for curSent in doc:
			c    = cosSim(map(lambda x: x[0],curSent),ws)
			dist = abs(sentNo-i)
			if c > maxCos or (c == maxCos and dist < minDist):
				maxCos = c
				minDist= dist
				sent   = curSent
			i = i + 1
			
		sentDict = {}
		for w,t in sent:
			if w in sentDict:
				sentDict[w].append(t)
			else:
				sentDict[w] = [t]

		print("# text = "+" ".join(ws))
		for i,w in enumerate(ws):
			if w not in sentDict or len(sentDict[w]) == 0:
				tag = "_"
			else:
				tag = sentDict[w].pop(0)

			if "id" in ass[i] and ass[i]["id"] in d:
				fns = d[ass[i]["id"]]
				ans1 = []
				ans2 = []
				for f,an,p in eng.lookupMorpho(w):
					if f not in fns:
						continue

					ans1.append((f,an,p))

					if tag == "rb" and an == "s (AAdj Posit Nom)":
						continue
					if tag == "vbd" and an == "s VPPart":
						continue
					if tag == "vbd" and an == "s VInf":
						continue
					if tag == "vbn" and an == "s VPast":
						continue
					if tag == "vbn" and an == "s VInf":
						continue
					if tag == "vb" and an == "s VPPart":
						continue
					if tag == "vb" and an == "s VPast":
						continue
					if tag == "nn" and an == "s Pl Nom":
						continue
					if tag == "nns" and an == "s Sg Nom":
						continue

					ans2.append((f,an,p))

				if len(ans2) == 0:
					ans = ans1
				else:
					ans = ans2

				ans2 = []
				for f,an,p in ans:
					tbl = eng.tabularLinearize(pgf.Expr(f,[]))
					if "p" in tbl and tbl["p"] != "" and tbl["p"] not in ws:
						continue
					if "c2" in tbl and tbl["c2"] != "" and tbl["c2"] not in ws:
						continue
					if "c3" in tbl and tbl["c3"] != "" and tbl["c3"] not in ws:
						continue
					ans2.append((f,an,p))
				ans = ans2
			else:
				ans = []

				if tag == "at" and w.lower() in ["a","an"]:
					ans.append(("IndefArt","_",0))

				for f,an,p in eng.lookupMorpho(w):
					cat = str(gr.functionType(f))
					if tag == "pp$" and cat == "Pron" and an == "s (NCase Gen)":
						ans.append((f,an,p))
					elif tag == "ppo" and cat == "Pron" and (an == "s (NCase Nom)" or an == "s NPAcc"):
						ans.append((f,"_",p))
					elif tag == "pps" and cat == "Pron" and (an == "s (NCase Nom)" or an == "s NPAcc"):
						ans.append((f,"_",p))
					elif tag == "ppss" and cat == "Pron" and (an == "s (NCase Nom)" or an == "s NPAcc"):
						ans.append((f,"_",p))
					elif tag == "in" and cat == "Prep" and an == "s":
						ans.append((f,an,p))
					elif tag == "abx" and cat == "Conj" and an == "s1":
						tbl = eng.tabularLinearize(pgf.Expr(f,[]))
						if "s2" in tbl and tbl["s2"] != "" and tbl["s2"] in ws:
							ans = [(f,an,p)]
							break
					elif tag == "cc" and cat == "Conj" and an == "s1":
						tbl = eng.tabularLinearize(pgf.Expr(f,[]))
						if "s2" in tbl and tbl["s2"] != "" and tbl["s2"] in ws:
							ans = [(f,an,p)]
							break
					elif tag == "cc" and cat == "Conj" and an == "s2":
						tbl = eng.tabularLinearize(pgf.Expr(f,[]))
						if "s1" in tbl and tbl["s1"] != "":
							if tbl["s1"] in ws:
								ans = [(f,an,p)]
								break
						else:
							ans.append((f,an,p))
					elif tag == "dtx" and cat == "Det" and an == "s":
						ans.append((f,an,p))
					elif tag == "dt" and cat == "Quant" and (an == "s False Sg" or an == "s True Sg"):
						ans.append((f,"_",p))
					elif tag == "dts" and cat == "Quant" and (an == "s False Pl" or an == "s True Pl"):
						ans.append((f,"_",p))
					elif tag == "dti" and cat == "Quant" and (an == "s False Sg" or an == "s False Pl" or an == "s True Sg" or an == "s True Pl"):
						ans.append((f,"_",p))
					elif tag == "dti" and cat == "Det" and an == "s":
						ans.append((f,"_",p))
					elif tag == "at" and cat == "Quant" and (an == "s False Sg" or an == "s False Pl" or an == "s True Sg" or an == "s True Pl"):
						ans.append((f,"_",p))

			ans = list(dict.fromkeys(ans))
			if len(ans) == 0:
				print(str(i+1)+"\t"+w+"\t"+ass[i]["lemma"]+"\t_\t"+tag.upper())
			else:
				if len(ans) > 1:
					print("# Ambiguity")
				for f,an,p in ans:
					print(str(i+1)+"\t"+w+"\t"+ass[i]["lemma"]+"\t"+str(gr.functionType(f))+"\t"+tag.upper()+"\t"+an+"\t"+f)

		print()
		sentNo = sentNo + 1
