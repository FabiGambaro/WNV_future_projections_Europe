# usage: python3 -I check_quotes.py bNNN_texts.json bNNN_records.jsonl bNNN_outcomes.jsonl
import json,sys
T=json.load(open(sys.argv[1])); bad=0; n=0
req=["article_id","place","country","place_level","admin_hint","role","status","human_case","certainty","n_cases","year_infection","year_basis","quote","note","text_source"]
enums={"place_level":{"municipality","province","region","country"},"role":{"infection","residence","reporting_area","hospital","possible_exposure"},
       "status":{"autochthonous","imported","not_stated"},"human_case":{"yes","unclear"},"certainty":{"confirmed","suspected"},"year_basis":{"explicit","onset","diagnosis","publication"}}
for i,l in enumerate(open(sys.argv[2])):
    if not l.strip(): continue
    r=json.loads(l); n+=1
    miss=[k for k in req if k not in r]
    if miss: print("line",i+1,"missing",miss); bad+=1; continue
    for k,v in enums.items():
        if r[k] not in v: print("line",i+1,k,"=",r[k],"not allowed"); bad+=1
    a=r["article_id"]
    if a not in T: print("line",i+1,"unknown article",a); bad+=1; continue
    if r["text_source"] not in T[a]: print("line",i+1,a,"text_source",r["text_source"],"not available; use one of",list(T[a])); bad+=1; continue
    if r["quote"] not in T[a][r["text_source"]]: print("line",i+1,a,"QUOTE NOT FOUND in",r["text_source"],":",r["quote"][:100]); bad+=1
outs={json.loads(l)["article_id"] for l in open(sys.argv[3]) if l.strip()}
if set(T)-outs: print("outcomes missing for",sorted(set(T)-outs)); bad+=1
print(n,"records,",bad,"problems,",len(outs),"of",len(T),"outcomes")
