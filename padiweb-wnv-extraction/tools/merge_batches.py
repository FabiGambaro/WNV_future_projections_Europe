import json,csv,glob,sys,os
B=sys.argv[1]; P=sys.argv[2]
cols="pid,article_id,place,country,place_level,admin_hint,role,status,human_case,certainty,n_cases,year_infection,year_basis,quote,note,text_source".split(',')
recs=[];outs=[]
for f in sorted(glob.glob(f'{B}/b*_records.jsonl')):
    for l in open(f,encoding='utf-8'):
        l=l.strip()
        if l: recs.append(json.loads(l))
for f in sorted(glob.glob(f'{B}/b*_outcomes.jsonl')):
    for l in open(f,encoding='utf-8'):
        l=l.strip()
        if l: outs.append(json.loads(l))
pid={};n=0
for r in recs:
    if r['article_id'] not in pid: n+=1; pid[r['article_id']]=n
    r['pid']=pid[r['article_id']]
with open(f'{P}/records_extracted.csv','w',newline='',encoding='utf-8') as fh:
    w=csv.DictWriter(fh,cols,extrasaction='ignore'); w.writeheader()
    for r in recs: w.writerow({k:('' if r.get(k) is None else r.get(k)) for k in cols})
seen={}
for o in outs: seen[o['article_id']]=o['outcome']
with open(f'{P}/article_outcomes.csv','w',newline='',encoding='utf-8') as fh:
    w=csv.writer(fh); w.writerow(['article_id','outcome'])
    for k,v in seen.items(): w.writerow([k,v])
print(len(recs),'records;',len(seen),'articles with outcomes;',len(pid),'articles with records')
