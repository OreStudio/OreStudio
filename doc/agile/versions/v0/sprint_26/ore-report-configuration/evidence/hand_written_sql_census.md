#+title: Hand-written SQL census

Which SQL files under =projects/ores.sql/create=, =drop= and =populate= codegen
produces, which another generator produces, and which people write by hand, and
why. Measured at =8107e1f421= (main after PR #2505), before this task changed
anything.

* Method

A "generated" marker cannot answer the question: no drop template writes one,
and some files carry a marker although their generator's input was deleted. So
the census asks the generator instead. =render_all.py= renders every catalogue
component at the whole =ores= address into a scratch root, with the same
functions =check_component_drift.py --sweep= uses. A file is *entity codegen*
exactly when that root contains it. A file with a marker that the root does not
contain is *another generator*. The rest is *hand-written*.

#+begin_src sh
python3 render_all.py <root>            # 21 catalogue components, 1119 SQL files
python3 classify.py <root> census.csv   # origin of every file
python3 reasons.py census.csv reasons.csv
python3 reach.py                        # files no script includes
python3 order.py                        # soft-FK parents created later
#+end_src

The scripts are at the end of this file.

* Origin

| Tree | Entity codegen | Other generator | Hand-written | Total |
|------+----------------+-----------------+--------------+-------|
| create | 579 | 47 | 145 | 771 |
| drop | 537 | 0 | 204 | 741 |
| populate | 3 | 117 | 168 | 288 |
| *all* | 1119 | 164 | 517 | 1800 |

Every file the render produces exists in the tree: 0 rendered files are absent.

* Why a create file is not entity codegen

Each of the 192 non-codegen create files is in exactly one class.

| Class | Files | Why no entity model produces it |
|-------+-------+---------------------------------|
| A. Aggregates and root scripts | 23 | Dependency order and prose per block. No generator exists. |
| B. Components with no codegen catalogue | 14 | =database=, =geo=, =mq=, =seed=, =utility= have no catalogue component. Mostly functions; =mq= holds the only hard =REFERENCES= in the tree. |
| C. RLS policy files | 14 | One per component, run last from =rls/rls_create.sql=, holding 251 policies. See "RLS" below. |
| D. Notify triggers for unmodelled tables | 16 | Fifteen are the FpML coded-scheme tables of class G2, one is =dq_fsm_machines=. |
| E2. DQ artefact staging tables, no model | 7 | Staging tables for imported datasets (LEI, ip2country, CRM topology, market data observations, synthetic configs). |
| E3. Tables with no model | 18 | Junctions (=iam_account_roles=, =iam_role_permissions=, =reporting_risk_report_config_books=, =_portfolios=), time series (=iam_auth_events=, =iam_session_samples=, telemetry), DQ publication logs and others. |
| F. Procedural | 48 | Functions, procedures and trigger guards: publish-from-DQ, tenant lifecycle, validators. Logic, not a table shape. |
| G1. IAM service bundle | 2 | Written by the service template and run from the bootstrap scripts, not from =create.sql=. |
| G2. FpML coded-scheme tables and artefacts | 44 | Marked "do not edit", written by the legacy JSON table generator. Those JSON models no longer exist, so nothing can regenerate them; they have been edited by hand since. |
| G3. Other generator | 1 | =variability_system_settings_functions_create.sql=. |
| H. View | 1 | =trading_ore_envelope_view_create.sql=. |
| I. Alter and other | 4 | Post-hoc =alter= scripts and FSM setup. |

* Why a populate file is not entity codegen

| Class | Files |
|-------+-------|
| Aggregates | 23 |
| Data generators (Acme =generate_sql.py=, crypto, FpML dataset and refdata templates, IAM service accounts, ip2country) | 117 |
| Hand: seed rows for lookup and system tables | 122 |
| Hand: DQ artefact staging rows | 11 |
| Hand: dataset registration | 11 |
| Hand: other | 8 |

The hand-written populate files are authored data, not schema.

* Files no script includes

=reach.py= follows =\ir= from =create/create.sql=, =drop/drop.sql=,
=populate/populate.sql= and =setup_schema.sql=, for every =.sql= file whatever
its suffix. =WIRE_001= only checks names ending =_create.sql= or =_drop.sql=, and
never the populate tree.

| File | Status |
|------+--------|
| =create/refdata/refdata_*_notify_trigger.sql= (15) | Dead. An older copy of the wired =_notify_trigger_create.sql=, without =tenant_id= in its payload; no code references it. Invisible to =WIRE_001= because of its suffix. |
| =create/iam/service_users_create.sql= | Run from =recreate_database.sql=. Exempt in =WIRE_001=. |
| =drop/drop_database.sql=, =drop/drop_test_databases.sql= | Manual =psql= entry points, documented in their headers. |
| =drop/refdata/refdata_curve_roles_drop.sql= and its trigger drop, =drop/iam/iam_session_samples_drop.sql=, =drop/iam/iam_tenant_terminator_drop.sql=, =drop/mq/mq_scrape_functions_drop.sql= | Real wiring gaps, held in =validation_ignore.txt=. |
| =populate/fpml/populate_fpml.sql= | Written and run by =fpml_parser.py=. |
| =populate/fpml/fpml_coding_schemes_populate.sql=, =populate/ip2country/ip2country_import.sql=, =populate/scheduler/scheduler_populate.sql=, =populate/trading/trading_sample_data_populate.sql= | No reference in code. Unchecked by any rule. |

Four =WIRE_001= ignore entries were stale: the trading equity and RPA drops are
wired now, and the compute =app_version_platform= file exists and is wired. They
are removed by this task.

* Does creation order depend on the models?

Only one hard foreign key exists in the create tree, in =mq=. Every other
reference is a soft foreign key checked inside a PL/pgSQL insert trigger, which
PostgreSQL resolves when the trigger runs, not when it is created.

=order.py= flattens the create order (755 files, 372 tables) and finds *88*
tables whose soft-FK parent is created after them, for example
=workspace_workspaces= before =ores_refdata_parties_tbl= and =ores_iam_accounts_tbl=,
and every trading instrument before =ores_trading_trades_tbl=. Every recreate
succeeds with that order. So a soft foreign key imposes no creation order.

What does impose order:
- a table before its own notify trigger, and on the drop side the trigger drop
  before the table drop, because =drop trigger if exists ... on t= fails when
  =t= is gone;
- functions other components define, which the root =create.sql= already
  orders (=ores_utility_*= in column defaults, =ores_iam_current_tenant_id_fn=
  in policies);
- hand files that name a table when they are created: SQL-language function
  bodies (13 files), views and =alter= scripts.

* RLS

The 14 policy files hold 251 policies. By keyword, which is rough because a
comment can mention a party: about 147 are plain tenant isolation, and 129 of
those are on a table whose model exists but does not set
=:rls_tenant_isolation:=. The rest add system-tenant visibility, party
isolation or per-command policies, which codegen also has flags for.

A policy calls =ores_iam_current_tenant_id_fn=, defined in =iam=. Components
created before =iam= (=dq=, =workspace=, =refdata=, =trading=) cannot carry an
inline policy, which is why none of their models sets the flag and why
=rls/rls_create.sql= runs last. Codegen taking these over needs a late,
per-component policy file, not the inline output.

* After this task

- The fifteen dead =refdata_*_notify_trigger.sql= files and the broken
  =populate/fpml/fpml_coding_schemes_populate.sql= (21 calls to
  =metadata.upsert_dq_coding_schemes=, a schema and function that no longer
  exist) are deleted.
- The five real drop gaps are wired. Running =drop/drop.sql= against a full
  database completes with no error; it removes every =ores_= table and leaves
  115 functions, recorded as a capture.
- =WIRE_001= covers every =.sql= file under =create=, =drop= and =populate=,
  reached from the tree roots and from the bootstrap scripts beside them. Six
  files are named exceptions in =validation_ignore.txt=, each with what runs
  it.
- CI runs the parser with =--strict=, and it reports no warning.
- Both drop templates write the generated marker, and all 537 generated drop
  files are regenerated with it. A regeneration of =ores.sql.schema= for every
  component changes nothing else.
- The origin census is committed as
  =projects/ores.codegen/scripts/census_sql_origin.py=. At the end of the task
  it reports 1119 codegen, 148 other generator and 517 hand-written of 1784
  files.

* Scripts

** =render_all.py=

#+begin_src python
import sys
from pathlib import Path
repo = Path.cwd()
sys.path.insert(0, str(repo / "projects/ores.codegen/scripts"))
import check_component_drift as d
root = Path(sys.argv[1]); root.mkdir(parents=True, exist_ok=True)
comps = d._catalogue_components()
d._seed_clang_format(root)
rc = d._render_components(comps, "ores", root, whole_address=True)
print("components", len(comps), "rc", rc)
#+end_src

** =classify.py=

#+begin_src python
import re, sys, csv
from pathlib import Path
repo=Path.cwd(); sweep=Path(sys.argv[1])
gen=set(p.relative_to(sweep) for p in (sweep/"projects/ores.sql").rglob("*.sql"))
marker=re.compile(r"GENERATED FILE|DO NOT EDIT|AUTO-GENERATED|Generated by|generated by", re.I)
rows=[]
for tree in ("create","drop","populate"):
    for p in sorted((repo/"projects/ores.sql"/tree).rglob("*.sql")):
        rel=p.relative_to(repo)
        head=p.read_text(errors="replace")[:3000]
        if rel in gen: kind="codegen"
        elif marker.search(head): kind="other-generator"
        else: kind="hand"
        comp=rel.parts[3] if len(rel.parts)>4 else "(root)"
        rows.append((tree,comp,str(rel),kind,bool(marker.search(head))))
w=csv.writer(open(sys.argv[2],"w")); w.writerow(["tree","component","path","kind","has_marker"]); w.writerows(rows)
from collections import Counter
c=Counter((r[0],r[3]) for r in rows)
for k in sorted(c): print(k, c[k])
print("total", len(rows))
missing=[g for g in gen if not (repo/g).exists()]
print("rendered but absent from repo:", len(missing))
for m in sorted(missing)[:10]: print("  ", m)
#+end_src

** =reasons.py=

#+begin_src python
import csv,sys,re,glob
from collections import Counter,defaultdict
rows=list(csv.DictReader(open(sys.argv[1])))
# tables that some model claims
modeled=set()
for f in glob.glob("projects/*/modeling/*.org")+glob.glob("projects/*/*/modeling/*.org"):
    for m in re.finditer(r"^:tablename:\s*(\S+)",open(f,errors="replace").read(),re.M): modeled.add(m.group(1))
uncatalogued={"database","geo","mq","rls","seed","utility"}
def strip(t): return re.sub(r"--[^\n]*|/\*.*?\*/","",t,flags=re.S).lower()
def classify(r):
    path=r["path"]; n=path.rsplit("/",1)[1]; comp=r["component"]; t=strip(open(path).read())
    tables=re.findall(r"create\s+(?:unlogged\s+)?table\s+(?:if\s+not\s+exists\s+)?\"?(\w+)",t)
    if r["kind"]=="other-generator": 
        if comp=="iam": return "G1 iam service bundle (service template)"
        if "_artefact_" in n or comp=="refdata": return "G2 FpML coded-scheme tables and artefacts (fpml generator)"
        return "G3 other generator"
    if n in("create.sql",) or n==f"{comp}_create.sql" or (comp=="rls" and n=="rls_create.sql"): return "A aggregate or root script"
    if comp in uncatalogued and comp!="rls": return f"B component with no codegen catalogue ({comp})"
    if n.endswith("_rls_policies_create.sql"): return "C RLS policy file"
    if "notify_trigger" in n: return "D notify trigger" + (" (table modelled)" if any(x in modeled for x in re.findall(r"on\s+\"?(\w+_tbl)",t)) else " (table not modelled)")
    if tables:
        if any(x in modeled for x in tables): return "E1 table whose model exists"
        if "_artefact_" in n: return "E2 DQ artefact staging table, no model"
        return "E3 table, no model"
    if re.search(r"create\s+(or\s+replace\s+)?(function|procedure)",t): return "F procedural: functions, procedures, trigger guards"
    if re.search(r"view",t): return "H view"
    return "I alter or other"
c=Counter(); ex=defaultdict(list)
for r in rows:
    if r["tree"]=="create" and r["kind"]!="codegen":
        k=classify(r); c[k]+=1; ex[k].append(r["path"].split("/")[-1])
for k in sorted(c): print(f"{c[k]:4}  {k}\n        e.g. {', '.join(ex[k][:4])}")
print("total", sum(c.values()))
w=csv.writer(open(sys.argv[2],"w")); w.writerow(["path","class"])
for r in rows:
    if r["tree"]=="create" and r["kind"]!="codegen": w.writerow([r["path"],classify(r)])
#+end_src

** =reach.py=

#+begin_src python
import re,sys
from pathlib import Path
base=Path("projects/ores.sql")
ir=re.compile(r"^\s*\\ir\s+(\S+)",re.M)
def reach(root,seen):
    root=root.resolve()
    if root in seen or not root.exists(): return
    seen.add(root)
    for m in ir.finditer(root.read_text(errors="replace")):
        reach((root.parent/m.group(1)),seen)
# every .sql file outside create/drop/populate that includes others is a possible root
roots=[p for p in base.rglob("*.sql") if not any(part in("create","drop","populate") for part in p.relative_to(base).parts[:1])]
seen=set()
for r in roots: reach(r,seen)
for r in (base/"create/create.sql",base/"drop/drop.sql",base/"populate/populate.sql"): reach(r,seen)
print("roots outside the three trees:", ", ".join(sorted(str(r.relative_to(base)) for r in roots if ir.search(r.read_text(errors='replace')))))
for tree in("create","drop","populate"):
    dead=sorted(p.relative_to(base) for p in (base/tree).rglob("*.sql") if p.resolve() not in seen)
    print(f"{tree}: {len(dead)} unreachable")
    for d in dead: print("   ",d)
#+end_src

** =order.py=

#+begin_src python
import re
from pathlib import Path
ir=re.compile(r"^\s*\\ir\s+(\S+)",re.M)
order=[]
def walk(p):
    p=p.resolve(); order.append(p)
    for m in ir.finditer(p.read_text(errors="replace")): walk(p.parent/m.group(1))
walk(Path("projects/ores.sql/create/create.sql"))
created={}
for i,p in enumerate(order):
    for t in re.findall(r"create\s+(?:unlogged\s+)?table\s+(?:if\s+not\s+exists\s+)?\"?(ores_\w+_tbl)",p.read_text(errors="replace"),re.I):
        created.setdefault(t,i)
late=[]
for i,p in enumerate(order):
    txt=p.read_text(errors="replace")
    mine=re.findall(r"create\s+(?:unlogged\s+)?table\s+(?:if\s+not\s+exists\s+)?\"?(ores_\w+_tbl)",txt,re.I)
    if not mine: continue
    for parent in set(re.findall(r"from\s+\"?(ores_\w+_tbl)",txt,re.I))-set(mine):
        if parent in created and created[parent]>i:
            late.append((p.name,parent))
print("files in create order:",len(order),"tables:",len(created))
print("tables whose soft-FK parent is created later:",len(late))
for a,b in late[:12]: print("  ",a,"->",b)
#+end_src
