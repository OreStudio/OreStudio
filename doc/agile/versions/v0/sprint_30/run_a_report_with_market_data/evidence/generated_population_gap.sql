-- The market objects the generated ACCOUK population names and no series
-- answers, per generated trade type and asset class.
-- Run with: compass sql -f doc/agile/versions/v0/sprint_30/run_a_report_with_market_data/evidence/generated_population_gap.sql
with p as (select id, tenant_id from ores_refdata_parties_tbl
           where short_code = 'ACCOUK' and valid_to > now() limit 1),
t as (select tr.id, tr.trade_type from ores_trading_trades_tbl tr, p
      where tr.valid_to > now()),
r as (select m.trade_id, m.oresmd_uri as u
      from p, ores_trading_market_references_fn((select tenant_id from p),
               array(select id from t), '2026-11-21T23:59:59Z') m),
a as (
  select distinct r.u
  from r
  where not (
    exists (select 1 from ores_marketdata_market_series_tbl s, p
            where s.tenant_id = p.tenant_id and s.party_id = p.id
              and s.oresmd_uri = r.u and s.valid_to > now())
    or (position('?type=fixing&' in r.u) > 0
        and exists (select 1 from ores_marketdata_market_series_tbl s
                    where s.tenant_id = ores_utility_system_tenant_id_fn()
                      and s.oresmd_uri = r.u and s.valid_to > now()))
  )
),
n as (select t.trade_type, t.id, r.u, split_part(r.u, '/', 3) as class
      from t join r on r.trade_id = t.id)
select n.trade_type, n.class,
       count(distinct n.u) as missing_addresses,
       count(distinct n.id) as trades_naming_them
from n join a on a.u = n.u
group by 1, 2
order by 1, 3 desc, 2;
