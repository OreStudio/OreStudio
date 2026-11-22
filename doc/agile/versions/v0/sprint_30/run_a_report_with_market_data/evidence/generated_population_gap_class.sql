-- The addresses the generated ACCOUK population names, per asset class: how many
-- the party's own series answer, how many a *fixing* the system tenant holds
-- answers, and how many no series holds at all.
-- Run with: compass sql -f doc/agile/versions/v0/sprint_30/run_a_report_with_market_data/evidence/generated_population_gap_class.sql
with p as (select id, tenant_id from ores_refdata_parties_tbl
           where short_code = 'ACCOUK' and valid_to > now() limit 1),
t as (select tr.id from ores_trading_trades_tbl tr, p
      where tr.valid_to > now()),
r as (select distinct m.oresmd_uri as u
      from p, ores_trading_market_references_fn((select tenant_id from p),
               array(select id from t), '2026-11-21T23:59:59Z') m),
a as (
  select r.u, split_part(r.u, '/', 3) as class,
    exists (select 1 from ores_marketdata_market_series_tbl s, p
            where s.tenant_id = p.tenant_id and s.party_id = p.id
              and s.oresmd_uri = r.u and s.valid_to > now()) as party_series,
    (position('?type=fixing&' in r.u) > 0
     and exists (select 1 from ores_marketdata_market_series_tbl s
                 where s.tenant_id = ores_utility_system_tenant_id_fn()
                   and s.oresmd_uri = r.u and s.valid_to > now())) as system_fixing,
    exists (select 1 from ores_marketdata_market_series_tbl s, p
            where s.tenant_id = p.tenant_id
              and s.oresmd_uri = r.u and s.valid_to > now()) as any_party_series
  from r
)
select class,
       count(*) as required,
       count(*) filter (where party_series) as party_series,
       count(*) filter (where system_fixing) as system_fixing,
       count(*) filter (where not party_series and not system_fixing) as unanswered,
       count(*) filter (where not party_series and not system_fixing
                        and any_party_series) as held_by_another_party,
       count(*) filter (where not any_party_series) as absent_entirely
from a
group by 1
order by 5 desc, 1;
