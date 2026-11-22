-- Where the stored market data sits: series per asset class and form in the
-- tenant that must read them and in the system tenant that keeps the fixings,
-- and the durable fixings per tenant.
-- Run with: compass sql -f doc/agile/versions/v0/sprint_30/run_a_report_with_market_data/evidence/market_data_storage.sql
with seller as (select id, code from ores_iam_tenants_tbl
                where valid_to > now() and code in ('acme_corporation', 'system'))
select t.code as tenant,
       split_part(s.oresmd_uri, '/', 3) as asset_class,
       case when s.oresmd_uri like '%type=fixing%' then 'fixing' else 'series' end as form,
       count(*) as series,
       count(distinct s.party_id) as parties
from ores_marketdata_market_series_tbl s
join seller t on t.id = s.tenant_id
where s.valid_to > now()
group by 1, 2, 3
order by 1, 2, 3;

-- The durable fixings, which are the ones a party reads by entitlement.
with seller as (select id, code from ores_iam_tenants_tbl where valid_to > now())
select t.code as tenant, count(*) as fixings, count(distinct f.party_id) as parties
from ores_marketdata_market_fixings_tbl f
join seller t on t.id = f.tenant_id
where f.valid_to > now()
group by 1
order by 2 desc;
