-- Who may read what: each party's routes, which name the source it uses for one
-- publisher and one asset class, and the subscriptions that grant it the rights.
-- The demo's tenant is found by code so the query runs on any checkout.
-- Run with: compass sql -f doc/agile/versions/v0/sprint_30/run_a_report_with_market_data/evidence/party_market_data_rights.sql
with seller as (select id, code from ores_iam_tenants_tbl
                where valid_to > now() and code = 'acme_corporation')
select p.short_code as party,
       'route' as grant,
       r.publisher_code as publisher,
       r.asset_class as asset_class,
       r.publisher_code as detail
from ores_marketdata_routes_tbl r
join seller t on t.id = r.tenant_id
join ores_refdata_parties_tbl p on p.id = r.party_id and p.valid_to > now()
where r.valid_to > now()
union all
select p.short_code,
       'subscription',
       s.provider_code,
       case when s.derived then 'derived' else 'no derived right' end,
       concat_ws(',',
                 case when s.display then 'display' end,
                 case when s.non_display then 'non-display' end,
                 case when s.derived then 'derived' end)
from ores_marketdata_subscriptions_tbl s
join seller t on t.id = s.tenant_id
join ores_refdata_parties_tbl p on p.id = s.party_id and p.valid_to > now()
where s.valid_to > now()
order by 1, 2, 3;
