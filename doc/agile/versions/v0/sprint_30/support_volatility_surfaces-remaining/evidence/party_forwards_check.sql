select p.short_code as party,
       count(*) filter (where f.pair_code like '%USD%') as usd_pairs,
       count(*) filter (where f.pair_code not like '%USD%') as crosses,
       count(distinct f.pair_code) as distinct_pairs,
       count(*) as sets
from ores_marketdata_fx_forward_publishes_tbl f
join ores_refdata_parties_tbl p
  on p.id = f.party_id and p.valid_to = ores_utility_infinity_timestamp_fn()
where f.valid_to = ores_utility_infinity_timestamp_fn()
  and f.published_at is not null
group by p.short_code
order by p.short_code;

select count(*) as conventions,
       count(*) filter (where pair_code not like '%USD%') as cross_conventions
from ores_refdata_currency_pair_conventions_tbl
where valid_to = ores_utility_infinity_timestamp_fn()
  and tenant_id = (select id from ores_iam_tenants_tbl where code like 'acme%' and valid_to = ores_utility_infinity_timestamp_fn() limit 1);

select r.auto_approval_code, r.state_code, count(*)
from ores_marketdata_fx_forward_publishes_tbl f
join ores_inbox_approval_requests_tbl r
  on r.id = f.request_id and r.valid_to = ores_utility_infinity_timestamp_fn()
where f.valid_to = ores_utility_infinity_timestamp_fn()
group by 1, 2;
