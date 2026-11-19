-- The fixing addresses a generated population names, and whether each one answers.
--
-- Run with: compass sql -f <this file>
-- The tenant is the acme_corporation tenant of the environment's provision.

\set tenant '''bcc2a567-ca9d-4310-a172-65c9f886f758'''
select set_config('app.current_tenant_id', :tenant, false);

create temp table all_trades as
select id from ores_trading_trades_tbl
where tenant_id = :tenant::uuid and valid_to = ores_utility_infinity_timestamp_fn();

create temp table refs as
select distinct oresmd_uri
from ores_trading_market_references_fn(:tenant::uuid, (select array_agg(id) from all_trades));

-- How many trades, how many distinct addresses, how many of them are fixings.
select (select count(*) from all_trades) as trades,
       (select count(*) from refs) as addresses,
       (select count(*) from refs where oresmd_uri like '%type=fixing%') as fixing_addresses;

-- The fixing addresses no fixing row answers, by asset class.
select split_part(split_part(r.oresmd_uri, '://', 2), '/', 1) as klass, count(*) as empty
from refs r
where r.oresmd_uri like '%type=fixing%'
  and not exists (select 1
                  from ores_marketdata_market_series_tbl s
                  join ores_marketdata_market_fixings_tbl f
                    on f.series_id = s.id and f.valid_to = ores_utility_infinity_timestamp_fn()
                  where s.oresmd_uri = r.oresmd_uri
                    and s.valid_to = ores_utility_infinity_timestamp_fn())
group by 1 order by 1;

-- The empty interest rate addresses themselves: a series requirement is not a fixing.
select r.oresmd_uri
from refs r
where r.oresmd_uri like 'oresmd://ir/%'
  and not exists (select 1
                  from ores_marketdata_market_series_tbl s
                  join ores_marketdata_market_fixings_tbl f
                    on f.series_id = s.id and f.valid_to = ores_utility_infinity_timestamp_fn()
                  where s.oresmd_uri = r.oresmd_uri
                    and s.valid_to = ores_utility_infinity_timestamp_fn())
order by 1;

-- Every fixing series the tenant holds, and where it lives.
select split_part(split_part(s.oresmd_uri, '://', 2), '/', 1) as klass,
       s.tenant_id, s.provider_code, count(distinct s.id) as series, count(f.id) as fixings
from ores_marketdata_market_series_tbl s
left join ores_marketdata_market_fixings_tbl f
  on f.series_id = s.id and f.valid_to = ores_utility_infinity_timestamp_fn()
where s.valid_to = ores_utility_infinity_timestamp_fn()
  and s.oresmd_uri like '%type=fixing%'
group by 1, 2, 3 order by 1;
