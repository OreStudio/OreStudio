-- The interest rate fixing addresses no fixing answers, by trade type, party and source column.
select set_config('app.current_tenant_id',
                  (select id::text from ores_iam_tenants_tbl
                   where code = 'acme_corporation' and valid_to = ores_utility_infinity_timestamp_fn()),
                  false);

create temp table all_trades as
select t.id, t.trade_type, p.short_code as party, p.business_center_code as centre
from ores_trading_trades_tbl t
join ores_refdata_parties_tbl p
  on p.id = t.party_id and p.valid_to = ores_utility_infinity_timestamp_fn()
where t.tenant_id = current_setting('app.current_tenant_id')::uuid
  and t.valid_to = ores_utility_infinity_timestamp_fn();

create temp table refs as
select r.trade_id, r.oresmd_uri, r.source
from ores_trading_market_references_fn(current_setting('app.current_tenant_id')::uuid,
                                       (select array_agg(id) from all_trades)) r;

create temp table empty_ir as
select distinct r.oresmd_uri
from refs r
where r.oresmd_uri like 'oresmd://ir/%type=fixing%'
  and not exists (select 1
                  from ores_marketdata_market_series_tbl s
                  join ores_marketdata_market_fixings_tbl f
                    on f.series_id = s.id and f.valid_to = ores_utility_infinity_timestamp_fn()
                  where s.oresmd_uri = r.oresmd_uri
                    and s.valid_to = ores_utility_infinity_timestamp_fn());

select (select count(*) from all_trades) as trades,
       (select count(distinct oresmd_uri) from refs where oresmd_uri like 'oresmd://ir/%type=fixing%') as ir_fixing_addresses,
       (select count(*) from empty_ir) as empty_ir_fixing_addresses;

select e.oresmd_uri, t.trade_type, t.party, t.centre, r.source, count(distinct t.id) as trades
from empty_ir e
join refs r on r.oresmd_uri = e.oresmd_uri
join all_trades t on t.id = r.trade_id
group by 1, 2, 3, 4, 5
order by 1, 2, 3;

select t.trade_type, count(distinct t.id) as trades_on_an_empty_ir_fixing
from empty_ir e
join refs r on r.oresmd_uri = e.oresmd_uri
join all_trades t on t.id = r.trade_id
group by 1 order by 2 desc;

select party, centre, count(*) as trades from all_trades group by 1, 2 order by 1;
