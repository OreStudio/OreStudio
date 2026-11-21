-- Who can supply an item: the catalogue's mappings, by provider, asset class and
-- form. A row here is one vendor instrument that maps to one oresmd item, so the
-- counts are what a party could demand from each provider.
-- Run with: compass sql -f doc/agile/versions/v0/sprint_30/run_a_report_with_market_data/evidence/market_data_suppliers.sql
select v.provider_code as supplier,
       split_part(m.oresmd_uri, '/', 3) as asset_class,
       case when m.oresmd_uri like '%type=fixing%' then 'fixing' else 'series' end as form,
       count(*) as items
from ores_marketdata_market_data_mappings_tbl m
join ores_marketdata_market_data_vendor_instruments_tbl v
  on v.id = m.vendor_instrument_id
where m.valid_to > now()
group by 1, 2, 3
order by 1, 2, 3;
