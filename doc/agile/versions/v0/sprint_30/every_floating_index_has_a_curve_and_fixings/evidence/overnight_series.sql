-- Every overnight fixing series (an ibor address with no tenor), by currency and name.
select substring(s.oresmd_uri from 'ir/([A-Z]{3})') as currency,
       substring(s.oresmd_uri from 'name=([A-Z0-9]+)') as name,
       count(distinct s.id) as series,
       count(f.*) as fixings
from ores_marketdata_market_series_tbl s
left join ores_marketdata_market_fixings_tbl f
  on f.series_id = s.id and f.valid_to = ores_utility_infinity_timestamp_fn()
where s.valid_to = ores_utility_infinity_timestamp_fn()
  and s.oresmd_uri like 'oresmd://ir/%type=fixing&index=ibor&name=%'
  and s.oresmd_uri not like '%&tenor=%'
group by 1, 2
order by 1, 2;
-- The three indices this task names once.
select count(*) as second_spellings
from ores_marketdata_market_series_tbl s
where s.valid_to = ores_utility_infinity_timestamp_fn()
  and s.oresmd_uri in ('oresmd://ir/EUR?type=fixing&index=ibor&name=ESTER',
                       'oresmd://ir/JPY?type=fixing&index=ibor&name=TONAR',
                       'oresmd://ir/MXN?type=fixing&index=ibor&name=TIIE');
