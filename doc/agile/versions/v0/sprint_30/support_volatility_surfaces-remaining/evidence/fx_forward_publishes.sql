-- Every FX forward set, its party, its tenors and spots, and whether it is
-- published. A refused raise leaves no row here.
select p.short_code party, f.pair_code, f.as_of, f.published_by, f.published_at,
       (select count(*) from ores_marketdata_fx_forward_publish_points_tbl x
         where x.publish_id = f.id and x.valid_to > now()) tenors,
       (select count(*) from ores_marketdata_fx_forward_publish_spots_tbl x
         where x.publish_id = f.id and x.valid_to > now()) spots
from ores_marketdata_fx_forward_publishes_tbl f
join ores_refdata_parties_tbl p on p.id = f.party_id and p.valid_to > now()
where f.valid_to > now() order by f.as_of;

-- The spot the EUR/USD set read, against the latest one the same series holds now.
select s.observed_at set_observed_at, s.value set_value,
       (select max(k.ts) from ores_marketdata_ticks_tbl k where k.series_id = s.series_id) latest_tick_at,
       (select k.value from ores_marketdata_ticks_tbl k where k.series_id = s.series_id
         order by k.ts desc limit 1) latest_value
from ores_marketdata_fx_forward_publish_spots_tbl s
where s.valid_to > now() and s.pair_code = 'EUR/USD';
