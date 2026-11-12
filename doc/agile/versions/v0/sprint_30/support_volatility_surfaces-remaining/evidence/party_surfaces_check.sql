-- Each party's surface blueprints, and how many it has published.
select p.short_code as party,
       count(distinct b.id) as blueprints,
       count(distinct s.blueprint_id) filter (where s.published_at is not null) as published
from ores_refdata_fx_vol_surface_blueprints_tbl b
join ores_refdata_parties_tbl p
  on p.id = b.party_id and p.valid_to = ores_utility_infinity_timestamp_fn()
left join ores_marketdata_fx_vol_surface_publishes_tbl s
  on s.blueprint_id = b.id and s.valid_to = ores_utility_infinity_timestamp_fn()
where b.valid_to = ores_utility_infinity_timestamp_fn()
group by p.short_code
order by p.short_code;

-- Each published surface's lineage: the forward set its calibration read, and whose that set is.
select p.short_code as party, b.name as surface, f.pair_code as forward_pair,
       f.party_id = s.party_id as same_party, f.published_at is not null as forward_published,
       r.auto_approval_code as rule
from ores_marketdata_fx_vol_surface_publishes_tbl s
join ores_refdata_fx_vol_surface_blueprints_tbl b
  on b.id = s.blueprint_id and b.valid_to = ores_utility_infinity_timestamp_fn()
join ores_refdata_parties_tbl p
  on p.id = s.party_id and p.valid_to = ores_utility_infinity_timestamp_fn()
join ores_marketdata_fx_vol_surface_calibrations_tbl c
  on c.id = s.calibration_id and c.valid_to = ores_utility_infinity_timestamp_fn()
left join ores_marketdata_fx_forward_publishes_tbl f
  on f.id = c.forward_publish_id and f.valid_to = ores_utility_infinity_timestamp_fn()
left join ores_inbox_approval_requests_tbl r
  on r.id = s.request_id and r.valid_to = ores_utility_infinity_timestamp_fn()
where s.valid_to = ores_utility_infinity_timestamp_fn()
  and s.published_at is not null
order by p.short_code, b.name;
