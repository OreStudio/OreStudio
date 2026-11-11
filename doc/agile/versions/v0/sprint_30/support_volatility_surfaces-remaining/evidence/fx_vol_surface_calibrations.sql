-- The candidate each calibration wrote: its fit and its checks.
select p.short_code party, c.as_of, c.version, c.smile_model, round(c.rho::numeric, 4) rho,
       round(c.eta::numeric, 4) eta, round(c.gamma::numeric, 4) gamma, c.converged,
       round(c.max_abs_error::numeric, 5) max_abs_error, c.pillars_beyond_tolerance,
       jsonb_array_length(c.pillar_fits) pillars, jsonb_array_length(c.violations) violations,
       jsonb_array_length(c.missing_pillars) missing
from ores_marketdata_fx_vol_surface_calibrations_tbl c
join ores_refdata_parties_tbl p on p.id = c.party_id and p.valid_to > now()
where c.valid_to > now() order by c.as_of;

-- The grid written to the output series, and the series' declared axes.
select s.oresmd_uri, s.derivation_kind, count(distinct o.oresmd_uri) grid_points,
       (select string_agg(a.axis_field, ',' order by a.sequence)
          from ores_marketdata_series_axes_tbl a where a.series_id = s.id) axes
from ores_marketdata_market_series_tbl s
join ores_marketdata_market_observations_tbl o on o.series_id = s.id and o.valid_to > now()
where s.valid_to > now() and s.oresmd_uri like '%instrument=fx_vol_surface%'
group by s.id, s.oresmd_uri, s.derivation_kind;

-- The lineage of one grid point: the blueprint version, the source series and their weights.
select l.oresmd_uri, l.derivation_config_version, l.source_series_ids, l.source_weights
from ores_marketdata_observation_lineages_tbl l
join ores_marketdata_market_series_tbl s on s.id = l.series_id and s.valid_to > now()
where l.valid_to > now() and s.oresmd_uri like '%instrument=fx_vol_surface%'
order by l.oresmd_uri limit 3;

-- A sample of the grid: volatility by expiry and strike.
select o.oresmd_uri, round(o.value::numeric, 5) volatility
from ores_marketdata_market_observations_tbl o
join ores_marketdata_market_series_tbl s on s.id = o.series_id and s.valid_to > now()
where o.valid_to > now() and s.oresmd_uri like '%instrument=fx_vol_surface%'
order by o.oresmd_uri;
