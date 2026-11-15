-- Every surface publish: its subject, the checks the raise recorded, and how it ended.
select p.short_code party, s.as_of, s.version, s.calibration_version, s.converged,
       round(s.max_abs_error::numeric, 6) max_abs_error, s.pillars_beyond_tolerance,
       s.violation_count, s.published_by, s.published_at, s.superseded_at, s.superseded_as_of
from ores_marketdata_fx_vol_surface_publishes_tbl s
join ores_refdata_parties_tbl p on p.id = s.party_id and p.valid_to > now()
where s.valid_to > now() order by s.as_of;

-- The history of each publish row: the raise, the publication and the supersession.
select s.request_id, s.version, s.change_reason_code, s.published_at, s.superseded_at
from ores_marketdata_fx_vol_surface_publishes_tbl s
order by s.request_id, s.version;

-- The notices the publishes sent, and where each points.
select n.kind_code, n.raised_at, n.link_route
from ores_inbox_notifications_tbl n
where n.kind_code = 'marketdata.surface_published' and n.valid_to > now()
order by n.raised_at;
