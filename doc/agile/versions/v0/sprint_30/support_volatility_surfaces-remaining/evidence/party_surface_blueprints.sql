-- The derived volatility series each Acme party holds: the ingest writes one when it delivers the
-- party's first quote of an item.
select coalesce(p.short_code, '?') party, s.provider_code, count(*) series
from ores_marketdata_market_series_tbl s
join ores_iam_tenants_tbl t on t.id = s.tenant_id and t.valid_to > now() and t.name ilike 'acme%'
left join ores_refdata_parties_tbl p on p.id = s.party_id and p.valid_to > now()
where s.valid_to > now() and s.oresmd_uri like '%instrument=fx_option%'
group by 1, 2 order by 1, 2;

-- The volatility ticks stored for those series. A tick belongs to its source, not to a party, so
-- every subscribed party reads the same rows.
select count(distinct k.stream_id) streams, count(*) ticks, min(k.ts) earliest, max(k.ts) latest
from ores_marketdata_ticks_tbl k
join ores_marketdata_tick_coords_tbl c on c.stream_id = k.stream_id and c.coord = k.coord
where c.oresmd_uri like '%instrument=fx_option%';

-- Each party's surface blueprints, their pillars, and whether every source reads the party's own series.
select p.short_code party, b.name,
       (select count(*) from ores_refdata_fx_vol_surface_pillars_tbl x
         where x.blueprint_id = b.id and x.tenant_id = b.tenant_id and x.valid_to > now()) pillars,
       (select count(*) from ores_refdata_fx_vol_surface_pillar_sources_tbl src
          join ores_refdata_fx_vol_surface_pillars_tbl x on x.id = src.pillar_id and x.tenant_id = src.tenant_id and x.valid_to > now()
          join ores_marketdata_market_series_tbl s on s.id = src.market_series_id and s.valid_to > now()
         where x.blueprint_id = b.id and src.valid_to > now() and s.party_id = b.party_id) sources_on_own_series
from ores_refdata_fx_vol_surface_blueprints_tbl b
join ores_iam_tenants_tbl t on t.id = b.tenant_id and t.valid_to > now()
join ores_refdata_parties_tbl p on p.id = b.party_id and p.valid_to > now()
where b.valid_to > now() order by t.name, 1;
