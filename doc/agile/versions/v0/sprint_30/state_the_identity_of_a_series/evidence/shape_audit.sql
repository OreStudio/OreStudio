-- Every current series held against the shape its family states, as identity_shape_book::check does.
with s as (
    select * from ores_marketdata_market_series_tbl where valid_to = '9999-12-31 23:59:59+00'
), i as (
    select s.id, s.tenant_id, s.oresmd_uri, s.series_subclass, r.identity_kind,
           coalesce(r.asset_class, '') as asset_class, coalesce(r."index", '') as idx,
           to_jsonb(r) as j, r.series_id is not null as projected
    from s left join ores_marketdata_market_series_identity_tbl r on r.series_id = s.id
), shapes as (
    select distinct on (code) * from ores_marketdata_series_identity_shapes_tbl
    where valid_to = '9999-12-31 23:59:59+00' order by code, tenant_id
), entries as (
    select distinct on (shape_code, field) * from ores_marketdata_series_identity_shape_entries_tbl
    where valid_to = '9999-12-31 23:59:59+00' order by shape_code, field, tenant_id
), held as (
    select i.*, sh.code from i join shapes sh
        on sh.asset_class = i.asset_class and sh.series_subclass = i.series_subclass
       and sh.index_family = i.idx
), breaks as (
    select h.oresmd_uri, h.code, e.field, e.requirement
    from held h join entries e on e.shape_code = h.code
    where (e.requirement = 'required' and coalesce(h.j->>e.field, '') = '')
       or (e.requirement = 'forbidden' and coalesce(h.j->>e.field, '') <> '')
)
select 'series' as what, count(*)::text as n from i
union all select 'unprojected', count(*)::text from i where not projected
union all select 'held to a shape', count(*)::text from held
union all select 'index series with another subclass', count(*)::text from i
    where identity_kind = 'index' and series_subclass <> 'index_fixing'
union all select 'shape breaks', count(*)::text from breaks
union all (select 'break: ' || code || ' ' || field || ' ' || requirement, count(*)::text from breaks group by 1)
union all (select 'family ' || code, count(*)::text from held group by 1)
order by 1;
