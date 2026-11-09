-- Each Acme party's published funding curves: how many, in how many currencies,
-- and the rule that closed each approval.
select p.short_code party,
       count(*) published,
       count(distinct c.currency_code) currencies,
       string_agg(distinct coalesce(r.auto_approval_code, 'person'), ' ') closed_by
from ores_marketdata_curve_publishes_tbl cp
join ores_iam_tenants_tbl t on t.id = cp.tenant_id and t.valid_to > now() and t.name ilike 'acme%'
join ores_refdata_parties_tbl p on p.id = cp.party_id and p.valid_to > now()
join ores_refdata_ir_curve_bootstrap_configs_tbl c
  on c.id = cp.bootstrap_config_id and c.tenant_id = cp.tenant_id and c.valid_to > now()
left join ores_inbox_approval_requests_tbl r
  on r.id = cp.request_id and r.valid_to > now()
where cp.valid_to > now() and cp.withdrawn_at is null and cp.superseded_at is null
group by p.short_code order by p.short_code;

-- The raw quotes one published curve read: its lineage names series of the party itself.
select p.short_code party, c.name curve, l.source_series_ids,
       (select string_agg(distinct s.provider_code || ' ' || coalesce(sp.short_code, '?'), ', ')
          from ores_marketdata_market_series_tbl s
          left join ores_refdata_parties_tbl sp on sp.id = s.party_id and sp.valid_to > now()
         where s.valid_to > now()
           and s.id::text in (select jsonb_array_elements_text(l.source_series_ids::jsonb))) read_from
from ores_marketdata_observation_lineages_tbl l
join ores_refdata_ir_curve_bootstrap_configs_tbl c
  on c.id = l.derivation_config_id and c.tenant_id = l.tenant_id and c.valid_to > now()
join ores_refdata_parties_tbl p on p.id = l.party_id and p.valid_to > now()
join ores_iam_tenants_tbl t on t.id = l.tenant_id and t.valid_to > now() and t.name ilike 'acme%'
where l.valid_to > now() and c.name = 'EUR-ESTR'
order by 1 limit 8;
