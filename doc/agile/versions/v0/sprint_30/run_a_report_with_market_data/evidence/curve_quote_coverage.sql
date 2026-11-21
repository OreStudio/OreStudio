-- The quotes of every ACME party's curve configuration that no series answers.
-- Run with: compass sql -f doc/agile/versions/v0/sprint_30/run_a_report_with_market_data/evidence/curve_quote_coverage.sql
select p.short_code as party, cc.name as configuration,
       (select count(*)
          from ores_refdata_curve_definitions_tbl d
          join ores_refdata_curve_quotes_tbl q
            on q.curve_definition_id = d.id
           and q.tenant_id = d.tenant_id
           and q.valid_to > now()
         where d.curve_configuration_id = cc.id
           and d.valid_to > now()) as quotes,
       (select count(*)
          from ores_marketdata_curve_quote_coverage_fn(cc.tenant_id, cc.party_id) g
         where g.curve_configuration_id = cc.id) as gaps
from ores_refdata_curve_configurations_tbl cc
join ores_refdata_parties_tbl p on p.id = cc.party_id and p.valid_to > now()
where cc.valid_to > now()
order by p.short_code;
