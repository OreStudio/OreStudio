-- For each Acme party: which of the 20 funding currencies it received IR ticks in,
-- and which of their USD spots, in the last 15 minutes, through which providers.
with funding(ccy) as (
    values ('AUD'), ('CAD'), ('CHF'), ('CLP'), ('COP'), ('CZK'), ('DKK'), ('EUR'), ('GBP'),
           ('HKD'), ('INR'), ('JPY'), ('KRW'), ('MXN'), ('NZD'), ('SEK'), ('SGD'), ('TRY'),
           ('USD'), ('ZAR')),
received as (
    select distinct k.party_id, s.provider_code, s.oresmd_uri
    from ores_marketdata_ticks_tbl k
    join ores_iam_tenants_tbl t on t.id = k.tenant_id and t.valid_to > now() and t.name ilike 'acme%'
    join ores_marketdata_market_series_tbl s on s.id = k.series_id and s.valid_to > now()
    where k.ts > now() - interval '15 minutes'),
per_party as (
    select p.short_code,
        string_agg(distinct r.provider_code, ' ') providers,
        count(distinct f.ccy) filter (
            where r.oresmd_uri like 'oresmd://ir/' || f.ccy || '?%') ir_ccys,
        count(distinct f.ccy) filter (
            where f.ccy <> 'USD' and (r.oresmd_uri like 'oresmd://fx/' || f.ccy || '?%ccy=USD'
                                   or r.oresmd_uri like 'oresmd://fx/USD?%ccy=' || f.ccy)) usd_spots
    from received r
    join ores_refdata_parties_tbl p on p.id = r.party_id and p.valid_to > now()
    cross join funding f
    group by p.short_code)
select * from per_party order by short_code;

select p.short_code, s.provider_code, count(*) subscriptions
from ores_marketdata_subscriptions_tbl s
join ores_iam_tenants_tbl t on t.id = s.tenant_id and t.valid_to > now() and t.name ilike 'acme%'
join ores_refdata_parties_tbl p on p.id = s.party_id and p.valid_to > now()
where s.valid_to > now() group by 1, 2 order by 1, 2;
