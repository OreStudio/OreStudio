/* -*- sql-product: postgres; tab-width: 4; indent-tabs-mode: nil -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation; either version 3 of the License, or (at your option) any later
 * version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */

/**
 * The Overnight Rates Catalogue
 *
 * Each publisher's overnight rate series is a vendor instrument of that
 * publisher, in its own notation, mapped to the fixing item of its index. The
 * item is the oresmd address that the index's built-in convention states, so a
 * public fixing and a synthetic feed of one index name the same item. The terms
 * of each publisher are in doc/knowledge/domain/synthetic_calibration_sources.org.
 * The ECB and the Bank of Canada are seeded by the official rates catalogue.
 *
 * Each publisher's adapter reads its series through a session and a feed, and the
 * feed's scope names the one item it fetches, so the demand never asks the feed
 * for the items of the publisher's other feeds. The system party subscribes to
 * each publisher and routes to its feed.
 */

\echo '--- Overnight Rates Providers ---'
insert into ores_marketdata_providers_tbl (
    tenant_id, code, version, name, kind, description, display_order,
    terms_url, terms_credit, allows_storage, credential_ref,
    modified_by, performed_by, change_reason_code, change_commentary
)
values
    (ores_utility_system_tenant_id_fn(), 'nyfed', 0, 'Federal Reserve Bank of New York', 'reference',
     'The reference rates of the US dollar, among them SOFR, published each business day at about 08:00 New York time.',
     30, 'https://www.newyorkfed.org/privacy/termsofuse',
     'Source: Federal Reserve Bank of New York', true, '',
     current_user, current_user, 'system.initial_load', 'Initial population of the overnight rates providers'),
    (ores_utility_system_tenant_id_fn(), 'boe', 0, 'Bank of England', 'reference',
     'The statistical database of the Bank of England, among them SONIA, published each London business day.',
     31, 'https://www.bankofengland.co.uk/legal',
     'SONIA and/or SONIA Compounded Index data licensed under the Open Government Licence v3.0 and copyright the Governor and Company of the Bank of England', true, '',
     current_user, current_user, 'system.initial_load', 'Initial population of the overnight rates providers'),
    (ores_utility_system_tenant_id_fn(), 'boj', 0, 'Bank of Japan', 'reference',
     'The time-series statistics of the Bank of Japan, among them the uncollateralised overnight call rate (TONA), published each Tokyo business day.',
     32, 'https://www.boj.or.jp/en/about/copyright.htm',
     'Source: Bank of Japan', true, '',
     current_user, current_user, 'system.initial_load', 'Initial population of the overnight rates providers'),
    (ores_utility_system_tenant_id_fn(), 'norgesbank', 0, 'Norges Bank', 'reference',
     'The open data of Norges Bank, among them NOWA, published each Norwegian business day.',
     33, 'https://www.norges-bank.no/en/about/terms-of-use/',
     'Source: Norges Bank', true, '',
     current_user, current_user, 'system.initial_load', 'Initial population of the overnight rates providers'),
    (ores_utility_system_tenant_id_fn(), 'riksbank', 0, 'Sveriges Riksbank', 'reference',
     'The Swedish krona short-term rate, SWESTR, published by the Riksbank each Swedish business day.',
     34, 'https://www.riksbank.se/en-gb/statistics/swestr/conditions-for-use-and-re-publishing/',
     'Source: Sveriges Riksbank', true, 'env:ORES_MARKETDATA_RIKSBANK_API_KEY',
     current_user, current_user, 'system.initial_load', 'Initial population of the overnight rates providers'),
    (ores_utility_system_tenant_id_fn(), 'rba', 0, 'Reserve Bank of Australia', 'reference',
     'The statistical tables of the Reserve Bank of Australia, among them AONIA, the interbank overnight cash rate, published each Australian business day.',
     35, 'https://www.rba.gov.au/copyright/',
     'Source: RBA', true, '',
     current_user, current_user, 'system.initial_load', 'Initial population of the overnight rates providers'),
    (ores_utility_system_tenant_id_fn(), 'bcb', 0, 'Banco Central do Brasil', 'reference',
     'The time series of the Banco Central do Brasil, among them the annualised CDI that B3 computes, published each Brazilian business day.',
     36, 'https://www.b3.com.br/pt_br/market-data-e-indices/servicos-de-dados/market-data/consultas/mercado-de-derivativos/indicadores/indicadores-financeiros/',
     'Source: B3, through the Banco Central do Brasil', true, '',
     current_user, current_user, 'system.initial_load', 'Initial population of the overnight rates providers'),
    (ores_utility_system_tenant_id_fn(), 'banxico', 0, 'Banco de Mexico', 'reference',
     'The economic information system of Banco de Mexico, among them the TIIE de Fondeo, the overnight funding rate, published each Mexican business day.',
     37, 'https://www.banxico.org.mx/footer-en/terms-and-conditions-banco-mexico-s-website.html',
     'Source: Banco de Mexico, https://www.banxico.org.mx', true, 'env:ORES_MARKETDATA_BANXICO_API_KEY',
     current_user, current_user, 'system.initial_load', 'Initial population of the overnight rates providers'),
    (ores_utility_system_tenant_id_fn(), 'bcch', 0, 'Banco Central de Chile', 'reference',
     'The statistical database of the Banco Central de Chile, among them the average overnight interbank rate on which the Camara index is built, published each Chilean business day. Its account is ORES_MARKETDATA_BCCH_USER and the password it names.',
     38, 'https://si3.bcentral.cl/estadisticas/Principal1/Web_Services/terminos_condiciones_en.html',
     'Source: Banco Central de Chile', true, 'env:ORES_MARKETDATA_BCCH_PASSWORD',
     current_user, current_user, 'system.initial_load', 'Initial population of the overnight rates providers'),
    (ores_utility_system_tenant_id_fn(), 'cnb', 0, 'Czech National Bank', 'reference',
     'CZEONIA, the overnight rate of the Czech crown, published each Czech business day in one file a year. The files need no key.',
     39, 'https://www.cnb.cz/en/privacy-statement-and-disclaimer/disclaimer-copyright/',
     'Source: CNB', true, '',
     current_user, current_user, 'system.initial_load', 'Initial population of the overnight rates providers')
on conflict (tenant_id, code)
where valid_to = ores_utility_infinity_timestamp_fn()
do nothing;

\echo '--- Overnight Rates Vendor Instruments and Mappings ---'
create temporary table t_overnight_inventory (
    provider_code text, id_scheme text, external_id text, field text, oresmd_uri text
) on commit drop;

insert into t_overnight_inventory (provider_code, id_scheme, external_id, field, oresmd_uri) values
    ('nyfed', 'nyfed_rate_type', 'SOFR', 'percentRate', 'oresmd://ir/USD?type=fixing&index=ibor&name=SOFR'),
    ('ecb', 'ecb_sdmx', 'EST.B.EU000A2X2A25.WT', 'OBS_VALUE', 'oresmd://ir/EUR?type=fixing&index=ibor&name=ESTER'),
    ('boe', 'boe_iadb', 'IUDSOIA', 'value', 'oresmd://ir/GBP?type=fixing&index=ibor&name=SONIA'),
    ('boc', 'boc_valet', 'AVG.INTWO', 'v', 'oresmd://ir/CAD?type=fixing&index=ibor&name=CORRA'),
    ('boj', 'boj_stat', 'FM01.STRDCLUCON', 'VALUES', 'oresmd://ir/JPY?type=fixing&index=ibor&name=TONAR'),
    ('norgesbank', 'nb_sdmx', 'SHORT_RATES.B.NOWA.ON.R', 'OBS_VALUE', 'oresmd://ir/NOK?type=fixing&index=ibor&name=NOWA'),
    ('riksbank', 'riksbank_swestr', 'SWESTR', 'rate', 'oresmd://ir/SEK?type=fixing&index=ibor&name=SWESTR'),
    ('rba', 'rba_table', 'f1.FIRMMCRID', 'value', 'oresmd://ir/AUD?type=fixing&index=ibor&name=AONIA'),
    ('bcb', 'bcb_sgs', '4389', 'valor', 'oresmd://ir/BRL?type=fixing&index=ibor&name=CDI'),
    ('banxico', 'banxico_sie', 'SF331451', 'dato', 'oresmd://ir/MXN?type=fixing&index=ibor&name=FTIIE'),
    ('bcch', 'bcch_bde', 'F022.TIB.TIP.D001.NO.Z.D', 'value', 'oresmd://ir/CLP?type=fixing&index=ibor&name=CAMARA'),
    ('cnb', 'cnb_czeonia', 'CZEONIA', 'Rate', 'oresmd://ir/CZK?type=fixing&index=ibor&name=CZEONIA');

insert into ores_marketdata_market_data_vendor_instruments_tbl (
    id, tenant_id, version, provider_code, id_scheme, external_id, field, minted, deviation_note,
    modified_by, performed_by, change_reason_code, change_commentary
)
select md5(t.provider_code || '|' || t.id_scheme || '|' || t.external_id || '|' || t.field)::uuid,
       ores_utility_system_tenant_id_fn(), 0, t.provider_code, t.id_scheme, t.external_id, t.field,
       false, '', current_user, current_user, 'system.initial_load',
       'Initial population of the overnight rates vendor instruments'
from t_overnight_inventory t
where not exists (
    select 1 from ores_marketdata_market_data_vendor_instruments_tbl v
    where v.tenant_id = ores_utility_system_tenant_id_fn()
      and v.provider_code = t.provider_code
      and v.id_scheme = t.id_scheme
      and v.external_id = t.external_id
      and v.field = t.field
      and v.valid_to = ores_utility_infinity_timestamp_fn()
);

insert into ores_marketdata_market_data_mappings_tbl (
    id, tenant_id, version, vendor_instrument_id, oresmd_uri, effective_from,
    modified_by, performed_by, change_reason_code, change_commentary
)
select gen_random_uuid(), ores_utility_system_tenant_id_fn(), 0, v.id, t.oresmd_uri,
       timestamptz '2026-01-01 00:00:00+00', current_user, current_user, 'system.initial_load',
       'Initial population of the overnight rates mappings'
from t_overnight_inventory t
join ores_marketdata_market_data_vendor_instruments_tbl v
  on v.tenant_id = ores_utility_system_tenant_id_fn()
 and v.provider_code = t.provider_code
 and v.id_scheme = t.id_scheme
 and v.external_id = t.external_id
 and v.field = t.field
 and v.valid_to = ores_utility_infinity_timestamp_fn()
where not exists (
    select 1 from ores_marketdata_market_data_mappings_tbl m
    where m.tenant_id = ores_utility_system_tenant_id_fn()
      and m.vendor_instrument_id = v.id
      and m.valid_to = ores_utility_infinity_timestamp_fn()
);

\echo '--- Overnight Rates Sessions, Feeds and Scopes ---'
insert into ores_marketdata_sessions_tbl (
    code, tenant_id, version, name, supplier_code, adapter_code, environment,
    endpoint, credential_ref, settings, enabled, description,
    modified_by, performed_by, change_reason_code, change_commentary
)
values
    ('nyfed-markets', ores_utility_system_tenant_id_fn(), 0, 'New York Fed markets API',
     'nyfed', 'nyfed', 'production', 'https://markets.newyorkfed.org', '',
     '{"url":"https://markets.newyorkfed.org","backfill_days":400,"poll_minutes":60}', true,
     'The session that reads the reference rates of the New York Fed.',
     current_user, current_user, 'system.initial_load', 'Initial population of the overnight rates sessions'),
    ('ecb-data-portal', ores_utility_system_tenant_id_fn(), 0, 'ECB Data Portal',
     'ecb', 'ecb_portal', 'production', 'https://data-api.ecb.europa.eu', '',
     '{"url":"https://data-api.ecb.europa.eu","backfill_days":400,"poll_minutes":60}', true,
     'The session that reads the series of the ECB Data Portal.',
     current_user, current_user, 'system.initial_load', 'Initial population of the overnight rates sessions'),
    ('boe-database', ores_utility_system_tenant_id_fn(), 0, 'Bank of England database',
     'boe', 'boe_iadb', 'production', 'https://www.bankofengland.co.uk', '',
     '{"url":"https://www.bankofengland.co.uk","backfill_days":400,"poll_minutes":60}', true,
     'The session that reads the series of the Bank of England statistical database.',
     current_user, current_user, 'system.initial_load', 'Initial population of the overnight rates sessions'),
    ('boc-valet', ores_utility_system_tenant_id_fn(), 0, 'Bank of Canada Valet API',
     'boc', 'boc_valet', 'production', 'https://www.bankofcanada.ca', '',
     '{"url":"https://www.bankofcanada.ca","backfill_days":400,"poll_minutes":60}', true,
     'The session that reads the series of the Bank of Canada Valet API.',
     current_user, current_user, 'system.initial_load', 'Initial population of the overnight rates sessions'),
    ('boj-stat', ores_utility_system_tenant_id_fn(), 0, 'Bank of Japan time-series statistics',
     'boj', 'boj_stat', 'production', 'https://www.stat-search.boj.or.jp', '',
     '{"url":"https://www.stat-search.boj.or.jp","backfill_days":400,"poll_minutes":60}', true,
     'The session that reads the series of the Bank of Japan time-series API.',
     current_user, current_user, 'system.initial_load', 'Initial population of the overnight rates sessions'),
    ('norgesbank-data', ores_utility_system_tenant_id_fn(), 0, 'Norges Bank open data',
     'norgesbank', 'nb_sdmx', 'production', 'https://data.norges-bank.no', '',
     '{"url":"https://data.norges-bank.no","backfill_days":400,"poll_minutes":60}', true,
     'The session that reads the series of the Norges Bank SDMX API.',
     current_user, current_user, 'system.initial_load', 'Initial population of the overnight rates sessions'),
    ('riksbank-api', ores_utility_system_tenant_id_fn(), 0, 'Riksbank API',
     'riksbank', 'riksbank_swestr', 'production', 'https://api.riksbank.se', '',
     '{"url":"https://api.riksbank.se","backfill_days":400,"poll_minutes":60}', true,
     'The session that reads the series of the Riksbank SWESTR API.',
     current_user, current_user, 'system.initial_load', 'Initial population of the overnight rates sessions'),
    ('rba-statistics', ores_utility_system_tenant_id_fn(), 0, 'RBA statistics',
     'rba', 'rba_table', 'production', 'https://www.rba.gov.au', '',
     '{"url":"https://www.rba.gov.au","backfill_days":400,"poll_minutes":60}', true,
     'The session that reads the series of the RBA statistical tables.',
     current_user, current_user, 'system.initial_load', 'Initial population of the overnight rates sessions'),
    ('bcb-sgs', ores_utility_system_tenant_id_fn(), 0, 'BCB SGS',
     'bcb', 'bcb_sgs', 'production', 'https://api.bcb.gov.br', '',
     '{"url":"https://api.bcb.gov.br","backfill_days":400,"poll_minutes":60}', true,
     'The session that reads the series of the Banco Central do Brasil SGS API.',
     current_user, current_user, 'system.initial_load', 'Initial population of the overnight rates sessions'),
    ('banxico-sie', ores_utility_system_tenant_id_fn(), 0, 'Banxico SIE',
     'banxico', 'banxico_sie', 'production', 'https://www.banxico.org.mx', '',
     '{"url":"https://www.banxico.org.mx","backfill_days":400,"poll_minutes":60}', true,
     'The session that reads the series of the Banco de Mexico SIE API.',
     current_user, current_user, 'system.initial_load', 'Initial population of the overnight rates sessions'),
    ('bcch-bde', ores_utility_system_tenant_id_fn(), 0, 'BCCh BDE',
     'bcch', 'bcch_bde', 'production', 'https://si3.bcentral.cl', '',
     '{"url":"https://si3.bcentral.cl","backfill_days":400,"poll_minutes":60}', true,
     'The session that reads the series of the Banco Central de Chile BDE web service.',
     current_user, current_user, 'system.initial_load', 'Initial population of the overnight rates sessions'),
    ('cnb-czeonia', ores_utility_system_tenant_id_fn(), 0, 'CNB CZEONIA',
     'cnb', 'cnb_czeonia', 'production', 'https://www.cnb.cz', '',
     '{"url":"https://www.cnb.cz","backfill_days":400,"poll_minutes":60}', true,
     'The session that reads the yearly CZEONIA files of the Czech National Bank.',
     current_user, current_user, 'system.initial_load', 'Initial population of the overnight rates sessions')
on conflict (tenant_id, code)
where valid_to = ores_utility_infinity_timestamp_fn()
do nothing;

insert into ores_marketdata_feeds_tbl (
    id, tenant_id, version, name, publisher_code, session_code, service, provider_key,
    is_default, description, modified_by, performed_by, change_reason_code, change_commentary
)
values
    ('491b9101-f1e7-5020-a960-9799f3a78c2a', ores_utility_system_tenant_id_fn(), 0,
     'nyfed-sofr', 'nyfed', 'nyfed-markets', 'end_of_day', 'SOFR', true,
     'SOFR as the New York Fed publishes it, read by the official rates adapter.',
     current_user, current_user, 'system.initial_load', 'Initial population of the overnight rates feeds'),
    ('52a9af16-42ac-5fa6-85f1-dfe87ab35d84', ores_utility_system_tenant_id_fn(), 0,
     'ecb-estr', 'ecb', 'ecb-data-portal', 'end_of_day', 'EST.B.EU000A2X2A25.WT', false,
     '€STR as the ECB publishes it, read from the ECB Data Portal by the official rates adapter.',
     current_user, current_user, 'system.initial_load', 'Initial population of the overnight rates feeds'),
    ('93de0868-9ab8-5a1e-a16a-66941e6b0acc', ores_utility_system_tenant_id_fn(), 0,
     'boe-sonia', 'boe', 'boe-database', 'end_of_day', 'IUDSOIA', true,
     'SONIA as the Bank of England publishes it, read from its statistical database by the official rates adapter.',
     current_user, current_user, 'system.initial_load', 'Initial population of the overnight rates feeds'),
    ('ef613368-bf4b-5367-ae33-b5df192549ac', ores_utility_system_tenant_id_fn(), 0,
     'boc-corra', 'boc', 'boc-valet', 'end_of_day', 'AVG.INTWO', false,
     'CORRA as the Bank of Canada publishes it, read from its Valet API by the official rates adapter.',
     current_user, current_user, 'system.initial_load', 'Initial population of the overnight rates feeds'),
    ('25fb75e6-8f2f-5fe1-9672-9a018b25d061', ores_utility_system_tenant_id_fn(), 0,
     'boj-tona', 'boj', 'boj-stat', 'end_of_day', 'FM01.STRDCLUCON', true,
     'The uncollateralised overnight call rate (TONA) as the Bank of Japan publishes it, read from its time-series API by the official rates adapter.',
     current_user, current_user, 'system.initial_load', 'Initial population of the overnight rates feeds'),
    ('4cb8961a-a60d-54bf-ac82-6a43752d42cd', ores_utility_system_tenant_id_fn(), 0,
     'norgesbank-nowa', 'norgesbank', 'norgesbank-data', 'end_of_day', 'SHORT_RATES.B.NOWA.ON.R', true,
     'NOWA as Norges Bank publishes it, read from its SDMX API by the official rates adapter.',
     current_user, current_user, 'system.initial_load', 'Initial population of the overnight rates feeds'),
    ('0e59f2e7-48d8-5ba2-8433-2ea7726f912c', ores_utility_system_tenant_id_fn(), 0,
     'riksbank-swestr', 'riksbank', 'riksbank-api', 'end_of_day', 'SWESTR', true,
     'SWESTR as the Riksbank publishes it, read from its SWESTR API by the official rates adapter.',
     current_user, current_user, 'system.initial_load', 'Initial population of the overnight rates feeds'),
    ('bff157c2-bc49-5ef0-8cf6-745fe868f43c', ores_utility_system_tenant_id_fn(), 0,
     'rba-aonia', 'rba', 'rba-statistics', 'end_of_day', 'f1.FIRMMCRID', true,
     'AONIA as the RBA publishes it, read from its statistical table F1 by the official rates adapter.',
     current_user, current_user, 'system.initial_load', 'Initial population of the overnight rates feeds'),
    ('2f903f0d-259c-59a8-9867-c632dd1af018', ores_utility_system_tenant_id_fn(), 0,
     'bcb-cdi', 'bcb', 'bcb-sgs', 'end_of_day', '4389', true,
     'The annualised CDI as the Banco Central do Brasil publishes it, read from its SGS API by the official rates adapter.',
     current_user, current_user, 'system.initial_load', 'Initial population of the overnight rates feeds'),
    ('5d31da1a-16cc-52b8-9d6e-8b3c77c6e5ab', ores_utility_system_tenant_id_fn(), 0,
     'banxico-tiie-fondeo', 'banxico', 'banxico-sie', 'end_of_day', 'SF331451', true,
     'The TIIE de Fondeo as Banco de Mexico publishes it, read from its SIE API by the official rates adapter.',
     current_user, current_user, 'system.initial_load', 'Initial population of the overnight rates feeds'),
    ('c2418b97-6603-5306-a00c-226b84671227', ores_utility_system_tenant_id_fn(), 0,
     'bcch-camara', 'bcch', 'bcch-bde', 'end_of_day', 'F022.TIB.TIP.D001.NO.Z.D', true,
     'The average overnight interbank rate as the Banco Central de Chile publishes it, read from its BDE web service by the official rates adapter.',
     current_user, current_user, 'system.initial_load', 'Initial population of the overnight rates feeds'),
    ('8ae278b7-d2f0-4e89-85a6-9913aaef894a', ores_utility_system_tenant_id_fn(), 0,
     'cnb-czeonia', 'cnb', 'cnb-czeonia', 'end_of_day', 'CZEONIA', true,
     'CZEONIA as the Czech National Bank publishes it, read from its yearly files by the official rates adapter.',
     current_user, current_user, 'system.initial_load', 'Initial population of the overnight rates feeds')
on conflict (tenant_id, id)
where valid_to = ores_utility_infinity_timestamp_fn()
do nothing;

insert into ores_marketdata_feed_scopes_tbl (
    id, tenant_id, version, feed_id, asset_class, item, vendor_instrument_id,
    modified_by, performed_by, change_reason_code, change_commentary
)
select gen_random_uuid(), ores_utility_system_tenant_id_fn(), 0, f.id, 'interest_rates',
       m.oresmd_uri, v.id,
       current_user, current_user, 'system.initial_load',
       'Initial population of the overnight rates scopes'
from (values ('nyfed-sofr', 'nyfed', 'nyfed_rate_type', 'SOFR'),
             ('ecb-estr', 'ecb', 'ecb_sdmx', 'EST.B.EU000A2X2A25.WT'),
             ('boe-sonia', 'boe', 'boe_iadb', 'IUDSOIA'),
             ('boc-corra', 'boc', 'boc_valet', 'AVG.INTWO'),
             ('boj-tona', 'boj', 'boj_stat', 'FM01.STRDCLUCON'),
             ('norgesbank-nowa', 'norgesbank', 'nb_sdmx', 'SHORT_RATES.B.NOWA.ON.R'),
             ('riksbank-swestr', 'riksbank', 'riksbank_swestr', 'SWESTR'),
             ('rba-aonia', 'rba', 'rba_table', 'f1.FIRMMCRID'),
             ('bcb-cdi', 'bcb', 'bcb_sgs', '4389'),
             ('banxico-tiie-fondeo', 'banxico', 'banxico_sie', 'SF331451'),
             ('bcch-camara', 'bcch', 'bcch_bde', 'F022.TIB.TIP.D001.NO.Z.D'),
             ('cnb-czeonia', 'cnb', 'cnb_czeonia', 'CZEONIA'))
     as s(feed, provider, scheme, external_id)
join ores_marketdata_feeds_tbl f
  on f.tenant_id = ores_utility_system_tenant_id_fn()
 and f.name = s.feed
 and f.valid_to = ores_utility_infinity_timestamp_fn()
join ores_marketdata_market_data_vendor_instruments_tbl v
  on v.tenant_id = ores_utility_system_tenant_id_fn()
 and v.provider_code = s.provider
 and v.id_scheme = s.scheme
 and v.external_id = s.external_id
 and v.valid_to = ores_utility_infinity_timestamp_fn()
join ores_marketdata_market_data_mappings_tbl m
  on m.vendor_instrument_id = v.id
 and m.tenant_id = ores_utility_system_tenant_id_fn()
 and m.valid_to = ores_utility_infinity_timestamp_fn()
where not exists (
    select 1 from ores_marketdata_feed_scopes_tbl x
    where x.tenant_id = ores_utility_system_tenant_id_fn()
      and x.feed_id = f.id
      and x.item = m.oresmd_uri
      and x.valid_to = ores_utility_infinity_timestamp_fn()
);

\echo '--- Overnight Rates Subscriptions and Routes of the System Party ---'
do $$
declare
    v_party uuid := ores_iam_account_parties_system_party_id_fn(ores_utility_system_tenant_id_fn());
begin
    if v_party is null then
        raise notice 'No system party yet: the overnight rates subscriptions wait for it';
        return;
    end if;

    insert into ores_marketdata_subscriptions_tbl (
        id, tenant_id, version, party_id, provider_code, display, non_display, derived,
        modified_by, performed_by, change_reason_code, change_commentary
    ) values (
        '42432343-3299-5a96-b6bb-3906468b7a94', ores_utility_system_tenant_id_fn(), 0, v_party, 'nyfed',
        true, true, true, current_user, current_user, 'system.initial_load',
        'The system party takes SOFR of the New York Fed for the calibration'
    ) on conflict do nothing;
    insert into ores_marketdata_routes_tbl (
        id, tenant_id, version, party_id, publisher_code, asset_class, feed_id,
        modified_by, performed_by, change_reason_code, change_commentary
    ) values (
        '2f1f4cde-543d-5d41-97ff-0ba1531bb130', ores_utility_system_tenant_id_fn(), 0, v_party, 'nyfed', 'interest_rates',
        '491b9101-f1e7-5020-a960-9799f3a78c2a',
        current_user, current_user, 'system.initial_load',
        'The system party reads SOFR from the New York Fed'
    ) on conflict do nothing;

    -- The system party subscribes to the ECB in the official rates catalogue.
    insert into ores_marketdata_routes_tbl (
        id, tenant_id, version, party_id, publisher_code, asset_class, feed_id,
        modified_by, performed_by, change_reason_code, change_commentary
    ) values (
        '943b73a2-7a70-5137-9f0c-ace0cbe3e645', ores_utility_system_tenant_id_fn(), 0, v_party, 'ecb', 'interest_rates',
        '52a9af16-42ac-5fa6-85f1-dfe87ab35d84',
        current_user, current_user, 'system.initial_load',
        'The system party reads €STR from the ECB Data Portal'
    ) on conflict do nothing;

    insert into ores_marketdata_subscriptions_tbl (
        id, tenant_id, version, party_id, provider_code, display, non_display, derived,
        modified_by, performed_by, change_reason_code, change_commentary
    ) values (
        '06aa6000-964d-5e8d-9c93-2af82eb9714e', ores_utility_system_tenant_id_fn(), 0, v_party, 'boe',
        true, true, true, current_user, current_user, 'system.initial_load',
        'The system party takes SONIA of the Bank of England for the calibration'
    ) on conflict do nothing;
    insert into ores_marketdata_routes_tbl (
        id, tenant_id, version, party_id, publisher_code, asset_class, feed_id,
        modified_by, performed_by, change_reason_code, change_commentary
    ) values (
        'efb38371-2756-583c-9f34-33f15ff47dc4', ores_utility_system_tenant_id_fn(), 0, v_party, 'boe', 'interest_rates',
        '93de0868-9ab8-5a1e-a16a-66941e6b0acc',
        current_user, current_user, 'system.initial_load',
        'The system party reads SONIA from the Bank of England'
    ) on conflict do nothing;

    -- The system party subscribes to the Bank of Canada in the official rates catalogue.
    insert into ores_marketdata_routes_tbl (
        id, tenant_id, version, party_id, publisher_code, asset_class, feed_id,
        modified_by, performed_by, change_reason_code, change_commentary
    ) values (
        '222a1169-9b03-55ab-a08e-ef4ff5eb80ec', ores_utility_system_tenant_id_fn(), 0, v_party, 'boc', 'interest_rates',
        'ef613368-bf4b-5367-ae33-b5df192549ac',
        current_user, current_user, 'system.initial_load',
        'The system party reads CORRA from the Bank of Canada'
    ) on conflict do nothing;

    insert into ores_marketdata_subscriptions_tbl (
        id, tenant_id, version, party_id, provider_code, display, non_display, derived,
        modified_by, performed_by, change_reason_code, change_commentary
    ) values (
        '9dd0357d-2bb9-55fd-b2d1-cd9bebbbbc5d', ores_utility_system_tenant_id_fn(), 0, v_party, 'boj',
        true, true, true, current_user, current_user, 'system.initial_load',
        'The system party takes the call rate of the Bank of Japan for the calibration'
    ) on conflict do nothing;
    insert into ores_marketdata_routes_tbl (
        id, tenant_id, version, party_id, publisher_code, asset_class, feed_id,
        modified_by, performed_by, change_reason_code, change_commentary
    ) values (
        'e691c083-89af-54cb-a33c-bf28913ebfb9', ores_utility_system_tenant_id_fn(), 0, v_party, 'boj', 'interest_rates',
        '25fb75e6-8f2f-5fe1-9672-9a018b25d061',
        current_user, current_user, 'system.initial_load',
        'The system party reads the call rate from the Bank of Japan'
    ) on conflict do nothing;

    insert into ores_marketdata_subscriptions_tbl (
        id, tenant_id, version, party_id, provider_code, display, non_display, derived,
        modified_by, performed_by, change_reason_code, change_commentary
    ) values (
        'c6d4da2d-4c15-5a4a-aae3-f10f43ff2872', ores_utility_system_tenant_id_fn(), 0, v_party, 'norgesbank',
        true, true, true, current_user, current_user, 'system.initial_load',
        'The system party takes NOWA of Norges Bank for the calibration'
    ) on conflict do nothing;
    insert into ores_marketdata_routes_tbl (
        id, tenant_id, version, party_id, publisher_code, asset_class, feed_id,
        modified_by, performed_by, change_reason_code, change_commentary
    ) values (
        '129fb7ef-862b-570e-8518-e92e0a829d52', ores_utility_system_tenant_id_fn(), 0, v_party, 'norgesbank', 'interest_rates',
        '4cb8961a-a60d-54bf-ac82-6a43752d42cd',
        current_user, current_user, 'system.initial_load',
        'The system party reads NOWA from Norges Bank'
    ) on conflict do nothing;

    insert into ores_marketdata_subscriptions_tbl (
        id, tenant_id, version, party_id, provider_code, display, non_display, derived,
        modified_by, performed_by, change_reason_code, change_commentary
    ) values (
        '07e59440-5204-529d-9e8b-b1424d8daafb', ores_utility_system_tenant_id_fn(), 0, v_party, 'riksbank',
        true, true, true, current_user, current_user, 'system.initial_load',
        'The system party takes SWESTR of Sveriges Riksbank for the calibration'
    ) on conflict do nothing;
    insert into ores_marketdata_routes_tbl (
        id, tenant_id, version, party_id, publisher_code, asset_class, feed_id,
        modified_by, performed_by, change_reason_code, change_commentary
    ) values (
        'c02e2f49-1d33-5c10-96e0-30aa7afe0f3d', ores_utility_system_tenant_id_fn(), 0, v_party, 'riksbank', 'interest_rates',
        '0e59f2e7-48d8-5ba2-8433-2ea7726f912c',
        current_user, current_user, 'system.initial_load',
        'The system party reads SWESTR from Sveriges Riksbank'
    ) on conflict do nothing;

    insert into ores_marketdata_subscriptions_tbl (
        id, tenant_id, version, party_id, provider_code, display, non_display, derived,
        modified_by, performed_by, change_reason_code, change_commentary
    ) values (
        '150c069d-baee-5006-9f5a-8ee72bfe775a', ores_utility_system_tenant_id_fn(), 0, v_party, 'rba',
        true, true, true, current_user, current_user, 'system.initial_load',
        'The system party takes AONIA of Reserve Bank of Australia for the calibration'
    ) on conflict do nothing;
    insert into ores_marketdata_routes_tbl (
        id, tenant_id, version, party_id, publisher_code, asset_class, feed_id,
        modified_by, performed_by, change_reason_code, change_commentary
    ) values (
        '92519b55-ba52-593e-b221-fbd19dc62972', ores_utility_system_tenant_id_fn(), 0, v_party, 'rba', 'interest_rates',
        'bff157c2-bc49-5ef0-8cf6-745fe868f43c',
        current_user, current_user, 'system.initial_load',
        'The system party reads AONIA from Reserve Bank of Australia'
    ) on conflict do nothing;

    insert into ores_marketdata_subscriptions_tbl (
        id, tenant_id, version, party_id, provider_code, display, non_display, derived,
        modified_by, performed_by, change_reason_code, change_commentary
    ) values (
        'ff89d5fb-738e-5fab-8b3d-7c68af56091c', ores_utility_system_tenant_id_fn(), 0, v_party, 'bcb',
        true, true, true, current_user, current_user, 'system.initial_load',
        'The system party takes the CDI of Banco Central do Brasil for the calibration'
    ) on conflict do nothing;
    insert into ores_marketdata_routes_tbl (
        id, tenant_id, version, party_id, publisher_code, asset_class, feed_id,
        modified_by, performed_by, change_reason_code, change_commentary
    ) values (
        'a9299cfc-2a37-5c09-8372-9b1dc2da2649', ores_utility_system_tenant_id_fn(), 0, v_party, 'bcb', 'interest_rates',
        '2f903f0d-259c-59a8-9867-c632dd1af018',
        current_user, current_user, 'system.initial_load',
        'The system party reads the CDI from Banco Central do Brasil'
    ) on conflict do nothing;

    insert into ores_marketdata_subscriptions_tbl (
        id, tenant_id, version, party_id, provider_code, display, non_display, derived,
        modified_by, performed_by, change_reason_code, change_commentary
    ) values (
        '2832ae4e-6ae7-582c-a4ee-84e31693f5d1', ores_utility_system_tenant_id_fn(), 0, v_party, 'banxico',
        true, true, true, current_user, current_user, 'system.initial_load',
        'The system party takes the TIIE de Fondeo of Banco de Mexico for the calibration'
    ) on conflict do nothing;
    insert into ores_marketdata_routes_tbl (
        id, tenant_id, version, party_id, publisher_code, asset_class, feed_id,
        modified_by, performed_by, change_reason_code, change_commentary
    ) values (
        '659924a0-4aed-5a92-b9bc-fdaf0c183409', ores_utility_system_tenant_id_fn(), 0, v_party, 'banxico', 'interest_rates',
        '5d31da1a-16cc-52b8-9d6e-8b3c77c6e5ab',
        current_user, current_user, 'system.initial_load',
        'The system party reads the TIIE de Fondeo from Banco de Mexico'
    ) on conflict do nothing;

    insert into ores_marketdata_subscriptions_tbl (
        id, tenant_id, version, party_id, provider_code, display, non_display, derived,
        modified_by, performed_by, change_reason_code, change_commentary
    ) values (
        'a3544d97-a532-52c7-835c-b225f16bf9b8', ores_utility_system_tenant_id_fn(), 0, v_party, 'bcch',
        true, true, true, current_user, current_user, 'system.initial_load',
        'The system party takes the overnight interbank rate of Banco Central de Chile for the calibration'
    ) on conflict do nothing;
    insert into ores_marketdata_routes_tbl (
        id, tenant_id, version, party_id, publisher_code, asset_class, feed_id,
        modified_by, performed_by, change_reason_code, change_commentary
    ) values (
        '77a91fa3-1218-569e-957d-9c25091d813c', ores_utility_system_tenant_id_fn(), 0, v_party, 'bcch', 'interest_rates',
        'c2418b97-6603-5306-a00c-226b84671227',
        current_user, current_user, 'system.initial_load',
        'The system party reads the overnight interbank rate from Banco Central de Chile'
    ) on conflict do nothing;

    insert into ores_marketdata_subscriptions_tbl (
        id, tenant_id, version, party_id, provider_code, display, non_display, derived,
        modified_by, performed_by, change_reason_code, change_commentary
    ) values (
        '9172e5e2-33bb-4fca-aa1b-9b7141b0a7ea', ores_utility_system_tenant_id_fn(), 0, v_party, 'cnb',
        true, true, true, current_user, current_user, 'system.initial_load',
        'The system party takes CZEONIA of the Czech National Bank for the calibration'
    ) on conflict do nothing;
    insert into ores_marketdata_routes_tbl (
        id, tenant_id, version, party_id, publisher_code, asset_class, feed_id,
        modified_by, performed_by, change_reason_code, change_commentary
    ) values (
        '05e4c166-ee61-47db-a731-a7c5e0643560', ores_utility_system_tenant_id_fn(), 0, v_party, 'cnb', 'interest_rates',
        '8ae278b7-d2f0-4e89-85a6-9913aaef894a',
        current_user, current_user, 'system.initial_load',
        'The system party reads CZEONIA from the Czech National Bank'
    ) on conflict do nothing;
end $$;
