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

-- =============================================================================
-- Which quotes of a party's curve configuration the market answers
-- =============================================================================

-- A curve configuration bootstraps each curve from quotes, and a quote names a
-- market datum in ORE's spelling of it. A datum's identity is its oresmd
-- address, so a quote is answered when the market holds the series that datum
-- belongs to. The two spellings name one datum: the key's tokens are the
-- datum's fields in schema order, and the address is the series those fields
-- that are identity keep -- the term is the point, so it is the one field a
-- series does not carry.
--
--   IR_SWAP/RATE/GBP/0D/1D/1M
--     -> oresmd://ir/GBP?type=series&instrument=ir_swap&quote=rate
--                &fwd_start=0D&tenor=1D
--
-- The codec in ores.marketdata writes the address from the datum, and
-- projects/ores.marketdata/api/tests/datum_market_reference_tests.cpp pins the
-- pair, so the two spellings are checked against each other rather than
-- assumed to agree.
--
-- Only the interest-rate swap rate form is read, because it is the only form
-- a curve configuration holds: every quote of every configuration a provision
-- publishes is one, measured at 420 quotes on 2026-11-21. A quote of another
-- form is reported as one this check cannot answer rather than passed over,
-- because a check that stays silent about what it does not read is a check
-- that reports the wrong answer.
create or replace function ores_marketdata_curve_pillar_series_uri_fn(
    p_quote_text text
) returns text as $$
declare
    v_parts text[];
    v_count integer;
    v_ccy text;
    v_index_name text;
    v_fwd_start text;
    v_tenor text;
begin
    if p_quote_text is null or p_quote_text = '' then
        return null;
    end if;

    v_parts := string_to_array(p_quote_text, '/');
    v_count := coalesce(cardinality(v_parts), 0);

    -- IR_SWAP/RATE/ccy[/index]/start/tenor/term, the form ORE's own
    -- parseMarketDatum reads and the codec writes.
    if v_count < 2
       or upper(v_parts[1]) <> 'IR_SWAP'
       or upper(v_parts[2]) <> 'RATE' then
        return null;
    end if;

    if v_count = 6 then
        v_ccy := v_parts[3];
        v_fwd_start := v_parts[4];
        v_tenor := v_parts[5];
    elsif v_count = 7 then
        v_ccy := v_parts[3];
        v_index_name := v_parts[4];
        v_fwd_start := v_parts[5];
        v_tenor := v_parts[6];
    else
        return null;
    end if;

    return 'oresmd://ir/' || v_ccy ||
           '?type=series&instrument=ir_swap&quote=rate' ||
           coalesce('&index_name=' || v_index_name, '') ||
           '&fwd_start=' || v_fwd_start ||
           '&tenor=' || v_tenor;
end;
$$ language plpgsql immutable;

comment on function ores_marketdata_curve_pillar_series_uri_fn(text) is
'The series address an interest-rate swap rate quote names, or null when the
 quote is of no form this check reads. The quote''s term is the point within
 the series, so the address carries the currency, the forward start and the
 index tenor, and not the term.';

-- =============================================================================
-- The quotes of a party's curve configurations that no series answers
-- =============================================================================

-- One row per quote the market cannot answer, so a caller reads the whole gap
-- in one query rather than a count of it. A quote is answered by a series of
-- the party or by one of the system tenant, which is the rule the export
-- writes a run's market data by: a published fixing or quote in the system
-- tenant reaches every party's run.
--
-- A party that holds no curve configuration has no quotes and so no gaps; the
-- function says nothing rather than reporting a gap the caller would read as a
-- failure of the market.
create or replace function ores_marketdata_curve_quote_coverage_fn(
    p_tenant_id uuid,
    p_party_id  uuid
)
returns table (
    curve_configuration_id   uuid,
    curve_configuration_name text,
    curve_id                 text,
    quote_text               text,
    reason                   text
) as $$
begin
    return query
    with quotes as (
        select cc.id   as configuration_id,
               cc.name as configuration_name,
               d.curve_id,
               q.quote_text
        from ores_refdata_curve_configurations_tbl cc
        join ores_refdata_curve_definitions_tbl d
          on d.curve_configuration_id = cc.id
         and d.tenant_id = cc.tenant_id
         and d.party_id = cc.party_id
         and d.valid_to = ores_utility_infinity_timestamp_fn()
        join ores_refdata_curve_quotes_tbl q
          on q.curve_definition_id = d.id
         and q.tenant_id = d.tenant_id
         and q.party_id = d.party_id
         and q.valid_to = ores_utility_infinity_timestamp_fn()
        where cc.tenant_id = p_tenant_id
          and cc.party_id = p_party_id
          and cc.valid_to = ores_utility_infinity_timestamp_fn()
    ),
    addressed as (
        select v.*,
               ores_marketdata_curve_pillar_series_uri_fn(v.quote_text) as series_uri
        from quotes v
    )
    select a.configuration_id,
           a.configuration_name,
           a.curve_id,
           a.quote_text,
           case
               when a.series_uri is null then
                   'The quote is not an interest-rate swap rate, the only form this check reads.'
               else
                   'The market holds no series for this quote, at the party or in the system tenant.'
           end
    from addressed a
    where a.series_uri is null
       or not exists (
           select 1
           from ores_marketdata_market_series_tbl s
           where s.valid_to = ores_utility_infinity_timestamp_fn()
             and s.oresmd_uri = a.series_uri
             and ((s.tenant_id = p_tenant_id and s.party_id = p_party_id)
                  or s.tenant_id = ores_utility_system_tenant_id_fn()));
end;
$$ language plpgsql stable;

comment on function ores_marketdata_curve_quote_coverage_fn(uuid, uuid) is
'Lists the quotes of a party''s curve configurations that no market series
 answers, one row per quote with the reason. A series of the party or of the
 system tenant answers a quote, as it does for a run''s exported market data.';
