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
-- address, so a quote is answered when the party holds the series that datum
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
-- The reader applies the checks the codec applies to this key, because a key
-- the codec refuses names no datum and a series cannot answer it: the token
-- count, the exact spelling of the instrument and quote tokens (the codec
-- compares them with ==, so a lower-case key is not a key), no empty field, a
-- period where read_ir_swap requires one, a date that exists where it requires
-- a date, and a start and end of the same kind. A key that fails any of these
-- states no address, and the caller reports it as unreadable rather than as a
-- gap the market could close.
--
-- A field value is written as the key spells it, without percent-encoding. A
-- value that needs encoding names no series here, and the quote is reported as
-- a gap rather than accepted, so the check errs toward reporting work.
--
-- Only the interest-rate swap rate form is read, because it is the only form a
-- curve configuration holds: every quote of every configuration a provision
-- publishes is one, measured at 420 quotes on 2026-11-21. A quote of another
-- form is reported as one this check cannot answer rather than passed over,
-- because a check that stays silent about what it does not read is a check
-- that reports the wrong answer.

-- =============================================================================
-- The shape of an ORE key token
-- =============================================================================

-- Whether a token is a date, as datum/value.cpp's is_date reads one: a
-- four-digit year and a real month and day, written with or without dashes.
-- to_date() rolls an impossible day into the next month, so the round trip is
-- what refuses 20261399.
create or replace function ores_marketdata_curve_is_date_fn(
    p_text text
) returns boolean as $$
declare
    v_date date;
begin
    if p_text ~ '^[0-9]{4}-[0-9]{2}-[0-9]{2}$' then
        v_date := to_date(p_text, 'YYYY-MM-DD');
    elsif p_text ~ '^[0-9]{8}$' then
        v_date := to_date(p_text, 'YYYYMMDD');
    else
        return false;
    end if;
    return to_char(v_date,
                   case when p_text like '%-%' then 'YYYY-MM-DD' else 'YYYYMMDD' end) = p_text;
exception
    when others then
        return false;
end;
$$ language plpgsql immutable;

comment on function ores_marketdata_curve_is_date_fn(text) is
'Whether the token is a date the codec reads, which is eight digits or a year,
 month and day with dashes, naming a day that exists.';

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
    v_term text;
begin
    if p_quote_text is null or p_quote_text = '' then
        return null;
    end if;

    v_parts := string_to_array(p_quote_text, '/');
    v_count := coalesce(cardinality(v_parts), 0);

    -- IR_SWAP/RATE/ccy[/index]/start/tenor/term, the form ORE's own
    -- parseMarketDatum reads and the codec writes. The two leading tokens are
    -- compared exactly, as the codec compares them.
    if v_count < 2
       or v_parts[1] <> 'IR_SWAP'
       or v_parts[2] <> 'RATE' then
        return null;
    end if;

    if v_count = 6 then
        v_ccy := v_parts[3];
        v_fwd_start := v_parts[4];
        v_tenor := v_parts[5];
        v_term := v_parts[6];
    elsif v_count = 7 then
        v_ccy := v_parts[3];
        v_index_name := v_parts[4];
        v_fwd_start := v_parts[5];
        v_tenor := v_parts[6];
        v_term := v_parts[7];
    else
        return null;
    end if;

    -- A token that holds nothing names no field.
    if v_ccy is null or v_ccy = ''
       or v_fwd_start is null or v_fwd_start = ''
       or v_index_name = ''
       or v_tenor is null or v_tenor = ''
       or v_term is null or v_term = '' then
        return null;
    end if;

    -- The start is a period or a date, the index tenor is a period, and the
    -- term is of the start's kind: the checks read_ir_swap applies.
    if not (ores_marketdata_curve_is_date_fn(v_fwd_start)
            or v_fwd_start ~ '^([0-9]+[DdWwMmYy])+$') then
        return null;
    end if;
    if v_tenor !~ '^([0-9]+[DdWwMmYy])+$' then
        return null;
    end if;
    if ores_marketdata_curve_is_date_fn(v_fwd_start)
       <> ores_marketdata_curve_is_date_fn(v_term) then
        return null;
    end if;
    if not ores_marketdata_curve_is_date_fn(v_term)
       and v_term !~ '^([0-9]+[DdWwMmYy])+$' then
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
 quote is of no form this check reads or the codec would refuse it. The quote''s
 term is the point within the series, so the address carries the currency, the
 forward start and the index tenor, and not the term.';

-- =============================================================================
-- The quotes of a party's curve configurations that no series answers
-- =============================================================================

-- One row per quote the market cannot answer, so a caller reads the whole gap
-- in one query rather than a count of it. The kind says which work the row
-- asks for: an unanswered quote needs the market to hold the series, and an
-- unreadable one needs this check to learn the quote's form.
--
-- A quote is answered only by a series of the party, which is the rule the
-- market data export writes a run by: a system tenant's fixing series reaches
-- every party's run, but a quote series does not, so a quote the system tenant
-- holds answers nothing for this party.
--
-- A session whose visible party set excludes the party cannot read its
-- configuration, and would return no rows, which a caller would read as "no
-- gaps". That is a false answer, so the function refuses the call instead.
create or replace function ores_marketdata_curve_quote_coverage_fn(
    p_tenant_id uuid,
    p_party_id  uuid
)
returns table (
    curve_configuration_id   uuid,
    curve_configuration_name text,
    curve_id                 text,
    quote_text               text,
    kind                     text,
    reason                   text
) as $$
begin
    if p_party_id is null then
        raise exception 'The check reads one party''s curve configuration, and a null party would return no rows, which reads as no gaps.'
            using errcode = '22023';
    end if;

    if ores_iam_visible_party_ids_fn() is not null
       and not (p_party_id = any(ores_iam_visible_party_ids_fn())) then
        raise exception 'The session cannot see party %, so the quotes of its curve configuration cannot be read and the check would report no gaps for a configuration it cannot see.', p_party_id
            using errcode = '42501';
    end if;

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
           case when a.series_uri is null then 'unreadable' else 'unanswered' end,
           case
               when a.series_uri is null then
                   'The quote is not an interest-rate swap rate key the codec reads, so this check cannot say which series answers it.'
               else
                   'The party holds no series for this quote.'
           end
    from addressed a
    where a.series_uri is null
       or not exists (
           select 1
           from ores_marketdata_market_series_tbl s
           where s.tenant_id = p_tenant_id
             and s.party_id = p_party_id
             and s.valid_to = ores_utility_infinity_timestamp_fn()
             and s.oresmd_uri = a.series_uri);
end;
$$ language plpgsql stable;

comment on function ores_marketdata_curve_quote_coverage_fn(uuid, uuid) is
'Lists the quotes of a party''s curve configurations that no series of the
 party answers, one row per quote, with the kind of work the row asks for and
 the reason. It refuses a party the session cannot see, because an unreadable
 configuration and a fully answered one would both return no rows.';
