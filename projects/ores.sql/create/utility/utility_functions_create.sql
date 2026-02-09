/* -*- sql-product: postgres; tab-width: 4; indent-tabs-mode: nil -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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

--
-- Utility functions used across the database schema
--

-- Returns the 'infinity' timestamp used for valid_to in temporal tables.
-- This centralizes the sentinel value representing records that are currently valid.
CREATE OR REPLACE FUNCTION ores_utility_infinity_timestamp_fn()
RETURNS timestamptz AS $$
    SELECT '9999-12-31 23:59:59'::timestamptz;
$$ LANGUAGE sql IMMUTABLE;

/**
 * Converts an ALL-CAPS name to Proper Case, preserving common corporate
 * and legal suffixes in uppercase.
 *
 * Algorithm:
 *   1. Apply initcap() for basic title-casing
 *   2. Restore well-known corporate suffixes (PLC, LLC, etc.) to uppercase
 *   3. Restore dotted abbreviations (S.A., B.V., N.V., S.R.L.) to uppercase
 *
 * Examples:
 *   'BARCLAYS PLC'                   -> 'Barclays PLC'
 *   'BNP PARIBAS S.A.'               -> 'BNP Paribas S.A.'
 *   'DEUTSCHE BANK AG'               -> 'Deutsche Bank AG'
 *   'JPMORGAN CHASE & CO.'           -> 'Jpmorgan Chase & Co.'
 *   'ACME HOLDINGS LLC'              -> 'Acme Holdings LLC'
 */
CREATE OR REPLACE FUNCTION ores_utility_proper_name_fn(p_name text)
RETURNS text AS $$
DECLARE
    v_result text;
    v_suffix text;
BEGIN
    -- Step 1: basic title-case
    v_result := initcap(p_name);

    -- Step 2: restore common corporate/legal suffixes to uppercase.
    -- Each pair: (initcap form, desired form)
    FOREACH v_suffix IN ARRAY ARRAY[
        'Plc:PLC', 'Llc:LLC', 'Llp:LLP', 'Ltd:LTD', 'Inc:INC', 'Corp:CORP',
        'Ag:AG', 'Nv:NV', 'Ab:AB', 'Asa:ASA', 'Gmbh:GmbH', 'Mbh:MBH',
        'Bv:BV', 'Bhd:BHD', 'Srl:SRL', 'Spa:SPA', 'Sarl:SARL', 'Kg:KG',
        'Oyj:OYJ', 'Tbk:TBK', 'Lp:LP'
    ] LOOP
        v_result := regexp_replace(v_result,
            '\m' || split_part(v_suffix, ':', 1) || '\M',
            split_part(v_suffix, ':', 2), 'g');
    END LOOP;

    -- Handle S.A./S.a./Se/Sa carefully (avoid matching mid-word "Sa" in "Samsung")
    -- Only replace standalone "Sa" (followed by end/space/punctuation)
    v_result := regexp_replace(v_result, '\mSa\M', 'SA', 'g');
    v_result := regexp_replace(v_result, '\mSe\M', 'SE', 'g');
    v_result := regexp_replace(v_result, '\mAs\M', 'AS', 'g');

    -- Step 3: restore dotted abbreviations to uppercase.
    -- S.A. -> S.A., B.V. -> B.V., N.V. -> N.V., S.R.L. -> S.R.L.
    -- After initcap, "S.A." becomes "S.a." — fix lowercase letters after dots.
    v_result := regexp_replace(v_result, '([A-Z])\.([a-z])\.', E'\\1.' || '', 'g');
    -- Use a simpler word-based approach for common dotted forms
    v_result := replace(v_result, 'S.a.', 'S.A.');
    v_result := replace(v_result, 'B.v.', 'B.V.');
    v_result := replace(v_result, 'N.v.', 'N.V.');
    v_result := replace(v_result, 'S.r.l.', 'S.R.L.');
    v_result := replace(v_result, 'S.p.a.', 'S.P.A.');
    v_result := replace(v_result, 'S.e.', 'S.E.');
    v_result := replace(v_result, 'S.c.a.', 'S.C.A.');
    v_result := replace(v_result, 'K.k.', 'K.K.');

    RETURN v_result;
END;
$$ LANGUAGE plpgsql IMMUTABLE;

