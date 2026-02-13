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

/**
 * Strips common corporate/legal suffixes from entity names.
 *
 * Used before short code generation so that "BARCLAYS PLC" becomes
 * "BARCLAYS" and "BNP PARIBAS S.A." becomes "BNP PARIBAS".
 *
 * The suffix list mirrors ores_utility_proper_name_fn() plus dotted forms.
 *
 * Examples:
 *   'BARCLAYS PLC'       -> 'BARCLAYS'
 *   'BNP PARIBAS S.A.'   -> 'BNP PARIBAS'
 *   'DEUTSCHE BANK AG'   -> 'DEUTSCHE BANK'
 */
CREATE OR REPLACE FUNCTION ores_utility_strip_corporate_suffix_fn(p_name text)
RETURNS text AS $$
DECLARE
    v_result text;
    v_suffix text;
BEGIN
    v_result := trim(p_name);

    -- Strip dotted abbreviations first (longer patterns before shorter)
    FOREACH v_suffix IN ARRAY ARRAY[
        'S.C.A.', 'S.R.L.', 'S.P.A.', 'S.A.', 'B.V.', 'N.V.', 'K.K.', 'S.E.'
    ] LOOP
        IF v_result ILIKE '% ' || v_suffix THEN
            v_result := trim(left(v_result, length(v_result) - length(v_suffix)));
        END IF;
    END LOOP;

    -- Strip word suffixes (case-insensitive, whole-word match at end)
    FOREACH v_suffix IN ARRAY ARRAY[
        'AND CO.', '& CO.', 'CO.',
        'PLC', 'LLC', 'LLP', 'LTD', 'INC', 'CORP',
        'AG', 'NV', 'AB', 'ASA', 'GMBH', 'MBH',
        'BV', 'BHD', 'SRL', 'SPA', 'SARL', 'KG',
        'OYJ', 'TBK', 'LP', 'SE', 'SA', 'AS'
    ] LOOP
        v_result := regexp_replace(v_result,
            '\s+' || v_suffix || '\s*$', '', 'i');
    END LOOP;

    RETURN trim(v_result);
END;
$$ LANGUAGE plpgsql IMMUTABLE;

/**
 * Generates a 3-8 character mnemonic short code from an entity name.
 *
 * Algorithm:
 *   1. If p_transliterated_name is not null/empty, use it; else apply
 *      unaccent(p_name) to strip diacritics.
 *   2. Strip corporate suffixes.
 *   3. Convert to UPPERCASE, remove non-alpha characters.
 *   4. If empty after cleanup, return 'UNKNWN'.
 *   5. Single word: consonant-heavy extraction (first letter + consonants,
 *      up to 6 chars).
 *   6. Multi-word:
 *        - 2 words: first 3 chars of each (6 total)
 *        - 3+ words: first 2 chars of each of first 3 words (6 total)
 *   7. Pad to minimum 3 chars if needed.
 *
 * Note: This generates base codes only. Collision resolution (appending
 * '2', '3', etc.) is handled by the caller.
 *
 * Examples:
 *   'BARCLAYS PLC'       -> 'BRCLYS'
 *   'BNP PARIBAS S.A.'   -> 'BNPPAR'
 *   'DEUTSCHE BANK AG'   -> 'DEUBAN'
 */
CREATE OR REPLACE FUNCTION ores_utility_generate_short_code_fn(
    p_name text,
    p_transliterated_name text default null
) RETURNS text AS $$
DECLARE
    v_input text;
    v_clean text;
    v_words text[];
    v_word_count int;
    v_result text;
    v_consonants text;
BEGIN
    -- Step 1: choose transliterated name or unaccent the original
    IF p_transliterated_name IS NOT NULL AND trim(p_transliterated_name) <> '' THEN
        v_input := trim(p_transliterated_name);
    ELSE
        v_input := unaccent(trim(p_name));
    END IF;

    -- Step 2: strip corporate suffixes
    v_input := ores_utility_strip_corporate_suffix_fn(v_input);

    -- Step 3: uppercase, remove non-alpha characters (keep spaces for splitting)
    v_input := upper(v_input);
    v_clean := regexp_replace(v_input, '[^A-Z ]', '', 'g');
    v_clean := regexp_replace(trim(v_clean), '\s+', ' ', 'g');

    -- Step 4: empty check
    IF v_clean = '' THEN
        RETURN 'UNKNWN';
    END IF;

    -- Split into words
    v_words := string_to_array(v_clean, ' ');
    v_word_count := array_length(v_words, 1);

    IF v_word_count = 1 THEN
        -- Step 5: single word — first letter + consonants, up to 6 chars
        v_consonants := regexp_replace(v_words[1], '[AEIOU]', '', 'g');
        v_result := left(v_words[1], 1) || substr(v_consonants, 2);
        v_result := left(v_result, 6);
    ELSIF v_word_count = 2 THEN
        -- Step 6a: two words — 3+3
        v_result := left(v_words[1], 3) || left(v_words[2], 3);
    ELSE
        -- Step 6b: three or more words — 2+2+2
        v_result := left(v_words[1], 2) || left(v_words[2], 2) || left(v_words[3], 2);
    END IF;

    -- Step 7: pad to minimum 3 chars
    IF length(v_result) < 3 THEN
        v_result := rpad(v_result, 3, 'X');
    END IF;

    RETURN v_result;
END;
$$ LANGUAGE plpgsql IMMUTABLE;

