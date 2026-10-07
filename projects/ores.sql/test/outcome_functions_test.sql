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
 * pgTAP tests for the outcome functions rendered from the outcome catalogue.
 *
 * Two things are under test. The renderer composes the sentence the catalogue
 * holds, and the raiser raises the SQLSTATE the catalogue binds to a code. A
 * trigger now states a code and a field and nothing else, so both have to work
 * for every refusal the store reports.
 *
 * The expected sentences are the same literals the C++ test
 * ores.database/tests/outcome_code_tests.cpp asserts, so a change to one
 * renderer alone fails a gate.
 *
 * Run with: pg_prove -d <database> test/outcome_functions_test.sql
 */

begin;

select plan(8);

-- =============================================================================
-- Test: the renderer fills the placeholders it is given
-- =============================================================================

-- Test 1: A name with a value is replaced, and one without keeps its braces.
select is(
    ores_outcome_fill_fn('{a} and {b}', array['a']::text[], array['x']::text[]),
    'x and {b}',
    'ores_outcome_fill_fn replaces the names it is given and leaves the rest'
);

-- =============================================================================
-- Test: each store outcome composes its own sentence
-- =============================================================================

-- Test 2: The version-conflict sentence names the record, the field and both versions.
select is(
    ores_outcome_version_conflict_fn('currency', 'iso_code', 'GBP', '3', '4'),
    'The currency with iso_code ''GBP'' is at version 4, and this write states version 3.',
    'the version-conflict sentence names the record, the field and both versions'
);

-- Test 3: The already-exists sentence names the record.
select is(
    ores_outcome_already_exists_fn('currency', 'iso_code', 'GBP'),
    'The currency with iso_code ''GBP'' already exists. State the version you read to replace it, or ask for a version replace.',
    'the already-exists sentence names the record and the field'
);

-- Test 4: The missing-field sentence names the entity.
select is(
    ores_outcome_missing_field_fn('currency'),
    'Invalid currency: value cannot be null or empty.',
    'the missing-field sentence names the entity'
);

-- =============================================================================
-- Test: the raiser raises the SQLSTATE the catalogue binds to the code, with
-- the sentence that code's own function composes
-- =============================================================================

-- Test 5
select throws_ok(
    $$select ores_outcome_raise_fn('version_conflict', 'currency', 'iso_code', 'GBP', '3', '4')$$,
    'P0002',
    'The currency with iso_code ''GBP'' is at version 4, and this write states version 3.',
    'the raiser raises P0002 for version_conflict and composes its sentence'
);

-- Test 6
select throws_ok(
    $$select ores_outcome_raise_fn('already_exists', 'currency', 'iso_code', 'GBP')$$,
    '23505',
    'The currency with iso_code ''GBP'' already exists. State the version you read to replace it, or ask for a version replace.',
    'the raiser raises 23505 for already_exists and composes its sentence'
);

-- Test 7
select throws_ok(
    $$select ores_outcome_raise_fn('missing_field', 'currency')$$,
    '23502',
    'Invalid currency: value cannot be null or empty.',
    'the raiser raises 23502 for missing_field and composes its sentence'
);

-- Test 8: A code the catalogue does not hold is refused rather than ignored.
select throws_ok(
    $$select ores_outcome_raise_fn('no_such_outcome')$$,
    'XX000',
    'Unknown outcome code: no_such_outcome',
    'the raiser refuses a code the catalogue does not hold'
);

select * from finish();

rollback;
