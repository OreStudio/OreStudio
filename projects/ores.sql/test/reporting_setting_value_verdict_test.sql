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
 * pgTAP tests for the verdict a setting's value set gives on one value.
 *
 * One function answers a caller that names a setting by its link code. It
 * states whether the setting names a set, which set, what kind, and whether the
 * value is in it, so a caller cannot meet an empty answer and read it as a
 * rule that allows everything.
 * - A setting that names a set answers found
 * - A value the set states is in it
 * - A value the set does not state is not in it
 * - A whole below the floor its set states is not in it
 * - A setting that names no set answers found = false rather than raising
 *
 * Run with: pg_prove -d <database> test/reporting_setting_value_verdict_test.sql
 */

begin;

select plan(5);

select is(
    (select found from ores_reporting_setting_value_verdict_fn('Setup:observationModel', 'None')),
    true,
    'a setting that names a set answers found');

select is(
    (select is_in_set from ores_reporting_setting_value_verdict_fn('Setup:observationModel', 'Defer')),
    true,
    'a value the set states is in it');

select is(
    (select is_in_set from ores_reporting_setting_value_verdict_fn('Setup:observationModel', 'NotAMode')),
    false,
    'a value the set does not state is not in it');

select is(
    (select is_in_set from ores_reporting_setting_value_verdict_fn('Setup:nThreads', '0')),
    false,
    'a whole below the floor its set states is not in it');

select is(
    (select found from ores_reporting_setting_value_verdict_fn('Setup:noSuchSetting', 'anything')),
    false,
    'a setting that names no set answers found = false');

select * from finish();
rollback;
