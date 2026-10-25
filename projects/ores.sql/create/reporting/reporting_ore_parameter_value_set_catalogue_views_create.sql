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
 * The catalogues a parameter's allowed values may come from.
 *
 * Each view presents a catalogue as the three columns a value set reads:
 * the value a configuration holds, the label a person reads, and whether
 * the row may still be chosen. The registry in
 * ores_reporting_ore_parameter_value_set_catalogues_tbl names the view, so a
 * parameter that draws on tenors, currencies, currency pairs, sensitivity
 * templates, instrument codes, calendars, day counters or conventions reads
 * the catalogue's current rows and never a copy of them.
 */

create or replace view ores_refdata_tenor_catalogue_v as
select code as value, display_name as label, true as is_active
from ores_refdata_tenors_tbl
where valid_to = ores_utility_infinity_timestamp_fn();

create or replace view ores_refdata_currency_catalogue_v as
select iso_code as value, name as label, true as is_active
from ores_refdata_currencies_tbl
where valid_to = ores_utility_infinity_timestamp_fn();

create or replace view ores_refdata_currency_pair_catalogue_v as
select pair_code as value, pair_code as label, true as is_active
from ores_refdata_currency_pairs_tbl
where valid_to = ores_utility_infinity_timestamp_fn();

create or replace view ores_analytics_sensitivity_template_catalogue_v as
select code as value, code as label, true as is_active
from ores_analytics_sensitivity_templates_tbl
where valid_to = ores_utility_infinity_timestamp_fn();

create or replace view ores_refdata_instrument_code_catalogue_v as
select code as value, name as label, true as is_active
from ores_refdata_instrument_codes_tbl
where valid_to = ores_utility_infinity_timestamp_fn();

create or replace view ores_refdata_calendar_catalogue_v as
select code as value, name as label, true as is_active
from ores_refdata_calendars_tbl
where valid_to = ores_utility_infinity_timestamp_fn();

create or replace view ores_refdata_day_counter_catalogue_v as
select code as value, description as label, true as is_active
from ores_refdata_day_counters_tbl
where valid_to = ores_utility_infinity_timestamp_fn();

create or replace view ores_refdata_business_day_convention_catalogue_v as
select code as value, name as label, true as is_active
from ores_refdata_business_day_convention_types_tbl
where valid_to = ores_utility_infinity_timestamp_fn();
