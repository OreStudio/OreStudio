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
 * Refuses a stored parameter value outside its setting's value set.
 *
 * The check lives on the row every writer reaches, so the analytics service,
 * the BFF and psql all get the same answer. The function resolves the set by
 * (scope, name) and is in the reporting component.
 */

drop trigger if exists ores_analytics_pricing_model_product_parameters_value_set_trg
    on ores_analytics_pricing_model_product_parameters_tbl;

create or replace trigger ores_analytics_pricing_model_product_parameters_value_set_trg
    before insert or update of parameter_scope, parameter_name, parameter_value
    on ores_analytics_pricing_model_product_parameters_tbl
    for each row
    execute function ores_reporting_validate_parameter_value_fn();
