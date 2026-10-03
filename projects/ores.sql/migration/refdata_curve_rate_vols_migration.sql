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
 * One-shot migration: the swaption and cap and floor volatility sections
 *
 * Adds the tables that hold the swaption volatility and cap and floor
 * volatility entries of a curve configuration, and the parametric smile, with
 * its parameters, that an FX, swaption or cap and floor volatility may be
 * calibrated with.
 *
 * Run after refdata_curve_credit_inflation_vols_migration.sql. Every statement
 * creates or replaces, so a database that already holds these objects is left
 * as it is.
 */

begin;

\ir ../create/refdata/refdata_curve_parametric_smiles_create.sql
\ir ../create/refdata/refdata_curve_parametric_smiles_notify_trigger_create.sql
\ir ../create/refdata/refdata_curve_parametric_smile_parameters_create.sql
\ir ../create/refdata/refdata_curve_parametric_smile_parameters_notify_trigger_create.sql
\ir ../create/refdata/refdata_swaption_volatilities_create.sql
\ir ../create/refdata/refdata_swaption_volatilities_notify_trigger_create.sql
\ir ../create/refdata/refdata_cap_floor_volatilities_create.sql
\ir ../create/refdata/refdata_cap_floor_volatilities_notify_trigger_create.sql
\ir ../create/refdata/refdata_rls_policies_create.sql

\ir ../populate/iam/iam_permissions_populate.sql

commit;
