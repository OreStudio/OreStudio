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
-- Synthetic IR Curve Config artefacts - denormalized parent+child IR curve
-- generation config rows for the synthetic market data bundle - Artefact Table
-- =============================================================================

create table if not exists "ores_dq_synthetic_ir_curve_configs_artefact_tbl" (
    "dataset_id" uuid not null,
    "tenant_id" uuid not null,
    "id" uuid not null,
    "version" integer not null,
    "name" text not null,
    "description" text null,
    "enabled" boolean not null,
    "currency_code" text not null,
    "index_name" text not null,
    "process_type" text not null,
    "kappa" double precision not null,
    "theta" double precision not null,
    "sigma" double precision not null,
    "initial_rate" double precision not null,
    "ticks_per_hour" integer not null,
    "fixed_leg_payment_frequency_code" text not null
);

create index if not exists dq_synthetic_ir_curve_configs_artefact_dataset_idx
on ores_dq_synthetic_ir_curve_configs_artefact_tbl (dataset_id);

create index if not exists dq_synthetic_ir_curve_configs_artefact_tenant_idx
on ores_dq_synthetic_ir_curve_configs_artefact_tbl (tenant_id);

create index if not exists dq_synthetic_ir_curve_configs_artefact_id_idx
on ores_dq_synthetic_ir_curve_configs_artefact_tbl (id);

create index if not exists dq_synthetic_ir_curve_configs_artefact_currency_code_idx
on ores_dq_synthetic_ir_curve_configs_artefact_tbl (currency_code);
