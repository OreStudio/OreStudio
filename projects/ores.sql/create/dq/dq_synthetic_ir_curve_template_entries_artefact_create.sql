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
-- Synthetic IR Curve Template Entry artefacts - per-currency Curve Template
-- rows backing an ir_curve_configs artefact row, keyed by currency+index
-- within the same dataset - Artefact Table
-- =============================================================================

create table if not exists "ores_dq_synthetic_ir_curve_template_entries_artefact_tbl" (
    "dataset_id" uuid not null,
    "tenant_id" uuid not null,
    "currency_code" text not null,
    "index_name" text not null,
    "sequence_index" integer not null,
    "start_tenor_code" text not null,
    "end_tenor_code" text not null,
    "instrument_code" text not null
);

create index if not exists dq_synthetic_ir_curve_template_entries_artefact_dataset_idx
on ores_dq_synthetic_ir_curve_template_entries_artefact_tbl (dataset_id);

create index if not exists dq_synthetic_ir_curve_template_entries_artefact_tenant_idx
on ores_dq_synthetic_ir_curve_template_entries_artefact_tbl (tenant_id);

create index if not exists dq_synthetic_ir_curve_template_entries_artefact_curve_idx
on ores_dq_synthetic_ir_curve_template_entries_artefact_tbl (dataset_id, currency_code, index_name);
