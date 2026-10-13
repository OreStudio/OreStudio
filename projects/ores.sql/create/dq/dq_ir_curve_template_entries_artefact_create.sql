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
/*
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: sql_schema_domain_entity_artefact_create.mustache
 * To modify, update the template and regenerate.
 */

-- =============================================================================
-- One row of the raw instrument grid (the "Curve Template") an ir_curve_generation_config publishes: which tenor period, priced as which instrument type (deposit-equivalent, FRA-equivalent, swap-equivalent -- see ores.refdata.instrument_code), in what order. Every entry is modelled as a genuine [start, end) period rather than a single maturity label: start_tenor_code/end_tenor_code are both ordinary tenor references resolved through the same ores::refdata::domain::resolve_end_date machinery (see ores.refdata.api/domain/tenor_resolution.hpp). Point instruments (deposits, swaps) set start_tenor_code to 'SPOT' (a genuine zero-duration PERIOD/DAY tenor already in the catalog, resolving directly to the horizon's spot date -- not a null/sentinel hack); interval instruments (FRAs) set it to the period's own front tenor (e.g. '3M' for a 3x6 FRA whose end_tenor_code is '6M'). This symmetric shape is what lets the tenor-collision validator (validate_curve_template in ores.synthetic.api) detect genuine period overlaps via plain windows_overlap(), without a special case for point vs. interval instruments. Entries belong to a parent config via ir_curve_config_id, the same one-config-many-children shape fx_spot_generation_config's gmm_component rows use. Every entry's published rate is derived from the parent config's short-rate process's discount_factor() at the tenor's maturity -- never an independently-noised value -- so the published tick batch is, by construction, a slice of one internally consistent curve. Party- and tenant-scoped. - Artefact Table
-- =============================================================================

create table if not exists "ores_dq_ir_curve_template_entries_artefact_tbl" (
    "dataset_id" uuid not null,
    "tenant_id" uuid not null,
    "currency_code" text not null,
    "index_family" text not null,
    "tenor" text not null default '',
    "sequence_index" integer not null,
    "start_tenor_code" text not null,
    "end_tenor_code" text not null,
    "instrument_code" text not null
);

create index if not exists dq_ir_curve_template_entries_artefact_dataset_idx
on ores_dq_ir_curve_template_entries_artefact_tbl (dataset_id);

create index if not exists dq_ir_curve_template_entries_artefact_tenant_idx
on ores_dq_ir_curve_template_entries_artefact_tbl (tenant_id);

create index if not exists dq_ir_curve_template_entries_artefact_currency_code_idx
on ores_dq_ir_curve_template_entries_artefact_tbl (currency_code);

create index if not exists dq_ir_curve_template_entries_artefact_curve_idx
on ores_dq_ir_curve_template_entries_artefact_tbl (dataset_id, currency_code, index_family, tenor);
