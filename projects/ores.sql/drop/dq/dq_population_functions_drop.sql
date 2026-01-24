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

/**
 * Drop Data Quality Population Functions
 *
 * Drops all helper functions used by DQ population scripts.
 * This script is safe to run even if functions don't exist (uses IF EXISTS).
 */

set schema 'ores';

-- Data Domains
drop function if exists ores.upsert_dq_data_domains(text, text);

-- Subject Areas
drop function if exists ores.upsert_dq_subject_areas(text, text, text);

-- Catalogs
drop function if exists ores.upsert_dq_catalogs(text, text, text);

-- Dataset Dependencies
drop function if exists ores.upsert_dq_dataset_dependency(text, text, text);

-- Dimensions
drop function if exists ores.upsert_dq_origin_dimensions(text, text, text);
drop function if exists ores.upsert_dq_nature_dimensions(text, text, text);
drop function if exists ores.upsert_dq_treatment_dimensions(text, text, text);

-- Change Reasons
drop function if exists ores.upsert_change_reason_category(text, text);
drop function if exists ores.upsert_change_reason(text, text, text, boolean, boolean, boolean, integer);

-- Coding Schemes
drop function if exists ores.upsert_dq_coding_scheme_authority_type(text, text, text);
drop function if exists ores.upsert_dq_coding_schemes(text, text, text, text, text, text, text);

-- Methodologies
drop function if exists ores.upsert_dq_methodologies(text, text, text, text);

-- Datasets and Tags
drop function if exists ores.upsert_dq_datasets(text, text, text, text, text, text, text, text, text, text, text, text, text, date, text);
drop function if exists ores.upsert_dq_tag(text, text, text, text, text);
