-- -*- sql-product: postgres; tab-width: 4; indent-tabs-mode: nil -*-
--
-- Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
--
-- This program is free software; you can redistribute it and/or modify it under
-- the terms of the GNU General Public License as published by the Free Software
-- Foundation; either version 3 of the License, or (at your option) any later
-- version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
-- details.
--
-- You should have received a copy of the GNU General Public License along with
-- this program; if not, write to the Free Software Foundation, Inc., 51
-- Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
--

-- =============================================================================
-- Report Definition Artefacts — tier and jurisdiction columns
-- =============================================================================
-- Adds audience-tier and jurisdiction filtering so the same artefact catalogue
-- can be published selectively: holding companies get strategic reports,
-- trading desks get desk-level analytics, and every entity gets common reports.

alter table if exists ores_dq_report_definitions_artefact_tbl
    add column if not exists tier text not null default 'trading',
    add column if not exists applicable_jurisdiction text null;

comment on column ores_dq_report_definitions_artefact_tbl.tier is
    'Audience tier: common, regulatory, strategic, or trading. Used to filter artefacts during per-entity provisioning.';
comment on column ores_dq_report_definitions_artefact_tbl.applicable_jurisdiction is
    'ISO 3166 alpha-2 country code for jurisdiction-specific regulatory reports (e.g. GB, US, HK). NULL for reports that apply universally.';
