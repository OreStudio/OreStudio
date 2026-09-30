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
 * Synthetic IR curve configs artefact: the vintage series identity
 *
 * A config that seeds its short rate from a vintage now names the series it
 * reads it from, so the dataset that publishes the vintage has to carry that
 * identity through to the config. The staging rows are re-derived from the
 * populate scripts, so a row written before this change carries none and is
 * left empty.
 *
 * The schema is applied by recreation, so this migration is for a database built
 * before the change: a rebuild has the column already.
 */

\echo '--- DQ synthetic IR curve configs artefact: vintage series identity ---'

alter table ores_dq_synthetic_ir_curve_configs_artefact_tbl
    add column if not exists vintage_series_uri text;
