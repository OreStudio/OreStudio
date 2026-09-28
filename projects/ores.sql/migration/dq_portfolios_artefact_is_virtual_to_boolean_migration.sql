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
 * One-shot migration: the dq portfolio artefact's is_virtual becomes boolean
 *
 * The staging column was integer while the store it feeds,
 * ores_refdata_portfolios_tbl.is_virtual, was already boolean, so
 * ores_refdata_publish_portfolios_from_dq_fn carried a conversion and
 * the populate scripts wrote 0 and 1. The staging column now matches
 * the store and the conversion is gone.
 *
 * On a freshly recreated database the create script already emits
 * boolean and this migration is unnecessary. It exists for databases
 * created before the change.
 */

alter table ores_dq_portfolios_artefact_tbl
    alter column is_virtual type boolean
    using (is_virtual <> 0);
