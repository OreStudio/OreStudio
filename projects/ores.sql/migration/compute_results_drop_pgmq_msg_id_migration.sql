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
 * One-shot migration: the result stops carrying a pgmq lease pointer
 *
 * projects/ores.compute/modeling/ores.compute.result.org declared pgmq_msg_id
 * as the id of the pgmq message that leased the result. pgmq was replaced by
 * the ores_mq_* tables, and nothing sets or reads the column since, so it goes.
 *
 * On a freshly recreated database the create script no longer emits the
 * column and this migration is unnecessary. It exists for databases created
 * before the change.
 */

alter table ores_compute_results_tbl
    drop column if exists "pgmq_msg_id";
