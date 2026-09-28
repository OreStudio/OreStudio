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
 * One-shot migration: is_initial and is_terminal become boolean
 *
 * Both columns were integer with a default of 0 and an in (0, 1) check
 * while every consumer already treated them as flags: the shell reads
 * them into a bool, the domain and protocol structs declare bool, and
 * the presentation mappers print "true" or "false". Only the storage
 * side disagreed, so a row read as text could be compared with an
 * integer by accident. Boolean is the convention elsewhere in the
 * schema, so the columns now match what the code always meant.
 *
 * The two check constraints are dropped rather than rewritten: they
 * existed only to keep an integer column inside the range a boolean
 * column now enforces by its own type, and the regenerated create
 * script no longer declares them.
 *
 * On a freshly recreated database the create script already emits
 * boolean and this migration is unnecessary. It exists for databases
 * created before the change.
 */

alter table ores_dq_fsm_states_tbl
    drop constraint if exists ores_dq_fsm_states_tbl_is_initial_check;

alter table ores_dq_fsm_states_tbl
    drop constraint if exists ores_dq_fsm_states_tbl_is_terminal_check;

alter table ores_dq_fsm_states_tbl
    alter column is_initial drop default;

alter table ores_dq_fsm_states_tbl
    alter column is_initial type boolean
    using (is_initial <> 0);

alter table ores_dq_fsm_states_tbl
    alter column is_initial set default false;

alter table ores_dq_fsm_states_tbl
    alter column is_terminal drop default;

alter table ores_dq_fsm_states_tbl
    alter column is_terminal type boolean
    using (is_terminal <> 0);

alter table ores_dq_fsm_states_tbl
    alter column is_terminal set default false;
