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
-- Drop FSM Component (Generic Finite State Machine Infrastructure)
-- =============================================================================
-- Drop in reverse dependency order: transitions first, then states, then machines.

-- Transitions depend on machines and states
\ir ./fsm_transitions_notify_trigger_drop.sql
\ir ./fsm_transitions_drop.sql

-- States depend on machines
\ir ./fsm_states_notify_trigger_drop.sql
\ir ./fsm_states_drop.sql

-- Machines have no FSM dependencies
\ir ./fsm_machines_notify_trigger_drop.sql
\ir ./fsm_machines_drop.sql
