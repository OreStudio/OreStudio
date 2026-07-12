/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
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
#ifndef ORES_ANALYTICS_QUANT_HPP
#define ORES_ANALYTICS_QUANT_HPP

/**
 * @brief Dependency-light quantitative math library -- currently the Cross
 * Rates Matrix (CRM) engine.
 *
 * A pure computation library: no database, no NATS, no refdata coupling.
 * Everything it needs (currency codes, spot days, short-term rates) is
 * supplied by the caller as plain parameters -- see @c domain::ccy_pair_input
 * and friends. Depends only on Boost (graph, for the topology build) and
 * immer (for the lock-free runtime rate engine); fully unit-testable in
 * isolation, consumable standalone as a library.
 *
 * Two-phase interface, matching two very different lifecycles:
 * - @b Build (rare): @c service::topology_builder validates a set of
 *   currency pairs into an immutable, fixed @c domain::crm_topology --
 *   a spanning tree with cycle detection at build time, so a config
 *   admitting more than one path between two currencies is always
 *   rejected, never silently resolved.
 * - @b Runtime (continuous, later task): a rate engine consumes a stream of
 *   driver-rate ticks -- possibly from a different thread than its readers
 *   -- and serves batched derived-rate reads via an atomically-swapped
 *   immutable snapshot, propagating staleness end to end.
 *
 * See doc/agile/versions/v0/sprint_23/crm_implementation/ for the story,
 * task and design diagrams this component implements.
 */
namespace ores::analytics::quant {}

#endif
