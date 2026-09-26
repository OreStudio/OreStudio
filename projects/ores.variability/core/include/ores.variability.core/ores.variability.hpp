/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#ifndef ORES_VARIABILITY_VARIABILITY_HPP
#define ORES_VARIABILITY_VARIABILITY_HPP

/**
 * @brief System variability and configuration management module for ORE Studio.
 *
 * This module holds the platform's typed runtime configuration as one
 * bitemporal table of named settings. A setting is named by a dotted string
 * such as =system.bootstrap_mode=, carries a type and a value, and is scoped
 * to a tenant and a party. Key features include:
 *
 * - Typed configuration: boolean, integer, string and json settings in one
 *   table, with the type stored beside the value
 * - Temporal versioning: bitemporal tracking of every configuration change
 * - Audit support: tracking who changed what, when, and why
 * - Two ways in: the canonical entity protocol over NATS, and the typed
 *   accessors components read in process
 *
 * The module is organised into namespaces: domain (the setting and its wire
 * types), repository (the generated store) and service (the typed accessors
 * components depend on).
 */
namespace ores::variability {}

#endif
