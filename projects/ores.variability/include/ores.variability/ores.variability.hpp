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
 * This module provides infrastructure for managing system configurability and
 * runtime behavior variation. It serves as the foundation for all aspects of
 * system adaptability. Key features include:
 *
 * - Feature flags: runtime toggles for controlling system features and behavior
 * - Temporal versioning: bitemporal tracking of configuration changes
 * - Configuration management: centralized storage of system settings
 * - Audit support: tracking who changed what and when
 * - Repository pattern: ORM-based persistence with database integration
 *
 * The module is designed to be a dependency for other ORE Studio components
 * that need runtime configurability. It provides a clean separation between
 * system configuration concerns and domain-specific business logic.
 *
 * The module is organized into namespaces: domain (core entities) and
 * repository (ORM and persistence).
 */
namespace ores::variability {}

#endif
