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
 * FOR A PARTICULAR PURPOSE. Seethe GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General PublicLicense along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#ifndef ORES_REFDATA_HPP
#define ORES_REFDATA_HPP

/**
 * @brief Reference data domain model for Open Risk Engine (ORE) types.
 *
 * Implements the core reference data domain model following ORE (Open Source
 * Risk Engine) specifications. Key features:
 *
 * - Domain model: Currency entities with temporal versioning
 * - ORE XML support: Import/export to native ORE XML format using pugixml
 * - CSV export: Structured CSV output for currencies
 * - JSON I/O: Serialization using reflection (rfl library)
 * - Table I/O: Formatted table output using fort library
 * - Database persistence: ORM entities, mappers, and repositories with temporal support
 * - Message-based API: Request/response handlers for currency operations (0x3000-0x3FFF)
 * - Synthetic data: Test data generation using faker-cxx
 * - Version history: Track all changes to entities with valid_from/valid_to fields
 *
 * The module is organized into namespaces: domain (core entities), repository
 * (ORM and persistence), orexml (ORE XML I/O), csv (CSV export), messaging
 * (API handlers), and generators (test data).
 */
namespace ores::refdata {}

#endif
