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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#ifndef ORES_CLI_ORES_CLI_HPP
#define ORES_CLI_ORES_CLI_HPP

/**
 * @brief Console tool for ORE Studio.
 *
 * Command-line interface for importing and exporting ORE data. Key features:
 *
 * - Import: Load currencies from JSON, XML, or CSV files into the database
 * - Export: Extract currencies to JSON, XML, or CSV format
 * - Temporal queries: Export data as-of specific timepoints or all versions
 * - Filter support: Export specific entities by key
 * - Configuration: Boost program_options parser for CLI arguments
 * - Database integration: Direct repository access for data operations
 *
 * The module is organized into namespaces: config (option parsing and
 * configuration), and app (application hosting and execution).
 */
namespace ores::cli { }

#endif
