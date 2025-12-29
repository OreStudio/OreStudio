/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2024 Marco Craveiro <marco.craveiro@gmail.com>
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
#ifndef ORES_HPP
#define ORES_HPP

/**
 * @brief ORE Studio - Graphical interface and data management for Open Source Risk Engine.
 *
 * ORE Studio is a C++20 application providing persistent storage, graphical user
 * interface, and orchestration tools for quantitative risk analysis using the
 * Open Source Risk Engine (ORE). The system is built on a layered architecture
 * with clear separation of concerns.
 *
 * @section architecture System Architecture
 *
 * The system is organized into five architectural layers:
 *
 * **Foundation Layer** (`ores.utility`)
 * - Core infrastructure: database connectivity, logging, filesystem utilities
 * - UUID generation, environment variables, string conversion
 * - No dependencies on other ORE Studio components
 *
 * **Infrastructure Layer** (`ores.comms`, `ores.testing`)
 * - Communications: Binary protocol over SSL/TLS for client-server messaging
 * - Testing: Catch2 integration with database isolation and logging
 * - Depends on: Foundation
 *
 * **Domain Layer** (`ores.risk`, `ores.iam`)
 * - Risk: ORE domain model with XML/CSV/JSON I/O and temporal versioning
 * - IAM: User authentication, authorization, and session management
 * - Depends on: Foundation, Infrastructure
 *
 * **Client Layer** (`ores.client`)
 * - Deprecated REPL client (functionality moved to ores.comms)
 * - Depends on: Foundation, Infrastructure, Domain
 *
 * **Application Layer** (`ores.cli`, `ores.qt`, `ores.comms.service`)
 * - CLI: Command-line import/export of currencies in multiple formats
 * - Qt: Desktop GUI with MDI interface for visual data management
 * - Service: Multi-client server hosting all backend services
 * - Depends on: All layers
 *
 * @section components Component Documentation
 *
 * For detailed component documentation, see:
 * - Component class diagrams: `projects/COMPONENT/modeling/COMPONENT.puml`
 * - Component headers: `projects/COMPONENT/include/COMPONENT/COMPONENT.hpp`
 * - System architecture: `projects/modeling/ores.puml`
 *
 * @section documentation Further Documentation
 *
 * - Full API documentation: See Doxygen mainpage
 * - Source code: https://github.com/OreStudio/OreStudio
 * - Project website: https://orestudio.github.io/OreStudio
 *
 * @note This project has no affiliation with Acadia, Open Source Risk Engine,
 * or QuantLib. See Doxygen documentation for full context.
 */
namespace ores { }

#endif
