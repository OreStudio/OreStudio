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
#ifndef ORES_IAM_API_ORES_IAM_API_HPP
#define ORES_IAM_API_ORES_IAM_API_HPP

/**
 * @brief Shared IAM contract: domain types, JSON/table I/O, and NATS protocol schemas.
 *
 * Header-only library defining the shared contract for the IAM domain. It
 * provides domain types for accounts, roles, tenants, parties, and session
 * tokens, JSON and table I/O via rfl, and the NATS message protocol schemas
 * for the 0x2000-0x2FFF range consumed by ores.iam.core and the client
 * components.
 */
namespace ores::iam::api {}

#endif
