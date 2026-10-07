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
#ifndef ORES_IAM_CORE_REPOSITORY_DATABASE_INFO_LOOKUPS_HPP
#define ORES_IAM_CORE_REPOSITORY_DATABASE_INFO_LOOKUPS_HPP

#include "ores.database/domain/context.hpp"
#include "ores.iam.api/messaging/login_protocol.hpp"
#include "ores.iam.core/export.hpp"

namespace ores::iam::repository {

/**
 * @brief Reads the newest row of ores_database_info_tbl.
 *
 * The table records the checkout the database was built from: the schema
 * fingerprint, the build environment, the commit and when the database was
 * created. Exactly one row exists, written when the database is created or
 * recreated. The table is deliberately not a codegen entity, so the read is
 * hand-written, like the tenant lookups beside it.
 *
 * The row answers what the deployment is talking to, so the login answer
 * carries it. A row that cannot be read returns an empty struct rather than
 * throwing: the sign-in still opens a session, and the screen states the
 * database as unknown instead of a refusal over build forensics.
 */
ORES_IAM_CORE_EXPORT messaging::database_info
read_database_info(const ores::database::context& ctx);

}

#endif
