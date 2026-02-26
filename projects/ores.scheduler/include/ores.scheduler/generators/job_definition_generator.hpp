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
#pragma once

#include <boost/uuid/uuid.hpp>
#include "ores.utility/uuid/tenant_id.hpp"
#include "ores.scheduler/domain/job_definition.hpp"

namespace ores::scheduler::generators {

/**
 * @brief Generate a synthetic job_definition for testing.
 *
 * Produces a valid job_definition with deterministic-but-unique values using
 * faker-cxx. The generated job runs a no-op SQL comment on a daily schedule.
 *
 * @param tenant_id Tenant to assign the generated definition to.
 * @param party_id  Party to assign the generated definition to.
 */
[[nodiscard]] domain::job_definition
generate_synthetic_job_definition(
    const utility::uuid::tenant_id& tenant_id,
    const boost::uuids::uuid& party_id);

} // namespace ores::scheduler::generators
