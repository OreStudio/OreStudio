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
#ifndef ORES_REPORTING_CORE_TESTS_ORE_PARAMETER_TEST_FIXTURE_HPP
#define ORES_REPORTING_CORE_TESTS_ORE_PARAMETER_TEST_FIXTURE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.reporting.api/generators/ore_parameter_generator.hpp"
#include "ores.reporting.api/generators/ore_parameter_value_domain_generator.hpp"
#include "ores.reporting.core/repository/ore_parameter_repository.hpp"
#include "ores.reporting.core/repository/ore_parameter_value_domain_repository.hpp"
#include "ores.utility/generation/generation_context.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstddef>
#include <string>
#include <vector>

namespace ores::reporting::tests {

/**
 * @brief Writes a few ore parameters of the test's own into the system tenant.
 *
 * A row that names an ore parameter checks the parameter under the system
 * tenant, so the test seeds the parameters there. The database build stages the
 * real parameters and publishes none, and a test must not need the real data, so
 * it writes the few rows it uses and a value domain for them to name.
 *
 * @return The ids of the parameters that were written.
 */
inline std::vector<boost::uuids::uuid>
write_system_parameters(const ores::database::context& party_ctx, const std::string& db_user,
                        ores::utility::generation::generation_context& gen,
                        std::size_t count = 5) {
    const auto system_ctx =
        party_ctx.with_tenant(ores::utility::uuid::tenant_id::system(), db_user);

    auto domain = ores::reporting::generators::generate_synthetic_ore_parameter_value_domain(gen);
    domain.change_reason_code = "system.test";
    ores::reporting::repository::ore_parameter_value_domain_repository().write(system_ctx, domain);

    std::vector<boost::uuids::uuid> ids;
    ores::reporting::repository::ore_parameter_repository parameters;
    for (std::size_t i = 0; i != count; ++i) {
        auto parameter = ores::reporting::generators::generate_synthetic_ore_parameter(gen);
        parameter.change_reason_code = "system.test";
        parameter.ore_parameter_value_domain_code = domain.code;
        parameters.write(system_ctx, parameter);
        ids.push_back(parameter.id);
    }
    return ids;
}

}

#endif
