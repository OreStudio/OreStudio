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
#ifndef ORES_TESTING_ORE_PARAMETER_FIXTURE_HPP
#define ORES_TESTING_ORE_PARAMETER_FIXTURE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.reporting.api/generators/ore_parameter_generator.hpp"
#include "ores.reporting.api/generators/ore_parameter_value_domain_generator.hpp"
#include "ores.reporting.core/repository/ore_parameter_repository.hpp"
#include "ores.reporting.core/repository/ore_parameter_value_domain_repository.hpp"
#include "ores.utility/generation/generation_context.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <cstddef>
#include <exception>
#include <set>
#include <string>
#include <vector>

namespace ores::testing {

/**
 * @brief A row that names an ore parameter checks the parameter under the system
 *        tenant, and the database build stages the real parameters and publishes
 *        none. A test seeds the few it uses with these helpers and does not read
 *        the real vocabulary.
 */

/**
 * @brief Writes a few ore parameters of the test's own into the system tenant.
 *
 * The parameters have random keys and a value domain of their own, so two runs
 * against one database never meet.
 *
 * @return The ids of the parameters that were written.
 */
inline std::vector<boost::uuids::uuid>
write_system_parameters(const ores::database::context& party_ctx,
                        const std::string& db_user,
                        ores::utility::generation::generation_context& gen,
                        std::size_t count = 5) {
    const auto system_ctx =
        party_ctx.with_tenant(ores::utility::uuid::tenant_id::system(), db_user);

    auto domain = ores::reporting::generators::generate_synthetic_ore_parameter_value_domain(gen);
    domain.change_reason_code = "system.test";
    domain.tenant_id = ores::utility::uuid::tenant_id::system();
    // The generator draws the code from a few words and a counter, so two runs
    // against one database could meet; a random uuid cannot.
    domain.code = "test-" + boost::uuids::to_string(boost::uuids::random_generator()());
    ores::reporting::repository::ore_parameter_value_domain_repository().write(system_ctx, domain);

    std::vector<boost::uuids::uuid> ids;
    ores::reporting::repository::ore_parameter_repository parameters;
    for (std::size_t i = 0; i != count; ++i) {
        auto parameter = ores::reporting::generators::generate_synthetic_ore_parameter(gen);
        parameter.change_reason_code = "system.test";
        parameter.tenant_id = ores::utility::uuid::tenant_id::system();
        parameter.ore_parameter_value_domain_code = domain.code;
        // The key of a parameter is its schema and XPath, which the generator
        // draws from a few words, so the test states both from the id.
        parameter.name = "test-" + boost::uuids::to_string(parameter.id);
        parameter.xpath = "/test/" + parameter.name;
        parameters.write(system_ctx, parameter);
        ids.push_back(parameter.id);
    }
    return ids;
}

/** One analytic parameter a test needs the system tenant to describe. */
struct analytic_parameter {
    std::string analytic;
    std::string name;
    std::string domain;
};

/**
 * @brief The analytic parameters of the ORE example the import and run document
 *        tests read: the npv, cashflow and curves analytics of Example_1.
 */
inline const std::vector<analytic_parameter>& example_analytic_parameters() {
    static const std::vector<analytic_parameter> parameters{
        {"npv", "active", "boolean"},
        {"npv", "baseCurrency", "string"},
        {"npv", "outputFileName", "string"},
        {"cashflow", "active", "boolean"},
        {"cashflow", "outputFileName", "string"},
        {"curves", "active", "boolean"},
        {"curves", "configuration", "string"},
        {"curves", "grid", "string"},
        {"curves", "outputFileName", "string"},
    };
    return parameters;
}

/**
 * @brief Describes the given analytic parameters in the system tenant, and the
 *        value domains they name, leaving any the system tenant holds already.
 *
 * A test process may run beside another that writes the same row, so a write that
 * fails is accepted when the row is there afterwards.
 */
inline void seed_system_analytic_parameters(
    const ores::database::context& party_ctx,
    const std::string& db_user,
    ores::utility::generation::generation_context& gen,
    const std::vector<analytic_parameter>& wanted = example_analytic_parameters()) {
    namespace repository = ores::reporting::repository;
    const auto system_ctx =
        party_ctx.with_tenant(ores::utility::uuid::tenant_id::system(), db_user);

    repository::ore_parameter_value_domain_repository domains;
    std::set<std::string> known;
    for (const auto& d : domains.read_latest(system_ctx))
        known.insert(d.code);
    for (const auto& want : wanted) {
        if (!known.insert(want.domain).second)
            continue;
        auto domain =
            ores::reporting::generators::generate_synthetic_ore_parameter_value_domain(gen);
        domain.change_reason_code = "system.test";
        domain.tenant_id = ores::utility::uuid::tenant_id::system();
        domain.code = want.domain;
        domain.name = want.domain;
        domain.storage_kind = want.domain;
        domain.referenced_entity = "";
        try {
            domains.write(system_ctx, domain);
        } catch (const std::exception&) {
            const auto now = domains.read_latest(system_ctx);
            if (std::none_of(
                    now.begin(), now.end(), [&](const auto& d) { return d.code == domain.code; }))
                throw;
        }
    }

    repository::ore_parameter_repository parameters;
    const auto held = parameters.read_latest(system_ctx);
    const auto holds = [&](const analytic_parameter& want) {
        return std::any_of(held.begin(), held.end(), [&](const auto& p) {
            return p.scope == "analytic" && p.subtype == want.analytic && p.name == want.name;
        });
    };
    int position = 0;
    for (const auto& want : wanted) {
        ++position;
        if (holds(want))
            continue;
        auto parameter = ores::reporting::generators::generate_synthetic_ore_parameter(gen);
        parameter.change_reason_code = "system.test";
        parameter.tenant_id = ores::utility::uuid::tenant_id::system();
        parameter.scope = "analytic";
        parameter.subtype = want.analytic;
        parameter.name = want.name;
        parameter.schema_name = "ore.xsd";
        parameter.xpath = "/ORE/Analytics/Analytic[@type='" + want.analytic +
                          "']/Parameter[@name='" + want.name + "']";
        parameter.position = position;
        parameter.ore_parameter_value_domain_code = want.domain;
        try {
            parameters.write(system_ctx, parameter);
        } catch (const std::exception&) {
            const auto now = parameters.read_latest(system_ctx);
            if (std::none_of(now.begin(), now.end(), [&](const auto& p) {
                    return p.scope == "analytic" && p.subtype == want.analytic &&
                           p.name == want.name;
                }))
                throw;
        }
    }
}

}

#endif
