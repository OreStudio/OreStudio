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
#ifndef ORES_REPORTING_CORE_TESTS_ENGINE_SETTINGS_PROPOSAL_FIXTURES_HPP
#define ORES_REPORTING_CORE_TESTS_ENGINE_SETTINGS_PROPOSAL_FIXTURES_HPP

#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.iam.api/generators/account_generator.hpp"
#include "ores.iam.core/repository/account_repository.hpp"
#include "ores.inbox.core/service/approval_lifecycle.hpp"
#include "ores.refdata.api/generators/party_generator.hpp"
#include "ores.refdata.core/repository/party_repository.hpp"
#include "ores.reporting.api/generators/configuration_generator.hpp"
#include "ores.reporting.api/generators/engine_settings_fixings_generator.hpp"
#include "ores.reporting.api/generators/engine_settings_generator.hpp"
#include "ores.reporting.api/generators/report_configuration_generator.hpp"
#include "ores.reporting.api/generators/report_definition_generator.hpp"
#include "ores.reporting.core/repository/configuration_repository.hpp"
#include "ores.reporting.core/repository/engine_settings_fixings_repository.hpp"
#include "ores.reporting.core/repository/engine_settings_repository.hpp"
#include "ores.reporting.core/repository/report_configuration_repository.hpp"
#include "ores.reporting.core/repository/report_definition_repository.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <optional>
#include <string>

namespace ores::reporting::tests {

/**
 * @brief One party's engine run settings, twice: one a report binds and one it
 * does not, and the two accounts a proposal needs.
 *
 * The bound configuration is the governed case, where a numbers-changing write
 * is a proposal. The unbound one is the draft, where the same write lands in
 * place.
 */
struct world {
    boost::uuids::uuid party_id;
    boost::uuids::uuid definition_id;
    /** The engine run settings a report binds. */
    boost::uuids::uuid bound_id;
    /** The engine run settings no report binds. */
    boost::uuids::uuid draft_id;
    boost::uuids::uuid maker_id;
    boost::uuids::uuid decider_id;
    std::string maker;
    std::string decider;
    ores::database::context ctx;
};

namespace detail {

inline boost::uuids::uuid write_account(ores::testing::scoped_database_helper& h,
                                        const std::string& username) {
    auto gen = ores::testing::make_generation_context(h);
    auto account = ores::iam::generators::generate_synthetic_account(gen);
    account.username = username;
    // A person's account written into the shared test tenant would end its
    // bootstrap, and later tests that write as the unit test would be refused.
    account.account_type = "service";
    account.change_reason_code = "system.test";
    ores::iam::repository::account_repository{}.write(h.context(), account);
    return account.id;
}

inline boost::uuids::uuid write_engine_settings(const ores::database::context& ctx,
                                                const std::string& name,
                                                const boost::uuids::uuid& configuration_id,
                                                const boost::uuids::uuid& party_id) {
    domain::engine_settings settings;
    settings.id = boost::uuids::random_generator{}();
    settings.party_id = party_id;
    settings.name = name;
    settings.description = "Engine run settings a test proposes a change to";
    settings.configuration_id = configuration_id;
    settings.modified_by = ctx.actor();
    settings.performed_by = ctx.service_account();
    settings.change_reason_code = "system.test";
    repository::engine_settings_repository{}.write(ctx, settings);
    return settings.id;
}

}

/**
 * @brief Seeds the party, the two configurations, the report that binds one of
 * them, and the two accounts.
 */
inline world seed_world(ores::testing::scoped_database_helper& h) {
    auto gen = ores::testing::make_generation_context(h);
    auto& base = h.context();

    const auto maker = "engine-settings-maker";
    const auto decider = "engine-settings-decider";
    const auto maker_id = detail::write_account(h, maker);
    const auto decider_id = detail::write_account(h, decider);

    auto party = ores::refdata::generators::generate_synthetic_party(gen);
    party.change_reason_code = "system.test";
    for (const auto& e : ores::refdata::repository::party_repository{}.read_latest(base)) {
        if (e.tenant_id == party.tenant_id) {
            party.parent_party_id = e.id;
            break;
        }
    }
    ores::refdata::repository::party_repository{}.write(base, party);

    const auto ctx = base.with_party(base.tenant_id(), party.id, {party.id}, maker);

    auto write_configuration = [&](const std::string& name) {
        domain::configuration configuration;
        configuration.id = boost::uuids::random_generator{}();
        configuration.party_id = party.id;
        configuration.name = name;
        configuration.configuration_type_code = "engine_settings";
        configuration.owning_component = "ores.reporting";
        configuration.modified_by = maker;
        configuration.performed_by = ctx.service_account();
        configuration.change_reason_code = "system.test";
        repository::configuration_repository{}.write(ctx, configuration);
        return configuration.id;
    };

    const auto bound_configuration = write_configuration("Engine run settings a report binds");
    const auto draft_configuration = write_configuration("Engine run settings no report binds");

    const auto bound_id = detail::write_engine_settings(ctx, "bound", bound_configuration, party.id);
    const auto draft_id = detail::write_engine_settings(ctx, "draft", draft_configuration, party.id);

    // The report definition the binding names, and the binding itself: a
    // numbers-changing write is a proposal only where a report binds it.
    auto definition = ores::reporting::generators::generate_synthetic_report_definition(gen);
    definition.change_reason_code = "system.test";
    definition.report_type = "risk";
    definition.party_id = party.id;
    ores::reporting::repository::report_definition_repository{}.write(ctx, definition);

    domain::report_configuration binding;
    binding.id = boost::uuids::random_generator{}();
    binding.party_id = party.id;
    binding.report_definition_id = definition.id;
    binding.configuration_type_code = "engine_settings";
    binding.configuration_id = bound_configuration;
    binding.modified_by = maker;
    binding.performed_by = ctx.service_account();
    binding.change_reason_code = "system.test";
    ores::reporting::repository::report_configuration_repository{}.write(ctx, binding);

    return {.party_id = party.id,
            .definition_id = definition.id,
            .bound_id = bound_id,
            .draft_id = draft_id,
            .maker_id = maker_id,
            .decider_id = decider_id,
            .maker = maker,
            .decider = decider,
            .ctx = ctx};
}

/**
 * @brief The fixings row in force for a configuration, or nothing when it has
 * none.
 */
inline std::optional<domain::engine_settings_fixings>
fixings_of(const ores::database::context& ctx, const boost::uuids::uuid& engine_settings_id) {
    const auto rows = repository::engine_settings_fixings_repository{}.read_latest_by_engine_settings_id(
        ctx, boost::uuids::to_string(engine_settings_id), 0, 1000);
    if (rows.empty())
        return std::nullopt;
    return rows.front();
}

}

#endif
