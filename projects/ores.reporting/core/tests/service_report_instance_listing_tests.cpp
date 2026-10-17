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
#include "ores.database/domain/context.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/generators/party_generator.hpp"
#include "ores.refdata.core/repository/party_repository.hpp"
#include "ores.reporting.api/generators/report_instance_generator.hpp"
#include "ores.reporting.api/messaging/report_instance_protocol.hpp"
#include "ores.reporting.core/repository/report_instance_repository.hpp"
#include "ores.reporting.core/service/report_instance_service.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include "ores.utility/generation/generation_context.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "report_instance_test_fixture.hpp"
#include <boost/log/sources/severity_feature.hpp>
#include <boost/uuid/uuid.hpp>
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <string_view>
#include <vector>

// The decision these pin: what a list of report instances shows when the
// caller states no kind. A package instance is a throwaway a developer makes
// to get a report's packages off the grid, so it is not one of the runs the
// list is about; the default view is production and the others are asked for
// by name.

namespace {

const std::string_view test_suite("ores.reporting.tests");
const std::string tags("[service][integration]");

// The seeded catalogue codes. A report instance's kind is reference data, so
// the test names a seeded code rather than writing a row of its own.
const std::string production_kind("production");
const std::string package_kind("package");

// Report instance writes are party-scoped: the session-level
// app.current_party_id GUC must be set before writing.
ores::database::context
write_test_party_and_scope_context(ores::testing::scoped_database_helper& h,
                                   ores::utility::generation::generation_context& ctx) {
    using ores::refdata::repository::party_repository;
    party_repository party_repo;
    auto party = ores::refdata::generators::generate_synthetic_party(ctx);
    party.change_reason_code = "system.test";
    auto existing = party_repo.read_latest(h.context());
    for (const auto& e : existing) {
        if (e.tenant_id == party.tenant_id) {
            party.parent_party_id = e.id;
            break;
        }
    }
    party_repo.write(h.context(), party);
    return h.context().with_party(h.tenant_id(), party.id, {party.id}, h.db_user());
}

bool lists(const std::vector<ores::reporting::domain::report_instance>& instances,
           const boost::uuids::uuid& id) {
    for (const auto& instance : instances) {
        if (instance.id == id)
            return true;
    }
    return false;
}

}

using namespace ores::logging;
using namespace ores::reporting::generators;
using ores::reporting::messaging::list_report_instances_request;
using ores::reporting::messaging::report_instances_filter;
using ores::reporting::repository::report_instance_repository;
using ores::reporting::service::report_instance_service;
using ores::testing::scoped_database_helper;

TEST_CASE("a list shows production instances unless the caller asks for a kind", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    auto party_ctx = write_test_party_and_scope_context(h, ctx);

    auto production = generate_synthetic_report_instance(ctx);
    production.change_reason_code = "system.test";
    production.party_id = *party_ctx.party_id();
    production.kind = production_kind;

    auto package = generate_synthetic_report_instance(ctx);
    package.change_reason_code = "system.test";
    package.party_id = *party_ctx.party_id();
    package.kind = package_kind;

    REQUIRE(production.id != package.id);

    // An instance names the trigger that admitted it, and the database holds
    // that as a real foreign key, so the trigger each instance names is written
    // first.
    ores::reporting::tests::write_trigger_for(party_ctx, ctx, production);
    ores::reporting::tests::write_trigger_for(party_ctx, ctx, package);

    report_instance_repository repo;
    repo.write(party_ctx, production);
    repo.write(party_ctx, package);

    report_instance_service service(party_ctx);

    // 1. The default: the caller states no filter at all, as the shell's plain
    // list does.
    list_report_instances_request plain;
    plain.limit = 1000;
    const auto defaulted = service.list_report_instances(plain);
    CHECK(lists(defaulted.instances, production.id));
    CHECK_FALSE(lists(defaulted.instances, package.id));

    // 2. Asked for by name, the package instance is there and the production
    // one is not.
    list_report_instances_request packages;
    packages.limit = 1000;
    packages.filter = report_instances_filter{};
    packages.filter->kind = package_kind;
    const auto only_packages = service.list_report_instances(packages);
    CHECK(lists(only_packages.instances, package.id));
    CHECK_FALSE(lists(only_packages.instances, production.id));
}

TEST_CASE("a list of runs answers only the definitions it is asked for", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    auto party_ctx = write_test_party_and_scope_context(h, ctx);

    // Two definitions a party owns, one run of each. The definition ids are
    // stated by the test rather than generated, so the filter names them.
    const auto first_definition = ctx.generate_uuid();
    const auto second_definition = ctx.generate_uuid();

    auto first = generate_synthetic_report_instance(ctx);
    first.change_reason_code = "system.test";
    first.party_id = *party_ctx.party_id();
    first.definition_id = first_definition;

    auto second = generate_synthetic_report_instance(ctx);
    second.change_reason_code = "system.test";
    second.party_id = *party_ctx.party_id();
    second.definition_id = second_definition;

    ores::reporting::tests::write_trigger_for(party_ctx, ctx, first);
    ores::reporting::tests::write_trigger_for(party_ctx, ctx, second);

    report_instance_repository repo;
    repo.write(party_ctx, first);
    repo.write(party_ctx, second);

    report_instance_service service(party_ctx);

    // The screen knows which definitions its party owns, so it asks for their
    // runs by id rather than for the party's whole run history.
    list_report_instances_request by_definition;
    by_definition.limit = 1000;
    by_definition.filter = report_instances_filter{};
    by_definition.filter->definition_id = first_definition;
    const auto only_first = service.list_report_instances(by_definition);
    CHECK(lists(only_first.instances, first.id));
    CHECK_FALSE(lists(only_first.instances, second.id));

    // The one-of form carries every definition of the party in one call.
    list_report_instances_request by_definitions;
    by_definitions.limit = 1000;
    by_definitions.filter = report_instances_filter{};
    by_definitions.filter->definition_id_one_of =
        std::vector<boost::uuids::uuid>{first_definition, second_definition};
    const auto both = service.list_report_instances(by_definitions);
    CHECK(lists(both.instances, first.id));
    CHECK(lists(both.instances, second.id));
}

TEST_CASE("two definitions and three runs read as two rows, each the latest run", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    auto party_ctx = write_test_party_and_scope_context(h, ctx);

    // A party owns two definitions and has run one of them twice, so three runs
    // in all. The third definition has never run.
    const auto run_often = ctx.generate_uuid();
    const auto run_once = ctx.generate_uuid();
    const auto never_run = ctx.generate_uuid();

    const auto now = std::chrono::system_clock::now();
    const auto older_run = now - std::chrono::hours(24);
    const auto latest_run = now - std::chrono::hours(1);
    const auto only_run = now - std::chrono::hours(2);

    auto first = generate_synthetic_report_instance(ctx);
    first.change_reason_code = "system.test";
    first.party_id = *party_ctx.party_id();
    first.definition_id = run_often;
    first.started_at = older_run;

    auto second = generate_synthetic_report_instance(ctx);
    second.change_reason_code = "system.test";
    second.party_id = *party_ctx.party_id();
    second.definition_id = run_often;
    second.started_at = latest_run;

    auto third = generate_synthetic_report_instance(ctx);
    third.change_reason_code = "system.test";
    third.party_id = *party_ctx.party_id();
    third.definition_id = run_once;
    third.started_at = only_run;

    ores::reporting::tests::write_trigger_for(party_ctx, ctx, first);
    ores::reporting::tests::write_trigger_for(party_ctx, ctx, second);
    ores::reporting::tests::write_trigger_for(party_ctx, ctx, third);

    report_instance_repository repo;
    repo.write(party_ctx, first);
    repo.write(party_ctx, second);
    repo.write(party_ctx, third);

    report_instance_service service(party_ctx);

    // The screen asks once, with the definitions it drew, and reads one row per
    // definition. A definition that has never run is absent from the reply, so
    // the screen states it as never run rather than as clean.
    ores::reporting::messaging::get_latest_report_instances_by_definition_request request;
    request.definition_ids = std::vector<boost::uuids::uuid>{run_often, run_once, never_run};
    const auto response = service.get_latest_report_instances_by_definition(request);
    REQUIRE(response.instances.size() == 2);

    const auto row_for = [&response](const boost::uuids::uuid& definition_id)
        -> std::optional<ores::reporting::domain::report_instance> {
        for (const auto& instance : response.instances) {
            if (instance.definition_id == definition_id)
                return instance;
        }
        return std::nullopt;
    };

    const auto often_row = row_for(run_often);
    REQUIRE(often_row.has_value());
    CHECK(often_row->id == second.id);

    const auto once_row = row_for(run_once);
    REQUIRE(once_row.has_value());
    CHECK(once_row->id == third.id);
}
