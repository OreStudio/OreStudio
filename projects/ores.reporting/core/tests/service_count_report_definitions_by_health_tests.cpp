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
#include "ores.logging/make_logger.hpp"
#include "ores.reporting.api/generators/report_definition_generator.hpp"
#include "ores.reporting.api/messaging/report_instance_protocol.hpp"
#include "ores.reporting.core/repository/report_definition_repository.hpp"
#include "ores.reporting.core/service/report_instance_service.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include "ores.utility/generation/generation_context.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include "report_instance_test_fixture.hpp"
#include "test_suite.hpp"
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid.hpp>
#include <catch2/catch_test_macros.hpp>
#include <cstdint>
#include <map>
#include <optional>
#include <string>
#include <string_view>
#include <vector>

// The decision these pin: the tally is the server's and counts the definitions
// a list would read. A definition with no run is "never run", a run whose state
// has no word yet is counted apart so the counts add up, and a party or a
// search narrows the tally exactly as it narrows the list.

namespace {

using ores::testing::scoped_database_helper;

boost::uuids::uuid write_definition(const ores::database::context& ctx,
                                    ores::utility::generation::generation_context& gen,
                                    const std::string& name,
                                    const std::string& description) {
    auto definition = ores::reporting::generators::generate_synthetic_report_definition(gen);
    definition.change_reason_code = "system.test";
    definition.report_type = "risk";
    definition.party_id = *ctx.party_id();
    definition.name = name;
    definition.description = description;
    ores::reporting::repository::report_definition_repository().write(ctx, definition);
    return definition.id;
}

std::map<std::string, std::int64_t>
by_word(const ores::reporting::messaging::count_report_definitions_by_health_response& response) {
    std::map<std::string, std::int64_t> counts;
    for (const auto& entry : response.counts)
        counts.emplace(entry.health, entry.count);
    return counts;
}

}

TEST_CASE("the tally counts each definition at the word its latest run reads as", "[service][database]") {
    auto lg(ores::logging::make_logger(test_suite));

    scoped_database_helper h;
    auto gen = ores::testing::make_generation_context(h);
    const auto ctx = ores::reporting::tests::write_party_and_scope_context(h, gen);
    const auto states = ores::reporting::tests::lifecycle_states(
        ctx.with_tenant(ores::utility::uuid::tenant_id::system(), h.db_user()), lg);
    ores::reporting::service::report_instance_service service(ctx);

    // One definition per word, a definition that never ran, and one whose only
    // run was cancelled, which has no word yet.
    write_definition(ctx, gen, "tally never", "has no run");
    const auto running = write_definition(ctx, gen, "tally running", "is in flight");
    const auto clean = write_definition(ctx, gen, "tally clean", "finished without errors");
    const auto failed = write_definition(ctx, gen, "tally failed", "did not finish");
    const auto cancelled = write_definition(ctx, gen, "tally cancelled", "was stopped");

    const auto run = [&](const boost::uuids::uuid& definition,
                         const std::vector<std::string>& path) {
        const auto id = ores::reporting::tests::write_run_in_state(
            ctx, h.db_user(), gen, states.at(path.front()), definition);
        for (std::size_t step = 1; step < path.size(); ++step)
            ores::reporting::tests::move_to(service, id, states.at(path[step]));
    };
    run(running, {"pending", "running"});
    run(clean, {"pending", "running", "completed"});
    run(failed, {"failed"});
    run(cancelled, {"pending", "cancelled"});

    const auto response = service.count_report_definitions_by_health({});
    REQUIRE(response.result.outcome == ores::utility::domain::outcome::ok);

    // The words come in the order the screen draws them, the empty one last.
    REQUIRE(response.counts.size() == 6);
    CHECK(response.counts.front().health == "failed");
    CHECK(response.counts.back().health.empty());

    const auto counts = by_word(response);
    CHECK(counts.at("failed") == 1);
    CHECK(counts.at("completed with errors") == 0);
    CHECK(counts.at("running") == 1);
    CHECK(counts.at("never run") == 1);
    CHECK(counts.at("completed clean") == 1);
    CHECK(counts.at("") == 1);
    CHECK(response.total == 5);
}

TEST_CASE("a search and a party narrow the tally as they narrow the list", "[service][database]") {
    scoped_database_helper h;
    auto gen = ores::testing::make_generation_context(h);
    const auto ctx = ores::reporting::tests::write_party_and_scope_context(h, gen);
    ores::reporting::service::report_instance_service service(ctx);

    write_definition(ctx, gen, "tally alpha", "the first one");
    write_definition(ctx, gen, "tally beta", "the second one");
    write_definition(ctx, gen, "tally gamma", "mentions the beta word");

    ores::reporting::messaging::count_report_definitions_by_health_request request;
    CHECK(service.count_report_definitions_by_health(request).total == 3);

    // The search reads the name and the description, as the list does.
    request.search = "beta";
    CHECK(service.count_report_definitions_by_health(request).total == 2);

    request.search = "no such report";
    const auto none = service.count_report_definitions_by_health(request);
    CHECK(none.total == 0);
    CHECK(by_word(none).at("never run") == 0);

    request.search = std::nullopt;
    request.party_id = *ctx.party_id();
    CHECK(service.count_report_definitions_by_health(request).total == 3);

    // A party that owns none of them counts none, whatever the search.
    request.party_id = boost::uuids::random_generator()();
    CHECK(service.count_report_definitions_by_health(request).total == 0);
}
