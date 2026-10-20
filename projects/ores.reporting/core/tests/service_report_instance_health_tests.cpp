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
#include "ores.reporting.api/messaging/report_instance_protocol.hpp"
#include "ores.reporting.core/service/report_error_recorder.hpp"
#include "ores.reporting.core/service/report_instance_service.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include "ores.utility/generation/generation_context.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "ores.utility/uuid/tenant_id.hpp"
#include "report_instance_test_fixture.hpp"
#include <boost/log/sources/severity_feature.hpp>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <catch2/catch_test_macros.hpp>
#include <map>
#include <string>
#include <string_view>
#include <vector>

// The decision these pin: the health word a screen draws is the server's, and
// every state the lifecycle machine seeds has one answer. A completed run that
// recorded an error is not clean; a state with no word yet says so by answering
// no word rather than by borrowing a nearby one.

namespace {

const std::string_view test_suite("ores.reporting.tests");
const std::string tags("[service][database]");

std::string word_of(const ores::reporting::messaging::get_report_instance_health_response& response,
                    const boost::uuids::uuid& instance_id) {
    for (const auto& row : response.health) {
        if (row.report_instance_id == instance_id)
            return row.health;
    }
    return "<no row>";
}

std::string
state_of(const ores::reporting::messaging::get_report_instance_health_response& response,
         const boost::uuids::uuid& instance_id) {
    for (const auto& row : response.health) {
        if (row.report_instance_id == instance_id)
            return row.state_name;
    }
    return "<no row>";
}

}

using ores::testing::scoped_database_helper;

TEST_CASE("each lifecycle state reads as the health word the screen draws", tags) {
    auto lg(ores::logging::make_logger(test_suite));

    scoped_database_helper h;
    auto gen = ores::testing::make_generation_context(h);
    const auto ctx = ores::reporting::tests::write_party_and_scope_context(h, gen);
    const auto states = ores::reporting::tests::lifecycle_states(
        ctx.with_tenant(ores::utility::uuid::tenant_id::system(), h.db_user()), lg);

    // Every state the machine seeds is answered here, including the two with no
    // word yet: a cancelled or skipped run must not read as failed or as clean.
    REQUIRE(states.contains("pending"));
    REQUIRE(states.contains("queued"));
    REQUIRE(states.contains("running"));
    REQUIRE(states.contains("completed"));
    REQUIRE(states.contains("failed"));
    REQUIRE(states.contains("cancelled"));
    REQUIRE(states.contains("skipped"));

    // Two completed runs, because a completed run is not necessarily clean:
    // one recorded no error and the other recorded one.
    struct expectation {
        std::string key;
        std::string state;
        bool with_error;
        std::string health;
    };
    const std::vector<expectation> expected{
        {"pending", "pending", false, "running"},
        {"queued", "queued", false, "running"},
        {"running", "running", false, "running"},
        {"clean", "completed", false, "completed clean"},
        {"with_error", "completed", true, "completed with errors"},
        {"failed", "failed", false, "failed"},
        {"cancelled", "cancelled", false, ""},
        {"skipped", "skipped", false, ""}};

    ores::reporting::service::report_error_recorder recorder(ctx);
    ores::reporting::service::report_instance_service service(ctx);

    // The path each run takes from a state the store admits on insert to the
    // state the case is about. A run is written in an initial state and moved
    // along the transitions the machine allows, which is the only way the store
    // accepts the later states.
    const std::map<std::string, std::vector<std::string>> paths{
        {"pending", {"pending"}},
        {"queued", {"queued"}},
        {"running", {"pending", "running"}},
        {"clean", {"pending", "running", "completed"}},
        {"with_error", {"pending", "running", "completed"}},
        {"failed", {"failed"}},
        {"cancelled", {"pending", "cancelled"}},
        {"skipped", {"skipped"}}};

    std::vector<boost::uuids::uuid> instance_ids;
    std::map<std::string, boost::uuids::uuid> run_of;
    for (const auto& entry : expected) {
        const auto& path = paths.at(entry.key);
        const auto id = ores::reporting::tests::write_run_in_state(
            ctx, h.db_user(), gen, states.at(path.front()));
        for (std::size_t step = 1; step < path.size(); ++step)
            ores::reporting::tests::move_to(service, id, states.at(path[step]));

        instance_ids.push_back(id);
        run_of.emplace(entry.key, id);
        if (!entry.with_error)
            continue;
        REQUIRE(
            recorder
                .record_engine_errors(boost::uuids::to_string(id),
                                      {{.severity = "error",
                                        .scope = "Analytics",
                                        .message = "Portfolio does not contain any built trades",
                                        .subject = "analytic",
                                        .subject_id = "PRICING",
                                        .detail = {}}})
                .empty());
    }

    ores::reporting::messaging::get_report_instance_health_request request;
    request.instance_ids = instance_ids;
    const auto response = service.get_report_instance_health(request);
    REQUIRE(response.health.size() == expected.size());

    for (const auto& entry : expected)
        CHECK(word_of(response, run_of.at(entry.key)) == entry.health);

    // The reply says what the word was derived from, so a word that is absent
    // is explained rather than left to the reader to guess.
    CHECK(state_of(response, run_of.at("cancelled")) == "cancelled");
    CHECK(state_of(response, run_of.at("with_error")) == "completed");
}
