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
#include "ores.reporting.api/messaging/report_error_event_protocol.hpp"
#include "ores.reporting.core/repository/report_instance_kind_repository.hpp"
#include "ores.reporting.core/repository/report_instance_repository.hpp"
#include "ores.reporting.core/service/report_error_event_service.hpp"
#include "ores.reporting.core/service/report_error_recorder.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include "ores.utility/generation/generation_context.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "ores.utility/uuid/tenant_id.hpp"
#include "report_instance_test_fixture.hpp"
#include "test_suite.hpp"
#include <boost/log/sources/severity_feature.hpp>
#include <boost/uuid/string_generator.hpp>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <catch2/catch_test_macros.hpp>
#include <string>
#include <string_view>
#include <vector>

// The decision these pin: a screen that draws one row per run pays one row for
// a run's problems, not one row per problem. The count is the store's, and the
// split into errors and warnings is the one the screen's Errors cell draws.

namespace {

/**
 * @brief A party of this test's own, with the session scoped to it.
 *
 * A run and the errors against it are party-scoped, so the session-level
 * app.current_party_id has to be set before either is written.
 */
ores::database::context
write_test_party_and_scope_context(ores::testing::scoped_database_helper& h,
                                   ores::utility::generation::generation_context& gen) {
    using ores::refdata::repository::party_repository;
    party_repository parties;
    auto party = ores::refdata::generators::generate_synthetic_party(gen);
    party.change_reason_code = "system.test";
    for (const auto& existing : parties.read_latest(h.context())) {
        if (existing.tenant_id == party.tenant_id) {
            party.parent_party_id = existing.id;
            break;
        }
    }
    parties.write(h.context(), party);
    return h.context().with_party(h.tenant_id(), party.id, {party.id}, h.db_user());
}

/**
 * @brief Stores a report instance and answers its id.
 *
 * The errors name the run with a real foreign key, so the run is written
 * before the recorder is given anything to store.
 */
std::string write_instance(const ores::database::context& party_ctx,
                           const std::string& db_user,
                           ores::utility::generation::generation_context& gen) {
    auto instance = ores::reporting::generators::generate_synthetic_report_instance(gen);
    instance.party_id = *party_ctx.party_id();

    // The kind catalogue is system-tenant reference data, so it is read under
    // that tenant, exactly as the run's own insert trigger resolves it.
    ores::reporting::repository::report_instance_kind_repository kinds;
    const auto catalogue =
        kinds.read_latest(party_ctx.with_tenant(ores::utility::uuid::tenant_id::system(), db_user));
    REQUIRE_FALSE(catalogue.empty());
    instance.kind = catalogue.front().code;
    instance.change_reason_code = "system.test";
    instance.trigger_id = ores::reporting::tests::write_trigger_for(party_ctx, gen, instance).id;

    ores::reporting::repository::report_instance_repository().write(party_ctx, instance);
    return boost::uuids::to_string(instance.id);
}

}

using ores::testing::scoped_database_helper;

TEST_CASE("the problems of a run read as one row split into errors and warnings",
          "[service][database]") {
    auto lg(ores::logging::make_logger(test_suite));

    scoped_database_helper h;
    auto gen = ores::testing::make_generation_context(h);
    const auto ctx = write_test_party_and_scope_context(h, gen);

    // Two runs of the party: one that recorded problems and one that did not,
    // so the reply shows both what a count is and what its absence means.
    const auto with_problems = write_instance(ctx, h.db_user(), gen);
    const auto clean = write_instance(ctx, h.db_user(), gen);

    ores::reporting::service::report_error_recorder recorder(ctx);
    std::vector<ores::reporting::service::engine_log_error> problems;
    for (int i = 0; i < 3; ++i) {
        problems.push_back({.severity = "error",
                            .scope = "Trade",
                            .message = "did not find object of type yield curve",
                            .subject = "trade",
                            .subject_id = "033b4053-fe1d-5901-bc2f-7d95afe91f0" + std::to_string(i),
                            .detail = R"({"exceptionType":"Error building trade"})"});
    }
    for (int i = 0; i < 2; ++i) {
        problems.push_back({.severity = "warning",
                            .scope = "Curve",
                            .message = "no fixings for the curve date",
                            .subject = "curve",
                            .subject_id = "GBP-SONIA",
                            .detail = {}});
    }
    REQUIRE(recorder.record_engine_errors(with_problems, problems).empty());

    ores::reporting::service::report_error_event_service service(ctx);
    ores::reporting::messaging::get_report_error_counts_request request;
    request.report_instance_ids = std::vector<boost::uuids::uuid>{
        boost::uuids::string_generator()(with_problems), boost::uuids::string_generator()(clean)};
    const auto response = service.get_report_error_counts(request);

    // One row for the run with problems, nothing for the run without: the
    // screen draws the absence as no problems.
    REQUIRE(response.counts.size() == 1);
    CHECK(response.counts[0].report_instance_id == boost::uuids::string_generator()(with_problems));
    CHECK(response.counts[0].errors == 3);
    CHECK(response.counts[0].warnings == 2);
}
