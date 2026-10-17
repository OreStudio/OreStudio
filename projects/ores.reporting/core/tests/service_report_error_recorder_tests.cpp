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
#include "ores.refdata.api/generators/party_generator.hpp"
#include "ores.refdata.core/repository/party_repository.hpp"
#include "ores.reporting.api/generators/report_instance_generator.hpp"
#include "ores.reporting.core/repository/report_error_event_repository.hpp"
#include "ores.reporting.core/repository/report_instance_kind_repository.hpp"
#include "ores.reporting.core/repository/report_instance_repository.hpp"
#include "ores.reporting.core/service/report_error_recorder.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include "ores.utility/generation/generation_context.hpp"
#include "report_instance_test_fixture.hpp"
#include <algorithm>
#include <boost/json.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <catch2/catch_test_macros.hpp>
#include <string>
#include <vector>

namespace {

using ores::testing::scoped_database_helper;

const std::string tags("[service][database]");

/**
 * @brief A party of this test's own, with the session scoped to it.
 *
 * A report instance and the errors against it are party-scoped, so the
 * session-level app.current_party_id has to be set before either is written.
 */
ores::database::context
write_test_party_and_scope_context(scoped_database_helper& h,
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
 * A report error names the run it belongs to with a real foreign key, so the
 * run has to exist before the recorder is given anything to write.
 */
std::string write_instance(const ores::database::context& party_ctx,
                           const std::string& db_user,
                           ores::utility::generation::generation_context& gen) {
    const auto& ctx = party_ctx;
    auto instance = ores::reporting::generators::generate_synthetic_report_instance(gen);
    // The run and the trigger that admits it are party-scoped, so both carry
    // the session party: a run owned by another party is a run this session
    // cannot see, and the trigger's insert is refused.
    instance.party_id = *party_ctx.party_id();

    // The kind catalogue is system-tenant reference data, so it is read under
    // that tenant: the row's own tenant sees the seeded vocabulary only
    // through this context, exactly as the insert trigger resolves it.
    ores::reporting::repository::report_instance_kind_repository kinds;
    const auto catalogue =
        kinds.read_latest(ctx.with_tenant(ores::utility::uuid::tenant_id::system(), db_user));
    REQUIRE_FALSE(catalogue.empty());
    instance.kind = catalogue.front().code;
    instance.change_reason_code = "system.test";
    instance.trigger_id = ores::reporting::tests::write_trigger_for(ctx, gen, instance).id;

    ores::reporting::repository::report_instance_repository().write(ctx, instance);
    return boost::uuids::to_string(instance.id);
}

std::vector<ores::reporting::domain::report_error_event>
errors_of(const ores::database::context& ctx, const std::string& instance_id) {
    ores::reporting::repository::report_error_event_repository errors;
    return errors.read_latest_by_report_instance_id(ctx, instance_id, 0, 100);
}

}

TEST_CASE("a run that failed its analytics carries one error row per trade it could not build",
          tags) {
    scoped_database_helper h;
    auto gen = ores::testing::make_generation_context(h);
    const auto ctx = write_test_party_and_scope_context(h, gen);
    const auto instance_id = write_instance(ctx, h.db_user(), gen);

    ores::reporting::service::report_error_recorder recorder(ctx);
    std::vector<ores::reporting::service::engine_log_error> errors;
    for (int i = 0; i < 3; ++i) {
        errors.push_back({.severity = "error",
                          .scope = "Trade",
                          .message = "did not find object of type yield curve",
                          .subject = "trade",
                          .subject_id = "033b4053-fe1d-5901-bc2f-7d95afe91f0" + std::to_string(i),
                          .detail = R"({"exceptionType":"Error building trade"})"});
    }
    errors.push_back({.severity = "error",
                      .scope = "Analytics",
                      .message = "Portfolio does not contain any built trades",
                      .subject = "analytic",
                      .subject_id = "PRICING",
                      .detail = {}});

    CHECK(recorder.record_engine_errors(instance_id, errors).empty());

    const auto rows = errors_of(ctx, instance_id);
    REQUIRE(rows.size() == 4);

    // Found by what each row says rather than by its position: a list read is
    // not a promise about the order the rows come back in.
    const auto trade = std::ranges::find_if(rows, [](const auto& row) {
        return row.scope_code == "Trade";
    });
    REQUIRE(trade != rows.end());
    CHECK(trade->severity == "error");
    CHECK(trade->subject == "trade");
    // Three trades were written and three rows came back, one per trade: a
    // count of trade-scoped errors is how a reader says three of eight trades
    // failed, so the count is what is checked and not which row came first.
    const auto trade_rows = std::ranges::count_if(rows, [](const auto& row) {
        return row.scope_code == "Trade";
    });
    CHECK(trade_rows == 3);
    CHECK(std::ranges::any_of(rows, [](const auto& row) {
        return row.subject_id == std::optional<std::string>("033b4053-fe1d-5901-bc2f-7d95afe91f00");
    }));
    // The detail survives the round trip as the engine's own sub-fields, and
    // the serialiser's spacing is not what a reader of it depends on.
    CHECK(boost::json::parse(trade->detail).as_object().at("exceptionType") ==
          "Error building trade");
    const auto analytic = std::ranges::find_if(rows, [](const auto& row) {
        return row.scope_code == "Analytics";
    });
    REQUIRE(analytic != rows.end());
    CHECK(analytic->subject == "analytic");
    CHECK(analytic->subject_id == "PRICING");
    REQUIRE(ctx.party_id().has_value());
    CHECK(analytic->party_id == *ctx.party_id());
}

TEST_CASE("a run the platform could not complete carries a report-scoped error and no subject",
          tags) {
    scoped_database_helper h;
    auto gen = ores::testing::make_generation_context(h);
    const auto ctx = write_test_party_and_scope_context(h, gen);
    const auto instance_id = write_instance(ctx, h.db_user(), gen);

    ores::reporting::service::report_error_recorder recorder(ctx);
    CHECK(recorder
              .record_run_failure(instance_id,
                                  "Report execution failed during result collection")
              .empty());

    const auto rows = errors_of(ctx, instance_id);
    REQUIRE(rows.size() == 1);
    CHECK(rows.front().scope_code == "Report");
    CHECK(rows.front().severity == "error");
    CHECK(rows.front().message == "Report execution failed during result collection");
    CHECK_FALSE(rows.front().subject.has_value());
    CHECK_FALSE(rows.front().subject_id.has_value());
    // The error carries the run's party, not the caller's: the row-level policy
    // on this table is what keeps one party's failures out of another's sight,
    // and it reads this column.
    REQUIRE(ctx.party_id().has_value());
    CHECK(rows.front().party_id == *ctx.party_id());
}

TEST_CASE("a run that succeeded records no error at all", tags) {
    scoped_database_helper h;
    auto gen = ores::testing::make_generation_context(h);
    const auto ctx = write_test_party_and_scope_context(h, gen);
    const auto instance_id = write_instance(ctx, h.db_user(), gen);

    ores::reporting::service::report_error_recorder recorder(ctx);
    CHECK(recorder.record_engine_errors(instance_id, {}).empty());

    CHECK(errors_of(ctx, instance_id).empty());
}

TEST_CASE("an error whose scope the vocabulary does not hold is refused rather than stored",
          tags) {
    scoped_database_helper h;
    auto gen = ores::testing::make_generation_context(h);
    const auto ctx = write_test_party_and_scope_context(h, gen);
    const auto instance_id = write_instance(ctx, h.db_user(), gen);

    ores::reporting::service::report_error_recorder recorder(ctx);
    const std::vector<ores::reporting::service::engine_log_error> errors{
        {.severity = "error",
         .scope = "Something the engine grew",
         .message = "a scope no reader can place",
         .subject = "trade",
         .subject_id = "033b4053-fe1d-5901-bc2f-7d95afe91f00",
         .detail = {}}};

    const auto failure = recorder.record_engine_errors(instance_id, errors);
    CHECK_FALSE(failure.empty());
    CHECK(failure.find("Something the engine grew") != std::string::npos);
    CHECK(errors_of(ctx, instance_id).empty());
}

TEST_CASE("an error carries the party of its run and not the party of the caller", tags) {
    scoped_database_helper h;
    auto gen = ores::testing::make_generation_context(h);

    // The run belongs to a party the session is not acting as, and both parties
    // are visible to it. The recorder writes the run's own party rather than
    // stamping the session's, so an error the run's own party cannot read is
    // never written, which is the leak the party policy exists to close.
    const auto run_ctx = write_test_party_and_scope_context(h, gen);
    const auto caller_ctx = write_test_party_and_scope_context(h, gen);
    const auto run_party_id = *run_ctx.party_id();
    const auto caller_party_id = *caller_ctx.party_id();
    REQUIRE(run_party_id != caller_party_id);

    const auto instance_id = write_instance(run_ctx, h.db_user(), gen);

    const auto mixed = caller_ctx.with_party(
        h.tenant_id(), caller_party_id, {run_party_id, caller_party_id}, h.db_user());
    ores::reporting::service::report_error_recorder recorder(mixed);
    CHECK(recorder
              .record_run_failure(instance_id, "Report execution failed during result collection")
              .empty());

    const auto rows = errors_of(mixed, instance_id);
    REQUIRE(rows.size() == 1);
    CHECK(rows.front().party_id == run_party_id);
    CHECK(rows.front().party_id != caller_party_id);
}
