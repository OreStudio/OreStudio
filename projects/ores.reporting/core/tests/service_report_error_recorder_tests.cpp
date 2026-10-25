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
#include "ores.platform/time/datetime.hpp"
#include "ores.refdata.api/generators/party_generator.hpp"
#include "ores.refdata.core/repository/party_repository.hpp"
#include "ores.reporting.api/generators/report_instance_generator.hpp"
#include "ores.reporting.core/repository/report_error_event_repository.hpp"
#include "ores.reporting.core/repository/report_input_bundle_entity.hpp"
#include "ores.reporting.core/repository/report_input_bundle_repository.hpp"
#include "ores.reporting.core/repository/report_instance_kind_repository.hpp"
#include "ores.reporting.core/repository/report_instance_repository.hpp"
#include "ores.reporting.core/service/report_error_recorder.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include "ores.utility/generation/generation_context.hpp"
#include "report_instance_test_fixture.hpp"
#include <boost/json.hpp>
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <optional>
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
 * run has to exist before the recorder is given anything to write. A run that
 * priced carries the date it priced at, which the errors of its trades inherit.
 */
std::string
write_instance(const ores::database::context& party_ctx,
               const std::string& db_user,
               ores::utility::generation::generation_context& gen,
               std::optional<std::chrono::system_clock::time_point> priced_at = std::nullopt) {
    const auto& ctx = party_ctx;
    auto instance = ores::reporting::generators::generate_synthetic_report_instance(gen);
    // The run and the trigger that admits it are party-scoped, so both carry
    // the session party: a run owned by another party is a run this session
    // cannot see, and the trigger's insert is refused.
    instance.party_id = *party_ctx.party_id();
    instance.priced_at = priced_at;

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

/**
 * @brief Stores the run's input bundle and answers its id.
 *
 * The bundle is what the gather step wrote: the storage keys of the gathered
 * trades, and the manifest that names the version of each trade the run read.
 */
std::string write_bundle(const ores::database::context& ctx,
                         const std::string& instance_id,
                         const std::string& trade_versions) {
    ores::reporting::repository::report_input_bundle_entity bundle;
    bundle.id = boost::uuids::to_string(boost::uuids::random_generator()());
    bundle.tenant_id = ctx.tenant_id().to_string();
    bundle.report_instance_id = instance_id;
    bundle.definition_id = boost::uuids::to_string(boost::uuids::random_generator()());
    bundle.trades_storage_key = instance_id + "/trades.msgpack";
    bundle.market_data_storage_key = instance_id + "/market_data.msgpack";
    bundle.fixings_storage_key = instance_id + "/fixings.msgpack";
    bundle.trade_count = 3;
    bundle.trade_versions = trade_versions;
    bundle.series_count = 2;
    bundle.created_at =
        ores::platform::time::datetime::to_db_string(std::chrono::system_clock::now());

    ores::reporting::repository::report_input_bundle_repository bundles;
    bundles.create(ctx, bundle);
    return bundle.id.value();
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
    const auto trade =
        std::ranges::find_if(rows, [](const auto& row) { return row.scope_code == "Trade"; });
    REQUIRE(trade != rows.end());
    CHECK(trade->severity == "error");
    CHECK(trade->subject == "trade");
    // Three trades were written and three rows came back, one per trade: a
    // count of trade-scoped errors is how a reader says three of eight trades
    // failed, so the count is what is checked and not which row came first.
    const auto trade_rows =
        std::ranges::count_if(rows, [](const auto& row) { return row.scope_code == "Trade"; });
    CHECK(trade_rows == 3);
    CHECK(std::ranges::any_of(rows, [](const auto& row) {
        return row.subject_id == std::optional<std::string>("033b4053-fe1d-5901-bc2f-7d95afe91f00");
    }));
    // The detail survives the round trip as the engine's own sub-fields, and
    // the serialiser's spacing is not what a reader of it depends on.
    CHECK(boost::json::parse(trade->detail).as_object().at("exceptionType") ==
          "Error building trade");
    const auto analytic =
        std::ranges::find_if(rows, [](const auto& row) { return row.scope_code == "Analytics"; });
    REQUIRE(analytic != rows.end());
    CHECK(analytic->subject == "analytic");
    CHECK(analytic->subject_id == "PRICING");
    REQUIRE(ctx.party_id().has_value());
    CHECK(analytic->party_id == *ctx.party_id());
}

TEST_CASE("an error about a trade carries the version the run read and the date it read it at",
          tags) {
    scoped_database_helper h;
    auto gen = ores::testing::make_generation_context(h);
    const auto ctx = write_test_party_and_scope_context(h, gen);
    const auto priced_at =
        ores::platform::time::datetime::from_db_string("2026-05-05 00:00:00.000000");
    const auto instance_id = write_instance(ctx, h.db_user(), gen, priced_at);
    const std::string trade_id = "033b4053-fe1d-5901-bc2f-7d95afe91f00";
    write_bundle(ctx, instance_id, std::string{"{\""} + trade_id + "\":3}");

    ores::reporting::service::report_error_recorder recorder(ctx);
    const std::vector<ores::reporting::service::engine_log_error> errors{
        {.severity = "error",
         .scope = "Trade",
         .message = "did not find object of type yield curve",
         .subject = "trade",
         .subject_id = trade_id,
         .detail = {}}};
    CHECK(recorder.record_engine_errors(instance_id, errors).empty());

    const auto rows = errors_of(ctx, instance_id);
    REQUIRE(rows.size() == 1);
    const auto& trade = rows.front();
    CHECK(trade.subject_id == std::optional<std::string>(trade_id));
    CHECK(trade.subject_version == std::optional<int>(3));
    REQUIRE(trade.subject_as_of.has_value());
    // The date says when the version the engine was handed was current, which
    // is the run's own pricing date, not the day the error was written.
    CHECK(ores::platform::time::datetime::to_db_string(*trade.subject_as_of) ==
          "2026-05-05 00:00:00.000000");
}

TEST_CASE("an error about a trade the manifest does not name is recorded with no version", tags) {
    scoped_database_helper h;
    auto gen = ores::testing::make_generation_context(h);
    const auto ctx = write_test_party_and_scope_context(h, gen);
    const auto instance_id = write_instance(
        ctx,
        h.db_user(),
        gen,
        ores::platform::time::datetime::from_db_string("2026-05-05 00:00:00.000000"));
    write_bundle(ctx, instance_id, R"({"033b4053-fe1d-5901-bc2f-7d95afe91f07":3})");

    ores::reporting::service::report_error_recorder recorder(ctx);
    const std::vector<ores::reporting::service::engine_log_error> errors{
        {.severity = "error",
         .scope = "Trade",
         .message = "did not find object of type yield curve",
         .subject = "trade",
         .subject_id = "033b4053-fe1d-5901-bc2f-7d95afe91f00",
         .detail = {}}};
    CHECK(recorder.record_engine_errors(instance_id, errors).empty());

    const auto rows = errors_of(ctx, instance_id);
    REQUIRE(rows.size() == 1);
    // The trade is what the reader needs, and the manifest cannot say which
    // version it was, so the row says nothing rather than guessing.
    CHECK(rows.front().subject_id ==
          std::optional<std::string>("033b4053-fe1d-5901-bc2f-7d95afe91f00"));
    CHECK_FALSE(rows.front().subject_version.has_value());
    CHECK_FALSE(rows.front().subject_as_of.has_value());
}

TEST_CASE("a run the platform could not complete carries a report-scoped error and no subject",
          tags) {
    scoped_database_helper h;
    auto gen = ores::testing::make_generation_context(h);
    const auto ctx = write_test_party_and_scope_context(h, gen);
    const auto instance_id = write_instance(ctx, h.db_user(), gen);

    ores::reporting::service::report_error_recorder recorder(ctx);
    CHECK(
        recorder.record_run_failure(instance_id, "Report execution failed during result collection")
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

TEST_CASE("an error whose scope the vocabulary does not hold costs its own row only", tags) {
    scoped_database_helper h;
    auto gen = ores::testing::make_generation_context(h);
    const auto ctx = write_test_party_and_scope_context(h, gen);
    const auto instance_id = write_instance(ctx, h.db_user(), gen);

    ores::reporting::service::report_error_recorder recorder(ctx);
    const std::vector<ores::reporting::service::engine_log_error> errors{
        {.severity = "error",
         .scope = "Curve",
         .message = "yield curve building failed for curve(s) Yield/GBP/GBP1D",
         .subject = "curve",
         .subject_id = "[IndexCurve(GBP-SONIA,Yield/GBP/GBP1D)]",
         .cause = "Yield/GBP/GBP1D",
         .detail = {}},
        {.severity = "error",
         .scope = "Something the engine grew",
         .message = "a scope no reader can place",
         .subject = "trade",
         .subject_id = "033b4053-fe1d-5901-bc2f-7d95afe91f00",
         .detail = {}}};

    CHECK(recorder.record_engine_errors(instance_id, errors).empty());

    const auto rows = errors_of(ctx, instance_id);
    // The curve the vocabulary names, and a warning saying what it could not store.
    // The read is ordered by a random id, so the rows are found by what they say.
    const auto by_scope = [&rows](std::string_view scope) {
        return std::ranges::find(
            rows, scope, &ores::reporting::domain::report_error_event::scope_code);
    };
    const auto curve = by_scope("Curve");
    const auto warning = by_scope("Report");
    CHECK(rows.size() == 2);
    REQUIRE(curve != rows.end());
    CHECK(curve->cause == "Yield/GBP/GBP1D");
    REQUIRE(warning != rows.end());
    CHECK(warning->severity == "warning");
    CHECK(warning->message.find("Something the engine grew") != std::string::npos);
}

TEST_CASE("the run's failure row points at the reasons when they are there", tags) {
    scoped_database_helper h;
    auto gen = ores::testing::make_generation_context(h);
    const auto ctx = write_test_party_and_scope_context(h, gen);
    const auto instance_id = write_instance(ctx, h.db_user(), gen);

    ores::reporting::service::report_error_recorder recorder(ctx);
    const std::vector<ores::reporting::service::engine_log_error> errors{
        {.severity = "error",
         .scope = "Curve",
         .message = "yield curve building failed for curve(s) Yield/GBP/GBP1D",
         .subject = "curve",
         .subject_id = "[IndexCurve(GBP-SONIA,Yield/GBP/GBP1D)]",
         .cause = "Yield/GBP/GBP1D",
         .detail = {}}};
    CHECK(recorder.record_engine_errors(instance_id, errors).empty());
    CHECK(recorder.record_run_failure(instance_id, "The run's compute job failed with outcome 3")
              .empty());

    const auto rows = errors_of(ctx, instance_id);
    const auto failure = std::ranges::find_if(rows, [](const auto& row) {
        return row.scope_code == "Report" && row.severity == "error";
    });
    REQUIRE(failure != rows.end());
    CHECK(failure->message == "The run failed. Its reasons are the errors recorded against it.");
}

TEST_CASE("the run's failure row carries the whole reason when it is the only account", tags) {
    scoped_database_helper h;
    auto gen = ores::testing::make_generation_context(h);
    const auto ctx = write_test_party_and_scope_context(h, gen);
    const auto instance_id = write_instance(ctx, h.db_user(), gen);

    ores::reporting::service::report_error_recorder recorder(ctx);
    const std::string account =
        "The run's compute job failed with outcome 3: The engine's analytic failed. The engine "
        "reported 20 errors; the last 5 are: yield curve building failed for curve(s) "
        "Yield/GBP/GBP1D";
    CHECK(recorder.record_run_failure(instance_id, account).empty());

    const auto rows = errors_of(ctx, instance_id);
    REQUIRE(rows.size() == 1);
    CHECK(rows.front().message == account);
}

TEST_CASE("the run-level failure is stored beside the engine's own errors", tags) {
    scoped_database_helper h;
    auto gen = ores::testing::make_generation_context(h);
    const auto ctx = write_test_party_and_scope_context(h, gen);
    const auto instance_id = write_instance(ctx, h.db_user(), gen);

    ores::reporting::service::report_error_recorder recorder(ctx);
    const std::vector<ores::reporting::service::engine_log_error> errors{
        {.severity = "error",
         .scope = "Curve",
         .message = "yield curve building failed for curve(s) Yield/GBP/GBP1D",
         .subject = "curve",
         .subject_id = "[IndexCurve(GBP-SONIA,Yield/GBP/GBP1D)]",
         .cause = "Yield/GBP/GBP1D",
         .detail = {}}};

    CHECK(recorder.record_engine_errors(instance_id, errors).empty());
    CHECK(recorder.record_run_failure(instance_id, "The run's compute job failed with outcome 3")
              .empty());

    const auto rows = errors_of(ctx, instance_id);
    const auto curve =
        std::ranges::find_if(rows, [](const auto& row) { return row.scope_code == "Curve"; });
    const auto failure = std::ranges::find_if(rows, [](const auto& row) {
        return row.scope_code == "Report" && row.severity == "error";
    });
    CHECK(rows.size() == 2);
    CHECK(curve != rows.end());
    CHECK(failure != rows.end());
}

TEST_CASE("a batch whose scopes were all unplaceable does not block the next one", tags) {
    scoped_database_helper h;
    auto gen = ores::testing::make_generation_context(h);
    const auto ctx = write_test_party_and_scope_context(h, gen);
    const auto instance_id = write_instance(ctx, h.db_user(), gen);

    ores::reporting::service::report_error_recorder recorder(ctx);
    const std::vector<ores::reporting::service::engine_log_error> unplaceable{
        {.severity = "error",
         .scope = "Something the engine grew",
         .message = "a scope no reader can place",
         .subject = "trade",
         .subject_id = "033b4053-fe1d-5901-bc2f-7d95afe91f00",
         .detail = {}}};
    const std::vector<ores::reporting::service::engine_log_error> placeable{
        {.severity = "error",
         .scope = "Curve",
         .message = "yield curve building failed for curve(s) Yield/GBP/GBP1D",
         .subject = "curve",
         .subject_id = "[IndexCurve(GBP-SONIA,Yield/GBP/GBP1D)]",
         .cause = "Yield/GBP/GBP1D",
         .detail = {}}};

    CHECK(recorder.record_engine_errors(instance_id, unplaceable).empty());
    CHECK(recorder.record_engine_errors(instance_id, placeable).empty());

    const auto rows = errors_of(ctx, instance_id);
    CHECK(std::ranges::any_of(rows, [](const auto& row) { return row.scope_code == "Curve"; }));
}

TEST_CASE("a run's errors are stored once however often its output is read", tags) {
    scoped_database_helper h;
    auto gen = ores::testing::make_generation_context(h);
    const auto ctx = write_test_party_and_scope_context(h, gen);
    const auto instance_id = write_instance(ctx, h.db_user(), gen);

    ores::reporting::service::report_error_recorder recorder(ctx);
    const std::vector<ores::reporting::service::engine_log_error> errors{
        {.severity = "error",
         .scope = "Curve",
         .message = "yield curve building failed for curve(s) Yield/GBP/GBP1D",
         .subject = "curve",
         .subject_id = "[IndexCurve(GBP-SONIA,Yield/GBP/GBP1D)]",
         .cause = "Yield/GBP/GBP1D",
         .detail = {}}};

    CHECK(recorder.record_engine_errors(instance_id, errors).empty());
    CHECK(recorder.record_engine_errors(instance_id, errors).empty());

    CHECK(errors_of(ctx, instance_id).size() == 1);
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
    CHECK(
        recorder.record_run_failure(instance_id, "Report execution failed during result collection")
            .empty());

    const auto rows = errors_of(mixed, instance_id);
    REQUIRE(rows.size() == 1);
    CHECK(rows.front().party_id == run_party_id);
    CHECK(rows.front().party_id != caller_party_id);
}
