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
#include "ores.reporting.core/service/report_error_recorder.hpp"
#include "ores.reporting.api/domain/report_error_event.hpp"
#include "ores.reporting.core/repository/report_error_event_repository.hpp"
#include "ores.reporting.core/repository/report_input_bundle_repository.hpp"
#include "ores.reporting.core/repository/report_instance_repository.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid_generators.hpp>
#include <algorithm>
#include <chrono>
#include <format>
#include <map>
#include <rfl/json.hpp>
#include <stdexcept>

namespace ores::reporting::service {

namespace {

/// The scope an error has when it is about the run itself and names no subject.
constexpr std::string_view scope_report = "Report";

/// The subject an error names when it is about one trade.
constexpr std::string_view subject_trade = "trade";

/// The scopes the engine emits, which the seed states and the store enforces.
constexpr std::string_view engine_scopes[] = {"Trade", "Analytics", "Curve", "Configuration"};

bool is_known_scope(std::string_view scope) {
    if (scope == scope_report)
        return true;
    for (const auto& known : engine_scopes)
        if (scope == known)
            return true;
    return false;
}

boost::uuids::uuid parse_instance_id(const std::string& report_instance_id) {
    boost::uuids::string_generator generate;
    return generate(report_instance_id);
}

/// What a run states about itself, which an error about it inherits.
struct run_facts {
    /// The party the run belongs to.
    ///
    /// An error is the run's own fact, so it carries the run's party rather than the
    /// caller's. The callers cannot supply it either: one holds a tenant and an
    /// instance id, and the other holds a reason and nothing else. Reading it from
    /// the run keeps the two in step, which is what the row-level policy on the
    /// errors table depends on.
    boost::uuids::uuid party_id;
    /// The date the run priced at. Absent on a run that has not priced yet, which
    /// includes every run older than the column.
    std::optional<std::chrono::system_clock::time_point> priced_at;
};

run_facts facts_of_run(const ores::database::context& ctx, const std::string& report_instance_id) {
    const auto runs = repository::report_instance_repository().read_latest(ctx, report_instance_id);
    if (runs.empty())
        throw std::runtime_error(
            std::format("No active report instance found with this id: {}", report_instance_id));
    return {.party_id = runs.front().party_id, .priced_at = runs.front().priced_at};
}

/// The version of each trade the run read, keyed by trade id.
///
/// A run writes its manifest when it gathers its trades, so a run that never got
/// that far, or one whose bundle predates the column, has none. An error is still
/// worth writing without one: the trade it names is the fact the reader needs,
/// and a missing manifest only costs the version.
std::map<std::string, int> trade_versions_of_run(const ores::database::context& ctx,
                                                 const std::string& report_instance_id) {
    const auto bundle =
        repository::report_input_bundle_repository().find_by_instance_id(ctx, report_instance_id);
    if (!bundle || !bundle->trade_versions)
        return {};
    const auto parsed = rfl::json::read<std::map<std::string, int>>(*bundle->trade_versions);
    if (!parsed)
        return {};
    return *parsed;
}

domain::report_error_event make_error(const utility::uuid::tenant_id& tenant_id,
                                      const boost::uuids::uuid& instance_id,
                                      const boost::uuids::uuid& party_id,
                                      std::chrono::system_clock::time_point event_time,
                                      std::string scope,
                                      std::string severity,
                                      std::string message,
                                      std::optional<std::string> subject,
                                      std::optional<std::string> subject_id,
                                      std::optional<std::string> cause,
                                      std::string detail) {
    domain::report_error_event error;
    error.tenant_id = tenant_id;
    error.report_instance_id = instance_id;
    error.party_id = party_id;
    error.id = boost::uuids::random_generator()();
    error.scope_code = std::move(scope);
    error.severity = std::move(severity);
    error.message = std::move(message);
    error.subject = std::move(subject);
    error.subject_id = std::move(subject_id);
    error.cause = std::move(cause);
    error.detail = std::move(detail);
    error.event_time = event_time;
    return error;
}

}

namespace {

/// Whether the run already holds the errors of its analytics. A warning row the
/// recorder wrote for an unplaceable scope is not one of them, so it never
/// stands in for a batch that has not been read yet.
bool run_holds_errors(const ores::database::context& ctx, const std::string& instance_id) {
    messaging::report_error_events_filter only_errors;
    only_errors.severity = std::string("error");
    return !repository::report_error_event_repository()
                .read_latest_by_report_instance_id(ctx, instance_id, 0, 1, {}, only_errors)
                .empty();
}

}

report_error_recorder::report_error_recorder(ores::database::context ctx)
    : ctx_(std::move(ctx)) {}

std::string
report_error_recorder::record_engine_errors(const std::string& report_instance_id,
                                            const std::vector<engine_log_error>& errors) {
    if (errors.empty())
        return {};

    try {
        // The step that reads a run's output runs at least once, so its errors
        // are written once.
        if (run_holds_errors(ctx_, report_instance_id))
            return {};

        const auto instance_id = parse_instance_id(report_instance_id);
        const auto facts = facts_of_run(ctx_, report_instance_id);
        const auto trade_versions = trade_versions_of_run(ctx_, report_instance_id);
        const auto event_time = std::chrono::system_clock::now();
        std::vector<domain::report_error_event> rows;
        std::vector<std::string> unknown_scopes;
        rows.reserve(errors.size() + unknown_scopes.capacity());
        for (const auto& error : errors) {
            if (!is_known_scope(error.scope)) {
                // The scope vocabulary has to grow before these rows can be
                // stored, but that is no reason to lose the ones it does name.
                if (std::ranges::find(unknown_scopes, error.scope) == unknown_scopes.end())
                    unknown_scopes.push_back(error.scope);
                continue;
            }
            auto row = make_error(
                ctx_.tenant_id(),
                instance_id,
                facts.party_id,
                event_time,
                error.scope,
                error.severity,
                error.message,
                error.subject.empty() ? std::nullopt : std::optional(error.subject),
                error.subject_id.empty() ? std::nullopt : std::optional(error.subject_id),
                error.cause.empty() ? std::nullopt : std::optional(error.cause),
                error.detail);
            // The version says which row of the trade the engine was given, and the
            // date says when that row was current. A trade the manifest does not
            // name keeps both absent, because a guessed version is worse than none.
            if (error.subject == subject_trade && row.subject_id) {
                if (const auto version = trade_versions.find(*row.subject_id);
                    version != trade_versions.end()) {
                    row.subject_version = version->second;
                    row.subject_as_of = facts.priced_at;
                }
            }
            rows.push_back(std::move(row));
        }

        // A scope the vocabulary does not name is said, so the loss is visible
        // on the run rather than only in a log line.
        for (const auto& scope : unknown_scopes)
            rows.push_back(make_error(ctx_.tenant_id(),
                                      instance_id,
                                      facts.party_id,
                                      event_time,
                                      std::string(scope_report),
                                      "warning",
                                      std::format("The engine reported errors under the scope "
                                                  "'{}', which the scope vocabulary does not name "
                                                  "yet, so those rows were not stored.",
                                                  scope),
                                      std::string("scope"),
                                      scope,
                                      std::nullopt,
                                      {}));

        repository::report_error_event_repository().write(ctx_, rows);
        return {};
    } catch (const std::exception& e) {
        // Two callers can both find no rows and both write. The unique indexes
        // let one win; the other is told the run already holds its errors,
        // which is what it set out to make true.
        if (std::string_view(e.what()).find("duplicate key") != std::string_view::npos)
            return {};
        return std::format("The run's errors could not be stored: {}", e.what());
    }
}

std::string report_error_recorder::record_run_failure(const std::string& report_instance_id,
                                                      const std::string& reason) {
    try {
        // The engine's own account repeats what the run's reasons already say.
        // The row points at them when they are there, and carries the account
        // when they are not, because then it is the only account a reader has.
        const auto failure_text =
            run_holds_errors(ctx_, report_instance_id) ?
                std::string("The run failed. Its reasons are the errors recorded against it.") :
                reason;

        const auto instance_id = parse_instance_id(report_instance_id);
        const auto party_id = facts_of_run(ctx_, report_instance_id).party_id;
        const auto event_time = std::chrono::system_clock::now();
        repository::report_error_event_repository().write(ctx_,
                                                          {make_error(ctx_.tenant_id(),
                                                                      instance_id,
                                                                      party_id,
                                                                      event_time,
                                                                      std::string(scope_report),
                                                                      "error",
                                                                      failure_text,
                                                                      std::nullopt,
                                                                      std::nullopt,
                                                                      std::nullopt,
                                                                      {})});
        return {};
    } catch (const std::exception& e) {
        return std::format("The run's failure could not be stored: {}", e.what());
    }
}

}
