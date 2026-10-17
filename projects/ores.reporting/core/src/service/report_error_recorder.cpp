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
#include "ores.reporting.api/domain/report_error.hpp"
#include "ores.reporting.core/repository/report_instance_repository.hpp"
#include "ores.reporting.core/service/report_error_service.hpp"
#include <boost/uuid/uuid_generators.hpp>
#include <format>
#include <stdexcept>

namespace ores::reporting::service {

namespace {

/// The scope an error has when it is about the run itself and names no subject.
constexpr std::string_view scope_report = "Report";

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

/// The party the run belongs to.
///
/// An error is the run's own fact, so it carries the run's party rather than the
/// caller's. The callers cannot supply it either: one holds a tenant and an
/// instance id, and the other holds a reason and nothing else. Reading it from
/// the run keeps the two in step, which is what the row-level policy on the
/// errors table depends on.
boost::uuids::uuid party_of_run(const ores::database::context& ctx,
                                const std::string& report_instance_id) {
    const auto runs =
        repository::report_instance_repository().read_latest(ctx, report_instance_id);
    if (runs.empty())
        throw std::runtime_error(std::format(
            "No active report instance found with this id: {}", report_instance_id));
    return runs.front().party_id;
}

domain::report_error
make_error(const boost::uuids::uuid& instance_id,
           const boost::uuids::uuid& party_id,
           std::string scope,
           std::string severity,
           std::string message,
           std::optional<std::string> subject,
           std::optional<std::string> subject_id,
           std::string detail) {
    domain::report_error error;
    error.report_instance_id = instance_id;
    error.party_id = party_id;
    error.id = boost::uuids::random_generator()();
    error.scope_code = std::move(scope);
    error.severity = std::move(severity);
    error.message = std::move(message);
    error.subject = std::move(subject);
    error.subject_id = std::move(subject_id);
    error.detail = std::move(detail);
    error.change_reason_code = "system.new_record";
    error.change_commentary = "Recorded from the run's own account of its errors.";
    return error;
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
        const auto instance_id = parse_instance_id(report_instance_id);
        const auto party_id = party_of_run(ctx_, report_instance_id);
        std::vector<domain::report_error> rows;
        rows.reserve(errors.size());
        for (const auto& error : errors) {
            if (!is_known_scope(error.scope))
                return std::format("The engine reported an error with an unknown scope '{}': {}. "
                                   "The scope vocabulary has to grow before the run's errors can "
                                   "be stored.",
                                   error.scope,
                                   error.message);
            rows.push_back(make_error(instance_id,
                                      party_id,
                                      error.scope,
                                      error.severity,
                                      error.message,
                                      error.subject.empty()
                                          ? std::nullopt
                                          : std::optional(error.subject),
                                      error.subject_id.empty()
                                          ? std::nullopt
                                          : std::optional(error.subject_id),
                                      error.detail));
        }

        report_error_service service(ctx_);
        service.save_errors(rows);
        return {};
    } catch (const std::exception& e) {
        return std::format("The run's errors could not be stored: {}", e.what());
    }
}

std::string report_error_recorder::record_run_failure(const std::string& report_instance_id,
                                                      const std::string& reason) {
    try {
        const auto instance_id = parse_instance_id(report_instance_id);
        const auto party_id = party_of_run(ctx_, report_instance_id);
        report_error_service service(ctx_);
        service.save_errors({make_error(instance_id,
                                        party_id,
                                        std::string(scope_report),
                                        "error",
                                        reason,
                                        std::nullopt,
                                        std::nullopt,
                                        {})});
        return {};
    } catch (const std::exception& e) {
        return std::format("The run's failure could not be stored: {}", e.what());
    }
}

}
