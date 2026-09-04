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
#include "ores.synthetic.core/service/yield_curve_process_parameter_definition_service.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include <cstdint>
#include <stdexcept>

using ores::service::messaging::stamp;

namespace ores::synthetic::service {

using namespace ores::logging;

yield_curve_process_parameter_definition_service::yield_curve_process_parameter_definition_service(
    context ctx)
    : ctx_(std::move(ctx)) {}

std::vector<domain::yield_curve_process_parameter_definition>
yield_curve_process_parameter_definition_service::list_parameter_definitions(std::uint32_t offset,
                                                                             std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing all yield curve process parameter definitions";
    return repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t yield_curve_process_parameter_definition_service::count_parameter_definitions() {
    BOOST_LOG_SEV(lg(), debug) << "Getting total yield curve process parameter definitions count";
    return repo_.get_total_parameter_definition_count(ctx_);
}


std::optional<domain::yield_curve_process_parameter_definition>
yield_curve_process_parameter_definition_service::get_parameter_definition_at_version(
    const std::string& id, std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Getting yield curve process parameter definition at version. "
                               << "id: " << id << " version: " << version;
    return repo_.read_at_version(ctx_, id, version);
}

std::optional<domain::yield_curve_process_parameter_definition>
yield_curve_process_parameter_definition_service::get_parameter_definition(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting yield curve process parameter definition. "
                               << "id: " << id;
    auto results = repo_.read_latest(ctx_, id);
    if (results.empty())
        return std::nullopt;
    return results.front();
}

void yield_curve_process_parameter_definition_service::save_parameter_definition(
    const domain::yield_curve_process_parameter_definition& v) {
    if (v.id.is_nil())
        throw std::invalid_argument("Yield Curve Process Parameter Definition id cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving yield curve process parameter definition. "
                               << "id: " << v.id;
    auto t = v;
    stamp(t, ctx_);
    repo_.write(ctx_, t);
    BOOST_LOG_SEV(lg(), info) << "Saved yield curve process parameter definition. "
                              << "id: " << v.id;
}

void yield_curve_process_parameter_definition_service::save_parameter_definitions(
    const std::vector<domain::yield_curve_process_parameter_definition>& parameter_definitions) {
    for (const auto& e : parameter_definitions) {
        if (e.id.is_nil())
            throw std::invalid_argument(
                "Yield Curve Process Parameter Definition id cannot be empty.");
    }
    BOOST_LOG_SEV(lg(), debug) << "Saving " << parameter_definitions.size()
                               << " yield curve process parameter definitions";
    auto ts = parameter_definitions;
    for (auto& e : ts)
        stamp(e, ctx_);
    repo_.write(ctx_, ts);
}

void yield_curve_process_parameter_definition_service::delete_parameter_definition(
    const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing yield curve process parameter definition. "
                               << "id: " << id;
    repo_.remove(ctx_, id);
    BOOST_LOG_SEV(lg(), info) << "Removed yield curve process parameter definition. "
                              << "id: " << id;
}

void yield_curve_process_parameter_definition_service::delete_parameter_definitions(
    const std::vector<std::string>& ids) {
    repo_.remove(ctx_, ids);
}

std::vector<domain::yield_curve_process_parameter_definition>
yield_curve_process_parameter_definition_service::get_parameter_definition_history(
    const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for yield curve process parameter definition. "
                               << "id: " << id;
    return repo_.read_all(ctx_, id);
}

}
