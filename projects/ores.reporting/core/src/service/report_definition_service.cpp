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
#include "ores.reporting.core/service/report_definition_service.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include <cstdint>
#include <stdexcept>

using ores::service::messaging::stamp;

namespace ores::reporting::service {

using namespace ores::logging;

report_definition_service::report_definition_service(context ctx)
    : ctx_(std::move(ctx)) {}

std::vector<domain::report_definition>
report_definition_service::list_definitions(std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing all report definitions";
    return repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t report_definition_service::count_definitions() {
    BOOST_LOG_SEV(lg(), debug) << "Getting total report definitions count";
    return repo_.get_total_definition_count(ctx_);
}


std::optional<domain::report_definition>
report_definition_service::get_definition_at_version(const std::string& id, std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Getting report definition at version. " << "id: " << id
                               << " version: " << version;
    return repo_.read_at_version(ctx_, id, version);
}

std::optional<domain::report_definition>
report_definition_service::get_definition(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting report definition. " << "id: " << id;
    auto results = repo_.read_latest(ctx_, id);
    if (results.empty())
        return std::nullopt;
    return results.front();
}

void report_definition_service::save_definition(const domain::report_definition& v) {
    if (v.id.is_nil())
        throw std::invalid_argument("Report Definition id cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving report definition. " << "id: " << v.id;
    auto t = v;
    stamp(t, ctx_);
    repo_.write(ctx_, t);
    BOOST_LOG_SEV(lg(), info) << "Saved report definition. " << "id: " << v.id;
}

void report_definition_service::save_definitions(
    const std::vector<domain::report_definition>& definitions) {
    for (const auto& e : definitions) {
        if (e.id.is_nil())
            throw std::invalid_argument("Report Definition id cannot be empty.");
    }
    BOOST_LOG_SEV(lg(), debug) << "Saving " << definitions.size() << " report definitions";
    auto ts = definitions;
    for (auto& e : ts)
        stamp(e, ctx_);
    repo_.write(ctx_, ts);
}

void report_definition_service::delete_definition(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing report definition. " << "id: " << id;
    repo_.remove(ctx_, id);
    BOOST_LOG_SEV(lg(), info) << "Removed report definition. " << "id: " << id;
}

void report_definition_service::delete_definitions(const std::vector<std::string>& ids) {
    repo_.remove(ctx_, ids);
}

std::vector<domain::report_definition>
report_definition_service::get_definition_history(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for report definition. " << "id: " << id;
    return repo_.read_all(ctx_, id);
}

}
