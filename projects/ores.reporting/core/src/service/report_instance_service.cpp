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
#include "ores.reporting.core/service/report_instance_service.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include <cstdint>
#include <stdexcept>

using ores::service::messaging::stamp;

namespace ores::reporting::service {

using namespace ores::logging;

report_instance_service::report_instance_service(context ctx)
    : ctx_(std::move(ctx)) {}

std::vector<domain::report_instance> report_instance_service::list_instances(std::uint32_t offset,
                                                                             std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing all report instances";
    return repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t report_instance_service::count_instances() {
    BOOST_LOG_SEV(lg(), debug) << "Getting total report instances count";
    return repo_.get_total_instance_count(ctx_);
}


std::optional<domain::report_instance>
report_instance_service::get_instance_at_version(const std::string& id, std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Getting report instance at version. " << "id: " << id
                               << " version: " << version;
    return repo_.read_at_version(ctx_, id, version);
}

std::optional<domain::report_instance>
report_instance_service::get_instance(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting report instance. " << "id: " << id;
    auto results = repo_.read_latest(ctx_, id);
    if (results.empty())
        return std::nullopt;
    return results.front();
}

void report_instance_service::save_instance(const domain::report_instance& v) {
    if (v.id.is_nil())
        throw std::invalid_argument("Report Instance id cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving report instance. " << "id: " << v.id;
    auto t = v;
    stamp(t, ctx_);
    repo_.write(ctx_, t);
    BOOST_LOG_SEV(lg(), info) << "Saved report instance. " << "id: " << v.id;
}

void report_instance_service::save_instances(
    const std::vector<domain::report_instance>& instances) {
    for (const auto& e : instances) {
        if (e.id.is_nil())
            throw std::invalid_argument("Report Instance id cannot be empty.");
    }
    BOOST_LOG_SEV(lg(), debug) << "Saving " << instances.size() << " report instances";
    auto ts = instances;
    for (auto& e : ts)
        stamp(e, ctx_);
    repo_.write(ctx_, ts);
}

void report_instance_service::delete_instance(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing report instance. " << "id: " << id;
    repo_.remove(ctx_, id);
    BOOST_LOG_SEV(lg(), info) << "Removed report instance. " << "id: " << id;
}

void report_instance_service::delete_instances(const std::vector<std::string>& ids) {
    repo_.remove(ctx_, ids);
}

std::vector<domain::report_instance>
report_instance_service::get_instance_history(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for report instance. " << "id: " << id;
    return repo_.read_all(ctx_, id);
}

}
