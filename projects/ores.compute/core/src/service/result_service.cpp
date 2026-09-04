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
#include "ores.compute.core/service/result_service.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include <cstdint>
#include <stdexcept>

using ores::service::messaging::stamp;

namespace ores::compute::service {

using namespace ores::logging;

result_service::result_service(context ctx)
    : ctx_(std::move(ctx)) {}

std::vector<domain::result> result_service::list_results(std::uint32_t offset,
                                                         std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing all compute results";
    return repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t result_service::count_results() {
    BOOST_LOG_SEV(lg(), debug) << "Getting total compute results count";
    return repo_.get_total_result_count(ctx_);
}


std::vector<domain::result> result_service::list_results_by_workunit_id(
    const std::string& workunit_id, std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing compute results by workunit_id: " << workunit_id;
    return repo_.read_latest_by_workunit_id(ctx_, workunit_id, offset, limit);
}

std::uint32_t result_service::count_results_by_workunit_id(const std::string& workunit_id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting total compute results count by workunit_id: "
                               << workunit_id;
    return repo_.get_total_result_count_by_workunit_id(ctx_, workunit_id);
}


std::optional<domain::result> result_service::get_result_at_version(const std::string& id,
                                                                    std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Getting compute result at version. " << "id: " << id
                               << " version: " << version;
    return repo_.read_at_version(ctx_, id, version);
}

std::optional<domain::result> result_service::get_result(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting compute result. " << "id: " << id;
    auto results = repo_.read_latest(ctx_, id);
    if (results.empty())
        return std::nullopt;
    return results.front();
}

void result_service::save_result(const domain::result& v) {
    if (v.id.is_nil())
        throw std::invalid_argument("Result id cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving compute result. " << "id: " << v.id;
    auto t = v;
    stamp(t, ctx_);
    repo_.write(ctx_, t);
    BOOST_LOG_SEV(lg(), info) << "Saved compute result. " << "id: " << v.id;
}

void result_service::save_results(const std::vector<domain::result>& results) {
    for (const auto& e : results) {
        if (e.id.is_nil())
            throw std::invalid_argument("Result id cannot be empty.");
    }
    BOOST_LOG_SEV(lg(), debug) << "Saving " << results.size() << " compute results";
    auto ts = results;
    for (auto& e : ts)
        stamp(e, ctx_);
    repo_.write(ctx_, ts);
}

void result_service::delete_result(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing compute result. " << "id: " << id;
    repo_.remove(ctx_, id);
    BOOST_LOG_SEV(lg(), info) << "Removed compute result. " << "id: " << id;
}

void result_service::delete_results(const std::vector<std::string>& ids) {
    repo_.remove(ctx_, ids);
}

std::vector<domain::result> result_service::get_result_history(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for compute result. " << "id: " << id;
    return repo_.read_all(ctx_, id);
}

std::vector<domain::result> result_service::list_by_state(int server_state) {
    BOOST_LOG_SEV(lg(), debug) << "Listing results by state: " << server_state;
    return repo_.read_by_state(ctx_, server_state);
}
}
