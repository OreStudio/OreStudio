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
#include "ores.compute.core/service/workunit_service.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include <cstdint>
#include <stdexcept>

using ores::service::messaging::stamp;

namespace ores::compute::service {

using namespace ores::logging;

workunit_service::workunit_service(context ctx)
    : ctx_(std::move(ctx)) {}

std::vector<domain::workunit> workunit_service::list_workunits(std::uint32_t offset,
                                                               std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing all workunits";
    return repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t workunit_service::count_workunits() {
    BOOST_LOG_SEV(lg(), debug) << "Getting total workunits count";
    return repo_.get_total_workunit_count(ctx_);
}


std::vector<domain::workunit> workunit_service::list_workunits_by_batch_id(
    const std::string& batch_id, std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing workunits by batch_id: " << batch_id;
    return repo_.read_latest_by_batch_id(ctx_, batch_id, offset, limit);
}

std::uint32_t workunit_service::count_workunits_by_batch_id(const std::string& batch_id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting total workunits count by batch_id: " << batch_id;
    return repo_.get_total_workunit_count_by_batch_id(ctx_, batch_id);
}


std::optional<domain::workunit> workunit_service::get_workunit_at_version(const std::string& id,
                                                                          std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Getting workunit at version. " << "id: " << id
                               << " version: " << version;
    return repo_.read_at_version(ctx_, id, version);
}

std::optional<domain::workunit> workunit_service::get_workunit(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting workunit. " << "id: " << id;
    auto results = repo_.read_latest(ctx_, id);
    if (results.empty())
        return std::nullopt;
    return results.front();
}

void workunit_service::save_workunit(const domain::workunit& v) {
    if (v.id.is_nil())
        throw std::invalid_argument("Workunit id cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving workunit. " << "id: " << v.id;
    auto t = v;
    stamp(t, ctx_);
    repo_.write(ctx_, t);
    BOOST_LOG_SEV(lg(), info) << "Saved workunit. " << "id: " << v.id;
}

void workunit_service::save_workunits(const std::vector<domain::workunit>& workunits) {
    for (const auto& e : workunits)
        if (e.id.is_nil())
            throw std::invalid_argument("Workunit id cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving " << workunits.size() << " workunits";
    auto ts = workunits;
    for (auto& e : ts)
        stamp(e, ctx_);
    repo_.write(ctx_, ts);
}

void workunit_service::delete_workunit(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing workunit. " << "id: " << id;
    repo_.remove(ctx_, id);
    BOOST_LOG_SEV(lg(), info) << "Removed workunit. " << "id: " << id;
}

void workunit_service::delete_workunits(const std::vector<std::string>& ids) {
    repo_.remove(ctx_, ids);
}

std::vector<domain::workunit> workunit_service::get_workunit_history(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for workunit. " << "id: " << id;
    return repo_.read_all(ctx_, id);
}

}
