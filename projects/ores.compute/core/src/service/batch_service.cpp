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
#include "ores.compute.core/service/batch_service.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include <cstdint>
#include <stdexcept>

using ores::service::messaging::stamp;

namespace ores::compute::service {

using namespace ores::logging;

batch_service::batch_service(context ctx)
    : ctx_(std::move(ctx)) {}

std::vector<domain::batch> batch_service::list_batches(std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing all compute batches";
    return repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t batch_service::count_batches() {
    BOOST_LOG_SEV(lg(), debug) << "Getting total compute batches count";
    return repo_.get_total_batch_count(ctx_);
}


std::optional<domain::batch> batch_service::get_batch_at_version(const std::string& id,
                                                                 std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Getting compute batch at version. " << "id: " << id
                               << " version: " << version;
    return repo_.read_at_version(ctx_, id, version);
}

std::optional<domain::batch> batch_service::get_batch(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting compute batch. " << "id: " << id;
    auto results = repo_.read_latest(ctx_, id);
    if (results.empty())
        return std::nullopt;
    return results.front();
}

void batch_service::save_batch(const domain::batch& v) {
    if (v.id.is_nil())
        throw std::invalid_argument("Batch id cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving compute batch. " << "id: " << v.id;
    auto t = v;
    stamp(t, ctx_);
    repo_.write(ctx_, t);
    BOOST_LOG_SEV(lg(), info) << "Saved compute batch. " << "id: " << v.id;
}

void batch_service::save_batches(const std::vector<domain::batch>& batches) {
    for (const auto& e : batches) {
        if (e.id.is_nil())
            throw std::invalid_argument("Batch id cannot be empty.");
    }
    BOOST_LOG_SEV(lg(), debug) << "Saving " << batches.size() << " compute batches";
    auto ts = batches;
    for (auto& e : ts)
        stamp(e, ctx_);
    repo_.write(ctx_, ts);
}

void batch_service::delete_batch(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing compute batch. " << "id: " << id;
    repo_.remove(ctx_, id);
    BOOST_LOG_SEV(lg(), info) << "Removed compute batch. " << "id: " << id;
}

void batch_service::delete_batches(const std::vector<std::string>& ids) {
    repo_.remove(ctx_, ids);
}

std::vector<domain::batch> batch_service::get_batch_history(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for compute batch. " << "id: " << id;
    return repo_.read_all(ctx_, id);
}

}
