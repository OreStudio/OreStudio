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
#include "ores.marketdata.core/service/observation_lineage_service.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include <cstdint>
#include <stdexcept>

using ores::service::messaging::stamp;

namespace ores::marketdata::service {

using namespace ores::logging;

observation_lineage_service::observation_lineage_service(context ctx)
    : ctx_(std::move(ctx)) {}

std::vector<domain::observation_lineage>
observation_lineage_service::list_observation_lineages(std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing all observation lineages";
    return repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t observation_lineage_service::count_observation_lineages() {
    BOOST_LOG_SEV(lg(), debug) << "Getting total observation lineages count";
    return repo_.get_total_observation_lineage_count(ctx_);
}


std::optional<domain::observation_lineage>
observation_lineage_service::get_observation_lineage_at_version(const std::string& id,
                                                                std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Getting observation lineage at version. " << "id: " << id
                               << " version: " << version;
    return repo_.read_at_version(ctx_, id, version);
}

std::optional<domain::observation_lineage>
observation_lineage_service::get_observation_lineage(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting observation lineage. " << "id: " << id;
    auto results = repo_.read_latest(ctx_, id);
    if (results.empty())
        return std::nullopt;
    return results.front();
}

void observation_lineage_service::save_observation_lineage(const domain::observation_lineage& v) {
    if (v.id.is_nil())
        throw std::invalid_argument("Observation Lineage id cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving observation lineage. " << "id: " << v.id;
    auto t = v;
    stamp(t, ctx_);
    repo_.write(ctx_, t);
    BOOST_LOG_SEV(lg(), info) << "Saved observation lineage. " << "id: " << v.id;
}

void observation_lineage_service::save_observation_lineages(
    const std::vector<domain::observation_lineage>& observation_lineages) {
    for (const auto& e : observation_lineages) {
        if (e.id.is_nil())
            throw std::invalid_argument("Observation Lineage id cannot be empty.");
    }
    BOOST_LOG_SEV(lg(), debug) << "Saving " << observation_lineages.size()
                               << " observation lineages";
    auto ts = observation_lineages;
    for (auto& e : ts)
        stamp(e, ctx_);
    repo_.write(ctx_, ts);
}

void observation_lineage_service::delete_observation_lineage(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing observation lineage. " << "id: " << id;
    repo_.remove(ctx_, id);
    BOOST_LOG_SEV(lg(), info) << "Removed observation lineage. " << "id: " << id;
}

void observation_lineage_service::delete_observation_lineages(const std::vector<std::string>& ids) {
    repo_.remove(ctx_, ids);
}

std::vector<domain::observation_lineage>
observation_lineage_service::get_observation_lineage_history(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for observation lineage. " << "id: " << id;
    return repo_.read_all(ctx_, id);
}

}
