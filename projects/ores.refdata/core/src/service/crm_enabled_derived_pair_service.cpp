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
#include "ores.refdata.core/service/crm_enabled_derived_pair_service.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include <cstdint>
#include <stdexcept>

using ores::service::messaging::stamp;

namespace ores::refdata::service {

using namespace ores::logging;

crm_enabled_derived_pair_service::crm_enabled_derived_pair_service(context ctx)
    : ctx_(std::move(ctx)) {}

std::vector<domain::crm_enabled_derived_pair>
crm_enabled_derived_pair_service::list_crm_enabled_derived_pairs(std::uint32_t offset,
                                                                 std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing all CRM enabled derived pairs";
    return repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t crm_enabled_derived_pair_service::count_crm_enabled_derived_pairs() {
    BOOST_LOG_SEV(lg(), debug) << "Getting total CRM enabled derived pairs count";
    return repo_.get_total_crm_enabled_derived_pair_count(ctx_);
}


std::optional<domain::crm_enabled_derived_pair>
crm_enabled_derived_pair_service::get_crm_enabled_derived_pair_at_version(const std::string& id,
                                                                          std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Getting CRM enabled derived pair at version: " << id
                               << " version: " << version;
    return repo_.read_at_version(ctx_, id, version);
}

std::optional<domain::crm_enabled_derived_pair>
crm_enabled_derived_pair_service::get_crm_enabled_derived_pair(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting CRM enabled derived pair: " << id;
    auto results = repo_.read_latest(ctx_, id);
    if (results.empty())
        return std::nullopt;
    return results.front();
}

void crm_enabled_derived_pair_service::save_crm_enabled_derived_pair(
    const domain::crm_enabled_derived_pair& v) {
    if (v.id.is_nil())
        throw std::invalid_argument("CRM Enabled Derived Pair id cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving CRM enabled derived pair: " << v.id;
    auto t = v;
    stamp(t, ctx_);
    repo_.write(ctx_, t);
    BOOST_LOG_SEV(lg(), info) << "Saved CRM enabled derived pair: " << v.id;
}

void crm_enabled_derived_pair_service::save_crm_enabled_derived_pairs(
    const std::vector<domain::crm_enabled_derived_pair>& crm_enabled_derived_pairs) {
    for (const auto& e : crm_enabled_derived_pairs)
        if (e.id.is_nil())
            throw std::invalid_argument("CRM Enabled Derived Pair id cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving " << crm_enabled_derived_pairs.size()
                               << " CRM enabled derived pairs";
    auto ts = crm_enabled_derived_pairs;
    for (auto& e : ts)
        stamp(e, ctx_);
    repo_.write(ctx_, ts);
}

void crm_enabled_derived_pair_service::delete_crm_enabled_derived_pair(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing CRM enabled derived pair: " << id;
    repo_.remove(ctx_, id);
    BOOST_LOG_SEV(lg(), info) << "Removed CRM enabled derived pair: " << id;
}

void crm_enabled_derived_pair_service::delete_crm_enabled_derived_pairs(
    const std::vector<std::string>& ids) {
    repo_.remove(ctx_, ids);
}

std::vector<domain::crm_enabled_derived_pair>
crm_enabled_derived_pair_service::get_crm_enabled_derived_pair_history(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for CRM enabled derived pair: " << id;
    return repo_.read_all(ctx_, id);
}

}
