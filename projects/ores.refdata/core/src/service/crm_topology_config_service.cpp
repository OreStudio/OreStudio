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
#include "ores.refdata.core/service/crm_topology_config_service.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include <cstdint>
#include <stdexcept>

using ores::service::messaging::stamp;

namespace ores::refdata::service {

using namespace ores::logging;

crm_topology_config_service::crm_topology_config_service(context ctx)
    : ctx_(std::move(ctx)) {}

std::vector<domain::crm_topology_config>
crm_topology_config_service::list_crm_topology_configs(std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing all CRM topology configs";
    return repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t crm_topology_config_service::count_crm_topology_configs() {
    BOOST_LOG_SEV(lg(), debug) << "Getting total CRM topology configs count";
    return repo_.get_total_crm_topology_config_count(ctx_);
}

std::optional<domain::crm_topology_config>
crm_topology_config_service::get_crm_topology_config_at_version(const std::string& id,
                                                                std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Getting CRM topology config at version: " << id
                               << " version: " << version;
    return repo_.read_at_version(ctx_, id, version);
}

std::optional<domain::crm_topology_config>
crm_topology_config_service::get_crm_topology_config(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting CRM topology config: " << id;
    auto results = repo_.read_latest(ctx_, id);
    if (results.empty())
        return std::nullopt;
    return results.front();
}

void crm_topology_config_service::save_crm_topology_config(const domain::crm_topology_config& v) {
    if (v.id.is_nil())
        throw std::invalid_argument("CRM Topology Config id cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving CRM topology config: " << v.id;
    auto t = v;
    stamp(t, ctx_);
    repo_.write(ctx_, t);
    BOOST_LOG_SEV(lg(), info) << "Saved CRM topology config: " << v.id;
}

void crm_topology_config_service::save_crm_topology_configs(
    const std::vector<domain::crm_topology_config>& crm_topology_configs) {
    for (const auto& e : crm_topology_configs)
        if (e.id.is_nil())
            throw std::invalid_argument("CRM Topology Config id cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving " << crm_topology_configs.size()
                               << " CRM topology configs";
    auto ts = crm_topology_configs;
    for (auto& e : ts)
        stamp(e, ctx_);
    repo_.write(ctx_, ts);
}

void crm_topology_config_service::delete_crm_topology_config(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing CRM topology config: " << id;
    repo_.remove(ctx_, id);
    BOOST_LOG_SEV(lg(), info) << "Removed CRM topology config: " << id;
}

void crm_topology_config_service::delete_crm_topology_configs(const std::vector<std::string>& ids) {
    repo_.remove(ctx_, ids);
}

std::vector<domain::crm_topology_config>
crm_topology_config_service::get_crm_topology_config_history(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for CRM topology config: " << id;
    return repo_.read_all(ctx_, id);
}

}
