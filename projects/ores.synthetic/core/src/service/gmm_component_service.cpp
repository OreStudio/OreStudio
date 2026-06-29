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
#include "ores.synthetic.core/service/gmm_component_service.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include <cstdint>
#include <stdexcept>

using ores::service::messaging::stamp;

namespace ores::synthetic::service {

using namespace ores::logging;

gmm_component_service::gmm_component_service(context ctx)
    : ctx_(std::move(ctx)) {}

std::vector<domain::gmm_component> gmm_component_service::list_gmm_components(std::uint32_t offset,
                                                                              std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing all GMM components";
    return repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t gmm_component_service::count_gmm_components() {
    BOOST_LOG_SEV(lg(), debug) << "Getting total GMM components count";
    return repo_.get_total_gmm_component_count(ctx_);
}

std::optional<domain::gmm_component>
gmm_component_service::get_gmm_component(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting GMM component: " << id;
    auto results = repo_.read_latest(ctx_, id);
    if (results.empty())
        return std::nullopt;
    return results.front();
}

void gmm_component_service::save_gmm_component(const domain::gmm_component& v) {
    if (v.id.is_nil())
        throw std::invalid_argument("GMM Component id cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving GMM component: " << v.id;
    auto t = v;
    stamp(t, ctx_);
    repo_.write(ctx_, t);
    BOOST_LOG_SEV(lg(), info) << "Saved GMM component: " << v.id;
}

void gmm_component_service::save_gmm_components(
    const std::vector<domain::gmm_component>& gmm_components) {
    for (const auto& e : gmm_components)
        if (e.id.is_nil())
            throw std::invalid_argument("GMM Component id cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving " << gmm_components.size() << " GMM components";
    auto ts = gmm_components;
    for (auto& e : ts)
        stamp(e, ctx_);
    repo_.write(ctx_, ts);
}

void gmm_component_service::delete_gmm_component(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing GMM component: " << id;
    repo_.remove(ctx_, id);
    BOOST_LOG_SEV(lg(), info) << "Removed GMM component: " << id;
}

void gmm_component_service::delete_gmm_components(const std::vector<std::string>& ids) {
    repo_.remove(ctx_, ids);
}

std::vector<domain::gmm_component>
gmm_component_service::get_gmm_component_history(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for GMM component: " << id;
    return repo_.read_all(ctx_, id);
}

}
