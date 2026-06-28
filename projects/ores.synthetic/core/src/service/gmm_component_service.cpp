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
#include <stdexcept>

using ores::service::messaging::stamp;

namespace ores::synthetic::service {

using namespace ores::logging;

namespace {

void validate(const domain::gmm_component& c) {
    if (c.stdev <= 0.0)
        throw std::invalid_argument("GMM component standard deviation must be positive.");
    if (c.weight < 0.0)
        throw std::invalid_argument("GMM component weight cannot be negative.");
}

}

gmm_component_service::gmm_component_service(context ctx)
    : ctx_(std::move(ctx))
    , repo_{} {}

std::vector<domain::gmm_component>
gmm_component_service::list_components(std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing components with offset=" << offset
                               << " limit=" << limit;
    return repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t gmm_component_service::count_components() {
    BOOST_LOG_SEV(lg(), debug) << "Counting components";
    return repo_.get_total_component_count(ctx_);
}

void gmm_component_service::save_component(const domain::gmm_component& component) {
    validate(component);
    BOOST_LOG_SEV(lg(), debug) << "Saving component: index " << component.component_index;
    auto c = component;
    stamp(c, ctx_);
    repo_.write(ctx_, c);
}

void gmm_component_service::save_components(const std::vector<domain::gmm_component>& components) {
    for (const auto& c : components)
        validate(c);
    BOOST_LOG_SEV(lg(), debug) << "Saving " << components.size() << " components";
    auto stamped = components;
    for (auto& c : stamped)
        stamp(c, ctx_);
    repo_.write(ctx_, stamped);
}

void gmm_component_service::delete_component(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Deleting component: " << id;
    repo_.remove(ctx_, id);
}

void gmm_component_service::delete_components(const std::vector<std::string>& ids) {
    repo_.remove(ctx_, ids);
}

std::optional<domain::gmm_component> gmm_component_service::get_component(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting component: " << id;
    auto results = repo_.read_latest(ctx_, id);
    if (results.empty()) {
        return std::nullopt;
    }
    return results.front();
}

std::vector<domain::gmm_component>
gmm_component_service::get_component_history(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting component history for: " << id;
    return repo_.read_all(ctx_, id);
}

}
