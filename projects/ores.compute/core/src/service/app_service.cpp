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
#include "ores.compute.core/service/app_service.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include <cstdint>
#include <stdexcept>

using ores::service::messaging::stamp;

namespace ores::compute::service {

using namespace ores::logging;

app_service::app_service(context ctx)
    : ctx_(std::move(ctx)) {}

std::vector<domain::app> app_service::list_apps(std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing all compute apps";
    return repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t app_service::count_apps() {
    BOOST_LOG_SEV(lg(), debug) << "Getting total compute apps count";
    return repo_.get_total_app_count(ctx_);
}


std::optional<domain::app> app_service::get_app_at_version(const std::string& id,
                                                           std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Getting compute app at version. " << "id: " << id
                               << " version: " << version;
    return repo_.read_at_version(ctx_, id, version);
}

std::optional<domain::app> app_service::get_app(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting compute app. " << "id: " << id;
    auto results = repo_.read_latest(ctx_, id);
    if (results.empty())
        return std::nullopt;
    return results.front();
}

void app_service::save_app(const domain::app& v) {
    if (v.id.is_nil())
        throw std::invalid_argument("App id cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving compute app. " << "id: " << v.id;
    auto t = v;
    stamp(t, ctx_);
    repo_.write(ctx_, t);
    BOOST_LOG_SEV(lg(), info) << "Saved compute app. " << "id: " << v.id;
}

void app_service::save_apps(const std::vector<domain::app>& apps) {
    for (const auto& e : apps) {
        if (e.id.is_nil())
            throw std::invalid_argument("App id cannot be empty.");
    }
    BOOST_LOG_SEV(lg(), debug) << "Saving " << apps.size() << " compute apps";
    auto ts = apps;
    for (auto& e : ts)
        stamp(e, ctx_);
    repo_.write(ctx_, ts);
}

void app_service::delete_app(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing compute app. " << "id: " << id;
    repo_.remove(ctx_, id);
    BOOST_LOG_SEV(lg(), info) << "Removed compute app. " << "id: " << id;
}

void app_service::delete_apps(const std::vector<std::string>& ids) {
    repo_.remove(ctx_, ids);
}

std::vector<domain::app> app_service::get_app_history(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for compute app. " << "id: " << id;
    return repo_.read_all(ctx_, id);
}

}
