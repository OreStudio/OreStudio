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
#include "ores.compute.core/service/app_version_service.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include <cstdint>
#include <stdexcept>

using ores::service::messaging::stamp;

namespace ores::compute::service {

using namespace ores::logging;

app_version_service::app_version_service(context ctx)
    : ctx_(std::move(ctx)) {}

std::vector<domain::app_version> app_version_service::list_app_versions(std::uint32_t offset,
                                                                        std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing all app versions";
    return repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t app_version_service::count_app_versions() {
    BOOST_LOG_SEV(lg(), debug) << "Getting total app versions count";
    return repo_.get_total_app_version_count(ctx_);
}


std::optional<domain::app_version>
app_version_service::get_app_version_at_version(const std::string& id, std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Getting app version at version. " << "id: " << id
                               << " version: " << version;
    return repo_.read_at_version(ctx_, id, version);
}

std::optional<domain::app_version> app_version_service::get_app_version(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting app version. " << "id: " << id;
    auto results = repo_.read_latest(ctx_, id);
    if (results.empty())
        return std::nullopt;
    return results.front();
}

void app_version_service::save_app_version(const domain::app_version& v) {
    if (v.id.is_nil())
        throw std::invalid_argument("App Version id cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving app version. " << "id: " << v.id;
    auto t = v;
    stamp(t, ctx_);
    repo_.write(ctx_, t);
    BOOST_LOG_SEV(lg(), info) << "Saved app version. " << "id: " << v.id;
}

void app_version_service::save_app_versions(const std::vector<domain::app_version>& app_versions) {
    for (const auto& e : app_versions)
        if (e.id.is_nil())
            throw std::invalid_argument("App Version id cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving " << app_versions.size() << " app versions";
    auto ts = app_versions;
    for (auto& e : ts)
        stamp(e, ctx_);
    repo_.write(ctx_, ts);
}

void app_version_service::delete_app_version(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing app version. " << "id: " << id;
    repo_.remove(ctx_, id);
    BOOST_LOG_SEV(lg(), info) << "Removed app version. " << "id: " << id;
}

void app_version_service::delete_app_versions(const std::vector<std::string>& ids) {
    repo_.remove(ctx_, ids);
}

std::vector<domain::app_version>
app_version_service::get_app_version_history(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for app version. " << "id: " << id;
    return repo_.read_all(ctx_, id);
}

}
