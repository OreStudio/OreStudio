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
#include "ores.compute/service/app_version_service.hpp"

#include <stdexcept>
#include "ores.service/messaging/handler_helpers.hpp"

using ores::service::messaging::stamp;

namespace ores::compute::service {

using namespace ores::logging;

app_version_service::app_version_service(context ctx)
    : ctx_(std::move(ctx)) {}

std::vector<domain::app_version> app_version_service::list() {
    BOOST_LOG_SEV(lg(), debug) << "Listing all app versions";
    return repo_.read_latest(ctx_);
}

std::optional<domain::app_version>
app_version_service::find(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Finding app version: " << id;
    auto results = repo_.read_latest(ctx_, id);
    if (results.empty()) return std::nullopt;
    return results.front();
}

void app_version_service::save(const domain::app_version& v) {
    if (v.id.is_nil())
        throw std::invalid_argument("App version id cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving app version: " << v.id;
    auto stamped = v;
    stamp(stamped, ctx_);
    repo_.write(ctx_, stamped);
    BOOST_LOG_SEV(lg(), info) << "Saved app version: " << v.id;
}

std::vector<domain::app_version>
app_version_service::history(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for app version: " << id;
    return repo_.read_all(ctx_, id);
}

}
