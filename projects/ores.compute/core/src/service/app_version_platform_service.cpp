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
#include "ores.compute.core/service/app_version_platform_service.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <stdexcept>

namespace ores::compute::service {

using namespace ores::logging;

app_version_platform_service::app_version_platform_service(context ctx)
    : repo_(ctx) {}

std::vector<domain::app_version_platform>
app_version_platform_service::list_app_version_platforms() {
    BOOST_LOG_SEV(lg(), debug) << "Listing all app version platforms";
    return repo_.read_latest();
}

std::vector<domain::app_version_platform>
app_version_platform_service::list_app_version_platforms_by_app_version(
    const boost::uuids::uuid& app_version_id) {
    BOOST_LOG_SEV(lg(), debug) << "Listing app version platforms for app version: "
                               << app_version_id;
    return repo_.read_latest_by_app_version(app_version_id);
}

std::vector<domain::app_version_platform>
app_version_platform_service::list_app_version_platforms_by_app_version(
    const boost::uuids::uuid& app_version_id, std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing app version platforms for app version: "
                               << app_version_id << " offset: " << offset << " limit: " << limit;
    return repo_.read_latest_by_app_version(app_version_id, offset, limit);
}

std::uint32_t app_version_platform_service::get_total_app_version_platform_count_by_app_version(
    const boost::uuids::uuid& app_version_id) {
    return repo_.get_total_app_version_platform_count_by_app_version(app_version_id);
}

void app_version_platform_service::save_app_version_platform(
    const domain::app_version_platform& app_version_platform) {
    if (app_version_platform.app_version_id.is_nil()) {
        throw std::invalid_argument("App Version cannot be empty.");
    }
    if (app_version_platform.platform_id.is_nil()) {
        throw std::invalid_argument("Platform cannot be empty.");
    }
    BOOST_LOG_SEV(lg(), debug) << "Saving app version platform: "
                               << app_version_platform.app_version_id << "/"
                               << app_version_platform.platform_id;
    repo_.write(app_version_platform);
    BOOST_LOG_SEV(lg(), info) << "Saved app version platform: "
                              << app_version_platform.app_version_id << "/"
                              << app_version_platform.platform_id;
}

void app_version_platform_service::remove_app_version_platform(
    const boost::uuids::uuid& app_version_id, const boost::uuids::uuid& platform_id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing app version platform: " << app_version_id << "/"
                               << platform_id;
    repo_.remove(app_version_id, platform_id);
    BOOST_LOG_SEV(lg(), info) << "Removed app version platform: " << app_version_id << "/"
                              << platform_id;
}

void app_version_platform_service::replace_app_version_platforms_by_app_version(
    const boost::uuids::uuid& app_version_id,
    const std::vector<domain::app_version_platform>& app_version_platforms,
    const std::string& modified_by,
    const std::string& performed_by,
    const std::string& change_reason_code,
    const std::string& change_commentary) {

    BOOST_LOG_SEV(lg(), debug) << "Replacing app version platforms for app version: "
                               << app_version_id;
    repo_.replace_by_app_version(app_version_id,
                                 app_version_platforms,
                                 modified_by,
                                 performed_by,
                                 change_reason_code,
                                 change_commentary);
    BOOST_LOG_SEV(lg(), info) << "Replaced app version platforms for app version: "
                              << app_version_id;
}

}
