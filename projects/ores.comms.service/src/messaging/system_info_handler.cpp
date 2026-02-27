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
#include "ores.comms.service/messaging/system_info_handler.hpp"

#include "ores.platform/time/datetime.hpp"
#include "ores.utility/version/version.hpp"
#include "ores.comms/messaging/protocol.hpp"
#include "ores.database/repository/database_info_repository.hpp"

namespace ores::comms::service::messaging {

using namespace ores::logging;

system_info_handler::system_info_handler(database::context ctx)
    : ctx_(std::move(ctx)) {}

void system_info_handler::populate_entries() {
    cached_entries_.clear();

    // Server compile-time entries
    cached_entries_.push_back({"server.version", ORES_VERSION});
    cached_entries_.push_back({"server.build_info", ORES_BUILD_INFO});
    cached_entries_.push_back({
        "server.protocol_version",
        std::to_string(comms::messaging::PROTOCOL_VERSION_MAJOR) + "." +
        std::to_string(comms::messaging::PROTOCOL_VERSION_MINOR)
    });

    // Database entries from the database_info table
    try {
        database::repository::database_info_repository repo;
        const auto db_infos = repo.read(ctx_);
        if (!db_infos.empty()) {
            const auto& di = db_infos.front();
            cached_entries_.push_back({"database.schema_version", di.schema_version});
            cached_entries_.push_back({"database.build_environment", di.build_environment});
            cached_entries_.push_back({"database.git_commit", di.git_commit});
            cached_entries_.push_back({"database.git_date", di.git_date});

            // Format created_at as an ISO-8601-like string (cross-platform)
            cached_entries_.push_back({"database.created_at",
                ores::platform::time::datetime::format_time_point_utc(
                    di.created_at, "%Y-%m-%dT%H:%M:%SZ")});
        } else {
            BOOST_LOG_SEV(lg(), warn) << "database_info table has no rows";
        }
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), warn)
            << "Could not read database_info for system_info response: " << e.what();
    }
}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
                                     ores::utility::serialization::error_code>>
system_info_handler::handle_message(comms::messaging::message_type type,
    std::span<const std::byte> /*payload*/,
    const std::string& remote_address) {

    BOOST_LOG_SEV(lg(), debug)
        << "Handling get_system_info_request from " << remote_address;

    if (type != comms::messaging::message_type::get_system_info_request) {
        BOOST_LOG_SEV(lg(), error)
            << "Unexpected message type in system_info_handler: "
            << static_cast<int>(type);
        co_return std::unexpected(
            ores::utility::serialization::error_code::invalid_message_type);
    }

    if (!entries_populated_) {
        populate_entries();
        entries_populated_ = true;
    }

    comms::messaging::system_info_response response;
    response.entries = cached_entries_;

    co_return comms::messaging::system_info_response::serialize(std::move(response));
}

}
