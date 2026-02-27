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
#ifndef ORES_COMMS_SERVICE_MESSAGING_SYSTEM_INFO_HANDLER_HPP
#define ORES_COMMS_SERVICE_MESSAGING_SYSTEM_INFO_HANDLER_HPP

#include <vector>
#include <boost/asio/awaitable.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.comms/messaging/message_handler.hpp"
#include "ores.comms/messaging/system_info_protocol.hpp"

namespace ores::comms::service::messaging {

/**
 * @brief Handles get_system_info_request messages.
 *
 * Returns a KVP list with server compile-time info (version, protocol) and
 * database schema metadata (schema_version, git_commit, etc.).
 * No authentication is required — the handler is safe to call immediately
 * after connection, before login.
 *
 * Entries are populated on the first request and cached for subsequent calls.
 */
class system_info_handler : public comms::messaging::message_handler {
private:
    inline static std::string_view logger_name =
        "ores.comms.service.messaging.system_info_handler";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    /**
     * @brief Construct handler with database context for reading schema metadata.
     *
     * @param ctx Database context initialised with the system tenant.
     */
    explicit system_info_handler(database::context ctx);

    boost::asio::awaitable<std::expected<std::vector<std::byte>,
                                         ores::utility::serialization::error_code>>
    handle_message(comms::messaging::message_type type,
                   std::span<const std::byte> payload,
                   const std::string& remote_address) override;

private:
    /**
     * @brief Populate cached_entries_ from the database and compile-time constants.
     *
     * Called once on the first request. Failures are caught and logged;
     * the cached list may be empty if the database is unavailable.
     */
    void populate_entries();

    database::context ctx_;
    std::vector<comms::messaging::system_info_entry> cached_entries_;
    bool entries_populated_{false};
};

}

#endif
