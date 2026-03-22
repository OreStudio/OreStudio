/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#ifndef ORES_HTTP_SERVER_MESSAGING_REGISTRAR_HPP
#define ORES_HTTP_SERVER_MESSAGING_REGISTRAR_HPP

#include <string>
#include <vector>
#include "ores.nats/service/client.hpp"
#include "ores.nats/service/subscription.hpp"

namespace ores::http_server::messaging {

class registrar {
public:
    /**
     * @brief Registers all NATS message handlers for the HTTP server.
     *
     * @param nats Connected NATS client.
     * @param base_url The HTTP server's externally accessible base URL.
     * @return Subscription handles that must be kept alive for the lifetime
     *         of the server.
     */
    static std::vector<ores::nats::service::subscription>
    register_handlers(ores::nats::service::client& nats,
                      const std::string& base_url);
};

} // namespace ores::http_server::messaging

#endif
