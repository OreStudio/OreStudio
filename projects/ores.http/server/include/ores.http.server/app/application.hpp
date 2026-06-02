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
#ifndef ORES_HTTP_SERVER_APP_APPLICATION_HPP
#define ORES_HTTP_SERVER_APP_APPLICATION_HPP

#include <boost/asio/io_context.hpp>
#include <boost/asio/awaitable.hpp>
#include "ores.http.server/config/options.hpp"
#include "ores.logging/make_logger.hpp"

namespace ores::http_server::app {

/**
 * @brief Main HTTP server application.
 */
class application final {
public:
    /**
     * @brief Runs the HTTP server application.
     */
    boost::asio::awaitable<void> run(boost::asio::io_context& io_ctx,
        const config::options& cfg);

private:
    inline static std::string_view logger_name = "ores.http.server.app.application";

    static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }
};

}

#endif
