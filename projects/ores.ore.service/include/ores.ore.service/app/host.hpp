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
#ifndef ORES_ORE_SERVICE_APP_HOST_HPP
#define ORES_ORE_SERVICE_APP_HOST_HPP

#include <iosfwd>
#include <vector>
#include <string>
#include <boost/asio/awaitable.hpp>
#include <boost/asio/io_context.hpp>
#include "ores.logging/make_logger.hpp"

namespace ores::ore::service::app {

/**
 * @brief Bootstrap host for ores.ore.service.
 *
 * Parses command-line arguments, initialises logging, and drives the
 * application coroutine.
 */
class host final {
private:
    inline static std::string_view logger_name = "ores.ore.service.app.host";

    static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    static boost::asio::awaitable<int>
    execute(const std::vector<std::string>& args, std::ostream& std_output,
        std::ostream& error_output, boost::asio::io_context& io_ctx);
};

}

#endif
