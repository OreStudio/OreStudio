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
#ifndef ORES_SERVICE_APP_HOST_HPP
#define ORES_SERVICE_APP_HOST_HPP

#if defined(_MSC_VER) && (_MSC_VER >= 1200)
#pragma once
#endif

#include <vector>
#include <string>
#include <ostream>
#include <boost/asio/awaitable.hpp>
#include "ores.utility/log/make_logger.hpp"

namespace ores::service::app {

/**
 * @brief Provides hosting services to the application.
 */
class host {
private:
    static auto& lg() {
        using namespace ores::utility::log;
        static auto instance = make_logger("ores.service.app.host");
        return instance;
    }

public:
    /**
     * @brief Executes the console workflow.
     */
    static boost::asio::awaitable<int> execute(const std::vector<std::string>& args,
        std::ostream& std_output, std::ostream& error_output,
        boost::asio::io_context& io_ctx);
};

}

#endif
