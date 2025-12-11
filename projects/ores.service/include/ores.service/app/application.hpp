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
#ifndef ORES_SERVICE_APP_APPLICATION_HPP
#define ORES_SERVICE_APP_APPLICATION_HPP

#include <optional>
#include <boost/asio/awaitable.hpp>
#include "ores.utility/log/make_logger.hpp"
#include "ores.database/context.hpp"
#include "ores.database/database_options.hpp"
#include "ores.service/config/options.hpp"

namespace ores::service::app {

/**
 * @brief Entry point for the ores command line application.
 */
class application final {
private:
    inline static std::string_view logger_name = "ores.service.app.application";

    static auto& lg() {
        using namespace ores::utility::log;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    application();
    application(const application&) = delete;
    application& operator=(const application&) = delete;

private:
    static utility::database::context
    make_context(const std::optional<utility::database::database_options>& db_opts);

public:
    boost::asio::awaitable<void> run(boost::asio::io_context& io_ctx,
        const config::options& cfg) const;
};

}

#endif
