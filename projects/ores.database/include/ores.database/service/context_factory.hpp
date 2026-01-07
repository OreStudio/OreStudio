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
#ifndef ORES_DATABASE_CONTEXT_FACTORY_HPP
#define ORES_DATABASE_CONTEXT_FACTORY_HPP

#include <iosfwd>
#include <string_view>
#include "ores.logging/make_logger.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.database/domain/database_options.hpp"

namespace ores::database {

/**
 * @brief Generates a new database context.
 */
class context_factory {
private:
    inline static std::string_view logger_name =
        "ores.utility.database.context_factory";

    static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    struct configuration {
        /// Database connection options.
        database::database_options database_options;
        /// Number of connections in the pool.
        std::size_t pool_size;
        /// Number of retry attempts when acquiring a connection.
        std::size_t num_attempts;
        /// Wait time between retry attempts.
        std::size_t wait_time_in_seconds;
    };

    static context make_context(const configuration& cfg);
};

std::ostream&
operator<<(std::ostream& s, const context_factory::configuration& v);

}

#endif
