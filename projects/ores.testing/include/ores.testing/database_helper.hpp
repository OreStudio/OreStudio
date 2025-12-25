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
#ifndef ORES_TESTING_DATABASE_HELPER_HPP
#define ORES_TESTING_DATABASE_HELPER_HPP

#include <string>
#include "ores.telemetry/log/make_logger.hpp"
#include "ores.database/domain/context.hpp"

namespace ores::testing {

/**
 * @brief Provides database setup and cleanup utilities for tests.
 */
class database_helper {
private:
    inline static std::string_view logger_name =
        "ores.testing.database_helper";

    static auto& lg() {
        using namespace ores::telemetry::log;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    database_helper();

    /**
     * @brief Truncates the specified table.
     *
     * @param table_name Fully qualified table name (e.g., "ores.accounts")
     */
    void truncate_table(const std::string& table_name);

    /**
     * @brief Gets the database context.
     */
    database::context& context() { return context_; }

private:
    database::context context_;
};

}

#endif
