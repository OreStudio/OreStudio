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
#ifndef ORES_TESTING_SCOPED_DATABASE_HELPER_HPP
#define ORES_TESTING_SCOPED_DATABASE_HELPER_HPP

#include <string>
#include "ores.testing/database_helper.hpp"

namespace ores::testing {

/**
 * @brief Provides database setup and cleanup utilities for tests.
 */
class scoped_database_helper {
public:
    explicit scoped_database_helper(const std::string& table_name) {
        helper_.truncate_table(table_name);
    }

    /**
     * @brief Gets the database context.
     */
    database::context& context() { return  helper_.context(); }

private:
    database_helper helper_;
};

}

#endif
