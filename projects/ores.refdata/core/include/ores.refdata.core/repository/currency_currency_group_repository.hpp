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
#ifndef ORES_REFDATA_CORE_REPOSITORY_CURRENCY_CURRENCY_GROUP_REPOSITORY_HPP
#define ORES_REFDATA_CORE_REPOSITORY_CURRENCY_CURRENCY_GROUP_REPOSITORY_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/domain/currency_currency_group.hpp"
#include "ores.refdata.core/export.hpp"
#include <sqlgen/postgres.hpp>
#include <string>
#include <vector>

namespace ores::refdata::repository {

/**
 * @brief Reads and writes currency groups to data storage.
 */
class ORES_REFDATA_CORE_EXPORT currency_currency_group_repository {
private:
    inline static std::string_view logger_name =
        "ores.refdata.repository.currency_currency_group_repository";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    explicit currency_currency_group_repository(context ctx);

    std::string sql();

    void write(const domain::currency_currency_group& currency_group);
    void write(const std::vector<domain::currency_currency_group>& currency_groups);

    std::vector<domain::currency_currency_group> read_latest();
    std::vector<domain::currency_currency_group>
    read_latest_by_currency(const std::string& currency_iso_code);
    std::vector<domain::currency_currency_group>
    read_latest_by_group(const std::string& currency_group_code);

    void remove(const std::string& currency_iso_code, const std::string& currency_group_code);
    void remove_by_currency(const std::string& currency_iso_code);

private:
    context ctx_;
};

}

#endif
