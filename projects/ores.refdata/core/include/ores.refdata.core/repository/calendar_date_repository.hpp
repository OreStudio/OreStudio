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
#ifndef ORES_REFDATA_CORE_REPOSITORY_CALENDAR_DATE_REPOSITORY_HPP
#define ORES_REFDATA_CORE_REPOSITORY_CALENDAR_DATE_REPOSITORY_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/domain/calendar_date.hpp"
#include "ores.refdata.core/export.hpp"
#include <sqlgen/postgres.hpp>
#include <string>
#include <vector>

namespace ores::refdata::repository {

/**
 * @brief Reads and writes calendar dates to data storage.
 */
class ORES_REFDATA_CORE_EXPORT calendar_date_repository {
private:
    inline static std::string_view logger_name = "ores.refdata.repository.calendar_date_repository";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    explicit calendar_date_repository(context ctx);

    std::string sql();

    void write(const domain::calendar_date& calendar_date);
    void write(const std::vector<domain::calendar_date>& calendar_dates);

    std::vector<domain::calendar_date> read_latest();
    std::vector<domain::calendar_date> read_latest_by_calendar(const std::string& calendar_code);
    std::vector<domain::calendar_date> read_latest_by_date(const std::string& date);

    void remove(const std::string& calendar_code, const std::string& date);
    void remove_by_calendar(const std::string& calendar_code);

private:
    context ctx_;
};

}

#endif
