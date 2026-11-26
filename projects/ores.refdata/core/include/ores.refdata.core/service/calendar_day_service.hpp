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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51 Franklin
 * Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#ifndef ORES_REFDATA_CORE_SERVICE_CALENDAR_DAY_SERVICE_HPP
#define ORES_REFDATA_CORE_SERVICE_CALENDAR_DAY_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/logger.hpp"
#include "ores.refdata.api/domain/calendar_date.hpp"
#include "ores.refdata.core/export.hpp"
#include <string>
#include <vector>

namespace ores::refdata::service {

/**
 * @brief The days of a calendar as the caller's party answers them.
 *
 * The days are materialised once, at the level that owns the calendar: the
 * installation's calendars in the system tenant, and a tenant's own calendar in
 * its tenant. That copy is the installation's answer to the calendar, and it
 * holds no party's exceptions.
 *
 * A party's exceptions are the part of the answer the party states itself, so
 * this read applies them over the stored days: the installation's exceptions
 * first, and the party's own after them, so the party's row decides a day both
 * of them name. A caller that acts in no party reads the installation's answer,
 * which is what a service account needs.
 *
 * An exception that names a day outside the materialised window is not applied,
 * because there is no stored day to move. The materialisation owns the window.
 */
class ORES_REFDATA_CORE_EXPORT calendar_day_service {
private:
    inline static std::string_view logger_name = "ores.refdata.service.calendar_day_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    explicit calendar_day_service(const context& ctx);

    /**
     * @brief Reads the days of one calendar, with the party's exceptions applied.
     *
     * The read admits the caller's tenant's rows and the system tenant's, so a
     * tenant that authors a calendar of a shared code reads its own days for it.
     * Where both levels hold a day, the caller's own row wins.
     *
     * @param calendar_code The calendar whose days are read.
     * @return The days in date order; empty when the calendar was never materialised.
     */
    [[nodiscard]] std::vector<domain::calendar_date> days(const std::string& calendar_code) const;

private:
    context ctx_;
};

}

#endif
