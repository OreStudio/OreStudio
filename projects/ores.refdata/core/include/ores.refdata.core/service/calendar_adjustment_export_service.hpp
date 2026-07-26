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
#ifndef ORES_REFDATA_CORE_SERVICE_CALENDAR_ADJUSTMENT_EXPORT_SERVICE_HPP
#define ORES_REFDATA_CORE_SERVICE_CALENDAR_ADJUSTMENT_EXPORT_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/domain/calendar_adjustment.hpp"
#include "ores.refdata.core/export.hpp"
#include <string>
#include <vector>

namespace ores::refdata::service {

/**
 * @brief Assembles the transient calendar_adjustment DTOs ORE needs for
 * source='user' calendar templates -- never itself persisted (see the
 * parent task's Decisions: calendar_adjustment is kept purely as an
 * export-time shape, built from calendar + calendar_exception rows).
 *
 * source='quantlib' calendars need no entry at all: ORE resolves them
 * natively via its own linked QuantLib, so a bare <Calendar> code
 * reference already is the notification (see the task's ORE-export
 * decision). Only source='user' calendars -- and any source='user'
 * ancestors reached by following base_calendar_code -- get an entry.
 * A source='quantlib' base is never itself exported; ORE resolves the
 * BaseCalendar attribute against its own native name directly.
 */
class ORES_REFDATA_CORE_EXPORT calendar_adjustment_export_service {
private:
    inline static std::string_view logger_name =
        "ores.refdata.service.calendar_adjustment_export_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    explicit calendar_adjustment_export_service(context ctx);

    /**
     * @brief Assembles calendar_adjustment DTOs for @p calendar_codes
     * (or every active source='user' calendar, if empty), in base-first
     * order so ORE can resolve each BaseCalendar reference against an
     * already-defined earlier <Calendar> entry in the same file.
     *
     * A requested code with source='quantlib' is silently skipped, not
     * an error -- callers scanning a set of referenced calendar codes
     * (e.g. from a curve config) don't need to know in advance which
     * ones are QuantLib-native versus user-authored.
     *
     * @throws std::runtime_error if a requested calendar_code does not
     * exist, or its base_calendar_code chain has a cycle.
     */
    std::vector<domain::calendar_adjustment>
    assemble(const std::vector<std::string>& calendar_codes = {});

private:
    context ctx_;
};

}

#endif
