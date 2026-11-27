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
#include "ores.refdata.core/service/calendar_day_service.hpp"
#include "ores.refdata.core/repository/calendar_date_repository.hpp"
#include "ores.refdata.core/repository/calendar_exception_repository.hpp"
#include <boost/uuid/uuid.hpp>
#include <map>
#include <optional>
#include <utility>

namespace ores::refdata::service {

calendar_day_service::calendar_day_service(const context& ctx)
    : ctx_(ctx) {}

std::vector<domain::calendar_date>
calendar_day_service::days(const std::string& calendar_code) const {
    ORES_LOG_SEV(lg(), debug) << "Reading calendar days for a party. calendar_code: "
                              << calendar_code;

    const auto own_tenant = ctx_.tenant_id().to_string();

    // One row for a date. The caller's own row wins over the shared one, so a
    // tenant that authors a calendar of a shared code reads its own days.
    std::map<std::chrono::year_month_day, domain::calendar_date> by_date;
    for (const auto& day :
         repository::calendar_date_repository(ctx_).read_latest_by_calendar(calendar_code)) {
        const auto [it, inserted] = by_date.try_emplace(day.date, day);
        if (!inserted && it->second.tenant_id != own_tenant && day.tenant_id == own_tenant)
            it->second = day;
    }

    repository::calendar_exception_repository exceptions_repo;
    const auto exceptions = exceptions_repo.read_for_party(ctx_, calendar_code, ctx_.party_id());

    // The installation's rows first, and the party's own after them, so the
    // party's answer wins on a day both of them name. The read returns both
    // kinds in no stated order, which is why the two passes are separate.
    const auto apply = [&](const std::optional<boost::uuids::uuid>& party) {
        for (const auto& e : exceptions) {
            if (e.party_id != party)
                continue;
            const auto it = by_date.find(e.exception_date);
            if (it != by_date.end())
                it->second.is_business_day = e.is_business_day;
        }
    };
    apply(std::nullopt);
    if (const auto party = ctx_.party_id(); party)
        apply(party);

    std::vector<domain::calendar_date> days;
    days.reserve(by_date.size());
    for (auto& [date, day] : by_date)
        days.push_back(std::move(day));
    return days;
}

}
