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
#include "ores.refdata.core/service/calendar_adjustment_export_service.hpp"
#include "ores.platform/time/datetime.hpp"
#include "ores.refdata.core/repository/calendar_exception_repository.hpp"
#include "ores.refdata.core/repository/calendar_repository.hpp"
#include <functional>
#include <stdexcept>
#include <unordered_set>

namespace ores::refdata::service {

using namespace ores::logging;

namespace {

domain::calendar_adjustment
to_adjustment(const domain::calendar& c,
             const std::vector<domain::calendar_exception>& exceptions) {
    domain::calendar_adjustment adj;
    adj.calendar_name = c.code;
    adj.base_calendar = c.base_calendar_code;
    for (const auto& e : exceptions) {
        const auto date_str = ores::platform::time::datetime::to_iso8601_date(e.exception_date);
        if (e.is_business_day)
            adj.additional_business_days.push_back(date_str);
        else
            adj.additional_holidays.push_back(date_str);
    }
    adj.modified_by = "system";
    adj.performed_by = "system";
    adj.change_reason_code = "system.calendar_materialisation";
    adj.change_commentary = "Assembled for ORE export";
    return adj;
}

} // namespace

calendar_adjustment_export_service::calendar_adjustment_export_service(context ctx)
    : ctx_(std::move(ctx)) {}

std::vector<domain::calendar_adjustment>
calendar_adjustment_export_service::assemble(const std::vector<std::string>& calendar_codes) {
    repository::calendar_repository calendar_repo;
    repository::calendar_exception_repository exception_repo;

    std::vector<domain::calendar> roots;
    if (calendar_codes.empty()) {
        for (auto& c : calendar_repo.read_latest(ctx_))
            if (c.source == "user")
                roots.push_back(std::move(c));
    } else {
        for (const auto& code : calendar_codes) {
            auto found = calendar_repo.read_latest(ctx_, code);
            if (found.empty())
                throw std::runtime_error("No such calendar: " + code);
            if (found.front().source == "user")
                roots.push_back(std::move(found.front()));
        }
    }

    // Base-first (post-order) DFS over each root's base_calendar_code
    // chain, deduplicated -- so a chain of user templates based on other
    // user templates always has its BaseCalendar already defined earlier
    // in the assembled list, and a shared base is never emitted twice.
    std::vector<domain::calendar_adjustment> ordered;
    std::unordered_set<std::string> emitted;
    std::unordered_set<std::string> in_progress;

    std::function<void(const domain::calendar&)> visit = [&](const domain::calendar& c) {
        if (emitted.contains(c.code))
            return;
        if (!in_progress.insert(c.code).second)
            throw std::runtime_error("base_calendar_code cycle detected at: " + c.code);

        if (c.base_calendar_code) {
            auto base = calendar_repo.read_latest(ctx_, *c.base_calendar_code);
            if (base.empty())
                throw std::runtime_error("Invalid base_calendar_code: " + *c.base_calendar_code);
            if (base.front().source == "user")
                visit(base.front());
        }

        auto exceptions = exception_repo.read_latest_by_calendar_code(ctx_, c.code, 0, 100000);
        ordered.push_back(to_adjustment(c, exceptions));
        emitted.insert(c.code);
        in_progress.erase(c.code);
    };

    for (const auto& c : roots)
        visit(c);

    return ordered;
}

}
