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
#ifndef ORES_IAM_MESSAGING_REPORT_SCHEDULE_SELECTION_HPP
#define ORES_IAM_MESSAGING_REPORT_SCHEDULE_SELECTION_HPP

#include "ores.scheduler.api/domain/cron_expression.hpp"
#include <cstdint>
#include <string>
#include <string_view>
#include <vector>

namespace ores::iam::messaging {

/**
 * @brief The schedule a report definition carries when it is not scheduled.
 *
 * A definition holds a cron expression whatever its owner intends, so a report
 * that is only ever asked for by hand states one that no calendar reaches: the
 * thirty-first of February. The catalogue reads it as "on demand", and so does
 * the step that schedules the catalogue, which must not turn a report a person
 * asks for into one that fires by itself.
 */
inline constexpr std::string_view on_demand_schedule_expression = "0 0 31 2 *";

/// Whether a definition's schedule is a cron the calendar reaches, which is the
/// only kind of schedule a step may put on the scheduler.
[[nodiscard]] inline bool is_schedulable_schedule(std::string_view schedule_expression) {
    if (schedule_expression == on_demand_schedule_expression)
        return false;
    return ores::scheduler::domain::cron_expression::from_string(schedule_expression).has_value();
}

/**
 * @brief One definition a step may schedule, as the step reads it.
 *
 * The step reads a party's definitions and states each one's identity, its
 * schedule and whether the definition already holds a scheduler job. Keeping
 * those three facts apart from the message that carries them is what lets the
 * selection be decided, and tested, without a server.
 */
struct report_schedule_candidate {
    std::string id;
    std::string schedule_expression;
    /// Whether the definition already names the job that fires it.
    bool holds_scheduler_job = false;
};

/// What one party's definitions came to, and what the step asks for.
struct report_schedule_selection {
    /// The definitions to hand to the scheduling operation, in the order read.
    std::vector<std::string> ids;
    /// Definitions that already hold a job. A re-provision finds them all here,
    /// so it asks for nothing and creates no second job or grant.
    std::uint32_t already_scheduled = 0;
    /// Definitions that hold the on-demand placeholder, which no person asked
    /// to schedule.
    std::uint32_t on_demand = 0;
    /// Definitions whose schedule is not a cron that parses. They cannot be
    /// scheduled at all, so they are passed over rather than failing the run.
    std::uint32_t not_a_cron = 0;
};

/**
 * @brief Selects the definitions one party's schedule step asks the reporting
 * operation to schedule.
 *
 * Three rules, in this order: a definition that already holds a job is left
 * alone, because scheduling is idempotent and a second provision must not mint
 * a second job or a second grant; a definition whose schedule is the on-demand
 * placeholder is left to be asked for by hand; and a schedule that is not a
 * cron is passed over, because the operation would refuse it and a tenant whose
 * catalogue holds one bad expression should still come up.
 */
[[nodiscard]] inline report_schedule_selection
select_report_definitions_to_schedule(const std::vector<report_schedule_candidate>& candidates) {
    report_schedule_selection selection;
    selection.ids.reserve(candidates.size());
    for (const auto& candidate : candidates) {
        if (candidate.holds_scheduler_job) {
            ++selection.already_scheduled;
            continue;
        }
        if (candidate.schedule_expression == on_demand_schedule_expression) {
            ++selection.on_demand;
            continue;
        }
        if (!is_schedulable_schedule(candidate.schedule_expression)) {
            ++selection.not_a_cron;
            continue;
        }
        selection.ids.push_back(candidate.id);
    }
    return selection;
}

}

#endif
