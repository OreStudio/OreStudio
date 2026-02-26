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
#ifndef ORES_SCHEDULER_RFL_REFLECTORS_HPP
#define ORES_SCHEDULER_RFL_REFLECTORS_HPP

#include <rfl.hpp>
#include "ores.scheduler/domain/cron_expression.hpp"
#include "ores.scheduler/domain/job_status.hpp"

namespace rfl {

/**
 * @brief Custom reflector for ores::scheduler::domain::cron_expression.
 *
 * Serializes cron_expression as its string representation.
 * Deserializes by parsing through cron_expression::from_string().
 */
template<>
struct Reflector<ores::scheduler::domain::cron_expression> {
    using ReflType = std::string;

    static ores::scheduler::domain::cron_expression to(const ReflType& str) {
        auto result = ores::scheduler::domain::cron_expression::from_string(str);
        if (!result) {
            throw std::runtime_error("Invalid cron expression: " + result.error());
        }
        return *result;
    }

    static ReflType from(const ores::scheduler::domain::cron_expression& v) {
        return v.to_string();
    }
};

/**
 * @brief Custom reflector for ores::scheduler::domain::job_status.
 *
 * Serializes as string to avoid enum reflection issues.
 */
template<>
struct Reflector<ores::scheduler::domain::job_status> {
    using ReflType = std::string;

    static ores::scheduler::domain::job_status to(const ReflType& str) {
        if (str == "starting") return ores::scheduler::domain::job_status::starting;
        if (str == "succeeded") return ores::scheduler::domain::job_status::succeeded;
        if (str == "failed") return ores::scheduler::domain::job_status::failed;
        throw std::runtime_error("Invalid job_status: " + str);
    }

    static ReflType from(const ores::scheduler::domain::job_status& v) {
        switch (v) {
        case ores::scheduler::domain::job_status::starting:  return "starting";
        case ores::scheduler::domain::job_status::succeeded: return "succeeded";
        case ores::scheduler::domain::job_status::failed:    return "failed";
        }
        throw std::logic_error("Unhandled ores::scheduler::domain::job_status enum value.");
    }
};

}

#endif
