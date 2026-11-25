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
#ifndef ORES_SCHEDULER_CORE_SERVICE_PUBLISH_SUBJECT_PLAN_HPP
#define ORES_SCHEDULER_CORE_SERVICE_PUBLISH_SUBJECT_PLAN_HPP

#include "ores.scheduler.core/export.hpp"
#include <array>
#include <string>
#include <string_view>

namespace ores::scheduler::service {

/**
 * @brief The subjects the scheduler publishes a dataset through.
 *
 * The housekeeping data arrives as two datasets, and each names its own
 * subject. They are stated here because two things read them: the registrar,
 * which subscribes, and the test that checks each subject expands through the
 * function the SQL defines.
 */
inline constexpr std::string_view publish_job_definitions_from_dq_subject =
    "scheduler.v1.ops.publish_job_definitions_from_dq";

inline constexpr std::string_view publish_retention_policies_from_dq_subject =
    "scheduler.v1.ops.publish_retention_policies_from_dq";

inline constexpr std::array<std::string_view, 2> publish_from_dq_subjects{
    publish_job_definitions_from_dq_subject, publish_retention_policies_from_dq_subject};

/**
 * @brief The SQL function a publish-from-dq subject expands through.
 *
 * "scheduler.v1.ops.publish_retention_policies_from_dq" expands through
 * "ores_scheduler_publish_retention_policies_from_dq_fn". The spelling matters
 * twice, once as the subject the registrar subscribes and once as the function
 * the handler calls, which is why it is a function with a test rather than a
 * string built at the call site.
 *
 * An empty result means the subject is not one this pattern serves, and the
 * caller must refuse it rather than call a function with no name.
 */
[[nodiscard]] ORES_SCHEDULER_CORE_EXPORT std::string
publish_from_dq_function(std::string_view subject);

}

#endif
