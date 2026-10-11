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
#ifndef ORES_INBOX_API_WORKFLOW_APPLY_STEP_COMMAND_HPP
#define ORES_INBOX_API_WORKFLOW_APPLY_STEP_COMMAND_HPP

#include <string>
#include <string_view>

namespace ores::inbox::workflow {

/**
 * @brief What every step of an apply workflow is told: the approved request.
 *
 * The owning component reads the request's lines itself. The step needs no
 * more than the request's id, because the lines, the parts and the decisions
 * are the record's own state.
 */
struct apply_step_command {
    std::string request_id;
};

/**
 * @brief The step that tells the person who asked how the apply ended.
 *
 * The inbox serves it for every kind, because telling a person is the inbox's
 * work and not the owning component's.
 */
inline constexpr std::string_view tell_apply_result_subject = "inbox.v1.steps.tell_apply_result";

}

#endif
