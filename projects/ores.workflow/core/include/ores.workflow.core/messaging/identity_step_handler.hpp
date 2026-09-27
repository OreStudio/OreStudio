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
#ifndef ORES_WORKFLOW_CORE_MESSAGING_IDENTITY_STEP_HANDLER_HPP
#define ORES_WORKFLOW_CORE_MESSAGING_IDENTITY_STEP_HANDLER_HPP

#include "ores.nats/service/client.hpp"
#include "ores.nats/service/subscription.hpp"
#include "ores.workflow.core/export.hpp"
#include <string_view>
#include <vector>

namespace ores::workflow::messaging {

/**
 * @brief Serves the identity workflow's step and compensation commands.
 *
 * A commissioner's step commands are served by the commissioner, because the work
 * is theirs. The identity workflow's are served here instead, because its work is
 * to report an outcome -- so the component can run a workflow end to end without
 * any other component being present, which is what makes the engine exercisable
 * on its own.
 *
 * Each command carries the outcome it wants, so this handler reads rather than
 * decides. It answers through the shared workflow_step_context helper, which
 * means the identity fixture exercises the same reply path a real step does.
 */
[[nodiscard]] ORES_WORKFLOW_CORE_EXPORT std::vector<ores::nats::service::subscription>
register_identity_step_handlers(ores::nats::service::client& nats, std::string_view queue_group);

}

#endif
