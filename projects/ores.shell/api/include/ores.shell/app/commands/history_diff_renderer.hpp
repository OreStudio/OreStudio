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
#ifndef ORES_SHELL_APP_COMMANDS_HISTORY_DIFF_RENDERER_HPP
#define ORES_SHELL_APP_COMMANDS_HISTORY_DIFF_RENDERER_HPP

#include "ores.nats/service/nats_client.hpp"
#include <optional>
#include <ostream>
#include <string>
#include <string_view>

namespace ores::shell::app::commands {

/**
 * @brief Renders a single version transition of one entity as a unified
 * diff, over the generic history.v1.get subject.
 *
 * Prints one transition: unchanged fields as context lines and the
 * server-computed changes as -/+ pairs, or (for the oldest version)
 * every field as an addition from /dev/null. All comparison happens
 * server-side (ores.history's dispatch_registry + per-entity
 * history_provider); this is pure formatting of an already-diffed
 * response, shared by every entity's "history --diff" command.
 *
 * @param out         Output stream for the rendered diff.
 * @param session     Client session for connectivity.
 * @param entity_type The entity_type_of() dispatch key (e.g. "ores.refdata.currency").
 * @param entity_id   The entity's own primary key rendered as a string.
 * @param version     The version to diff against its immediate predecessor.
 *                    Defaults to the latest version when not given.
 */
void render_history_diff(std::ostream& out,
                         ores::nats::service::nats_client& session,
                         std::string_view entity_type,
                         std::string entity_id,
                         std::optional<int> version = std::nullopt);

}

#endif
