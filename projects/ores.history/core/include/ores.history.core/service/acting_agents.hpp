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
#ifndef ORES_HISTORY_CORE_SERVICE_ACTING_AGENTS_HPP
#define ORES_HISTORY_CORE_SERVICE_ACTING_AGENTS_HPP

#include "ores.database/domain/context_fwd.hpp"
#include "ores.history.api/messaging/history_protocol.hpp"
#include "ores.history.core/export.hpp"
#include <string>
#include <vector>

namespace ores::history::service {

/**
 * @brief The table a record's versions live in, and the key that finds them.
 *
 * The names come from the model at generation time, never from a request.
 */
struct versioned_table final {
    std::string table;
    std::string key_column;
    bool key_is_uuid = false;
};

/**
 * @brief Marks each version an LLM agent wrote with the agent.
 *
 * A row's acting_agent is stamped by its insert trigger from the agent the
 * call was made by, and a version's recorded_at is its row's valid_from, so the
 * two are joined on that instant for the record's own key. A version a person
 * wrote keeps an empty acting agent. A failed read leaves the history as it
 * was: the mark is an addition to it, not a condition of it.
 */
ORES_HISTORY_CORE_EXPORT void
mark_acting_agents(const ores::database::context& ctx,
                   const versioned_table& source,
                   const std::string& key,
                   std::vector<messaging::entity_history_version>& versions);

}

#endif
