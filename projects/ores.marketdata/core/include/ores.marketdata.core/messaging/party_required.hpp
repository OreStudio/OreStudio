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
#ifndef ORES_MARKETDATA_CORE_MESSAGING_PARTY_REQUIRED_HPP
#define ORES_MARKETDATA_CORE_MESSAGING_PARTY_REQUIRED_HPP

#include "ores.database/domain/context.hpp"
#include "ores.database/domain/outcome_code.hpp"
#include <string>

namespace ores::marketdata::messaging {

/**
 * @brief Whether the session of @p ctx names a party.
 *
 * The party policy of the tick table lists the parties a session may see, so a session with no
 * party sees no tick. A read that returns ticks must not answer such a session with a result
 * that looks complete, so its handler refuses first.
 */
[[nodiscard]] inline bool names_a_party(const ores::database::context& ctx) {
    return !ctx.visible_party_ids().empty();
}

/**
 * @brief The sentence of a refusal of a tick read by a session with no party.
 *
 * It is the sentence of the outcome party_required, so every read that refuses says it in the
 * same words, the history call included.
 */
[[nodiscard]] inline std::string party_required_message() {
    return ores::database::domain::refuse(ores::database::domain::outcome_code::party_required,
                                          {.entity = "ticks"})
        .message;
}

}

#endif
