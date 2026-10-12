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
#ifndef ORES_MARKETDATA_CORE_SERVICE_MANUAL_POINT_SERVICE_HPP
#define ORES_MARKETDATA_CORE_SERVICE_MANUAL_POINT_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.marketdata.api/messaging/operations_protocol.hpp"
#include "ores.marketdata.core/export.hpp"
#include <chrono>

namespace ores::marketdata::service {

/**
 * @brief Keys and clears a manual point on a node of a composite object.
 *
 * The object is named by its typed identity and the node by its coordinates, so
 * no caller builds an oresmd URI. The node's URI is composed here through the
 * codec, from the series' own URI and the coordinates, after the coordinates are
 * checked against the shape the object declares. A node the shape does not hold is
 * refused and nothing is written.
 *
 * A caller may write only to a series its party owns: the observation takes the
 * context's party, so a context for another party is refused.
 *
 * A request the service cannot act on, such as an unknown object, a node outside
 * the shape or a value that is no number, is answered with success false and a
 * message. A stated identity the codec cannot spell is an invalid argument.
 */
class ORES_MARKETDATA_CORE_EXPORT manual_point_service final {
public:
    /**
     * @brief Keys a manual point on the node, stamped at @p now.
     *
     * @param ctx Database context, tenant- and party-scoped by the caller.
     * @throws std::invalid_argument when the identity is not one the codec can
     * spell.
     */
    [[nodiscard]] static messaging::key_manual_point_response
    key(ores::database::context ctx,
        const messaging::key_manual_point_request& request,
        std::chrono::system_clock::time_point now = std::chrono::system_clock::now());

    /**
     * @brief Clears every manual point the node holds.
     *
     * @throws std::invalid_argument when the identity is not one the codec can
     * spell.
     */
    [[nodiscard]] static messaging::clear_manual_point_response
    clear(ores::database::context ctx, const messaging::clear_manual_point_request& request);
};

}

#endif
