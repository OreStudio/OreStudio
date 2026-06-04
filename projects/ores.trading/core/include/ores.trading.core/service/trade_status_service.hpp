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
#ifndef ORES_TRADING_SERVICE_TRADE_STATUS_SERVICE_HPP
#define ORES_TRADING_SERVICE_TRADE_STATUS_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.dq.api/domain/fsm_transition.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.trading.core/export.hpp"
#include <boost/container_hash/hash.hpp>
#include <boost/uuid/uuid.hpp>
#include <optional>
#include <string>
#include <unordered_map>

namespace ores::trading::service {

/**
 * @brief Lookup map of FSM transitions keyed by their UUID.
 *
 * Fetched once per request from the DQ service via NATS and passed into
 * resolve_status() to avoid a cross-service DB read.
 */
using fsm_transition_map = std::unordered_map<boost::uuids::uuid,
                                              ores::dq::domain::fsm_transition,
                                              boost::hash<boost::uuids::uuid>>;

/**
 * @brief Resolves trade status transitions via the FSM.
 *
 * Given an activity_type_code and the trade's current status_id, determines
 * the resulting status_id by looking up the FSM transition linked to the
 * activity type.  Throws std::logic_error if the transition is not allowed
 * from the current status.
 */
class ORES_TRADING_CORE_EXPORT trade_status_service {
private:
    inline static std::string_view logger_name = "ores.trading.service.trade_status_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Resolves the new status_id for a trade.
     *
     * Given an activity_type_code, the trade's current status_id
     * (std::nullopt for brand-new trades), and a pre-fetched transitions
     * map (keyed by UUID, retrieved once per batch from the DQ service
     * via NATS), returns the resulting status_id after applying the FSM
     * transition linked to the activity type.
     *
     * If the activity type has no linked FSM transition, the current
     * status_id is returned unchanged (nil UUID for new trades).
     *
     * @throws std::invalid_argument if the activity_type_code is unknown.
     * @throws std::logic_error if the transition is invalid from the
     *         current status, or if the transition id is not in the map.
     */
    static boost::uuids::uuid resolve_status(context ctx,
                                             const std::string& activity_type_code,
                                             std::optional<boost::uuids::uuid> current_status_id,
                                             const fsm_transition_map& transitions);
};

}

#endif
