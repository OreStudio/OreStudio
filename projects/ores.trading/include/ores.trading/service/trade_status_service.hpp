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

#include <optional>
#include <string>
#include <boost/uuid/uuid.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.database/domain/context.hpp"

namespace ores::trading::service {

/**
 * @brief Resolves trade status transitions via the FSM.
 *
 * Given an activity_type_code and the trade's current status_id, determines
 * the resulting status_id by looking up the FSM transition linked to the
 * activity type.  Throws std::logic_error if the transition is not allowed
 * from the current status.
 */
class trade_status_service {
private:
    inline static std::string_view logger_name =
        "ores.trading.service.trade_status_service";

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
     * Given an activity_type_code and the trade's current status_id
     * (std::nullopt for brand-new trades not yet in the database), returns
     * the resulting status_id after applying the FSM transition linked to
     * the activity type.
     *
     * If the activity type has no linked FSM transition, the current
     * status_id is returned unchanged (nil UUID for new trades).
     *
     * @throws std::invalid_argument if the activity_type_code is unknown.
     * @throws std::logic_error if the transition is invalid from the
     *         current status.
     */
    static boost::uuids::uuid resolve_status(
        context ctx,
        const std::string& activity_type_code,
        std::optional<boost::uuids::uuid> current_status_id);
};

}

#endif
