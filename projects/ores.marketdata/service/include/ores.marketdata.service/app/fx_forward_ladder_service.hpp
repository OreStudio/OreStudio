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
#ifndef ORES_MARKETDATA_SERVICE_APP_FX_FORWARD_LADDER_SERVICE_HPP
#define ORES_MARKETDATA_SERVICE_APP_FX_FORWARD_LADDER_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.marketdata.api/messaging/fx_forward_operation_protocol.hpp"
#include "ores.marketdata.service/export.hpp"
#include <chrono>
#include <string>
#include <vector>

namespace ores::marketdata::service::app {

/**
 * @brief Answers a pair's FX forward ladder from its spot, its conventions and the published
 * funding curve of each currency.
 *
 * Reads only. A missing input is a reply with success false and a message that names it.
 */
/// What a forward set is computed for: a pair, the instant it reads the market at, and the
/// party whose spot and curves it reads.
struct fx_forward_inputs final {
    std::string pair_code;
    std::chrono::system_clock::time_point as_of;
    std::string party_id;
};

/// A pair's forward ladder and the convention figures it was quoted with.
struct fx_forward_ladder final {
    std::string pair_code;
    std::chrono::system_clock::time_point as_of;
    double spot = 0.0;
    std::chrono::system_clock::time_point spot_observed_at;
    std::chrono::year_month_day spot_date{};
    int spot_days = 0;
    double pip_factor = 0.0;
    int decimal_places = 0;
    ores::marketdata::messaging::fx_forward_curve_source base_curve;
    ores::marketdata::messaging::fx_forward_curve_source quote_curve;
    std::vector<ores::marketdata::messaging::fx_forward_ladder_row> rows;
};

/// A ladder with where each input came from, as a publish records it.
struct computed_fx_forward_set final {
    fx_forward_ladder ladder;
    /// The spots read: one, its own, for a USD pair; the two USD legs for a cross.
    std::vector<ores::marketdata::messaging::fx_forward_spot_source> spots;
    int convention_version = 0;
    /// The calendars the dates roll on, comma separated, in the convention's order.
    std::string calendar_codes;
};

/**
 * @brief Computes a pair's forward ladder for a publish.
 *
 * No screen reads this: a forward is shown from a published set, so a number on a screen is
 * always one a person approved.
 */
class ORES_MARKETDATA_SERVICE_EXPORT fx_forward_ladder_service final {
public:
    /**
     * @brief Computes a pair's ladder and names its inputs, or throws the input that is missing.
     *
     * Inputs that name a party read that party's spot and funding curves only.
     */
    [[nodiscard]] static computed_fx_forward_set compute(ores::database::context ctx,
                                                         const fx_forward_inputs& inputs);
};

}

#endif
