/* -*- mode: c++; tab-width: 4; c-indent-level: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
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
#ifndef ORES_MARKETDATA_SERVICE_APP_STALENESS_POLICY_HPP
#define ORES_MARKETDATA_SERVICE_APP_STALENESS_POLICY_HPP

#include "ores.marketdata.service/export.hpp"
#include <chrono>
#include <cstdint>

namespace ores::marketdata::service::app {

/**
 * @brief The one place the server decides what "stale" means.
 *
 * Every read that states a verdict asks here, so two clients cannot disagree
 * about the same value: the threshold is the server's configuration, not a
 * constant a client applies.
 *
 * The rule, stated once:
 * - `age_seconds = max(0, now - as_of)` in whole seconds;
 * - `stale = age_seconds > threshold`;
 * - a value with no instant is stale.
 *
 * The threshold is read from =ORES_MARKETDATA_STALE_SECONDS= on first use and
 * defaults to an hour. It moves into the service's options, beside the other
 * transport and storage settings, when that struct is next touched.
 *
 * The *decision* is not restated here: the rate engine evaluates ages with
 * =ores::analytics::quant::domain::staleness_policy=, and this class is where a
 * market-data read gets the same threshold from. One number, one verdict.
 */
class ORES_MARKETDATA_SERVICE_EXPORT staleness_policy final {
public:
    /// The threshold in whole seconds.
    [[nodiscard]] static std::int64_t threshold_seconds();

    /// Whether the threshold came from the environment rather than the default.
    [[nodiscard]] static bool is_configured();

    /// Replaces the threshold. For a test, or for a caller that read its options.
    static void configure(std::int64_t seconds);

    /// =max(0, now - as_of)= in whole seconds; zero for a value with no instant.
    [[nodiscard]] static std::int64_t age_seconds(std::chrono::system_clock::time_point as_of,
                                                  std::chrono::system_clock::time_point now);

    /// The verdict: a value with no instant is stale, and so is one past the threshold.
    [[nodiscard]] static bool is_stale(std::chrono::system_clock::time_point as_of,
                                       std::chrono::system_clock::time_point now);
};

}

#endif
