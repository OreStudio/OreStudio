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
#ifndef ORES_MARKETDATA_API_DOMAIN_MARKET_CLOCK_HPP
#define ORES_MARKETDATA_API_DOMAIN_MARKET_CLOCK_HPP

#if defined(_MSC_VER)
#    pragma once
#endif

#include <chrono>
#include <format>
#include <string>

namespace ores::marketdata::domain {

/**
 * @brief The date that the feeds of market data read as today.
 *
 * Every feed asks one clock for the date its start value is read for, so two feeds never start
 * from two different days. The clock gives a calendar date in UTC. The system clock gives the
 * date of the host. A fixed clock gives a date that a test or a replay chose, so a run is
 * repeatable.
 *
 * The clock is for market data first. A clock that the whole system shares is the idea of the
 * backlog capture "A system clock drives the date the feeds read", and this class is the
 * interface that clock would implement.
 */
class market_clock {
public:
    virtual ~market_clock() = default;

    /// The date that feeds read as today.
    [[nodiscard]] virtual std::chrono::sys_days today() const = 0;

    /// The date as "2026-10-23", the form of a vintage date.
    [[nodiscard]] std::string today_text() const {
        return std::format("{:%F}", today());
    }
};

/// The date of the host, in UTC.
class system_market_clock final : public market_clock {
public:
    [[nodiscard]] std::chrono::sys_days today() const override {
        return std::chrono::floor<std::chrono::days>(std::chrono::system_clock::now());
    }
};

/// A date that never changes, for a test or a replay.
class fixed_market_clock final : public market_clock {
public:
    explicit fixed_market_clock(std::chrono::sys_days day)
        : day_(day) {}

    [[nodiscard]] std::chrono::sys_days today() const override {
        return day_;
    }

private:
    std::chrono::sys_days day_;
};

/**
 * @brief The date a feed starts from.
 *
 * A config that names a date keeps it. A config that names none starts from the date of the
 * clock, which every feed shares.
 */
[[nodiscard]] inline std::string start_date(const std::string& configured,
                                            const market_clock& clock) {
    return configured.empty() ? clock.today_text() : configured;
}

/// The same, for a clock that may be absent: no clock (a null pointer) means the system clock.
[[nodiscard]] inline std::string start_date(const std::string& configured,
                                            const market_clock* clock) {
    return clock ? start_date(configured, *clock) : start_date(configured, system_market_clock{});
}

}

#endif
