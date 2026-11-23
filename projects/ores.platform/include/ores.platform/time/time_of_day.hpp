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
#ifndef ORES_PLATFORM_TIME_TIME_OF_DAY_HPP
#define ORES_PLATFORM_TIME_TIME_OF_DAY_HPP

#include "ores.platform/export.hpp"
#include <chrono>
#include <compare>
#include <iosfwd>

namespace ores::platform::time {

/**
 * @brief A time of day on a wall clock, without its zone.
 *
 * The time is held as whole seconds since midnight and is always a valid time
 * of day: from 00:00:00 to 23:59:59. A zone is not part of the value. A column
 * that holds a time of day names its zone in a column of its own, because the
 * offset of a zone changes with daylight saving and a fixed offset cannot
 * follow it.
 *
 * The type is distinct from std::chrono::seconds so that a time of day cannot
 * be passed where a duration is meant, such as a timeout.
 */
class ORES_PLATFORM_EXPORT time_of_day final {
public:
    /**
     * @brief Midnight.
     */
    constexpr time_of_day() noexcept = default;

    /**
     * @brief The time of day @p since_midnight after midnight.
     *
     * @throws std::out_of_range if @p since_midnight is negative or is a day
     *         or more.
     */
    explicit time_of_day(std::chrono::seconds since_midnight);

    /**
     * @brief The time of day @p h, @p m and @p s after midnight.
     *
     * @throws std::out_of_range if the sum is not a time of day.
     */
    time_of_day(std::chrono::hours h, std::chrono::minutes m, std::chrono::seconds s = {});

    /**
     * @brief The seconds since midnight.
     */
    constexpr std::chrono::seconds since_midnight() const noexcept {
        return since_midnight_;
    }

    /**
     * @brief The hours, minutes and seconds, for display and for text.
     */
    std::chrono::hh_mm_ss<std::chrono::seconds> hms() const noexcept {
        return std::chrono::hh_mm_ss<std::chrono::seconds>(since_midnight_);
    }

    auto operator<=>(const time_of_day&) const = default;

private:
    std::chrono::seconds since_midnight_{0};
};

/**
 * @brief Writes the time as HH:MM:SS, the form the text and wire codecs carry.
 */
ORES_PLATFORM_EXPORT std::ostream& operator<<(std::ostream& s, const time_of_day& t);

}

#endif
