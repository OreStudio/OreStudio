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
#ifndef ORES_UTILITY_CODEC_DURATION_FORM_HPP
#define ORES_UTILITY_CODEC_DURATION_FORM_HPP

#include "ores.utility/codec/member_count.hpp"
#include "ores.utility/codec/wire_fields.hpp"
#include <chrono>
#include <cstdint>
#include <ratio>
#include <stdexcept>
#include <string>
#include <tuple>
#include <type_traits>

namespace ores::utility::codec {

/**
 * @brief A duration on the wire: a count and the name of its unit.
 *
 * This is the object reflect-cpp writes for a std::chrono::duration, so a
 * configuration dump and a message read the same way after the move. Only the
 * units reflect-cpp names are accepted.
 */
struct duration_form {
    std::int64_t count = 0;
    std::string unit;
};

[[nodiscard]] constexpr auto wire_fields(const duration_form&) {
    return std::tuple{field{"count", &duration_form::count}, field{"unit", &duration_form::unit}};
}

static_assert(::ores::utility::codec::names_every_member<duration_form>(),
              "duration_form has a member its wire shape does not name");

namespace detail {

template <typename Period>
constexpr const char* unit_name() {
    if constexpr (std::is_same_v<Period, std::nano>)
        return "nanoseconds";
    else if constexpr (std::is_same_v<Period, std::micro>)
        return "microseconds";
    else if constexpr (std::is_same_v<Period, std::milli>)
        return "milliseconds";
    else if constexpr (std::is_same_v<Period, std::ratio<1>>)
        return "seconds";
    else if constexpr (std::is_same_v<Period, std::ratio<60>>)
        return "minutes";
    else if constexpr (std::is_same_v<Period, std::ratio<3600>>)
        return "hours";
    else
        static_assert(sizeof(Period) == 0, "This duration has no unit the wire names");
}

}

template <typename Rep, typename Period>
duration_form to_duration_form(const std::chrono::duration<Rep, Period>& d) {
    return {static_cast<std::int64_t>(d.count()), detail::unit_name<Period>()};
}

template <typename Duration>
Duration from_duration_form(const duration_form& f) {
    using namespace std::chrono;
    if (f.unit == "nanoseconds")
        return duration_cast<Duration>(nanoseconds(f.count));
    if (f.unit == "microseconds")
        return duration_cast<Duration>(microseconds(f.count));
    if (f.unit == "milliseconds")
        return duration_cast<Duration>(milliseconds(f.count));
    if (f.unit == "seconds")
        return duration_cast<Duration>(seconds(f.count));
    if (f.unit == "minutes")
        return duration_cast<Duration>(minutes(f.count));
    if (f.unit == "hours")
        return duration_cast<Duration>(hours(f.count));
    throw std::invalid_argument("Not a duration: '" + f.unit + "' is not a unit the wire names");
}

}

#endif
