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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#ifndef ORES_TRADING_SCRIPTING_ORE_ORE_COMPAT_HPP
#define ORES_TRADING_SCRIPTING_ORE_ORE_COMPAT_HPP

#include <cstddef>
#include <limits>
#include <memory>
#include <sstream>
#include <stdexcept>
#include <string>

/**
 * @file ore_compat.hpp
 * @brief The few QuantLib and ORE utilities the copied scripting code uses,
 * stated with the standard library, so that the copy links no QuantLib.
 *
 * The import script replaces each QuantLib name in ORE's sources with the name
 * this header gives it, and changes nothing else, so the copy stays close to
 * ORE's text.
 */
namespace ores::trading::scripting {

using Size = std::size_t;
using Real = double;

/**
 * @brief QuantLib's null value: the largest value of the type, which a
 * comparison reads as "no value".
 */
template <class T>
class Null {
public:
    operator T() const {
        return std::numeric_limits<T>::max();
    }
};

/**
 * @brief The error the copied code raises where ORE raises a QuantLib error.
 */
class script_error : public std::runtime_error {
public:
    using std::runtime_error::runtime_error;
};

/**
 * @brief ORE's streaming conversion to text.
 */
template <class T>
std::string to_string(const T& value) {
    std::ostringstream out;
    out << value;
    return out.str();
}

}

#define ORES_SCRIPTING_FAIL(message)                                                   \
    do {                                                                               \
        std::ostringstream ores_scripting_message_;                                    \
        ores_scripting_message_ << message;                                            \
        throw ::ores::trading::scripting::script_error(ores_scripting_message_.str()); \
    } while (false)

#define ORES_SCRIPTING_REQUIRE(condition, message) \
    do {                                           \
        if (!(condition))                          \
            ORES_SCRIPTING_FAIL(message);          \
    } while (false)

#endif
