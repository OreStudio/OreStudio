/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2024 Marco Craveiro <marco.craveiro@gmail.com>
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
#ifndef ORES_CONSOLE_CONFIGURATION_HPP
#define ORES_CONSOLE_CONFIGURATION_HPP

#if defined(_MSC_VER) && (_MSC_VER >= 1200)
#pragma once
#endif

#include <iosfwd>
#include <algorithm>
#include <optional>
#include "ores.utility/log/logging_configuration.hpp"
#include "ores.console/importing_configuration.hpp"

namespace ores::console {

/**
 * @brief All of the configuration required by the command line application.
 */
class configuration final {
public:
    configuration() = default;
    configuration(const configuration&) = default;
    ~configuration() = default;
    configuration(configuration&& rhs) noexcept;
    configuration(
        std::optional<ores::utility::log::logging_configuration> logging,
        std::optional<importing_configuration> importing);

    /**
     * @brief Configuration related to logging, if any.
     */
    /**@{*/
    std::optional<ores::utility::log::logging_configuration> logging() const;
    void logging(std::optional<ores::utility::log::logging_configuration> v);
    /**@}*/

    /**
     * @brief Configuration related to importing, if any.
     */
    /**@{*/
    std::optional<importing_configuration> importing() const;
    void importing(std::optional<importing_configuration> v);
    /**@}*/

    void swap(configuration& other) noexcept;
    configuration& operator=(configuration other);

private:
    std::optional<ores::utility::log::logging_configuration> logging_;
    std::optional<importing_configuration> importing_;
};

std::ostream& operator<<(std::ostream& s, const configuration& v);

}

namespace std {

template<>
inline void swap(
    ores::console::configuration& lhs, ores::console::configuration& rhs) {
    lhs.swap(rhs);
}

}

#endif
