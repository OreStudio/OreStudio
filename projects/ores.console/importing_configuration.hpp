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
#ifndef ORES_CONSOLE_IMPORTING_CONFIGURATION_HPP
#define ORES_CONSOLE_IMPORTING_CONFIGURATION_HPP

#if defined(_MSC_VER) && (_MSC_VER >= 1200)
#pragma once
#endif

#include <iosfwd>
#include <vector>
#include <algorithm>
#include <filesystem>

namespace ores::console {

/**
 * @brief Configuration related to importing data into the system.
 */
class importing_configuration final {
public:
    importing_configuration() = default;
    importing_configuration(const importing_configuration&) = default;
    ~importing_configuration() = default;
    importing_configuration(importing_configuration&& rhs) noexcept;

    /**
     * @brief Currency configuration files to import.
     */
    /**@{*/
    std::vector<std::filesystem::path> currency_configurations() const;
    void currency_configurations(std::vector<std::filesystem::path> v);
    /**@}*/


    bool operator==(const importing_configuration& rhs) const;
    bool operator!=(const importing_configuration& rhs) const {
        return !this->operator==(rhs);
    }

    void swap(importing_configuration& other) noexcept;
    importing_configuration& operator=(importing_configuration other);

private:
    std::vector<std::filesystem::path> currency_configurations_;
};

std::ostream& operator<<(std::ostream& s, const importing_configuration& v);

}

namespace std {

template<>
inline void swap(
    ores::console::importing_configuration& lhs,
    ores::console::importing_configuration& rhs) {
    lhs.swap(rhs);
}

}

#endif
