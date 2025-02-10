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
#ifndef ORES_CONSOLE_DUMPING_CONFIGURATION_HPP
#define ORES_CONSOLE_DUMPING_CONFIGURATION_HPP

#if defined(_MSC_VER) && (_MSC_VER >= 1200)
#pragma once
#endif

#include <iosfwd>
#include <algorithm>

namespace ores::console {

/**
 * @brief Configuration related to dumping data from the system.
 */
class dumping_configuration final {
public:
    dumping_configuration() = default;
    dumping_configuration(const dumping_configuration&) = default;
    dumping_configuration(dumping_configuration&&) noexcept = default;
    ~dumping_configuration() = default;

    dumping_configuration& operator=(const dumping_configuration&) = default;
    dumping_configuration& operator=(dumping_configuration&&) noexcept = default;

    /**
     * @brief Whether to dump currency configurations or not.
     */
    /**@{*/
    bool currency_configurations() const {
        return currency_configurations_;
    }
    void currency_configurations(const bool v) {
        currency_configurations_ = v;
    }
    /**@}*/

    bool operator==(const dumping_configuration& rhs) const;
    bool operator!=(const dumping_configuration& rhs) const {
        return !this->operator==(rhs);
    }

    void swap(dumping_configuration& other) noexcept {
        std::swap(currency_configurations_, other.currency_configurations_);
    }

private:
    bool currency_configurations_;
};

std::ostream& operator<<(std::ostream& s, const dumping_configuration& v);

inline void swap(dumping_configuration& lhs, dumping_configuration& rhs) {
    lhs.swap(rhs);
}

}

#endif
