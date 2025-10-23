/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#ifndef ORES_ACCOUNTS_DOMAIN_FEATURE_FLAGS_HPP
#define ORES_ACCOUNTS_DOMAIN_FEATURE_FLAGS_HPP

#if defined(_MSC_VER) && (_MSC_VER >= 1200)
#pragma once
#endif

#include <string>
#include <iosfwd>

namespace ores::accounts::domain {

/**
 * @brief Represents a feature flag in the domain layer.
 */
struct feature_flags final {
    /**
     * @brief Name of the feature flag, serves as the unique identifier.
     */
    std::string name;

    /**
     * @brief Flag indicating whether the feature is enabled (1) or disabled (0).
     */
    int enabled;

    /**
     * @brief Description of what the feature flag controls.
     */
    std::string description;

    /**
     * @brief Username of the user who last modified this feature flag.
     */
    std::string modified_by;
};

std::ostream& operator<<(std::ostream& s, const feature_flags& v);

}

#endif