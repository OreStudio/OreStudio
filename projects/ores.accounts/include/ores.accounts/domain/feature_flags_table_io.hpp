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
#ifndef ORES_ACCOUNTS_DOMAIN_FEATURE_FLAGS_TABLE_IO_HPP
#define ORES_ACCOUNTS_DOMAIN_FEATURE_FLAGS_TABLE_IO_HPP

#include <iosfwd>
#include <vector>
#include "ores.accounts/domain/feature_flags.hpp"

namespace ores::accounts::domain {

/**
 * @brief Prints feature flags to a stream in table format.
 */
void print_feature_flags_table(std::ostream& s, const std::vector<feature_flags>& v);

/**
 * @brief Dumps the feature flags object to a stream in table format.
 */
std::ostream& operator<<(std::ostream& s, const std::vector<feature_flags>& v);

}

#endif
