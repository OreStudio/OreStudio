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
#ifndef ORES_IAM_DOMAIN_CHANGE_REASON_CATEGORY_TABLE_IO_HPP
#define ORES_IAM_DOMAIN_CHANGE_REASON_CATEGORY_TABLE_IO_HPP

#include <iosfwd>
#include <vector>
#include "ores.iam/domain/change_reason_category.hpp"

namespace ores::iam::domain {

/**
 * @brief Dumps the change_reason_category objects to a stream in table format.
 */
std::ostream& operator<<(std::ostream& s, const std::vector<change_reason_category>& v);

}

#endif
