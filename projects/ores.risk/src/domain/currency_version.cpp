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
#include <ostream>
#include "ores.risk/domain/currency_version.hpp"

namespace ores::risk::domain {

std::ostream& operator<<(std::ostream& s, const currency_version& v) {
    s << "currency_version { "
      << "version_number: " << v.version_number << ", "
      << "modified_by: " << v.modified_by << ", "
      << "modified_at: " << v.modified_at << ", "
      << "change_summary: " << v.change_summary << ", "
      << "data: { iso_code: " << v.data.iso_code << ", name: " << v.data.name << " }"
      << " }";
    return s;
}

std::ostream& operator<<(std::ostream& s, const currency_version_history& v) {
    s << "currency_version_history { "
      << "iso_code: " << v.iso_code << ", "
      << "versions: " << v.versions.size() << " versions"
      << " }";
    return s;
}

}
