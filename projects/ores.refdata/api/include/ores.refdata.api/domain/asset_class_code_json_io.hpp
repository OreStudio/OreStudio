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
#ifndef ORES_REFDATA_API_DOMAIN_ASSET_CLASS_CODE_JSON_IO_HPP
#define ORES_REFDATA_API_DOMAIN_ASSET_CLASS_CODE_JSON_IO_HPP

#include "ores.refdata.api/domain/asset_class_code.hpp"
#include "ores.refdata.api/export.hpp"
#include <iosfwd>

namespace ores::refdata::domain {

/**
 * @brief Dumps the asset_class_code to a stream in JSON format.
 */
ORES_REFDATA_API_EXPORT std::ostream& operator<<(std::ostream& s, const asset_class_code& v);

}

#endif
