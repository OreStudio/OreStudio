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

#ifndef ORES_REFDATA_API_DOMAIN_REGULATORY_BOOK_TYPE_CONSTANTS_HPP
#define ORES_REFDATA_API_DOMAIN_REGULATORY_BOOK_TYPE_CONSTANTS_HPP

#include <string_view>

namespace ores::refdata::domain::regulatory_book_type_constants {

/**
 * @brief The regulatory book type code the code compares.
 *
 * The full vocabulary lives in the ores_refdata_regulatory_book_types_tbl
 * table, which refdata_regulatory_book_types_populate.sql seeds. This holds
 * only the code a caller has to name. See the FRTB trading book / banking
 * book boundary knowledge note for the Basel III/IV background.
 */
namespace codes {

constexpr std::string_view trading = "Trading";

} // namespace codes

} // namespace ores::refdata::domain::regulatory_book_type_constants

#endif
