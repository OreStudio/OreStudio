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

#include "ores.refdata.api/domain/book.hpp"
#include <array>
#include <string_view>

namespace ores::refdata::domain::regulatory_book_type_constants {

/**
 * @brief Regulatory book type codes used throughout the system.
 *
 * These codes must match entries in the
 * ores_refdata_regulatory_book_types_tbl table (see
 * refdata_regulatory_book_types_populate.sql). See the FRTB trading
 * book / banking book boundary knowledge note for the Basel III/IV
 * background.
 */
namespace codes {

constexpr std::string_view trading = "Trading";
constexpr std::string_view banking = "Banking";

} // namespace codes

constexpr std::array<std::string_view, 2> all = {
    codes::trading,
    codes::banking,
};

/**
 * @brief Whether a book's regulatory classification is Trading Book.
 */
[[nodiscard]] inline bool is_trading_book(const ores::refdata::domain::book& b) {
    return b.regulatory_book_type == codes::trading;
}

/**
 * @brief Whether a book's regulatory classification is Banking Book.
 */
[[nodiscard]] inline bool is_banking_book(const ores::refdata::domain::book& b) {
    return b.regulatory_book_type == codes::banking;
}

} // namespace ores::refdata::domain::regulatory_book_type_constants

#endif
