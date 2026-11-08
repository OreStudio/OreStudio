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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#ifndef ORES_MARKETDATA_API_DATUM_MARKET_REFERENCE_HPP
#define ORES_MARKETDATA_API_DATUM_MARKET_REFERENCE_HPP

#include "ores.marketdata.api/datum/market_datum.hpp"
#include "ores.marketdata.api/datum/market_index.hpp"
#include "ores.marketdata.api/export.hpp"
#include <expected>
#include <string>
#include <string_view>
#include <variant>

namespace ores::marketdata::datum {

/**
 * @brief A reference to market data, as an oresmd address.
 *
 * Whatever names market data -- a trade's index, the curve it is discounted
 * on, the credit name it protects -- names it by the oresmd address of what
 * it means: a fixing is an index, a quote or a series is a datum. ORE names
 * are a spelling of the address for one market configuration, written where
 * an ORE document is.
 *
 * Parsing is the only way to get one, so a reference is always an address the
 * grammar admits, and its text is the codec's own spelling of it.
 */
class ORES_MARKETDATA_API_EXPORT market_reference final {
public:
    /// The reference @p uri addresses, or the codec's reason for refusing it.
    [[nodiscard]] static std::expected<market_reference, std::string> parse(std::string_view uri);

    /// The reference to @p index, or why the grammar gives it no address.
    [[nodiscard]] static std::expected<market_reference, std::string> of(market_index index);

    /// The reference to @p datum, or why the grammar gives it no address.
    [[nodiscard]] static std::expected<market_reference, std::string> of(market_datum datum);

    /// The address, as the codec writes it.
    [[nodiscard]] const std::string& uri() const noexcept {
        return uri_;
    }

    /// The index the reference addresses, or nullptr when it addresses a datum.
    [[nodiscard]] const market_index* index() const noexcept {
        return std::get_if<market_index>(&address_);
    }

    /// The datum the reference addresses, or nullptr when it addresses an index.
    [[nodiscard]] const market_datum* datum() const noexcept {
        return std::get_if<market_datum>(&address_);
    }

    friend bool operator==(const market_reference& lhs, const market_reference& rhs) {
        return lhs.uri_ == rhs.uri_;
    }

private:
    market_reference(std::variant<market_index, market_datum> address, std::string uri);

    std::variant<market_index, market_datum> address_;
    std::string uri_;
};

}

#endif
